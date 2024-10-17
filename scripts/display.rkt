#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require quickscript
         racket/gui/base racket/class racket/match racket/case racket/async-channel
         racket-mupdf
         "../private/help.rkt"
         (for-syntax racket/base))
(provide pdf-display-script)

(define-script pdf-display-script
  #:label "PDF displayer"
  #:help-string "Display PDF files in DrRacket"
  #:shortcut #\p
  #:shortcut-prefix (ctl alt)
  #:output-to #f
  (lambda ((_ ""))
    (define open-document/ctx (open-document (make-context)))

    (define event-channel (make-async-channel))

    (define mupdf-canvas%
      (class canvas%
        (super-new)
        (define/override (on-char evt)
          (collect-garbage 'incremental)
          (let/cc ex
            (async-channel-put
             event-channel
             (case/eqv (send evt get-key-code)
                       ((left) 'left)
                       ((right) 'right)
                       ((up) 'up)
                       ((down) 'down)
                       ((return) 'refresh)
                       ((#\a) 'last)
                       ((#\d) 'next)
                       ((#\w) 'zoom-in)
                       ((#\s) 'zoom-out)
                       #;((#\q) 'rotate1)
                       #;((#\e) 'rotate2)
                       ((#\q) 'save-bitmap)
                       ((#\e) 'jump)
                       ((#\x) 'reset-settings)
                       ((#\c) (collect-garbage) (ex (void)))
                       ((#\z) 'next-session)
                       (else (ex (void)))))))))

    (define file-choosing-window-style '())
    (define window-name "mupdf")

    (define frame (new frame% [label window-name] [min-width 800] [min-height 600]))
    (define canvas (new mupdf-canvas% [parent frame] [style '(hscroll vscroll)]
                        [min-width 800] [min-height 600]
                        [paint-callback (lambda (c d) (async-channel-put event-channel 'refresh))]))
    (define dc (send canvas get-dc))

    (send dc set-smoothing 'aligned)
    (send canvas init-auto-scrollbars 1000 1200 0 0)
    (send canvas enable #t)
    (send canvas focus)

    ;; Rotation using racket-mupdf is currently buggy
    (struct states (doc cnt cursor bitmap zoom1 zoom2 #;rotate x y) #:constructor-name make-states)

    ;; Auxiliary functions
    (define (open-pdf-document)
      (cond ((get-file "Please choose a PDF file" frame #f #f ".pdf"
                       file-choosing-window-style '(("PDF files" "*.pdf")))
             => open-document/ctx)
            (else #f)))
    (define (new-session-states orig)
      (define doc (open-pdf-document))
      (cond (doc
             (define cnt (document-count-pages doc))
             (if (zero? cnt)
                 (begin (message-box "mupdf" "This is an empty pdf document." frame '(ok)) orig)
                 (load-page (make-states doc cnt 0 #f 1.0 1.0 #;0.0 0.0 0.0))))
            (else orig)))
    (define (load-page sts)
      (match sts
        ((states doc cnt cursor _ zoom1 zoom2 #;rotate _ _)
         (struct-copy states sts (bitmap (pixmap->bitmap ((extract-pixmap doc (make-matrix zoom1 zoom2 #;rotate 0.0)) cursor)))))))
    (define (update-states sts/f instruction)
      (let/cc cc
        (define sts (cond
                      (sts/f)
                      ((or (eq? instruction 'next-session) (eq? instruction 'refresh)) #f)
                      (else (cc #f))))
        (define (check/fallback v)
          (cond (v) (else (cc sts))))
        (case/eq instruction
                 ((refresh) sts)
                 ((last) (load-page (struct-copy states sts (cursor ((lambda (n) (if (zero? n) n (sub1 n))) (states-cursor sts))))))
                 ((next) (load-page (struct-copy states sts (cursor ((lambda (n) (if (>= n (sub1 (states-cnt sts))) n (add1 n))) (states-cursor sts))))))
                 ((zoom-in)
                  (load-page
                   (struct-copy states sts
                                (zoom1 ((lambda (n) (+ n 0.01)) (states-zoom1 sts)))
                                (zoom2 ((lambda (n) (+ n 0.01)) (states-zoom2 sts))))))
                 ((zoom-out)
                  (load-page
                   (struct-copy states sts
                                (zoom1 ((lambda (n) (- n 0.01)) (states-zoom1 sts)))
                                (zoom2 ((lambda (n) (- n 0.01)) (states-zoom2 sts))))))
                 #;((rotate1) (load-page (struct-copy states sts (rotate ((lambda (n) (- n 0.15)) (states-rotate sts))))))
                 #;((rotate2) (load-page (struct-copy states sts (rotate ((lambda (n) (+ n 0.15)) (states-rotate sts))))))
                 ((save-bitmap)
                  (send (states-bitmap sts) save-file
                        (check/fallback
                         (put-file "Specify the file name of the bitmap"
                                   frame
                                   #f #f #f
                                   file-choosing-window-style))
                        ((compose1 (lambda (str/f) (string->symbol (check/fallback str/f))) get-text-from-user)
                         window-name
                         "Specify the format of the bitmap"
                         frame
                         "jpeg"
                         (list 'disallow-invalid)
                         #:validate
                         (lambda (n) (memq (string->symbol n) (list 'png 'jpeg 'xbm 'xpm 'bmp)))))
                  sts)
                 ((jump)
                  ((compose1
                    (lambda (str/f) (load-page (struct-copy states sts (cursor (sub1 (string->number (check/fallback str/f)))))))
                    get-text-from-user)
                   window-name
                   "Specify the page number"
                   frame
                   "1"
                   (list 'disallow-invalid)
                   #:validate
                   (lambda (sn)
                     (define cnt (states-cnt sts))
                     (define n (string->number sn))
                     (and n (exact-positive-integer? n) (<= n cnt)))))
                 ((left) (struct-copy states sts (x ((lambda (n) (- n 10.0)) (states-x sts)))))
                 ((right) (struct-copy states sts (x ((lambda (n) (+ n 10.0)) (states-x sts)))))
                 ((up) (struct-copy states sts (y ((lambda (n) (- n 10.0)) (states-y sts)))))
                 ((down) (struct-copy states sts (y ((lambda (n) (+ n 10.0)) (states-y sts)))))
                 ((reset-settings) (load-page (make-states (states-doc sts) (states-cnt sts) (states-cursor sts) 1.0 1.0 #;0.0 0.0 0.0)))
                 ((next-session) (new-session-states sts)))))
    (define (render sts/f)
      (if sts/f
          (match-let (((states doc cnt cursor bitmap zoom1 zoom2 #;rotate x y) sts/f))
            ;; Rendering
            (send dc clear)
            (send dc draw-bitmap bitmap x y))
          (let ()
            (send dc clear)
            (send dc draw-bitmap help-bitmap 0.0 0.0))))

    ;; The main thread is used as the server thread
    (define client-thread
      (thread (lambda ()
                (let loop ((sts #f))
                  (sync (handle-evt
                         event-channel
                         (lambda (ins)
                           (define ns (update-states sts ins))
                           (render ns)
                           (loop ns))))))))

    (send frame show #t)))

(module+ main
  (pdf-display-script))
