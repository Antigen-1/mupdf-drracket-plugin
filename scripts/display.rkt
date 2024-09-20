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

(require quickscript racket/gui/base racket/class racket/match racket/case racket/async-channel racket-mupdf)

(define-script pdf-display-script
  #:label "PDF displayer"
  #:help-string "Display PDF files in DrRacket"
  #:shortcut #\p
  #:shortcut-prefix (ctl alt)
  #:output-to #f
  (lambda (_)
    (let/cc cc
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
                 ((#\q) 'rotate1)
                 ((#\e) 'rotate2)
                 ((#\x) 'reset-settings)
                 ((#\c) (collect-garbage) (ex (void)))
                 ((#\z) 'next-session)
                 (else (ex (void)))))))))

      (define frame (new frame% [label "mupdf"] [min-width 800] [min-height 600]))
      (define canvas (new mupdf-canvas% [parent frame] [style '(hscroll vscroll)]
                          [min-width 800] [min-height 600]
                          [paint-callback (lambda (c d) (async-channel-put event-channel 'refresh))]))
      (define dc (send canvas get-dc))

      (send dc set-smoothing 'aligned)
      (send canvas init-auto-scrollbars 1600 1200 0 0)
      (send canvas enable #t)
      (send canvas focus)

      (struct states (doc cnt cursor zoom1 zoom2 rotate x y) #:constructor-name make-states)

      ;; Auxiliary functions
      (define (open-pdf-document)
        (cond ((get-file "Please choose a PDF file" frame #f #f ".pdf"
                         '(common) '(("PDF files" "*.pdf")))
               => open-document/ctx)
              (else (cc (void)))))
      (define (new-session-states)
        (define doc (open-pdf-document))
        (define cnt (document-count-pages doc))
        (make-states doc cnt 0 1.0 1.0 0.0 0.0 0.0))
      (define (update-states sts instruction)
        (case/eq instruction
                 ((refresh) sts)
                 ((last) (struct-copy states sts (cursor ((lambda (n) (if (zero? n) n (sub1 n))) (states-cursor sts)))))
                 ((next) (struct-copy states sts (cursor ((lambda (n) (if (>= n (sub1 (states-cnt sts))) n (add1 n))) (states-cursor sts)))))
                 ((zoom-in) (struct-copy states sts
                                         (zoom1 ((lambda (n) (+ n 0.01)) (states-zoom1 sts)))
                                         (zoom2 ((lambda (n) (+ n 0.01)) (states-zoom2 sts)))))
                 ((zoom-out) (struct-copy states sts
                                          (zoom1 ((lambda (n) (- n 0.01)) (states-zoom1 sts)))
                                          (zoom2 ((lambda (n) (- n 0.01)) (states-zoom2 sts)))))
                 ((rotate1) (struct-copy states sts (rotate ((lambda (n) (- n 0.15)) (states-rotate sts)))))
                 ((rotate2) (struct-copy states sts (rotate ((lambda (n) (+ n 0.15)) (states-rotate sts)))))
                 ((left) (struct-copy states sts (x ((lambda (n) (- n 10.0)) (states-x sts)))))
                 ((right) (struct-copy states sts (x ((lambda (n) (+ n 10.0)) (states-x sts)))))
                 ((up) (struct-copy states sts (y ((lambda (n) (- n 10.0)) (states-y sts)))))
                 ((down) (struct-copy states sts (y ((lambda (n) (+ n 10.0)) (states-y sts)))))
                 ((reset-settings) (make-states (states-doc sts) (states-cnt sts) (states-cursor sts) 1.0 1.0 0.0 0.0 0.0))
                 ((next-session) (new-session-states))))
      (define (render sts)
        (match-let (((states doc cnt cursor zoom1 zoom2 rotate x y) sts))
          ;; Rendering
          (send dc clear)
          ;; We use racket-side bitmap rotation
          (send dc set-rotation rotate)
          (send dc draw-bitmap (pixmap->bitmap ((extract-pixmap doc (make-matrix zoom1 zoom2 0.0)) cursor)) x y)
          (send canvas flush)))

      ;; The main thread is used as the server thread
      (define client-thread
        (thread (lambda ()
                  (let loop ((sts (new-session-states)))
                    (sync (handle-evt
                           event-channel
                           (lambda (ins)
                             (define ns (update-states sts ins))
                             (render ns)
                             (loop ns))))))))

      (send frame show #t))))
