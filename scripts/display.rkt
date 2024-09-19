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

(require quickscript racket/gui/base racket/class racket/vector racket/match racket/case racket-mupdf)

(define-script pdf-display-script
  #:label "PDF displayer"
  #:help-string "Display PDF files in DrRacket"
  #:shortcut #\p
  #:shortcut-prefix (ctl alt)
  #:output-to #f
  (lambda (_)
    (let/cc cc
      (define open-document/ctx (open-document (make-context)))

      (define mupdf-canvas%
        (class canvas%
          (super-new)
          (define/override (on-char evt)
            (collect-garbage 'incremental)
            (let/cc ex
              ((unbox cont-box)
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
                          [paint-callback (lambda (c d) ((unbox cont-box) 'refresh))]))
      (define dc (send canvas get-dc))

      (send dc set-smoothing 'aligned)
      (send canvas init-auto-scrollbars 1600 1200 0 0)
      (send canvas enable #t)
      (send canvas focus)

      (define (open-pdf-document)
        (cond ((get-file "Please choose a PDF file" frame #f #f ".pdf"
                         '(common) '(("PDF files" "*.pdf")))
               => open-document/ctx)
              (else (cc (void)))))
      (define (new-renderer)
        (define nd (open-pdf-document))
        (define ncnt (document-count-pages nd))
        ((render nd ncnt)))
      (define (vector-update vec pos proc)
        (define nv (vector-copy vec))
        (vector-set! nv pos (proc (vector-ref vec pos)))
        nv)
      (define (((render doc cnt) (cursor 0) (zoom1 1.0) (zoom2 1.0) (rotate 0.0) (x 0.0) (y 0.0)) instruction)
        (let/cc ex
          (define vec (vector cursor zoom1 zoom2 rotate x y))
          (match-let (((vector nc nz1 nz2 nr nx ny)
                       (case/eq instruction
                         ((refresh) vec)
                         ((last) (vector-update vec 0 (lambda (n) (if (zero? n) n (sub1 n)))))
                         ((next) (vector-update vec 0 (lambda (n) (if (>= n (sub1 cnt)) n (add1 n)))))
                         ((zoom-in) (vector-update (vector-update vec 1 (lambda (n) (+ n 0.01))) 2 (lambda (n) (+ n 0.01))))
                         ((zoom-out) (vector-update (vector-update vec 1 (lambda (n) (- n 0.01))) 2 (lambda (n) (- n 0.01))))
                         ((rotate1) (vector-update vec 3 (lambda (n) (- n 0.15))))
                         ((rotate2) (vector-update vec 3 (lambda (n) (+ n 0.15))))
                         ((left) (vector-update vec 4 (lambda (n) (- n 10.0))))
                         ((right) (vector-update vec 4 (lambda (n) (+ n 10.0))))
                         ((up) (vector-update vec 5 (lambda (n) (- n 10.0))))
                         ((down) (vector-update vec 5 (lambda (n) (+ n 10.0))))
                         ((reset-settings) (vector cursor 1.0 1.0 0.0 0.0 0.0))
                         ((next-session)
                          (define ncont (new-renderer))
                          (set-box! cont-box ncont)
                          (ex (ncont 'refresh))))))

            ;; Rendering
            (send dc clear)
            ;; We use racket-side bitmap rotation
            (send dc set-rotation nr)
            (send dc draw-bitmap (pixmap->bitmap ((extract-pixmap doc (make-matrix nz1 nz2 0.0)) nc)) nx ny)
            (send canvas flush)

            (set-box! cont-box ((render doc cnt) nc nz1 nz2 nr nx ny)))))

      ;; (box/c (-> symbol? any))
      (define cont-box (box (new-renderer)))

      (send frame show #t))))
