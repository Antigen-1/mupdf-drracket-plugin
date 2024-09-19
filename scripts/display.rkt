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

(require quickscript racket/gui/base racket/class racket-mupdf)

(define-script pdf-display-script
  #:label "PDF displayer"
  #:help-string "Display PDF files in DrRacket"
  #:shortcut #\p
  #:shortcut-prefix (ctl alt)
  #:output-to #f
  (lambda (_)
    (define ctx (make-context))
    (define frame (new frame% [label "mupdf"] [min-width 800] [min-height 600]))

    (let/cc cc
      (let loop ()
        (define file
          (let loop ()
            (cond ((get-file "Please choose a PDF file"))
                  (else (loop)))))

        (define doc (open-document ctx file))
        (define cnt (document-count-pages doc))

        ;; state variables
        (define cursor 0)
        (define zoom1 1.0)
        (define zoom2 1.0)
        (define rotate 0.0)
        (define x 0.0)
        (define y 0.0)

        ;; Instructions
        (define (current) (pixmap->bitmap (extract-pixmap doc cursor (make-matrix zoom1 zoom2 0.0))))
        (define (last)
          (if (zero? cursor)
              (void)
              (set! cursor (sub1 cursor))))
        (define (next)
          (if (= cursor (sub1 cnt))
              (void)
              (set! cursor (add1 cursor))))
        (define (left)
          (set! x (- x 10.0)))
        (define (right)
          (set! x (+ x 10.0)))
        (define (up)
          (set! y (- y 10.0)))
        (define (down)
          (set! y (+ y 10.0)))
        (define (zoom-in)
          (set! zoom1 (+ zoom1 0.01))
          (set! zoom2 (+ zoom2 0.01)))
        (define (zoom-out)
          (unless (or (<= zoom1 0.01) (<= zoom2 0.01))
            (set! zoom1 (- zoom1 0.01))
            (set! zoom2 (- zoom2 0.01))))
        (define (rotate1)
          (set! rotate (+ rotate 0.15)))
        (define (rotate2)
          (set! rotate (- rotate 0.15)))
        (define (reset-settings)
          (set! rotate 0.0)
          (set! zoom1 1.0)
          (set! zoom2 1.0)
          (set! x 0.0)
          (set! y 0.0))
        (define (next-session)
          (send frame show #f)
          (send frame delete-child canvas)
          (cc (loop)))

        (define mupdf-canvas%
          (class canvas%
            (super-new)
            (define/override (on-char evt)
              (collect-garbage 'incremental)
              (case (send evt get-key-code)
                ((left) (left))
                ((right) (right))
                ((up) (up))
                ((down) (down))
                ((#\a) (last))
                ((#\d) (next))
                ((#\w) (zoom-in))
                ((#\s) (zoom-out))
                ((#\q) (rotate1))
                ((#\e) (rotate2))
                ((#\x) (reset-settings))
                ((#\c) (collect-garbage))
                ((#\z) (next-session)))
              (draw (current)))))

        (define canvas (new mupdf-canvas% [parent frame] [style '(hscroll vscroll)]
                            [min-width 800] [min-height 600]
                            [paint-callback (lambda (_1 _2) (draw (current)))]))
        (send canvas init-auto-scrollbars 1600 1200 0 0)
        (send canvas enable #t)
        (send canvas focus)

        (define dc (send canvas get-dc))
        (send dc set-smoothing 'aligned)

        (define (draw bp)
          (send dc erase)
          ;; We use racket-side bitmap rotation.
          (send dc set-rotation rotate)
          (send dc draw-bitmap bp x y)
          (send canvas flush))

        (send frame show #t)))))
