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

(define-script mupdf-script
  #:label "MuPDF script"
  #:help-string "Display PDF files in DrRacket"
  #:shortcut #\p
  #:shortcut-prefix (ctl alt)
  #:output-to #f
  (lambda (_ #:definitions editor)
    (define file
      (let loop ()
        (cond ((get-file "Please choose a PDF file"))
              (else (loop)))))

    (define ctx (make-context))
    (define doc (open-document ctx file))
    (define cnt (document-count-pages doc))

    ;; state variables
    (define cursor 0)
    (define zoom1 1.0)
    (define zoom2 1.0)
    (define rotate 0.0)

    (define (current) (pixmap->bitmap (extract-pixmap doc cursor (make-matrix zoom1 zoom2 rotate))))
    (define (last)
      (if (zero? cursor)
          (void)
          (set! cursor (sub1 cursor)))
      (current))
    (define (next)
      (if (= cursor (sub1 cnt))
          (void)
          (set! cursor (add1 cursor)))
      (current))
    (define (zoom-in)
      (set! zoom1 (+ zoom1 0.01))
      (set! zoom2 (+ zoom2 0.01)))
    (define (zoom-out)
      (unless (or (<= zoom1 0.01) (<= zoom2 0.01))
        (set! zoom1 (- zoom1 0.01))
        (set! zoom2 (- zoom2 0.01))))
    (define (rotate1)
      (set! rotate (+ rotate 10.0)))
    (define (rotate2)
      (set! rotate (- rotate 10.0)))

    (define dc (send editor get-dc))

    (define km (new keymap%))
    (send km add-function "Page Operation"
          (lambda (_ evt)
            (cond ((is-a? evt key-event%)
                   (case (send evt get-key-code)
                     (('left #\a) (send dc draw-bitmap (last)))
                     (('right #\d) (send dc draw-bitmap (next)))
                     (('up #\w) (zoom-in) (send dc draw-bitmap (current)))
                     (('down #\s) (zoom-out) (send dc draw-bitmap (current)))
                     ((#\q) (rotate1) (send dc draw-bitmap (current)))
                     ((#\e) (rotate2) (send dc draw-bitmap (current))))))))

    (send editor set-keymap km)

    (send dc draw-bitmap (current))))
