#lang racket/base
(require pict)
(provide help-bitmap)

(define vs 15.0)
(define hs 15.0)
(define fs 30.0)

(define (make-pict/rectangle p (make-rectangle rectangle))
  (cc-superimpose
   (make-rectangle (+ hs (pict-width p))
                   (+ vs (pict-height p)))
   p))

(define (combine-lines . ls) (apply vl-append vs ls))
(define (format-line key doc-string)
  (hc-append hs
             (make-pict/rectangle (text (symbol->string key) null fs) rounded-rectangle)
             (text ":" null fs)
             (text doc-string null fs)))

(define help-data
  '((left "move to the left")
    (right "move to the right")
    (up "move upwards")
    (down "move down")
    (return "refresh the canvas")
    (a "page forward")
    (d "page backward")
    (w "enlarge the image")
    (s "shrink the image")
    (q "save the current page as a bitmap")
    (e "jump to the specified page")
    (c "(collect-garbage 'major)")
    (z "open a document")))

(define help-bitmap
  (pict->bitmap
   (make-pict/rectangle
    (apply combine-lines (map (lambda (r) (apply format-line r)) help-data)))))
