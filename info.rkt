#lang info

(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names (list "DrWhitespace"))
(define drracket-tool-icons '(#f))

(define blurb '("Remove trailing whitespace in DrRacket"))
(define categories '(devtools))
(define primary-file "tool.rkt")

(define deps '("base" "gui-lib" "drracket-plugin-lib"))
(define single-collection "drwhitespace")
