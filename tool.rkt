#lang racket/unit

(require drracket/tool
         framework
         racket/class
         racket/gui)

(import drracket:tool^)
(export drracket:tool-exports^)

(define strip-whitespace-mixin
  (mixin ((class->interface text%)) ()
    (define/augment (on-save-file filename format)
      (unless (eq? format 'copy)
        (strip-trailing-whitespace! this)
        (remove-empty-trailing-paragraphs! this)))

    (super-new)))

(define (strip-trailing-whitespace! text-editor)
  (for ([i (in-range (send text-editor last-paragraph) -1 -1)])
    (define start (send text-editor paragraph-start-position i))
    (define end (send text-editor paragraph-end-position i))

    (define start-delete
      (add1
       (or
        (for*/first ([j (in-range (sub1 end) (sub1 start) -1)]
                     [c (in-value (send text-editor get-character j))]
                     #:unless (char-whitespace? c))
          j)
        (sub1 start))))

    (log-debug "delete in paragraph ~a [~s] from ~a - ~a" i (send text-editor get-text start end) start-delete end)
    (log-debug "~s" (send text-editor get-character end))

    (when (< start-delete end)
      (send text-editor delete start-delete end))))

(define (last-non-empty-paragraph text-editor)
  (for*/first ([i (in-range (send text-editor last-paragraph) -1 -1)]
               [s (in-value (send text-editor paragraph-start-position i))]
               [e (in-value (send text-editor paragraph-end-position i))]
               #:when (< s e))
    i))

(define (remove-empty-trailing-paragraphs! text-editor)
  (define last (send text-editor last-paragraph))
  (define last-nonempty (last-non-empty-paragraph text-editor))

  (when (< last-nonempty last)
    (send text-editor
          delete
          (send text-editor paragraph-start-position (add1 last-nonempty))
          (send text-editor paragraph-end-position last))))


(define (phase1) (void))
(define (phase2) (void))

(drracket:get/extend:extend-definitions-text strip-whitespace-mixin)
