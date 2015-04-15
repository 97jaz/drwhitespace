#lang racket/unit

(require drracket/tool
         framework
         racket/class
         racket/gui)

(import drracket:tool^)
(export drracket:tool-exports^)

(define strip-whitespace-mixin
  (mixin ((class->interface text%)) ()
    (define/public (get-trailing-whitespace-ranges start end)
      (define start-paragraph (send this position-paragraph start))
      (define end-paragraph (send this position-paragraph end))

      (define (para-whitespace p lower upper)
        (define start (send this paragraph-start-position p))
        (define end (send this paragraph-end-position p))
        (define ws-start
          (add1
           (or (for*/first ([i (in-range (sub1 end) (sub1 start) -1)]
                            [c (in-value (send this get-character i))]
                            #:unless (char-whitespace? c))
                 i)
               (sub1 start))))

        (define lo (max ws-start lower))
        (define hi (min end upper))
        (and (< lo hi)
             (cons lo hi)))

      (for*/list ([p (in-range start-paragraph (add1 end-paragraph))]
                  [r (in-value (para-whitespace p start end))]
                  #:when r)
        r))

    (define/public (delete-whitespace-ranges start end)
      (for ([r (in-list (reverse (send this get-trailing-whitespace-ranges start end)))])
        (send this delete (car r) (cdr r))))

    (define/public (last-non-empty-paragraph)
      (for*/first ([i (in-range (send this last-paragraph) -1 -1)]
                   [s (in-value (send this paragraph-start-position i))]
                   [e (in-value (send this paragraph-end-position i))]
                   #:when (< s e))
        i))

    (define/public (delete-empty-trailing-lines)
      (define last (send this last-paragraph))
      (define last-nonempty (send this last-non-empty-paragraph))

      (when (< last-nonempty last)
        (send this
              delete
              (send this paragraph-start-position (add1 last-nonempty))
              (send this paragraph-end-position last))))

    (define/augment (on-save-file filename format)
      (when (and (not (eq? format 'copy))
                 (preferences:get PREF:ON-SAVE))
        (define end (send this last-position))
        (send this delete-whitespace-ranges 0 end)

        (when (preferences:get PREF:DELETE-TRAILING-NEWLINES)
          (send this delete-empty-trailing-lines))))

    (super-new)))


(define PREF:ON-SAVE 'drwhitespace:strip-on-save)
(define PREF:DELETE-TRAILING-NEWLINES 'drwhitespace:delete-trailing-newlines)

(preferences:set-default PREF:ON-SAVE #f boolean?)
(preferences:set-default PREF:DELETE-TRAILING-NEWLINES #f boolean?)

(preferences:add-to-editor-checkbox-panel
 (λ (panel)
   (define vcontainer (new vertical-pane% [parent panel] [alignment '(left center)]))

   (preferences:add-check
    vcontainer
    PREF:ON-SAVE
    "Strip trailing whitespace before saving a file")

   (define hcontainer (new horizontal-pane% [parent vcontainer]))
   (define spacer (new horizontal-pane% [parent hcontainer] [min-width 20] [stretchable-width #f]))

   (preferences:add-check
    hcontainer
    PREF:DELETE-TRAILING-NEWLINES
    "Delete empty lines at the end of the file")))

(drracket:get/extend:extend-definitions-text strip-whitespace-mixin)

(define (phase1) (void))
(define (phase2) (void))
