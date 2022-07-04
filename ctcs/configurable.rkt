#lang at-exp racket

(provide define/configurable-ctc
         define/configurable-ctc-impl
         (rename-out [configurable-#%module-begin #%module-begin])
         (except-out (all-from-out racket)
                     #%module-begin))

(require syntax/parse/define
         (for-syntax syntax/parse
                     syntax/parse/lib/function-header))

(define-for-syntax enabled-ctc-levels '(none types max))

;; Let id? := symbol?, referring specifically to top level ids of the module

(begin-for-syntax
  (define-syntax-class name-or-header
    #:attributes [id]
    (pattern id:id)
    (pattern head:function-header
             #:with id (attribute head.name)))

  (define-syntax-class ctc-map-clause
    #:attributes [level ctc]
    (pattern [level:id ctc:expr]
             #:when (member (syntax->datum #'level) enabled-ctc-levels)))

  (define-syntax-class ctc-map
    #:attributes [[clause 1] map]
    (pattern {clause:ctc-map-clause ...}
             ;; ctc-map? := (hash/c id? syntax?)
             #:attr map (hash-set (for/hash ([level (in-list (attribute clause.level))]
                                             [ctc (in-list (attribute clause.ctc))])
                                    (values (syntax->datum level) ctc))
                                  'none
                                  #'any/c)))

  (define-syntax-class module-directory
    #:attributes [map]
    (pattern {[id:id id-map:ctc-map] ...}
             ;; (hash/c id? ctc-map?)
             #:attr map (for/hash ([id (in-list (attribute id))]
                                   [ctc-map (in-list (attribute id-map.map))])
                          (values (syntax->datum id)
                                  ctc-map))))

  (define (ctc-map->stx m)
    (datum->syntax #f
                   (for/list ([{level ctc-stx} (in-hash m)])
                     (list level ctc-stx))))
  (define (module-directory->stx directory)
    (datum->syntax #f
                   (for/list ([{id ctc-map} (in-hash directory)])
                     (list id (ctc-map->stx ctc-map))))))

;; this implementation is never used!
;; #%module-begin below transforms all occurrences of this macro into `define/configurable-ctc-impl`
(define-simple-macro (define/configurable-ctc head:name-or-header
                       cm:ctc-map
                       body ...)
  (define head body ...))

(define-simple-macro (define/configurable-ctc-impl head:name-or-header
                       #:map cm:ctc-map
                       #:config config:expr
                       #:directory directory:module-directory
                       body ...)
  #:do [(define current-config
          ;; (hash/c id? level?)
          (syntax->datum #'config))
        (define module-directory
          ;; (hash/c id? ctc-map?)
          (attribute directory.map))
        (define this-form-name
          ;; id?
          (syntax->datum #'head.id))
        (define this-form-level
          ;; symbol?
          (hash-ref current-config this-form-name))]
  #:with export-ctc (hash-ref (attribute cm.map) this-form-level)
  #:with [[import-id import-ctc] ...] (for/list ([other-name (in-hash-keys current-config)]
                                                 #:unless (equal? other-name this-form-name))
                                        (define other-ctc-map
                                          (hash-ref module-directory
                                                    other-name))
                                        (list (datum->syntax (attribute head)
                                                             other-name)
                                              (hash-ref other-ctc-map
                                                        this-form-level)))
  (define/contract head
    export-ctc
    {~@ #:freevar import-id import-ctc} ...
    body ...))

(begin-for-syntax
  (define-syntax-class top-level-form
    #:attributes [id cm make-expanded]
    (pattern ({~literal define/configurable-ctc} head:name-or-header
                                                 inner-cm:ctc-map
                                                 body ...)
             #:with id (attribute head.id)
             #:attr make-expanded (λ (module-config module-directory)
                                    (quasisyntax/loc this-syntax
                                      (define/configurable-ctc-impl head
                                        #:map inner-cm
                                        #:config #,module-config
                                        #:directory #,module-directory
                                        body ...)))
             #:attr cm (attribute inner-cm.map))
    (pattern _
             #:attr id #f
             #:attr cm #f
             #:attr make-expanded (λ _ this-syntax))))

(define-simple-macro (configurable-#%module-begin {~optional {~seq #:config config-e:expr}
                                                             #:defaults ([config-e #'#f])}
                                                  tlf:top-level-form ...)
  #:do [(define top-level-names
          (map syntax->datum (filter values (attribute tlf.id))))
        (define ctc-maps
          (filter values (attribute tlf.cm)))
        (define module-directory
          (for/hash ([id (in-list top-level-names)]
                     [cm (in-list ctc-maps)])
            (values id cm)))
        (define config-from-stx (syntax->datum #'config-e))
        (define config
          (or config-from-stx
              (for/hash ([id (in-list top-level-names)])
                (values id 'none))))

        (define config-stx (datum->syntax #f config))
        (define directory-stx (module-directory->stx module-directory))]
  #:with [expanded-tlf ...] (map (λ (make-expanded)
                                   (make-expanded config-stx
                                                  directory-stx))
                                 (attribute tlf.make-expanded))
  #:do [(local-require racket/pretty)
        (pretty-write
         (syntax->datum #'[expanded-tlf ...]))]
  ;; lltodo: add checking for all this stuff
  ;; (e.g. every top id is in config, every configurable-ctc-form is well formed)
  (#%module-begin expanded-tlf ...))

;; lltodo: need to implement some knot tying to resolve backward and circular
;; references in the #:freevars.
;; Easiest perhaps is to have each def/configurbale expand into a plain def, and a contracted def
;; and the plain defs all go to the top of the module, and are referenced only by the #:freevars.
;; Possibly useful:
;; https://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._syntax-local-lift-expression%29%29

;; lltodo: can I carry along the program directory and config better using
;; splicing-syntax-parameterize ?
