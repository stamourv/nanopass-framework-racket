#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/lib/function-header)
         syntax/parse
         syntax/parse/lib/function-header)
(provide syntax->datum-helper
         ~define-pass)

(define-syntax (syntax->datum-helper x)
  (syntax-case x ()
    [(_ ?stx)
     #`(let ([stx ?stx])
         (unless (syntax? stx)
           (printf "expected syntax, got ~s at ~s\n" stx #'#,x))
         (let f ([stx stx])
           (cond
             [(syntax? stx) (syntax->datum stx)]
             [(list? stx) (map f stx)]
             [(pair? stx) (cons (f (car stx)) (f (cdr stx)))]
             [(null? stx) '()]
             [else (error 'syntax->datum-helper "unexpected type" stx)])))]))

(define-syntax-class language-specifier
  (pattern (~or (~var _ id)
                ((~var _ id) (~var _ id))
                (~datum *))))

;; Pattern for define-pass
;; (Used in variants of define-pass macros)
(define-syntax ~define-pass
  (pattern-expander
   (lambda (stx)
     (syntax-parse stx
       [(_ name:id ilang:id olang:id e:id fml:id erv:id rest:id)
        #'(~seq (~var name id) (~datum :) (~var ilang language-specifier)
                (~datum ->) (~var olang language-specifier)
                (~or (~optional (~seq #:input (~var e id)) #:defaults ([e #'e]))
                     (~optional (~seq #:formals ((~and fml (~or ((~var _ id) _) (~var _ id))) (... ...)))
                                #:defaults ([(fml 1) null]))
                     (~optional (~seq #:extra-return-values (erv (... ...)))
                                #:defaults ([(erv 1) null])))
                (... ...)
                rest (... ...))]))))

