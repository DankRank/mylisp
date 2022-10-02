(DEFLIST '(
  (DEFUN (LAMBDA (ARGS A)
                 (DEFINE (LIST
                           (LIST
                             (CAR ARGS)
                             (CONS 'LAMBDA (CDR ARGS)))))))
  (DEFUNF (LAMBDA (ARGS A)
                  (DEFLIST (LIST
                             (LIST
                               (CAR ARGS)
                               (CONS 'LAMBDA (CDR ARGS))))
                           'FEXPR)))
) 'FEXPR)
; Define all the CAAAAR..CDDDDR combinations
((LAMBDA (F)
         (BEGIN
           (F '(EXPR (LAMBDA (P) (CAR P))) 'CAAR 'CDAR)
           (F '(EXPR (LAMBDA (P) (CDR P))) 'CADR 'CDDR)
           (F 'CAAR 'CAAAR 'CDAAR)
           (F 'CADR 'CAADR 'CDADR)
           (F 'CDAR 'CADAR 'CDDAR)
           (F 'CDDR 'CADDR 'CDDDR)
           (F 'CAAAR 'CAAAAR 'CDAAAR)
           (F 'CAADR 'CAAADR 'CDAADR)
           (F 'CADAR 'CAADAR 'CDADAR)
           (F 'CADDR 'CAADDR 'CDADDR)
           (F 'CDAAR 'CADAAR 'CDDAAR)
           (F 'CDADR 'CADADR 'CDDADR)
           (F 'CDDAR 'CADDAR 'CDDDAR)
           (F 'CDDDR 'CADDDR 'CDDDDR)))
 '(LAMBDA (BASE ANAME DNAME)
          (DEFINE
            (LIST
              (LIST
                ANAME
                (LIST
                  'LAMBDA
                  (CAR (CDR (GET BASE 'EXPR)))
                  (CONS 'CAR (CDR (CDR (GET BASE 'EXPR))))))
              (LIST
                DNAME
                (LIST
                  'LAMBDA
                  (CAR (CDR (GET BASE 'EXPR)))
                  (CONS 'CDR (CDR (CDR (GET BASE 'EXPR))))))))))
(DEFUN MAPLIST (X F)
       (COND ((NULL X) NIL)
             (T (CONS (F X) (MAPLIST (CDR X) F)))))
(DEFUNF LET (ARGS A)
        (APPLY
          (LIST
            'LABEL
            (CAR ARGS)
            (LIST
              'LAMBDA
              (MAPLIST (CADR ARGS) 'CAAR)
              (CADDR ARGS)))
          (EVLIS (MAPLIST (CADR ARGS) 'CADAR) A)))
