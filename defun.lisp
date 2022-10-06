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
; List Handling Functions
(DEFUN APPEND (X Y)
       (COND ((NULL X) Y)
             (T (CONS (CAR X) (APPEND (CDR X) Y)))))
(DEFUNF CONC (ARGS A)
        (COND ((NULL ARGS) NIL)
              (T (BEGIN
                   (LET F ((P (EVLIS ARGS A)))
                        (COND ((NULL (CDR P)) NIL)
                              (T (BEGIN
                                    (NCONC (CAR P) (CADR P))
                                    (F (CDR P))))))
                   (CAR ARGS)))))
(DEFUN NCONC (X Y)
       (COND ((NULL X) Y)
             (T (LET F ((P X))
                     (COND ((NULL (CDR P)) (BEGIN (RPLACD P Y) X))
                           (T (F (CDR P))))))))
(DEFUN COPY (X)
       (COND ((NULL X) NIL)
             ((ATOM X) X)
             (T (CONS (COPY (CAR X)) (COPY (CDR X))))))
(DEFUN REVERSE (LS)
       (LET F ((X NIL) (Y LS))
            (COND ((NULL Y) X)
                  (T (F (CONS (CAR Y) X) (CDR Y))))))
(DEFUN MEMBER (X LS)
       (COND ((NULL LS) NIL)
             ((EQUAL X (CAR LS)) T)
             (T (MEMBER X (CDR LS)))))
(DEFUN LENGTH (LS)
       (LET F ((X 0) (LS LS))
            (COND ((NULL LS) X)
                  (T (F (ADD1 X) (CDR LS))))))
(DEFUN EFFACE (X LS)
       (COND ((NULL LS) NIL)
             ((EQUAL X (CAR LS)) (CDR LS))
             (T (RPLACD LS (EFFACE X (CDR LS))))))
; Functionals
(DEFUN MAPLIST (X F)
       (COND ((NULL X) NIL)
             (T (CONS (F X) (MAPLIST (CDR X) F)))))
(DEFUN MAPCON (X F)
       (COND ((NULL X) NIL)
             (T (NCONC (F X) (MAPCON (CDR X) F)))))
(DEFUN MAP (X F)
       (COND ((NULL X) NIL)
             (T (BEGIN (F X) (MAP (CDR X) F)))))
(DEFUN SEARCH (X P F U)
       (COND ((NULL X) (U X))
             ((P X) (F X))
             (T (SEARCH (CDR X) P F U))))
; Other stuff
(DEFUN SASSOC (X Y U)
    (COND ((NULL Y) (U))
          ((EQ (CAAR Y) X) (CAR Y))
          (T (SASSOC X (CDR Y) U))))
(DEFUNF LET (LET*ARGS LET*A)
        (APPLY
          (LIST
            'LABEL
            (CAR LET*ARGS)
            (LIST
              'LAMBDA
              (MAPLIST (CADR LET*ARGS) 'CAAR)
              (CADDR LET*ARGS)))
          (EVLIS (MAPLIST (CADR LET*ARGS) 'CADAR) LET*A)))
