
(DEFUNF PROG (ARGS A)
        ((LAMBDA (A GO)
                 (LET F ((PC (CDR ARGS)))
                      (COND ((NULL PC) NIL)
                            ((ATOM (CAR PC)) (F (CDR PC)))
                            ((EQ (CAAR PC) 'GO) (F (SASSOC (CADAR PC) GO '(LAMBDA () (ERROR 'A6)))))
                            ((EQ (CAAR PC) 'RETURN) (EVAL (CADAR PC) A))
                            ((EQ (CAAR PC) 'COND)
                             (LET G ((P (CDAR PC)))
                                  (COND ((NULL P) (F (CDR PC)))
                                        ((EVAL (CAAR P) A) ; True case
                                         (COND ((ATOM (CADAR P)) (EVAL (CADAR P) A))
                                               ((EQ (CAADAR P) 'GO) (F (SASSOC (CADR (CADAR P)) GO '(LAMBDA () (ERROR 'A6)))))
                                               ((EQ (CAADAR P) 'RETURN) (EVAL (CADR (CADAR P)) A))
                                               (T (F (BEGIN (EVAL (CADAR P) A) (CDR PC))))))
                                        (T (G (CDR P)))))) ; False case
                            (T (F (BEGIN (EVAL (CAR PC) A) (CDR PC)))))))
         (NCONC (MAPLIST (CAR ARGS) '(LAMBDA (X) (CONS (CAR X) NIL))) A) ; A-list
         (LET F ((GO ()) (P (CDR ARGS))) ; GO-list
              (COND ((NULL P) GO)
                    ((ATOM (CAR P)) (F (CONS P GO) (CDR P)))
                    (T (F GO (CDR P)))))))
