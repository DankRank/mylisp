((LABEL F (LAMBDA (LS)
        (COND ((NULL LS) NIL)
              (T (F ((LAMBDA (X Y) Y)
                     (PRINT (COND ((OR (GET (CAR LS) (QUOTE FEXPR))
                                       (GET (CAR LS) (QUOTE FSUBR)))
                                   (EVAL (CONS (CAR LS) (CADR LS)) NIL))
                                  (T (APPLY (CAR LS) (CADR LS) NIL))))
                     (CDDR LS)))))))
(QUOTE (
;; START
DEFINE((
; everything is namespaced with Q
(QEQUAL (LAMBDA (X Y) (COND ((ATOM X) (COND ((ATOM Y) (EQ X Y)) (T NIL)))
                            ((QEQUAL (CAR X) (CAR Y)) (QEQUAL (CDR X) (CDR Y)))
                            (T NIL)) ))
(QPAIRLIS (LAMBDA (X Y A) (COND ((NULL X) A) (T (CONS (CONS (CAR X) (CAR Y))
                                                (QPAIRLIS (CDR X) (CDR Y) A)))) ))
(QASSOC (LAMBDA (X A) (COND ((QEQUAL (CAAR A) X) (CAR A)) (T (QASSOC X (CDR A)))) ))
(QEVALQUOTE (LAMBDA (FN X) (QAPPLY FN X NIL) ))

(QAPPLY (LAMBDA (FN X A)
        (COND ((ATOM FN) (COND ((EQ FN (QUOTE CAR)) (CAAR X))
                               ((EQ FN (QUOTE CDR)) (CDAR X))
                               ((EQ FN (QUOTE CONS)) (CONS (CAR X) (CADR X)))
                               ((EQ FN (QUOTE ATOM)) (ATOM (CAR X)))
                               ((EQ FN (QUOTE EQ)) (EQ (CAR X) (CADR X)))
                               (T (QAPPLY (QEVAL FN A) X A))))
              ((EQ (CAR FN) (QUOTE LAMBDA)) (QEVAL (CADDR FN) (QPAIRLIS (CADR FN) X A)))
              ((EQ (CAR FN) (QUOTE LABEL)) (QAPPLY (CADDR FN) X (CONS (CONS (CADR FN)
                                                                            (CADDR FN)) A)))) ))

(QEVAL (LAMBDA (E A)
       (COND ((ATOM E) (CDR (QASSOC E A)))
             ((ATOM (CAR E)) (COND
                               ((EQ (CAR E) (QUOTE QUOTE)) (CADR E))
                               ((EQ (CAR E) (QUOTE COND)) (QEVCON (CDR E) A))
                               (T (QAPPLY (CAR E) (QEVLIS (CDR E) A) A))))
             (T (QAPPLY (CAR E) (QEVLIS (CDR E) A) A))) ))

(QEVCON (LAMBDA (C A) (COND ((QEVAL (CAAR C) A) (QEVAL (CADAR C) A))
                            (T (QEVCON (CDR C) A))) ))

(QEVLIS (LAMBDA (M A) (COND ((NULL M) NIL)
                            (T (CONS (QEVAL (CAR M) A) (QEVLIS (CDR M) A)))) ))


))
;; END
)))
