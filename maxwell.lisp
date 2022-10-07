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
; symbols both used and handled:
; CAR CDR CONS ATOM EQ LAMBDA (LABEL) QUOTE COND
; these work in cond, but they need to be added to the environment:
; NIL T
; stuff we don't define:
; DEFINE
(QNULL (LAMBDA (X) (EQ X NIL) ))
(QCAAR (LAMBDA (P) (CAR (CAR P)) ))
(QCADR (LAMBDA (P) (CAR (CDR P)) ))
(QCDAR (LAMBDA (P) (CDR (CAR P)) ))
(QCADAR (LAMBDA (P) (CAR (CDR (CAR P))) ))
(QCADDR (LAMBDA (P) (CAR (CDR (CDR P))) ))
(QEQUAL (LAMBDA (X Y) (COND ((ATOM X) (COND ((ATOM Y) (EQ X Y)) (T NIL)))
                            ((QEQUAL (CAR X) (CAR Y)) (QEQUAL (CDR X) (CDR Y)))
                            (T NIL)) ))
(QPAIRLIS (LAMBDA (X Y A) (COND ((QNULL X) A) (T (CONS (CONS (CAR X) (CAR Y))
                                                       (QPAIRLIS (CDR X) (CDR Y) A)))) ))
(QASSOC (LAMBDA (X A) (COND ((QEQUAL (QCAAR A) X) (CAR A)) (T (QASSOC X (CDR A)))) ))
(QEVALQUOTE (LAMBDA (FN X) (QAPPLY FN X NIL) ))

(QAPPLY (LAMBDA (FN X A)
        (COND ((ATOM FN) (COND ((EQ FN (QUOTE CAR)) (QCAAR X))
                               ((EQ FN (QUOTE CDR)) (QCDAR X))
                               ((EQ FN (QUOTE CONS)) (CONS (CAR X) (QCADR X)))
                               ((EQ FN (QUOTE ATOM)) (ATOM (CAR X)))
                               ((EQ FN (QUOTE EQ)) (EQ (CAR X) (QCADR X)))
                               (T (QAPPLY (QEVAL FN A) X A))))
              ((EQ (CAR FN) (QUOTE LAMBDA)) (QEVAL (QCADDR FN) (QPAIRLIS (QCADR FN) X A)))
              ((EQ (CAR FN) (QUOTE LABEL)) (QAPPLY (QCADDR FN) X (CONS (CONS (QCADR FN)
                                                                             (QCADDR FN)) A)))) ))

(QEVAL (LAMBDA (E A)
       (COND ((ATOM E) (CDR (QASSOC E A)))
             ((ATOM (CAR E)) (COND
                               ((EQ (CAR E) (QUOTE QUOTE)) (QCADR E))
                               ((EQ (CAR E) (QUOTE COND)) (QEVCON (CDR E) A))
                               (T (QAPPLY (CAR E) (QEVLIS (CDR E) A) A))))
             (T (QAPPLY (CAR E) (QEVLIS (CDR E) A) A))) ))

(QEVCON (LAMBDA (C A) (COND ((QEVAL (QCAAR C) A) (QEVAL (QCADAR C) A))
                            (T (QEVCON (CDR C) A))) ))

(QEVLIS (LAMBDA (M A) (COND ((QNULL M) NIL)
                            (T (CONS (QEVAL (CAR M) A) (QEVLIS (CDR M) A)))) ))


))
;; END
)))
