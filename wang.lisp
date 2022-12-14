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
(THEOREM (LAMBDA (S) (TH1 NIL NIL (CADR S) (CADDR S))))

(TH1 (LAMBDA (A1 A2 A C) (COND ((NULL A)
    (TH2 A1 A2 NIL NIL C)) (T
    (OR (MEMBER (CAR A) C) (COND ((ATOM (CAR A))
    (TH1 (COND ((MEMBER (CAR A) A1) A1)
    (T (CONS (CAR A) A1))) A2 (CDR A) C))
    (T (TH1 A1 (COND ((MEMBER (CAR A) A2) A2)
    (T (CONS (CAR A) A2))) (CDR A) C))))))))

(TH2 (LAMBDA (A1 A2 C1 C2 C) (COND
    ((NULL C) (TH A1 A2 C1 C2))
    ((ATOM (CAR C)) (TH2 A1 A2 (COND
    ((MEMBER (CAR C) C1) C1) (T
    (CONS (CAR C) C1))) C2 (CDR C)))
    (T (TH2 A1 A2 C1 (COND ((MEMBER
    (CAR C) C2) C2) (T (CONS (CAR C) C2)))
    (CDR C))))))

(TH (LAMBDA (A1 A2 C1 C2) (COND ((NULL A2) (AND (NOT (NULL C2))
    (THR (CAR C2) A1 A2 C1 (CDR C2)))) (T (THL (CAR A2) A1 (CDR A2)
    C1 C2)))))

(THL (LAMBDA (U A1 A2 C1 C2) (COND
    ((EQ (CAR U) (QUOTE NOT)) (TH1R (CADR U) A1 A2 C1 C2))
    ((EQ (CAR U) (QUOTE AND)) (TH2L (CDR U) A1 A2 C1 C2))
    ((EQ (CAR U) (QUOTE OR)) (AND (TH1L (CADR U) A1 A2 C1 C2)
    (TH1L (CADDR U) A1 A2 C1 C2) ))
    ((EQ (CAR U) (QUOTE IMPLIES)) (AND (TH1L (CADDR U) A1 A2 C1
    C2) (TH1R (CADR U) A1 A2 C1 C2) ))
    ((EQ (CAR U) (QUOTE EQUIV)) (AND (TH2L (CDR U) A1 A2 C1 C2)
    (TH2R (CDR U) A1 A2 C1 C2) ))
    (T (ERROR (LIST (QUOTE THL) U A1 A2 C1 C2)))
    )))

(THR (LAMBDA (U A1 A2 C1 C2) (COND
    ((EQ (CAR U) (QUOTE NOT)) (TH1L (CADR U) A1 A2 C1 C2))
    ((EQ (CAR U) (QUOTE AND)) (AND (TH1R (CADR U) A1 A2 C1 C2)
    (TH1R (CADDR U) A1 A2 C1 C2) ))
    ((EQ (CAR U) (QUOTE OR)) (TH2R (CDR U) A1 A2 C1 C2))
    ((EQ (CAR U) (QUOTE IMPLIES)) (TH11 (CADR U) (CADDR U)
    A1 A2 C1 C2))
    ((EQ (CAR U) (QUOTE EQUIV)) (AND (TH11 (CADR U) (CADDR U)
    A1 A2 C1 C2) (TH11 (CADDR U) (CADR U) A1 A2 C1 C2) ))
    (T (ERROR (LIST (QUOTE THR) U A1 A2 C1 C2)))
    )))

(TH1L (LAMBDA (V A1 A2 C1 C2) (COND
    ((ATOM V) (OR (MEMBER V C1)
    (TH (CONS V A1) A2 C1 C2) ))
    (T (OR (MEMBER V C2) (TH A1 (CONS V A2) C1 C2) ))
    )))

(TH1R (LAMBDA (V A1 A2 C1 C2) (COND
    ((ATOM V) (OR (MEMBER V A1)
    (TH A1 A2 (CONS V C1) C2) ))
    (T (OR (MEMBER V A2) (TH A1 A2 C1 (CONS V C2))))
    )))

(TH2L (LAMBDA (V A1 A2 C1 C2) (COND
    ((ATOM (CAR V)) (OR (MEMBER (CAR V) C1)
    (TH1L (CADR V) (CONS (CAR V) A1) A2 C1 C2)))
    (T (OR (MEMBER (CAR V) C2) (TH1L (CADR V) A1 (CONS (CAR V)
    A2) C1 C2)))
    )))

(TH2R (LAMBDA (V A1 A2 C1 C2) (COND
    ((ATOM (CAR V)) (OR (MEMBER (CAR V) A1)
    (TH1R (CADR V) A1 A2 (CONS (CAR V) C1) C2)))
    (T (OR (MEMBER (CAR V) A2) (TH1R (CADR V) A1 A2 C1
    (CONS (CAR V) C2))))
    )))

(TH11 (LAMBDA (V1 V2 A1 A2 C1 C2) (COND
    ((ATOM V1) (OR (MEMBER V1 C1) (TH1R V2 (CONS V1 A1) A2 C1
    C2)))
    (T (OR (MEMBER V1 C2) (TH1R V2 A1 (CONS V1 A2) C1 C2)))
    )))
))

TRACE ((THEOREM TH1 TH2 TH THL THR TH1L TH1R TH2L TH2R TH11))

THEOREM
((ARROW (P) ((OR P Q))))

UNTRACE ((THEOREM TH1 TH2 THL THR TH1L TH1R TH2L TH2R TH11))

THEOREM
((ARROW ((OR A (NOT B))) ((IMPLIES (AND P Q) (EQUIV P Q))) ))

;; END
)))
