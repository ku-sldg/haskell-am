AT 1 (ASPT (ASPC 1 ["target.txt"]))

(AT 1(LN (BRS (ALL,NONE) (ASPT CPY) (ASPT (ASPC 1 ["target.txt"]))) (ASPT SIG)))


LN (AT 1 (LN (AT 2 (LN (ASP 1 []) SIG)) (LN (ASP 2 []) SIG))) (LN (ASP 3 []) SIG)

(@1 ((@2 ((ASP 1 []) ; SIG)) ; (ASP 2 []) ; SIG)) ; (ASP 3 []) ; SIG

@1 ((ASP 2 []) ; SIG) ; (ASP 3 []) ; SIG

@1 ((ASP 1 []) ; (ASP 2 []) ; SIG) ; (ASP 3 []) ; SIG

where ASP 1 = boot signature
      ASP 2 = UserAM VM inspection
      ASP 3 = Ground Station inspection


LN (ASP 1 ["target.txt"]) (AT 1(LN (BRS (ALL,ALL) CPY (ASP 1 ["target.txt"])) SIG))

BRP (NONE,NONE) (ASP 1 ["target.txt"]) (ASP 1 ["target.txt"])

(AT 1(LN (BRS (ALL,NONE) CPY (ASP 1 ["target.txt"])) SIG))

SIG

LN (ASP 1 ["target.txt"]) (AT 1(LN (BRS (NONE,NONE) (ASP 1 ["target.txt"])  (ASP 1 ["target.txt"])) SIG))

LN (ASP 1 ["target.txt"]) (AT 1(LN (BRS (NONE,NONE) CPY (ASP 1 ["target.txt"])) SIG))

(ASP 1 ["target.txt"])
SIG
LN (ASP 1 ["target.txt"]) SIG



LN (ASP 1 ["target.txt"]) (AT 1(LN (BRS (NONE,NONE) (ASP 1 ["target.txt"])  (ASP 1 ["target.txt"])) SIG))

LN (ASP 1 ["target.txt"]) (AT 1(LN (BRS (ALL,ALL) CPY (ASP 1 ["target.txt"])) SIG))

LN (ASP 1 ["target.txt"]) (AT 1(LN (BRS (ALL,NONE) (ASP 1 ["target.txt"])  (ASP 1 ["target.txt"])) SIG))

LN (ASP 1 ["target.txt"]) (AT 1(LN (BRS (ALL,NONE) CPY (ASP 1 ["target.txt"])) SIG))

LN (ASP 1 ["target.txt"]) SIG
BRP (NONE,NONE) (ASP 1 ["target.txt"]) (ASP 1 ["target.txt"]) 
LN (ASP 1 ["target.txt"]) (ASP 1 ["target.txt"])
(ASP 1 ["target.txt"])
SIG

(AT 1(LN (BRS (ALL,NONE) CPY (ASP 1 ["target.txt"])) SIG))
(AT 1((BRS (ALL,NONE) CPY (ASP 1 ["target.txt"]))))
(AT 1(LN (BRS (ALL,NONE) CPY (ASP 1 ["target.txt"])) SIG))
LN (BRS (ALL,NONE) CPY (ASP 1 ["target.txt"])) SIG


CPY
