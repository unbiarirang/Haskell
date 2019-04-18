module ChurchNumerals where
c0 = const id
cSucc cn f = cn f . f
cToInt cn = cn (+1) 0
cPlus c1 c2 = c1 cSucc c2
cMult = (.)
cExp c1 c2 = c2 (. c1) (cSucc c0)
