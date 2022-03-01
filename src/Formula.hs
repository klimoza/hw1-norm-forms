module MyLib where

data Formula = Atom String | T | F | Not Formula | Formula :&: Formula | Formula :|: Formula | Formula :=>: Formula | Formula :<=>: Formula
    deriving (Eq, Show)

to_nnf (Atom s) = Atom s
to_nnf T = T
to_nnf F = F
to_nnf (Not (Atom s)) = Not (Atom s)
to_nnf (Not T) = F
to_nnf (Not F) = T
to_nnf (Not (a :&: b)) = (to_nnf (Not a)) :|: (to_nnf (Not b))
to_nnf (Not (a :|: b)) = (to_nnf(Not a)) :&: (to_nnf (Not b))
to_nnf (Not (a :=>: b)) = to_nnf (Not ((Not a) :|: b))
to_nnf (Not (a :<=>: b)) = to_nnf (Not ((a :=>: b) :&: (b :=>: a)))
to_nnf (a :&: b) = (to_nnf a) :&: (to_nnf b)
to_nnf (a :|: b) = (to_nnf a) :|: (to_nnf b)
to_nnf (a :=>: b) = (to_nnf a) :=>: (to_nnf b)
to_nnf (a :<=>: b) = (to_nnf a) :<=>: (to_nnf b)

to_dnf = apply_rules . to_nnf where
    apply_rules (Atom s) = Atom s
    apply_rules T = T
    apply_rules F = F
    apply_rules (Not l) = Not l
    apply_rules (a :|: b) = (apply_rules a) :|: (apply_rules b)
    apply_rules (a :&: b) 
        | (c :|: d) <- v = apply_rules (c :&: b) :|: apply_rules (d :&: b)
        | (c :|: d) <- u = apply_rules (a :&: c) :|: apply_rules (a :&: d)
        | otherwise      = a :&: b
            where v = apply_rules a 
                  u = apply_rules b

to_cnf = apply_rules . to_nnf where
    apply_rules (Atom s) = Atom s
    apply_rules T = T
    apply_rules F = F
    apply_rules (Not l) = Not l
    apply_rules (a :&: b) = (apply_rules a) :&: (apply_rules b)
    apply_rules (a :|: b)
        | (c :&: d) <- v = apply_rules (c :|: b) :&: apply_rules (d :|: b)
        | (c :&: d) <- u = apply_rules (a :|: c) :&: apply_rules (a :|: d)
        | otherwise      = a :|: b
            where v = apply_rules a 
                  u = apply_rules b