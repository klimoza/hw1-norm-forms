import Test.Tasty
import Test.Tasty.HUnit
import Formula

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
    testGroup "fun to_nnf" [
      testCase "Atom" $ (to_nnf (Atom "x") @?= Atom "x"),
      testCase "Implication" $ to_nnf (Atom "x" :=>: Atom "y") @?= (Not $ Atom "x") :|: (Atom "y"),
      testCase "Equivalence" $ to_nnf (Atom "x" :<=>: Atom "y") @?= ((Not $ Atom "x") :|: Atom "y") :&: ((Not $  Atom "y") :|: Atom "x"),
      testCase "De Morgan OR" $ to_nnf (Not $ Not (Atom "a") :|: Atom "b") @?= (Atom "a") :&: (Not $ Atom "b"),
      testCase "De Morgan AND" $ to_nnf (Not $ Not (Atom "a") :&: Atom "b") @?= (Atom "a") :|: (Not $ Atom "b"),
      testCase "Big Formula" $ to_nnf ((Not $ ((Atom "a") :|: (Atom "b")) :=>: (Atom "c")) :<=>: (Not $ (Atom "d") :&: (Atom "e"))) @?=
        (:&:) ((:|:) ((:|:) ((Not $ Atom "a") :&: (Not $ Atom "b")) (Atom "c")) ((Not $ Atom "d") :|: (Not $ Atom "e"))) ((Atom "d" :&: Atom "e") :|: ((Atom "a" :|: Atom "b") :&: Not (Atom "c")))
    ],
    testGroup "fun to_dnf" [
      testCase "Atom" $ (to_dnf (Atom "x") @?= Atom "x"),
      testCase "Implication" $ to_dnf (Atom "x" :=>: Atom "y") @?= Not (Atom "x") :|: Atom "y",
      testCase "Equivalence" $ to_dnf (Atom "x" :<=>: Atom "y") @?= 
        ((Not (Atom "x") :&: Not (Atom "y")) :|: (Not (Atom "x") :&: Atom "x")) :|: ((Atom "y" :&: Not (Atom "y")) :|: (Atom "y" :&: Atom "x")),
      testCase "Left distr" $ to_dnf ((Atom "x") :&: (Atom "y" :|: Atom "z")) @?= (Atom "x" :&: Atom "y") :|: (Atom "x" :&: Atom "z"),
      testCase "Right distr" $ to_dnf ((Atom "y" :|: Atom "z") :&: (Atom "x")) @?= (Atom "y" :&: Atom "x") :|: (Atom "z" :&: Atom "x")
    ],
    testGroup "fun to_cnf" [
      testCase "Atom" $ (to_cnf (Atom "x") @?= Atom "x"),
      testCase "Implication" $ to_cnf (Atom "x" :=>: Atom "y") @?= Not (Atom "x") :|: Atom "y",
      testCase "Equivalence" $ to_cnf (Atom "x" :<=>: Atom "y") @?= (Not (Atom "x") :|: Atom "y") :&: (Not (Atom "y") :|: Atom "x"),
      testCase "Left distr" $ to_cnf (Atom "x" :|: (Atom "y" :&: Atom "z")) @?= (Atom "x" :|: Atom "y") :&: (Atom "x" :|: Atom "z"),
      testCase "Right distr" $ to_cnf ((Atom "y" :&: Atom "z") :|: Atom "x") @?= (Atom "y" :|: Atom "x") :&: (Atom "z" :|: Atom "x")
    ]
  ]