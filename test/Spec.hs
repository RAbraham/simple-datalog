import Test.Hspec
import Control.Exception              ( evaluate )
import Lib

iteration0OutputKB = [ Atom {_predSym = "adviser", _terms = [Sym "Alan Mycroft",Sym "Dominic Orchard"]}
                     , Atom {_predSym = "adviser", _terms = [Sym "Robin Milner",Sym "Alan Mycroft"]}]

iteration1OutputKB = [ Atom {_predSym = "adviser", _terms = [Sym "Alan Mycroft",Sym "Dominic Orchard"]}
                     , Atom {_predSym = "adviser", _terms = [Sym "Robin Milner",Sym "Alan Mycroft"]}
                     , Atom {_predSym = "academicAncestor", _terms = [Sym "Alan Mycroft",Sym "Dominic Orchard"]}
                     , Atom {_predSym = "academicAncestor", _terms = [Sym "Robin Milner",Sym "Alan Mycroft"]}]

iteration2OutputKB = [ Atom {_predSym = "adviser", _terms = [Sym "Alan Mycroft",Sym "Dominic Orchard"]}
                     , Atom {_predSym = "adviser", _terms = [Sym "Robin Milner",Sym "Alan Mycroft"]}
                     , Atom {_predSym = "academicAncestor", _terms = [Sym "Alan Mycroft",Sym "Dominic Orchard"]}
                     , Atom {_predSym = "academicAncestor", _terms = [Sym "Robin Milner",Sym "Alan Mycroft"]} 
                     , Atom {_predSym = "academicAncestor", _terms = [Sym "Robin Milner",Sym "Dominic Orchard"]}]

main :: IO ()
main = hspec $ 
  describe "Prelude.head" $ do
    -- it "returns the first element of a list" $ 
      -- head [23 ..] `shouldBe` (23 :: Int)

    it "tests datalog" $
      query "query1" ancestor `shouldBe` [[(Var "Intermediate",Sym "Dominic Orchard")],[(Var "Intermediate",Sym "Alan Mycroft")]]
    
    it "evaluates Rules" $
      -- evalRule :: KnowledgeBase -> Rule -> KnowledgeBase
     evalRule [] ancestorRuleBase `shouldBe` [] 
    
    it "finds immediateConsequence" $
      -- immediateConsequence :: Program -> KnowledgeBase -> KnowledgeBase
      
      immediateConsequence simpleAncestor [] `shouldBe` iteration0OutputKB 
      -- immediateConsequence simpleAncestor iteration0OutputKB `shouldBe` iteration1OutputKB 
      -- immediateConsequence simpleAncestor iteration1OutputKB `shouldBe` iteration2OutputKB 

    it "walks" $
      walk [] [ Atom "adviser" [ Var "X", Var "Y" ] ] `shouldBe` []
    -- it "throws an exception if used with an empty list" $ 
      -- evaluate (head []) `shouldThrow` anyException
    
    -- it "unifies" $ do
      -- let male_atom = Atom "male" [Var "X"]
      -- unify male_atom male_atom `shouldBe` Nothing