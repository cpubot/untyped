import Test.Hspec
import qualified Data.Set as S

import Parser
import Term
import PrettyPrinter
import Evaluator

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    
    it "parses λx.x" $ do
      unsafeRunParse "λx.x" `shouldBe` Lambda 'x' (Var 'x')

    it "parses λx.y" $ do
      unsafeRunParse "λx.y" `shouldBe` Lambda 'x' (Var 'y')
    
    it "handles currying of binding variables" $ do
      unsafeRunParse "λxyz.y" `shouldBe` Lambda 'x' (Lambda 'y' (Lambda 'z' (Var 'y')))

    it "parses (λxyz.xz(yz))(λx.z)(λx.a)" $ do
      unsafeRunParse "(λxyz.xz(yz))(λx.z)(λx.a)" `shouldBe` 
        App (App (Lambda 'x' (Lambda 'y' (Lambda 'z' (App (App (Var 'x') (Var 'z')) (App (Var 'y') (Var 'z')))))) (Lambda 'x' (Var 'z'))) (Lambda 'x' (Var 'a'))
    
    it "parses (λabc.cba)zz(λwv.w)" $ do
      unsafeRunParse "(λabc.cba)zz(λwv.w)" `shouldBe`
        App (App (App (Lambda 'a' (Lambda 'b' (Lambda 'c' (App (App (Var 'c') (Var 'b')) (Var 'a'))))) (Var 'z')) (Var 'z')) (Lambda 'w' (Lambda 'v' (Var 'w')))
  
  describe "PrettyPrinter" $ do
    it "Prints beautiful lambda expressions (uncurried)" $ do
      termToString (unsafeRunParse "(λabc.cba)zz(λwv.w)") `shouldBe` "(λa.λb.λc.cba)zz(λw.λv.w)"
  
  describe "Evaluator" $ do
    describe "Free variables" $ do
      it "derives FV(λx.x) = ∅ " $ do
        fv' (Lambda 'x' (Var 'x')) `shouldBe` (∅)

      it "derives FV(λx.y) = {y}" $ do
        fv' (Lambda 'x' (Var 'y')) `shouldBe` S.fromList ['y']

      it "derives FV((λxyz.xz(yz))(λx.z)(λx.a)) = {z,a}" $ do
        fv' (unsafeRunParse "(λxyz.xz(yz))(λx.z)(λx.a)") `shouldBe` S.fromList ['z','a']

      it "derives FV((λabc.cba)zz(λwv.w)) = {z}" $ do
        fv' (unsafeRunParse "(λabc.cba)zz(λwv.w)") `shouldBe` S.fromList ['z']

    describe "Substitution" $ do
      it "substitutes free occurances of x for N in Λ" $ do
        let
          n = Lambda 'w' (Var 'w')
          λ = Lambda 'y' (Var 'x')
        σ n 'x' λ `shouldBe` Lambda 'y' n
    
      it "does not subtitute y for N when y ∉ FV(P)" $ do
        let
          n = Lambda 'w' (Var 'w')
          λ = Lambda 'y' (Var 'q')
        σ n 'x' λ `shouldBe` λ
      
      it "renames y when y ∈ FV(N)" $ do
        let
          n = Lambda 'w' (Var 'y')
          λ = Lambda 'y' (App (Var 'y') (Var 'x'))
        σ n 'x' λ `shouldBe` Lambda 'z' (App (Var 'z') n)

    describe "Redex" $ do
      it "identifies (λx.x)z as a redex" $ do
        isRedex (App (Lambda 'x' (Var 'x')) (Var 'z'))
      
      it "identifies λx.(λy.y)z as containing a redex" $ do
        isRedex (Lambda 'x' (App (Lambda 'y' (Var 'y')) (Var 'z')))
      
      it "identifies λx.x as not containing a redex" $ do
        not $ isRedex (Lambda 'x' (Var 'x'))

      it "identifies x as not containing a redex" $ do
        not $ isRedex (Var 'x')
      
      it "identifies xz as not containing a redex" $ do
        not $ isRedex (App (Var 'x') (Var 'z'))

    describe "β-reduction" $ do
      it "reduces (λx.x)z to β-normal-form, z" $ do
        βnf (App (Lambda 'x' (Var 'x')) (Var 'z')) `shouldBe` Var 'z'
      
      it "does not reduce λx.x" $ do
        βnf (Lambda 'x' (Var 'x')) `shouldBe` Lambda 'x' (Var 'x')

      it "does not reduce xz" $ do
        βnf (App (Var 'x') (Var 'z')) `shouldBe` App (Var 'x') (Var 'z')
      
      it "reduces (λabc.cba)zz(λwv.w) to β-normal-form, z" $ do
        βnf (unsafeRunParse "(λabc.cba)zz(λwv.w)") `shouldBe` Var 'z'

      it "reduces (λxyz.xz(yz))(λx.z)(λx.a) to β-normal-form, λy.za" $ do
        βnf (unsafeRunParse "(λxyz.xz(yz))(λx.z)(λx.a)") `shouldBe` Lambda 'y' (App (Var 'z') (Var 'a'))

      it "reduces (λa.aa)(λb.ba)c to β-normal-form, aac" $ do
        βnf (unsafeRunParse "(λa.aa)(λb.ba)c") `shouldBe` App (App (Var 'a') (Var 'a')) (Var 'c')
  
    describe "Equivalence" $ do
      describe "α-equivalence" $ do
        it "determines α-equivalence of (λa.a) and (λx.x)" $ do
          αEquiv (Lambda 'a' (Var 'a')) (Lambda 'x' (Var 'x'))
        
        it "determines α-equivalence of (λx.xyz) and (λw.wyz)" $ do
          αEquiv 
            (Lambda 'x' (App (App (Var 'x') (Var 'y')) (Var 'z')))
            (Lambda 'w' (App (App (Var 'w') (Var 'y')) (Var 'z')))

      describe "η-equivalence" $ do
        it "determines (λa.a)z ≡ z" $ do
          App (Lambda 'a' (Var 'a')) (Var 'z') ≡ Var 'z'
        
        it "determines (λabc.cba)zz(λwv.w) ≡ (λa.a)z" $ do
          unsafeRunParse "(λabc.cba)zz(λwv.w)" ≡ App (Lambda 'a' (Var 'a')) (Var 'z')
        
        it "determines (λz.z)(λz.zz)(λz.zy) ≡ (λx.λy.xyy)(λy.y)y" $ do
          unsafeRunParse "(λz.z)(λz.zz)(λz.zy)" ≡ unsafeRunParse "(λx.λy.xyy)(λy.y)y"