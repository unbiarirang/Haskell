import qualified APlusB(solution)
import qualified LCM -- 不加小括号，则导入所有在 LCM 模块里定义的函数和类型
import Distance(solution) -- 不qualified，则使⽤ solution 时不加“distance.”
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "APlusB" $ do
        it "returns sum of two integers" $ do
            let aplusb = uncurry APlusB.solution
                input = [(2,2), ((-1),2), (1000, 9999)]
                output = [4, 1, 10999]
            map aplusb input `shouldBe` output 

    describe "LCM" $ do
        it "returns the least common multiple" $ do
            let lcm = uncurry LCM.solution
                input = [(6, 4), (24, 36), (6, 7), (6, 8), (0, 10), (1, 0)]
                output = [12, 72, 42, 24, 0, 0]
            map lcm input `shouldBe` output

    describe "Distance" $ do
        it "returns the distance between two points" $ do
            let temp a b c = solution c b a
                distance = uncurry (uncurry temp)
                input = [(((1,2),(2,3)),1), (((1,2),(2,3)),2), (((1,2),(2,4)),3)]
                output = [2, 1.4142135623730951, 2]
            map distance input `shouldBe` output

--main :: IO ()
--main = do
    --print $ APlusB.solution 2 2 == 4
    --print $ APlusB.solution (-1) 2 == 1
    --print $ LCM.solution 6 4 == 12
    --print $ LCM.solution 24 36 == 72
    --print $ LCM.solution 6 7 == 42
    --print $ LCM.solution 6 8 == 24 
    --print $ solution 1 (1,2) (2,3) == 2 
    --print $ solution 2 (1,2) (2,3) == 1.4142135623730951 
    --print $ solution 3 (1,2) (2,4) == 2
