import Graphics.Gloss
import AI.HNN.FF.Network
import Control.Arrow ((&&&))
import Numeric.LinearAlgebra

trainingSet :: Samples Float
trainingSet = [ (fromList [0, 0], fromList [0])
              , (fromList [0, 1], fromList [1])
              , (fromList [1, 0], fromList [1])
              , (fromList [1, 1], fromList [0]) ]

samples :: [Numeric.LinearAlgebra.Vector Float]
samples = map fst trainingSet
       ++ [ fromList [x, y] | x <- [0.0, 0.025..1], y <- [0.0, 0.025..1] ]

toRepr :: (Numeric.LinearAlgebra.Vector Float, Numeric.LinearAlgebra.Vector Float) -> (Float, Float, Color)
toRepr (inp, out) = ( (inp @> 0)*600 - 300, (inp @> 1)*600 - 300, if (out @> 0) >= 0.5 then red else blue)

toPicture :: (Float, Float, Color) -> Picture
toPicture (x, y, color) = Color color $ 
                            Translate x (y-100) $
                              circleSolid 5

main :: IO ()
main = do
  n <- createNetwork 2 [2] 1
  let n' = trainNTimes 1000 0.8 tanh tanh' n trainingSet
  let outputs = map (id &&& output n' tanh) samples
  let prePics = map toRepr outputs
  let pictures = Pictures $ map toPicture prePics ++ header
  display (InWindow "hnn HEAD meets gloss on XOR" (700,700) (0,0)) white pictures

header :: [Picture]
header = [ Translate (-300) 310 $ Scale 0.2 0.2 $ Text "hnn feed-forward net, trained to simulate XOR"
         , Translate (-250) 280 $ Color red $ circleSolid 5
         , Translate (-200) 280 $ Scale 0.1 0.1 $ Text "x `xor` y near 1"
         , Translate 50 280 $ Color blue $ circleSolid 5
         , Translate 100 280 $ Scale 0.1 0.1 $ Text "x `xor` y near 0" 
         ]
