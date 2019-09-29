import FRP.BearRiver as B
import FRP.Yampa as Y
import Control.Concurrent
  
type Pos = Double
type Vel = Double

fallingBall :: Pos -> Y.SF () (Pos, Vel)
fallingBall y0 = (constant (-9.81) >>> integral) >>> ((integral >>^ (+ y0)) &&& identity)

main :: IO ()
main =
  reactimate (return ())
                  (\ _ -> threadDelay 100000 >> return (0.1, Nothing))
                  (\ _  -> render)
                  (fallingBall 10.0)

render (pos, vel) = putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel) >> return False
