import Prelude     (IO, putStrLn)
import Application (appMain)

main :: IO ()
main = do
  putStrLn "Started"
  appMain
