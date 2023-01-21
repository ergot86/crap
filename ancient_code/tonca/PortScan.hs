module PortScan where
import System.IO
import Network
import Control.Monad

data ScannedPort = ScannedPort {
      portNumber :: Int,
      isPortOpen :: Bool
    } deriving Show

type ScannedPorts = [ScannedPort]

portScan :: String -> [Int] -> IO ScannedPorts
portScan target = mapM (\x -> f x `catch` \_ -> return $ ScannedPort x False)
    where
      f x = do
        connectTo target (PortNumber $ fromIntegral x) >>= hClose
        return $ ScannedPort x True

openPorts :: ScannedPorts -> [Int]
openPorts = map portNumber . filter isPortOpen

scanOpenPorts :: String -> [Int] -> IO [Int]
scanOpenPorts target = portScan target >=> return . openPorts
