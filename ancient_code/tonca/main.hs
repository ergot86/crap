import TomcatScan
import System
import Data.List
import Data.List.Split
import Control.Concurrent 
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad

data ScanStatus = Scanning | Scanned | NotScanned
                  deriving Eq

data IpRange = IpRange {
      ipRange :: (Int, Int),
      rangeStatus :: ScanStatus
    } deriving Eq

main :: IO ()
main = do
  (a:b:c:_) <- getArgs
  lock <- newMVar ()
  tv   <- newTVarIO $ map (`IpRange` NotScanned) (ipSubranges a b $ workers c * 4)
  replicateM_ (workers c) (forkIO $ scanWorker (lock, tv))
  atomically $ waitWorkers tv
    where
      workers n = read n :: Int

ip4ToInt :: String -> Int
ip4ToInt = sum . snd . mapAccumR (\acc x -> (acc + 1, x * (2 ^ (8 * acc :: Int)))) 0 .
           map read . splitOn "."
    
intToIp4 :: Int -> String
intToIp4 = intercalate "." . map show . reverse . take 4 .
           unfoldr (\x -> Just (x `rem` 256, x `div` 256))

ipSubranges :: String -> String -> Int -> [(Int, Int)]
ipSubranges a b c = f (ip4ToInt a) c
    where
      ipsPerRange = (ip4ToInt b - ip4ToInt a) `div` c
      f ip 0 = [(ip, ip4ToInt b)]
      f ip n = (ip, ip + ipsPerRange - 1) : f (ip + ipsPerRange) (n - 1)

waitWorkers :: TVar [IpRange] -> STM ()
waitWorkers = readTVar >=> f
    where
      f a
        | all ((Scanned ==) . rangeStatus) a = return ()
        | otherwise = retry 

scanWorker :: (MVar (), TVar [IpRange]) -> IO ()
scanWorker (lock, tv) = atomically (pickRange tv) >>= maybe (return ()) f
    where
      f a@IpRange{ ipRange = (b, c) } = do
        mapM_ (tomcatScan . intToIp4 >=> atomicPrint lock) [b .. c]
        atomically $ doneRange tv a
        scanWorker (lock, tv)

pickRange :: TVar [IpRange] -> STM (Maybe IpRange)
pickRange tv = readTVar tv >>= f `ap` find ((NotScanned ==) . rangeStatus)
    where
      f _ Nothing = return Nothing
      f xs (Just x) = do
                 writeTVar tv $ x{ rangeStatus = Scanning } : delete x xs
                 return $ Just x{ rangeStatus = Scanning }

doneRange :: TVar [IpRange] -> IpRange -> STM ()
doneRange tv x = readTVar tv >>= writeTVar tv . (x{ rangeStatus = Scanned }:) . delete x

atomicPrint :: MVar () -> Maybe String -> IO ()
atomicPrint a = maybe (return ()) (withMVar a . const . putStrLn)





