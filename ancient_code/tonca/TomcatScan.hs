module TomcatScan where
import PortScan
import Network.HTTP.EnumeratorPatched
import Network.HTTP.Types (Ascii, RequestHeaders, methodHead)
import Network.TLS
import Data.Maybe
import Data.List
import Data.Either
import Data.Function
import Data.String.Utils (join)
import Data.CaseInsensitive (CI, mk)
import Data.ByteString.Char8 (unpack, pack)
import Control.Monad ((>=>), liftM)
import Control.Arrow
import qualified Control.Exception as E

-- common Tomcat and JBoss ports
webServerPortList :: [Int]
webServerPortList = [80, 81, 8080, 8081, 11080, 11081]

webServerPortListSSL :: [Int]
webServerPortListSSL = [443, 447, 8443, 8447, 11443, 11447]

portList :: [Int]
portList =
    [
     1090, 1091,               -- JMX
     1093, 11093,              -- JMS
     1098, 1099, 11098, 11099, -- JBoss Naming Service
     1701, 11701,              -- HSQLDB
     4444, 14444,              -- JBoss invoker jrmp
     4445, 14445,              -- JBoss invoker pooled
     4447,                     -- remoting
     4712, 4713, 4714,         -- TXN
     5445, 5455,               -- messaging
     8005, 11000, 11005,       -- Shutdown
     8007, 11007,              -- AJP12
     8009, 11009,              -- AJP13
     8090,                     -- osgi-http
     9990,                     -- http-api
     9999                      -- native-api
    ]

-- Common signatures in Tomcat headers
tcHeaders :: [CI Ascii]
tcHeaders = map (mk . pack) ["Server", "X-Powered-By", "Set-Cookie", "Location"]

tcSignatures :: [String]
tcSignatures =
    [
     "Coyote", "Tomcat", "JBoss", "mod_jk", "CJServer", "Servlet", "JSESSIONID",
     ".jsp", ".do", ".action", ".seam"
    ]

-- set custom headers for request here
myHeaders :: RequestHeaders
myHeaders = map (mk . pack *** pack)
            [
             ("Connection", "keep-alive"),
             ("Cache-Control", "max-age=0"),
             ("User-Agent",
              "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.11 " ++
              "(KHTML, like Gecko) Chrome/17.0.963.83 Safari/535.11"),
             ("Accept",
              "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
             ("Accept-Language", "en-US,en;q=0.8"),
             ("Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.3")
            ]

tomcatScan :: String -> IO (Maybe String)
tomcatScan target = webServerScan target >>= f
    where
      f [] = return Nothing
      f xs = liftM (Just . join "\n" . (: xs) . g) (scanOpenPorts target portList)
      g [] = []
      g x  = target ++ "\tOpen Ports: " ++ show x

webServerScan :: String -> IO [String]
webServerScan = mapM ((`E.catch` (return . Left)) . headUrl) . urlList >=>
                return . mapMaybe detectServer . rights
    where
      urlList a = map (\x -> "http://"  ++ a ++ ":" ++ show x) webServerPortList ++
                  map (\x -> "https://" ++ a ++ ":" ++ show x) webServerPortListSSL

buildRequest :: Request m -> Request m
buildRequest a = a{
                   method = methodHead,
                   checkCerts = \_ _ -> return CertificateUsageAccept,
                   requestHeaders = unionBy ((==) `on` fst) myHeaders $ requestHeaders a
                 }

headUrl :: String -> IO (Either E.SomeException (String, Response))
headUrl url = parseUrl url >>= withManager . httpLbs . buildRequest >>=
              return . Right . (,) url

detectServer :: (String, Response) -> Maybe String
detectServer (a, b)
    | any (`isInfixOf` f) tcSignatures = Just $ a ++ "|" ++ f
    | otherwise = Nothing
    where
      f = join "|" $ map unpack $ mapMaybe (`lookup` responseHeaders b) tcHeaders