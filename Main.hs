{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import Control.Lens
import qualified Control.Monad as CM
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as NHC

import Discord
import qualified Network.Wreq as Wreq

loop value f = CM.forever $ do
  f
  CC.threadDelay value

class Num a => ThreadSeconds a where
  loopInfinitely :: a -> IO () -> IO ()

instance ThreadSeconds Int where
  loopInfinitely seconds f = loop (seconds * 10 ^ 6) f

instance ThreadSeconds Double where
  loopInfinitely seconds f = loop (round $ seconds * 10 ** 6) f

ping url = do
  r <- Wreq.getWith opts url
  putStrLn $ url ++ ": " ++ (T.unpack $ TE.decodeUtf8 $ (r ^. Wreq.responseHeader "Date"))
  where
    -- `opts` allows us to ignore suppress exceptions and grab status codes instead.
    opts = set Wreq.checkResponse (Just $ \_ _ -> return ()) Wreq.defaults

main :: IO ()
main = do
  token <- T.strip <$> TIO.readFile "./auth-token.secret"
  discord <- loginRest (Auth token)
  let urls = ["https://zottrail.xyz/api/v1/ok", "https://zottrail.xyz/api/v1/okk"]
  E.finally (loopInfinitely (25 :: Int) (mapM_ ping urls)) (stopDiscord discord)