{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import qualified Control.Monad as CM
import Control.Lens
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as NHC

import Discord
import qualified Network.Wreq as Wreq

setInterval :: Double -> IO () -> IO ()
setInterval seconds fn = CM.forever $ do
  fn 
  CC.threadDelay (round $ seconds * 10 ** 6)

ping discord url = do
  let channelId = 562112322185723904
  chan <- restCall discord (GetChannel channelId)
  r <- Wreq.getWith opts url
  putStrLn $ T.unpack $ TE.decodeUtf8 $ (r ^. Wreq.responseHeader "Date")
  where
    -- `opts` allows us to ignore suppress exceptions and grab status codes instead.
    opts = set Wreq.checkResponse (Just $ \_ _ -> return ()) Wreq.defaults

main :: IO ()
main = do
  token <- T.strip <$> TIO.readFile "./auth-token.secret"
  let url = "https://zottrail.xyz/api/v1/ok"
  discord <- loginRest (Auth token)
  setInterval 25.0 (ping discord url)
  stopDiscord discord