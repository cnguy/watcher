{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import Control.Lens
import qualified Control.Monad as CM
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
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

ping :: (String) -> IO ()
ping url = do
  r <- Wreq.getWith opts url
  putStrLn $ url ++ ": " ++ (T.unpack $ TE.decodeUtf8 $ (r ^. Wreq.responseHeader "Date"))
  where
    -- `opts` allows us to ignore suppress exceptions and grab status codes instead.
    opts = set Wreq.checkResponse (Just $ \_ _ -> return ()) Wreq.defaults

waitForDiscordPing :: (RestChan, Gateway, z) -> IO ()
waitForDiscordPing discord = do
    e <- nextEvent discord
    case e of
      Left error -> putStrLn ("Event error: " <> show error)
      Right (MessageCreate m) -> do
        CM.when (isPing (messageText m) && not (fromBot m)) $ do
          resp <- restCall discord (CreateMessage (messageChannel m) "Pong!")
          return ()
        waitForDiscordPing discord
      _ -> waitForDiscordPing discord

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing m = (== "ping") $ (T.map toLower m)

main = do
  token <- T.strip <$> TIO.readFile "./auth-token.secret"
  discord <- loginRestGateway (Auth token)
  let urls = ["https://zottrail.xyz/api/v1/ok", "https://zottrail.xyz/api/v1/okk"]
  CC.forkIO $ (waitForDiscordPing discord)
  E.finally (loopInfinitely (25 :: Int) (mapM_ ping urls)) (stopDiscord discord)