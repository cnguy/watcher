{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as NHC
import System.Environment (getEnv)

import Configuration.Dotenv (loadFile, defaultConfig)
import Discord
import qualified Network.Wreq as Wreq
import Twilio
import Twilio.Calls as Calls
import Twilio.Messages

-- | Generic, blocking loop function that runs forever.
loop value f = forever $ do
  f
  CC.threadDelay value

class Num a => ThreadSeconds a where
  loopInfinitely :: a -> IO () -> IO ()

instance ThreadSeconds Int where
  loopInfinitely seconds f = loop (seconds * 10 ^ 6) f

instance ThreadSeconds Double where
  loopInfinitely seconds f = loop (round $ seconds * 10 ** 6) f

-- | Pings some URL without throwing an exception.
ping :: (String) -> IO ()
ping url = do
  r <- Wreq.getWith opts url
  putStrLn $ url ++ ": " ++ (T.unpack $ TE.decodeUtf8 $ (r ^. Wreq.responseHeader "Date"))
  where
    -- `opts` allows us to ignore suppress exceptions and grab status codes instead.
    opts = set Wreq.checkResponse (Just $ \_ _ -> return ()) Wreq.defaults

-- | Blocks the program and continually makes the Discord bot wait
-- for a user to type "ping".
waitForDiscordPing :: (RestChan, Gateway, z) -> IO ()
waitForDiscordPing discord = do
    e <- nextEvent discord
    case e of
      Left error -> putStrLn ("Event error: " <> show error)
      Right (MessageCreate m) -> do
        when (isPing (messageText m) && not (fromBot m)) $ do
          resp <- restCall discord (CreateMessage (messageChannel m) "Pong!")
          return ()
        waitForDiscordPing discord
      _ -> waitForDiscordPing discord

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing m = (== "ping") $ (T.map toLower m)

postMessageNoOpts to from message = PostMessage to from message Nothing

send to from = runTwilio' (getEnv "TWILIO_ACCOUNT_SID") (getEnv "TWILIO_AUTH_TOKEN") $ do
  let warn = postMessageNoOpts to from
  let body = warn "yabai"
  message <- Twilio.Messages.post body
  return ()

getEnvAndPack :: String -> IO T.Text
getEnvAndPack env = do
  value <- getEnv env
  let text = T.pack(value)
  return text

-- | Calls `send` and cleans up the Discord instance.
-- Generally, this is used to clean up resources in the case
-- of exceptions.
die discord = do
  putStrLn "die"
  to <- getEnvAndPack "TWILIO_TO_PHONE_NUMBER"
  from <- getEnvAndPack "TWILIO_FROM_PHONE_NUMBER"
  send to from
  stopDiscord discord

main = do
  token <- T.strip <$> TIO.readFile "./auth-token.secret"
  loadFile defaultConfig
  discord <- loginRestGateway (Auth token)
  let urls = ["https://zottrail.xyz/api/v1/ok", "https://zottrail.xyz/api/v1/okk"]
  CC.forkIO $ (waitForDiscordPing discord)
  E.finally (loopInfinitely (10 :: Int) (mapM_ ping urls)) (die discord)