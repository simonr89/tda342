{-# LANGUAGE OverloadedStrings #-}
module Webforms (Web
                ,runWeb
                ) where

import Control.Monad.IO.Class
import Data.ByteString.Base64 as Base64
import Data.ByteString.Char8 as Char8
import Data.Text.Encoding as Encoding
import Data.Text.Lazy as Lazy
import Replay
import Web.Scotty

type Web a = Replay Question Answer a

type Question = [Field]

type Answer = [Text]

data Field = Field { id :: Text
                   , description :: Text
                   }
             deriving (Show, Read)

-- TODO deal with line break interspersing
encodeTrace :: Trace Answer -> Text
encodeTrace = Lazy.pack . Char8.unpack . Base64.encode . Char8.pack . show

decodeTrace   :: Text -> Maybe (Trace Answer)
decodeTrace t = case Base64.decode $ Char8.pack $ Lazy.unpack t of
                  Left _err -> Nothing
                  Right s -> Just $ read $ Char8.unpack s

-- (Trace Answer -> IO ((Either Question ()), Trace Answer) -> ActionM ()
runWeb   :: Web () -> ActionM ()
runWeb w = do
  input <- param "trace" `rescue` (\_ -> return "")
  let t = case decodeTrace input of
            Nothing -> emptyTrace
            Just t -> t
  r <- liftIO $ run w t
  case r of
    Left (q, t') -> sendForm q
    Right x -> return x

sendForm :: Question -> ActionM ()
sendForm q = html "<html><body></body></html>"

