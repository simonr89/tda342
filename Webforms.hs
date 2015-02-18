{-# LANGUAGE OverloadedStrings #-}
module Webforms (Web
                ,runWeb
                ) where

import Control.Monad.IO.Class
import Data.Text.Lazy
import Replay
import Web.Scotty

type Web a = Replay Question Answer a

type Question = [Field]

type Answer = [Text]

data Field = Field { id :: Text
                   , description :: Text
                   }

encodeTrace   :: Trace Answer -> Text
encodeTrace t = error "undefined"

decodeTrace   :: Text -> Maybe (Trace Answer)
decodeTrace t = error "undefined"

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

