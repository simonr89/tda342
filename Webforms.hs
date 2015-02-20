{-# LANGUAGE OverloadedStrings #-}
module Webforms
    ( Web
    , Field(..)
    , Question
    , Answer
    , runWeb
    ) where

import Control.Monad.IO.Class
import Data.ByteString.Base64 as Base64 (encode, decode) 
import Data.ByteString.Char8 as Char8 (pack, unpack)
import Data.Monoid
import Data.Text.Lazy as Lazy (Text, pack, unpack, append)
import Replay
import Web.Scotty

type Web a = Replay Question Answer a

type Question = [Field]

type Answer = [Text]

data Field = Field { ident :: Text
                   , description :: Text
                   }
             deriving (Show, Read)

-- TODO deal with line break interspersing
encodeTrace :: Trace Answer -> Text
encodeTrace = Lazy.pack . Char8.unpack . Base64.encode . Char8.pack . show

maybeRead     :: String -> Maybe (Trace Answer)
maybeRead str = case reads str of
                 [(x,"")] -> Just x
                 _ -> Nothing            

decodeTrace   :: Text -> Maybe (Trace Answer)
decodeTrace t = case Base64.decode $ Char8.pack $ Lazy.unpack t of
                  Left _ -> Nothing
                  Right bstr -> maybeRead $ Char8.unpack bstr

-- not 100% sure what the argument type in Web ? should be
runWeb   :: Web Answer -> ActionM ()
runWeb w = do
  input <- param "trace" `rescue` (\_ -> return "")
  let trace = case decodeTrace input of
                Nothing -> emptyTrace
                Just t -> t
  r <- liftIO $ run w trace
  case r of
    Left (q, t') -> sendForm q
    Right x -> return ()

sendForm :: Question -> ActionM ()
sendForm q = html $
             "<html><body><form method=post>" `append`
             mconcat (map printField q) `append`
             "<input type=submit value=OK></form></body></html>"

printField   :: Field -> Text
printField f =
    "<p>" `append` (description f) `append` "</p><input name=" `append` (ident f) `append` ">"
