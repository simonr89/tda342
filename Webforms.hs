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
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy as Lazy (Text, pack, unpack, append, fromChunks)
import Text.Read as Read (read)
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

exampleTrace = addAnswer [Lazy.pack "Inari"] $
               emptyTrace 

-- not 100% sure what the argument type in Web ? should be
runWeb   :: Web Answer -> ActionM ()
runWeb w = do
  input <- param "trace" `rescue` (\_ -> return "")
  let trace = case decodeTrace input of
                Nothing -> exampleTrace --With this starts from the second page
--                Nothing -> emptyTrace
                Just t -> t
  r <- liftIO $ run w trace
  case r of
    Left (q, t') -> sendForm q (addAnswer ([Lazy.pack "this value added by runWeb"]) t')
    Right x -> return ()

sendForm :: Question -> Trace Answer -> ActionM ()
sendForm q t = html $
             "<html><body><form method=get />\n" `append`
             hiddenFields t `append`
             mconcat (map printField q) `append`
             "<input type=submit value=OK /></form></body></html>\n"

printField   :: Field -> Text
printField f =
    "<p>" `append` (description f) `append` "</p>\t<input name=" `append` (ident f) `append` "/>\n"

hiddenFields :: Trace Answer -> Text
hiddenFields t = "<input type=hidden name=HIDDEN value=\"" `append` values `append` "\" />\n"
  where values = foldr1 append (concat $ getAnswers t)
