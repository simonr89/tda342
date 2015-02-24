{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Webforms
    ( Web
    , Field(..)
    , Question(..)
    , Answer
    , runWeb
    ) where

import Control.Monad.IO.Class
import Data.ByteString.Base64 as Base64 (encode, decode) 
import Data.ByteString.Char8 as Char8 (pack, unpack)
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy as Lazy (Text, pack, unpack, append)
import Text.Read as Read (read)
import Replay
import System.IO.Unsafe
import Web.Scotty
import Web.Scotty.Trans (ScottyError)

type Web a = Replay Question Answer a

data Question = Question { par :: Text       -- ^ some descriptive text
                         , fields :: [Field] -- ^ the fields
                         } 
                deriving (Show,Read)

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
  play trace
  where
    play t = do
      r <- liftIO $ run w t
      case r of 
        Left (q, t') -> do
          answers <- mapM (maybeParam . ident) $ fields q
          if Nothing `elem` answers
            then sendForm q t'
            else play $ addAnswer (catMaybes answers) t 
        Right x -> return ()

-- A more convenient way to get parameters, with Maybe rather than an exception
maybeParam :: Text -> ActionM (Maybe Text)                        
maybeParam x = (param x >>= return . Just) `rescue` (\_ -> return Nothing)

sendForm :: Question -> Trace Answer -> ActionM ()
sendForm q t = html $
             "<html><body><p>" `append`
             par q `append`
             "</p><form method=post />\n" `append`
             hiddenTraceField t `append`
             mconcat (map printField $ fields q) `append`
             "<input type=submit value=OK /></form></body></html>\n"

printField   :: Field -> Text
printField f =
    "<p>" `append` description f `append` "</p><input name=" `append` ident f `append` " />\n"

hiddenTraceField   :: Trace Answer -> Text
hiddenTraceField t =
    "<input type=hidden name=trace value=\"" `append` encodeTrace t `append` "\" />\n"
