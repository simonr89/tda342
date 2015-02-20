{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Webforms
    ( Web
    , Field(..)
    , Question
    , Answer
    , runWeb
    ) where

import Control.Exception (try)
import Control.Monad.IO.Class
import Data.ByteString.Base64 as Base64 (encode, decode) 
import Data.ByteString.Char8 as Char8 (pack, unpack)
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy as Lazy (Text, pack, unpack, append, fromChunks)
import Text.Read as Read (read)
import Replay
import System.IO.Unsafe
import Web.Scotty
import Web.Scotty.Trans (ScottyError)

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
  play trace
  where
    play t = do
      r <- liftIO $ run w t
      case r of 
        Left (q, t') -> do
          answers <- mapM (maybeParam . ident) q
          --sendForm q $ addAnswer (catMaybes answers) t'
          liftIO $ putStrLn (show answers)
          if any (==Nothing) answers 
            then sendForm q t'
            else play $ addAnswer (catMaybes answers) t 
        Right x -> return ()
      where unsafeMaybeParam :: Text -> ActionM (Maybe Text)
            unsafeMaybeParam x = do liftIO $ print x
                                    t <- param x
                                    liftIO $ print (x,t)
                                    return (Just t)
            maybeParam :: Text -> ActionM (Maybe Text)                        
            maybeParam x = unsafeMaybeParam x `rescue` (\_ -> return Nothing)
{- do
  input <- param "trace" `rescue` (\_ -> return "")
  let trace = case decodeTrace input of
                Nothing -> emptyTrace
                Just t -> t
  r <- liftIO $ run w trace
  case r of
    Left (q, t') -> sendForm q (addAnswer ([Lazy.pack "**added by runWeb**"]) t')
    Right x -> return ()
-}
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
hiddenFields t = "<input type=hidden name=trace value=\"" `append` encodeTrace t `append` "\" />\n"
