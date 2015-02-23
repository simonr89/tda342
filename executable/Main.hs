{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Text.Lazy as Lazy (Text, pack, unpack, append)
import Replay
import System.IO.Unsafe
import System.Random
import Web.Scotty
import Webforms



form :: Text -> Text -> Question
form c f = Question { par = "You typed " `append` c
--                  , fields = [ Field "fixedname" "Input?" ]
                    , fields = [ Field f "Input?" ]
                    }

exampleMonad :: Text -> Web Answer
exampleMonad t = do ans <- ask $ form t (Lazy.pack "foo")
                    rep <- ask $ form (head ans) (head ans)
                    ask $ form (Lazy.pack "loop?") (head rep)
                    forever $ ask $ form (Lazy.pack "loop!") (randomise (Lazy.pack "foo"))


randomise :: Text -> Text
randomise t = t `append` Lazy.pack [(unsafePerformIO (randomIO :: IO Char))]


main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb $ exampleMonad (Lazy.pack "...nothing yet! First time here?")
