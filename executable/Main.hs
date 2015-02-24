{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Text.Lazy as Lazy (Text, append)
import Replay
import Web.Scotty
import Webforms

form :: Text -> Question
form t = Question { par = "You typed " `append` t
                  , fields = [ Field "thatfield" "Input?" ]
                  }

exampleMonad :: Text -> Web Answer
exampleMonad t = do ans <- ask $ form t
                    exampleMonad $ snd $ head ans

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb $ exampleMonad "...nothing yet! First time here?"
