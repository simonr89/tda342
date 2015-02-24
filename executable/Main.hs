{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Text.Lazy as Lazy (Text, pack, append)
import Replay
import Web.Scotty
import Webforms

form          :: Text -> Int -> Question
form t formid = Question { par = "You typed " `append` t
                         , fields = [ Field "thatfield" "Input?" True
                                    , Field (append "form" (Lazy.pack $ show formid)) "" False
                                    ]
                         }

exampleMonad      :: Text -> Int -> Web Answer
exampleMonad t id = do ans <- ask $ form t id
                       exampleMonad (snd $ head ans) (id + 1)

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb $ exampleMonad "...nothing yet! First time here?" 0
