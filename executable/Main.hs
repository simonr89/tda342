{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Map
import Data.Text.Lazy as Text (Text, pack, unpack, append, split)
import Replay
import System.Process
import Web.Scotty
import Webforms

form          :: Text -> Int -> Question
form t formid = Question { par = "Available commands are: ls, cd, pwd and cat\n\n" `append` t
                         , fields = [ Field "cmd" "Command?" True
                                    , Field (append "form" (Text.pack $ show formid)) "" False
                                    ]
                         }

exampleMonad      :: Text -> Int -> Web Answer
exampleMonad t id = do ans <- ask $ form t id
                       let cmd = Data.Map.findWithDefault "" "cmd" ans
                           str = case Text.split (==' ') cmd of
                                   [] -> ""
                                   v:_ -> Text.unpack v
                       res <- io $ readProcess str [] []
                       exampleMonad (Text.pack res) (id + 1)

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb $ exampleMonad "" 0
