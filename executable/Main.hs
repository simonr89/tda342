{-# LANGUAGE OverloadedStrings #-}
module Main where

import Replay
import Web.Scotty
import Webforms

exampleForm :: Question
exampleForm =
    [ Field "lastname" "Last name?"
    , Field "firstname" "First name?"
    ]

exampleMonad :: Web Answer
exampleMonad = ask exampleForm

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb exampleMonad
