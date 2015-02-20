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

secondPage :: Question
secondPage =
    [ Field "catname" "What's the name of your cat?"
    , Field "havecat" "What do you mean you don't have a cat?"
    ]

exampleMonad :: Web Answer
exampleMonad = do first <- ask exampleForm
                  second <- ask secondPage
                  return second

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb exampleMonad
