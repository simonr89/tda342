{-# LANGUAGE OverloadedStrings #-}
module Main where

import Replay
import Web.Scotty
import Webforms

exampleForm :: Question
exampleForm = Question { par = "hello"
                       , fields = [ Field "lastname" "Last name?"
                                  , Field "firstname" "First name?"
                                  ]
                       }

secondPage :: Question
secondPage = Question { par = "you again?"
                      , fields = [ Field "catname" "What's the name of your cat?"
                                 , Field "havecat" "What do you mean you don't have a cat?"
                                 ]
                      }

exampleMonad :: Web Answer
exampleMonad = do ask exampleForm
                  ask secondPage

                  

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb exampleMonad
