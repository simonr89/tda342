{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Webforms

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb undefined
