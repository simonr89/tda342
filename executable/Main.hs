{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Char (isSpace)
import Data.Map (findWithDefault)
import Data.Text.Lazy as Text (Text, pack, unpack, append, split)
import Replay
import System.Process
import Web.Scotty
import Webforms


main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb $ exampleMonad "" 0

--------------------------------------------------------------------------------

form          :: Text -> Int -> Question
form t formid = Question { par = "Available commands are: " `append` 
                                 Text.pack (joinText commands) `append` 
                                 "<br/><br/>" `append` t
                         , fields = [ Field "cmd" "Command?" True
                                    , Field (append "form" (Text.pack $ show formid)) "" False
                                    ]
                         }

exampleMonad      :: Text -> Int -> Web Answer
exampleMonad t id = do ans <- ask $ form t id
                       let cmd = Data.Map.findWithDefault "" "cmd" ans
                           (str,args) = case words (Text.unpack cmd) of
                                   []   -> ("", [])
                                   [c]  -> (checkSanity c, [])
                                   c:a  -> (checkSanity c, a)
                       res <- io $ readProcess str args []
                       exampleMonad (Text.pack res) (id + 1)



--------------------------------------------------------------------------------


commands = ["ls", "cd", "pwd", "cat", "grep"]

checkSanity :: String -> String
checkSanity c | c `elem` commands = c
              | c == "rm"         = error "you're a funny one"
              | otherwise         = error "command not allowed"                    

joinText [] =  ""
joinText ws =  foldr1 (\w s -> w ++ ',':' ':s) ws

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace