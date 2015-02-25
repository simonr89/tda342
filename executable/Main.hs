{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
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

form       :: Text -> Int -> Question
form t fid = Question { par = "Available commands are: " `append` 
                              Text.pack (joinText (sort cmds)) `append` 
                              " (with flags)<br/><br/>" `append` t                                         , fields = [ Field "cmd" "Command?" True
                                 , Field (append "form" (Text.pack $ show fid)) "" False
                                 ]
                      }

exampleMonad      :: Text -> Int -> Web Answer
exampleMonad t id = do ans <- ask $ form t id
                       let cmd = Data.Map.findWithDefault "" "cmd" ans
                           (str,args) = case words (Text.unpack cmd) of
                                   []  -> ("", [])
                                   [c] -> checkSanity (c:[])
                                   c   -> checkSanity c
                       res <- io $ readProcess str args []
                       exampleMonad (Text.pack $ "<pre>"++res++"</pre>") (id + 1)



--------------------------------------------------------------------------------


cmds = ["ls", "cd", "pwd", "cat", "grep", "wget", "echo", "xargs", "git"]

checkSanity :: [String] -> (String,[String])
checkSanity (c:as) | c `elem` cmds      = (c,as)
                   | "rm" `elem` (c:as) = ("echo", ["you're a funny one"])
                   | otherwise          = ("echo", ["command `" ++inp++"' not allowed"])
  where inp = unwords (c:as)

joinText    :: [String] -> String
joinText ws = foldr1 (\w s -> w++", "++s) ws

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

