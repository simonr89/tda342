{-# LANGUAGE OverloadedStrings #-}
module Webforms (Web
                ,runWeb
                ) where

import Data.Monoid
import Data.Text.Lazy
import Replay
import Web.Scotty

type Web a = Replay Question Answer a

type Question = [String]

type Answer = [String]

runWeb   :: Web a -> ActionM ()
runWeb w = do
  i <- getInput
  html (page i)
    where
      getInput :: ActionM Text
      getInput = param "text_input_id" `rescue` \ _ -> return ""

      page :: Text -> Text
      page s = mconcat $
           [ "<html><body>"
           , "<p>Input was: ", s, "</p>"
           , "<form method=post>"
           , "<p>Type something here:</p>"
           , "<input name=text_input_id>"
           , "<input type=submit value=OK>"
           , "</form>"
           , "</body></html>"
           ]
