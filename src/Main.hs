{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod
import Prelude
import Data.Aeson()
import GHC.Generics
-- import Data.Text ( Text )
import Data.Text.Lazy
-- import System.Environment ( getArgs )
-- import Data.Text.Lazy.IO ( writeFile )
import Data.Aeson.Text (encodeToLazyText)

-- project imports
import Parser ( parseProgram )

data SourceFile
   = SourceFile
     {
         filename :: String,
         content :: String
     }
     deriving ( Generic, ToJSON, FromJSON )

data App = App

mkYesod "App" [parseRoutes|
/to/dhscanner/ast HomeR POST
|]

instance Yesod App

postHomeR :: Handler Value
postHomeR = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case parseProgram (filename src) (content src) of
        Left errorMsg -> returnJson (pack errorMsg)
        Right ast -> returnJson ast

main :: IO ()
main = warp 3000 App
