{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Reddit.Types where

import Control.Applicative
import Control.Monad
import Data.Aeson          (FromJSON (..), ToJSON, (.:))
import Data.Text           (Text)

newtype DisplayName = DisplayName { unDisplayName :: Text }
    deriving (Show,Read,FromJSON,ToJSON)

newtype Title = Title { unTitle :: Text }
    deriving (Show,Read,FromJSON,ToJSON)

newtype Fullname = Fullname { unFullname :: Text }
    deriving (Show,Read,FromJSON,ToJSON)

data Subreddit = Subreddit
    { displayName :: DisplayName
    , fullname    :: Fullname
    }
    deriving (Show,Read)

data Thing = Thing
    { kind      :: Text
    , thingData :: Subreddit
    }
    deriving (Show,Read)

---------------------------------------------
-- Instancies
---------------------------------------------

instance FromJSON Subreddit where
    parseJSON = parseJSON >=> go
      where
        go o = Subreddit
            <$> (DisplayName <$> (o .: "display_name"))
            <*> (Fullname <$> (o .: "name"))

instance FromJSON Thing where
    parseJSON = parseJSON >=> go
      where
        go o = Thing
            <$> o .: "kind"
            <*> o .: "data"

