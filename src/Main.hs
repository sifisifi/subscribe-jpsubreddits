module Main where

import           Control.Applicative
import           Control.Concurrent       (threadDelay)
import           Control.Exception.Lifted
import qualified Data.Aeson               as A
import qualified Data.ByteString.Char8    as BS
import           Data.Conduit             (($$))
import qualified Data.Conduit             as C
import qualified Data.Conduit.List        as CL
import           Data.Maybe               (catMaybes)
import qualified Data.Text                as T
import qualified JPSubreddits.Types       as JPSS
import           Network.HTTP.Conduit     (simpleHttp)
import qualified Reddit.Types             as R
import qualified Subscriber.Generate      as SG

main :: IO ()
main = do
    ejpss <- getJPSubreddits
    case ejpss of
        Right jpss -> go jpss
        Left err -> putStrLn err
  where
    go jpss = do
        ss <- getSubredditInfo =<< flattenJPSubreddits jpss
        BS.writeFile "subscribe-jpsubreddits.js" $ SG.generate ss

getJPSubreddits :: IO (Either String JPSS.JPSubreddits)
getJPSubreddits = A.eitherDecode <$> simpleHttp "http://rv.sifisifi.space/reddit/jpsubreddits.json"

sourceJPSubreddits :: JPSS.JPSubreddits -> C.Source IO JPSS.Subreddit
sourceJPSubreddits = go . JPSS.categories
  where
    go = mapM_ yieldSubreddit
    yieldSubreddit = mapM_ C.yield . JPSS.subreddits

flattenJPSubreddits :: JPSS.JPSubreddits -> IO [JPSS.Subreddit]
flattenJPSubreddits jpss = sourceJPSubreddits jpss $$ CL.consume

getSubredditInfo :: [JPSS.Subreddit] -> IO [R.Subreddit]
getSubredditInfo xs = catMaybes <$> mapM (f 5) xs
  where
    f :: Int -> JPSS.Subreddit -> IO (Maybe R.Subreddit)
    f n x
        | n > 0 = go x `catch` errHandler n x
        | otherwise = return Nothing

    go jps = do
        print jps
        let url = concat
                    [ "http://www.reddit.com"
                    , T.unpack $ JPSS.unUrl $ JPSS.url jps
                    , "/about.json"
                    ]
        threadDelay $ milliseconds 1000
        ((R.thingData <$>) . A.decode) <$> simpleHttp url

    errHandler n jps (SomeException e) = do
        print e
        f (n - 1) jps

-- | マイクロ秒からミリ秒へ変換
milliseconds :: Int -> Int
milliseconds = (* 1000)
