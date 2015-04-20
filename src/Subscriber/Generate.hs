{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Subscriber.Generate
    ( generate
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Reddit.Types
import           Text.Shakespeare.Text (st)

generate :: [Subreddit] -> BS.ByteString
generate = TE.encodeUtf8 . js . toList

toList :: [Subreddit] -> T.Text
toList ss = [st|[ #{T.intercalate "," $ map toObject ss} ]|]
  where
    toObject s = [st|{ display_name: "#{unDisplayName $ displayName s}", fullname: "#{unFullname $ fullname s}" }|]

js :: T.Text -> T.Text
js list = [st|
(function() {
    // 初期購読リスト。ここを動的生成する
    var xs = #{list};

var indicater = function() {
    var box = document.createElement('div');
    var s = box.style;
    s.backgroundColor = "#fefefe";
    s.position = "fixed";
    s.top = "50%";
    s.left = "35%";
    s.width = "30%";
    s.height = "50px";
    s.lineHeight = "50px";
    s.textAlign = "center";
    s.border = "1px solid gray";
    s.borderRadius = "3px";
    s.boxShadow = "2px 2px 1px rgba(55,55,55,0.3)";
    document.body.appendChild(box);
    return box;
};

var box = indicater();

var closeIndicater = function() {
    document.body.removeChild(box);
};

var updateIndicater = function(n, name) {
    box.innerHTML = "日本語subredditの購読中(" + n + "/" + xs.length + "完了) : " + name;
};

var i = 0;
var subscribeRecursive = function() {
    if (i < xs.length) {
        var x = xs[i];
        updateIndicater(i + 1, x.display_name);
        subscribe(x.fullname)();
        ++i;
        setTimeout(subscribeRecursive, 1000);
    } else {
        closeIndicater();
    }
};

subscribeRecursive();

})();
|]

