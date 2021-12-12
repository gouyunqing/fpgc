module Fetch
    ( download
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

-- Produce the URL request for a given movie ID, then download the URL.
-- E.g. for movie_id = 550, the URL will be "https://api.themoviedb.org/3/movie/550?api_key=ed416751bdc0f9f32680e2c327ebf6a4".
download :: Int -> IO L8.ByteString
download movie_id = do
    request <- parseRequest $ concat ["https://api.themoviedb.org/3/movie/", show movie_id, "?api_key=ed416751bdc0f9f32680e2c327ebf6a4"]
    response <- httpLBS request
    return $ getResponseBody response
