module Parse
    ( parseMovieInfo
    , parseCollection
    , parseGenre
    , parseCompany
    , parseCountry
    , parseLanguage
    ) where

import Types

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

-- ############ parseMovieInfo ##############

instance FromJSON MovieInfo where
    parseJSON = withObject "MovieInfo" $ \v -> MovieInfo
        <$> v .: "adult"
        <*> v .: "backdrop_path"
        <*> v .: "belongs_to_collection"
        <*> v .: "budget"
        <*> v .: "genres"
        <*> v .: "homepage"
        <*> v .: "id"
        <*> v .: "imdb_id"
        <*> v .: "original_language"
        <*> v .: "original_title"
        <*> v .: "overview"
        <*> v .: "popularity"
        <*> v .: "poster_path"
        <*> v .: "production_companies"
        <*> v .: "production_countries"
        <*> v .: "release_date"
        <*> v .: "revenue"
        <*> v .: "runtime"
        <*> v .: "sopken_languages"
        <*> v .: "status"
        <*> v .: "tagline"
        <*> v .: "title"
        <*> v .: "video"
        <*> v .: "vote_average"
        <*> v .: "vote_count"

parseMovieInfo :: L8.ByteString -> Either String [MovieInfo]
parseMovieInfo json = eitherDecode json :: Either String [MovieInfo]
