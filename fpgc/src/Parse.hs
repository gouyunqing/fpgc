{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Parse
    ( parseMovieInfo
    , parseMovie
    , parseCollection
    , parseGenre
    , parseCompany
    , parseCountry
    , parseLanguage
    , parseMovie2Collection
    , parseMovie2Company
    , parseMovie2Genre
    , parseMovie2Country
    , parseMovie2Language
    ) where

import Type

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

-- ############ parseMovieInfo ############

instance FromJSON MovieInfo where
    parseJSON (Object v) = MovieInfo
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
        <*> v .: "spoken_languages"
        <*> v .: "status"
        <*> v .: "tagline"
        <*> v .: "title"
        <*> v .: "video"
        <*> v .: "vote_average"
        <*> v .: "vote_count"

-- Parse a given byte string which represents a JSON to a list of MovieInfo.
parseMovieInfo :: L8.ByteString -> Either String [MovieInfo]
parseMovieInfo json = eitherDecode json :: Either String [MovieInfo]

-- ############ parseMovie ############

instance FromJSON Movie where
    parseJSON (Object v) = Movie
        <$> v .: "adult_"
        <*> v .: "backdrop_path_"
        <*> v .: "budget_"
        <*> v .: "homepage_"
        <*> v .: "id_"
        -- <*> v .: "imdb_id_"
        -- <*> v .: "original_language_"
        -- <*> v .: "original_title_"
        -- <*> v .: "overview_"
        -- <*> v .: "popularity_"
        -- <*> v .: "poster_path_"
        -- <*> v .: "release_date_"
        -- <*> v .: "revenue_"
        -- <*> v .: "runtime_"
        -- <*> v .: "status_"
        -- <*> v .: "tagline_"
        -- <*> v .: "title_"
        -- <*> v .: "video_"
        -- <*> v .: "vote_average_"
        -- <*> v .: "vote_count_"

-- Parse a given byte string which represents a JSON to a list of Movie.
parseMovie :: L8.ByteString -> Either String [Movie]
parseMovie json = eitherDecode json :: Either String [Movie]

-- ############ parseCollection ############

instance FromJSON Collection where
    parseJSON (Object v) = Collection
        <$> v .: "collection_id"
        <*> v .: "collection_name"
        <*> v .: "collection_poster_path"
        <*> v .: "collection_backdrop_path"

-- Parse a given byte string which represents a JSON to a list of Collection.
parseCollection :: L8.ByteString -> Either String [Collection]
parseCollection json = eitherDecode json :: Either String [Collection]

-- ############ parseGenre ############

instance FromJSON Genre where
    parseJSON (Object v) = Genre
        <$> v .: "genre_id"
        <*> v .: "genre_name"

-- Parse a given byte string which represents a JSON to a list of Genre.
parseGenre :: L8.ByteString -> Either String [Genre]
parseGenre json = eitherDecode json :: Either String [Genre]

-- ############ parseCompany ############

instance FromJSON Company where
    parseJSON (Object v) = Company
        <$> v .: "company_id"
        <*> v .: "logo_path"
        <*> v .: "company_name"
        <*> v .: "original_country"

-- Parse a given byte string which represents a JSON to a list of Company.
parseCompany :: L8.ByteString -> Either String [Company]
parseCompany json = eitherDecode json :: Either String [Company]

-- ############ parseCountry ############

instance FromJSON Country where
    parseJSON (Object v) = Country
        <$> v .: "iso_3166_1"
        <*> v .: "country_name"

-- Parse a given byte string which represents a JSON to a list of Country.
parseCountry :: L8.ByteString -> Either String [Country]
parseCountry json = eitherDecode json :: Either String [Country]

-- ############ parseLanguage ############

instance FromJSON Language where
    parseJSON (Object v) = Language
        <$> v .: "english_name"
        <*> v .: "iso_639_1"
        <*> v .: "language_name"

-- Parse a given byte string which represents a JSON to a list of Language.
parseLanguage :: L8.ByteString -> Either String [Language]
parseLanguage json = eitherDecode json :: Either String [Language]

-- ############ parseMovie2Collection ############

instance FromJSON Movie2Collection where
    parseJSON (Object v) = Movie2Collection
        <$> v .: "m2cl_movie_id"
        <*> v .: "m2cl_collection_id"

-- Parse a given byte string which represents a JSON to a list of Movie2Collection.
parseMovie2Collection :: L8.ByteString -> Either String [Movie2Collection]
parseMovie2Collection json = eitherDecode json :: Either String [Movie2Collection]

-- ############ parseMovie2Genre ############

instance FromJSON Movie2Genre where
    parseJSON (Object v) = Movie2Genre
        <$> v .: "m2g_movie_id"
        <*> v .: "m2g_genre_id"

-- Parse a given byte string which represents a JSON to a list of Movie2Genre.
parseMovie2Genre :: L8.ByteString -> Either String [Movie2Genre]
parseMovie2Genre json = eitherDecode json :: Either String [Movie2Genre]

-- ############ parseMovie2Company ############

instance FromJSON Movie2Company where
    parseJSON (Object v) = Movie2Company
        <$> v .: "m2cp_movie_id"
        <*> v .: "m2cp_company_id"

-- Parse a given byte string which represents a JSON to a list of Movie2Company.
parseMovie2Company :: L8.ByteString -> Either String [Movie2Company]
parseMovie2Company json = eitherDecode json :: Either String [Movie2Company]

-- ############ parseMovie2Country ############

instance FromJSON Movie2Country where
    parseJSON (Object v) = Movie2Country
        <$> v .: "m2ct_movie_id"
        <*> v .: "m2ct_country_id"

-- Parse a given byte string which represents a JSON to a list of Movie2Country.
parseMovie2Country :: L8.ByteString -> Either String [Movie2Country]
parseMovie2Country json = eitherDecode json :: Either String [Movie2Country]

-- ############ parseMovie2Language ############

instance FromJSON Movie2Language where
    parseJSON (Object v) = Movie2Language
        <$> v .: "m2l_movie_id"
        <*> v .: "m2l_language_id"

-- Parse a given byte string which represents a JSON to a list of Movie2Language.
parseMovie2Language :: L8.ByteString -> Either String [Movie2Language]
parseMovie2Language json = eitherDecode json :: Either String [Movie2Language]
