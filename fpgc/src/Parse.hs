{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Parse
    ( parseMovieInfo
    , parseCollection
    , parseGenre
    , parseCompany
    , parseCountry
    , parseLanguage
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

-- ############ parseCollection ############

instance FromJSON Collection where
    parseJSON = withObject "Collection" $ \v -> Collection
        <$> v .: "collection_id"
        <*> v .: "collection_name"
        <*> v .: "collection_poster_path"
        <*> v .: "collection_backdrop_path"

-- Parse a given byte string which represents a JSON to a list of Collection.
parseCollection :: L8.ByteString -> Either String [Collection]
parseCollection json = eitherDecode json :: Either String [Collection]

-- ############ parseGenre ############

instance FromJSON Genre where
    parseJSON = withObject "Genre" $ \v -> Genre
        <$> v .: "genre_id"
        <*> v .: "genre_name"

-- Parse a given byte string which represents a JSON to a list of Genre.
parseGenre :: L8.ByteString -> Either String [Genre]
parseGenre json = eitherDecode json :: Either String [Genre]

-- ############ parseCompany ############

instance FromJSON Company where
    parseJSON = withObject "Company" $ \v -> Company
        <$> v .: "company_id"
        <*> v .: "logo_path"
        <*> v .: "company_name"
        <*> v .: "original_country"

-- Parse a given byte string which represents a JSON to a list of Company.
parseCompany :: L8.ByteString -> Either String [Company]
parseCompany json = eitherDecode json :: Either String [Company]

-- ############ parseCountry ############

instance FromJSON Country where
    parseJSON = withObject "Country" $ \v -> Country
        <$> v .: "iso_3166_1"
        <*> v .: "country_name"

-- Parse a given byte string which represents a JSON to a list of Country.
parseCountry :: L8.ByteString -> Either String [Country]
parseCountry json = eitherDecode json :: Either String [Country]

-- ############ parseLanguage ############

instance FromJSON Language where
    parseJSON = withObject "Language" $ \v -> Language
        <$> v .: "english_name"
        <*> v .: "iso_639_1"
        <*> v .: "language_name"

-- Parse a given byte string which represents a JSON to a list of Language.
parseLanguage :: L8.ByteString -> Either String [Language]
parseLanguage json = eitherDecode json :: Either String [Language]
