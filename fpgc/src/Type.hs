{-# LANGUAGE DeriveGeneric #-}

module Type 
( MovieInfo(..)
, Movie(..)
, Collection(..)
, Genre(..)
, Company(..)
, Country(..)
, Language(..)
, Movie2Collection(..)
, Movie2Company(..)
, Movie2Genre(..)
, Movie2Country(..)
, Movie2Language(..)
) where

import GHC.Generics


data MovieInfo = MovieInfo {
        adult :: Bool,
        backdrop_path :: String,
        belongs_to_collection :: Maybe [Collection],
        budget :: String,
        genres :: [Genre],
        homepage :: String,
        mid :: Int,
        imdb_id :: String,
        original_language :: String,
        original_title :: String,
        overview :: String,
        popularity :: String,
        poster_path :: String,
        production_companies :: [Company],
        production_countries :: [Country],
        release_date :: String,
        revenue :: String,
        runtime :: Int,
        spoken_languages :: [Language],
        status :: String,
        tagline :: String,
        title :: String,
        video :: Bool,
        vote_average :: String,
        vote_count :: Int
    } deriving (Show,Generic)


data Movie = Movie {
        adult_ :: Bool,
        backdrop_path_ :: String,
        -- belongs_to_collection :: [Collection],
        budget_ :: String,
        -- genres :: [Genre],
        homepage_ :: String,
        id_ :: Int
    } deriving (Show)


data Collection = Collection {
        collection_id :: Int,
        collection_name :: String,
        collection_poster_path :: String,
        collection_backdrop_path :: String
    } deriving (Show,Generic)

data Genre = Genre {
        genre_id :: Int,
        genre_name :: String
    } deriving (Show,Generic)

data Company = Company {
        company_id :: Int,
        logo_path :: Maybe String,
        company_name :: String,
        original_country :: String
    } deriving (Show,Generic)

data Country = Country {
        iso_3166_1 :: String,
        country_name :: String
    } deriving (Show,Generic)

data Language = Language {
        english_name :: String,
        iso_639_1 :: String,
        language_name :: String
    } deriving (Show,Generic)

data Movie2Collection = Movie2Collection {
    -- id :: Int,
    m2cl_movie_id :: Int,
    m2cl_collection_id :: Int
} deriving (Show)

data Movie2Genre = Movie2Genre {
    -- id :: Int,
    m2g_movie_id :: Int,
    m2g_genre_id :: Int
} deriving (Show)

data Movie2Company = Movie2Company {
    -- id :: Int,
    m2cp_movie_id :: Int,
    m2cp_company_id :: Int
} deriving (Show)

data Movie2Country = Movie2Country {
    -- id :: Int,
    m2ct_movie_id :: Int,
    m2ct_country_id :: Int
} deriving (Show)

data Movie2Language = Movie2Language {
    -- id :: Int,
    m2l_movie_id :: Int,
    m2l_language_id :: Int
} deriving (Show)