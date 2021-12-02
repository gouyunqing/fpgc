module Type 
( MovieInfo(..)
, Collection(..)
, Genre(..)
, Company(..)
, Country(..)
, Language(..)
) where


data MovieInfo = MovieInfo {
        adult :: Bool,
        backdrop_path :: String,
        belongs_to_collection :: [Collection],
        budget :: Integer,
        genres :: [Genre],
        homepage :: String,
        id :: Int,
        imdb_id :: String,
        original_language :: String,
        original_title :: String,
        overview :: String,
        popularity :: Double,
        poster_path :: String,
        production_companies :: [Company],
        production_countries :: [Country],
        release_date :: String,
        revenue :: Integer,
        runtime :: Int,
        sopken_languages :: [Language],
        status :: String,
        tagline :: String,
        title :: String,
        vidio :: Bool,
        vote_average :: Double,
        vote_count :: Int
    } deriving (Show)

data Collection = Collection {
        collection_id :: Int,
        collection_name :: String,
        collection_poster_path :: String,
        collection_backdrop_path :: String
    } deriving (Show)

data Genre = Genre {
        genre_id :: Int,
        genre_name :: String
    } deriving (Show)

data Company = Company {
        company_id :: Int,
        logo_path :: String,
        company_name :: String,
        original_country :: String
    } deriving (Show)

data Country = Country {
        iso_3166_1 :: String,
        country_name :: String
    } deriving (Show)

data Language = Language {
        english_name :: String,
        iso_639_1 :: String,
        language_name :: String
    } deriving (Show)