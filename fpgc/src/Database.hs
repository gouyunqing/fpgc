{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreateCountry,
    saveRecords,
    queryCountryAllEntries,
    queryCountryTotalCases
) where


instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Country where
    fromRow = Country <$> field <*> field <*> field <*> field

instance ToRow Country where
    toRow (Country id_ country_ continent_ population_)
        = toRow (id_, country_, continent_, population_)

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
    toRow (Entry date_ day_ month_ year_ cases_ deaths_ fk_country)
        = toRow (date_, day_, month_, year_, cases_, deaths_, fk_country)

import Types
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html


initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "movie.sqlite"
        execute_ conn "CREATE TABLE `movie` (\
            \`adult` BLOB NOT NULL,\
            \`backdrop_path` VARCHAR(45) NULL,\
            \`belongs_to_collection` VARCHAR(45) NULL,\
            \`budget` DECIMAL NULL,\
            \`homepage` VARCHAR(120) NULL,\
            \`id` INT NOT NULL,\
            \`imdb_id` VARCHAR(45) NULL,\
            \`original_language` VARCHAR(45) NULL,\
            \`original_title` VARCHAR(45) NULL,\
            \`overview` MEDIUMTEXT NULL,\
            \`popularity` DOUBLE NULL,\
            \`poster_path` VARCHAR(45) NULL,\
            \`release_date` VARCHAR(45) NULL,\
            \`revenue` DECIMAL(20) NULL,\
            \`runtime` INT NULL,\
            \`status` VARCHAR(45) NULL,\
            \`tagline` VARCHAR(45) NULL,\
            \`title` VARCHAR(45) NULL,\
            \`video` BLOB NULL,\
            \`new_tablecol` DOUBLE NULL,\
            \`new_tablecol1` DECIMAL NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE `genre` (\
            \`id` INT NOT NULL,\
            \`name` VARCHAR(45) NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE `movie_genre` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NOT NULL,\
            \`genre_id` INT NOT NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE `production_companies` (\
            \`id` INT NOT NULL,\
            \`logo_path` VARCHAR(120) NULL,\
            \`name` VARCHAR(45) NULL,\
            \`origin_country` VARCHAR(45) NULL,\
           \ PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE `movie_production_companies` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NULL,\
            \`company_id` INT NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE `production_countries` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`iso_3166_1` VARCHAR(45) NULL,\
            \`name` VARCHAR(45) NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE `movie_production_countries` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NOT NULL,\
            \`country_id` INT NOT NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE spoken_languages` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`english_name` VARCHAR(45) NULL,\
            \`iso_639_1` VARCHAR(45) NULL,\
            \`name` VARCHAR(45) NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE `movie_spoken_languages` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NOT NULL,\
            \`language_id` VARCHAR(45) NOT NULL,\
            \PRIMARY KEY (`id`))"    

        return conn

-- search adult movie
getAdultMovie conn adult = do
    results <- queryNamed conn "SELECT * FROM movie WHERE adult=:adult" [":adult" := adult]
getMovieByName conn name = do
    results <- queryNamed conn "SELECT * FROM movie WHERE name LIKE "%:name%" [":name" := name]
getMovieById conn id = do
    results <- queryNamed conn "SELECT * FROM movie WHERE id=:id" [":id" := id]


createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    country <- getOrCreateCountry conn (country record) (continent record) (population record)
    let entry = Entry {
        date_ = date record,
        day_ = day record,
        month_ = month record,
        year_ = year record,
        cases_ = cases record,
        deaths_ = deaths record,
        fk_country = id_ country
    }
    execute conn "INSERT INTO entries VALUES (?,?,?,?,?,?,?)" entry

saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)

queryCountryAllEntries :: Connection -> IO [Record]
queryCountryAllEntries conn = do
    putStr "Enter country name > "
    countryName <- getLine
    putStrLn $ "Looking for " ++ countryName ++ " entries..."
    let sql = "SELECT date, day, month, year, cases, deaths, country, continent, population FROM entries inner join countries on entries.fk_country == countries.id WHERE country=?"
    query conn sql [countryName]

queryCountryTotalCases :: Connection -> IO ()
queryCountryTotalCases conn = do
    countryEntries <- queryCountryAllEntries conn
    let total = sum (map cases countryEntries)
    print $ "Total entries: " ++ show(total)
