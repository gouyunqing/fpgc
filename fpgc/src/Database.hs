{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getAdultMovie,
    getMovieByName,
    getMovieById,
    createMovieInfo,
    saveMovieInfo
) where


-- instance FromRow Record where
--     fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- instance FromRow Country where
--     fromRow = Country <$> field <*> field <*> field <*> field

-- instance ToRow Country where
--     toRow (Country id_ country_ continent_ population_)
--         = toRow (id_, country_, continent_, population_)

-- instance FromRow Entry where
--     fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- instance ToRow Entry where
--     toRow (Entry date_ day_ month_ year_ cases_ deaths_ fk_country)
--         = toRow (date_, day_, month_, year_, cases_, deaths_, fk_country)

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
        execute_ conn "CREATE TABLE `collection` (\
            \`id` INT NOT NULL,\
            \`collection_name` VARCHAR(45) NULL,\
            \`collection_poster_path` VARCHAR(45) NULL,\
            \`collection_backdrop_path` VARCHAR(45) NULL,\
            \PRIMARY KEY (`id`))"    
        execute_ conn "CREATE TABLE `movie_collection` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NOT NULL,\
            \`collection_id` INT NOT NULL,\
            \PRIMARY KEY (`id`))"

        return conn

-- search adult movie
getAdultMovie conn adult = do
    results <- queryNamed conn "SELECT * FROM movie WHERE adult=:adult" [":adult" := adult]
-- search by name
getMovieByName conn name = do
    results <- queryNamed conn "SELECT * FROM movie WHERE name LIKE '%:name%'" [":name" := name]
-- search by id
getMovieById conn id = do
    results <- queryNamed conn "SELECT * FROM movie WHERE id=:id" [":id" := id]

createMovieInfo :: Connection -> MovieInfo -> IO ()
createMovieInfo conn movieInfo = do
    let movie = Movie {
        adult = adult movieInfo,
        backdrop_path = backdrop_path movieInfo,
        budget = budget movieInfo,
        homepage = homepage movieInfo,
        id = id movieInfo,
        imdb_id = imdb_id movieInfo,
        original_language = original_language movieInfo,
        original_title = original_title movieInfo,
        overview = overview movieInfo,
        popularity = popularity movieInfo,
        poster_path = poster_path movieInfo,
        release_date = release_date movieInfo,
        revenue = revenue movieInfo,
        runtime = runtime movieInfo,
        status = status movieInfo,
        tagline = tagline movieInfo,
        title = title movieInfo,
        video = video movieInfo,
        vote_average = vote_average movieInfo,
        vote_count = vote_count movieInfo
    }

    execute conn "INSERT INTO movie VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)" movie

saveMovieInfo :: Connection -> [MovieInfo] -> IO ()
saveMovieInfo conn = mapM_ (createMovieInfo conn)

