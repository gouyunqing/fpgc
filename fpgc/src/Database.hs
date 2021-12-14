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

import Type
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
        execute_ conn "CREATE TABLE IF NOT EXISTS `movie` (\
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
        execute_ conn "CREATE TABLE IF NOT EXISTS `genre` (\
            \`id` INT NOT NULL,\
            \`name` VARCHAR(45) NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS `movie_genre` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NOT NULL,\
            \`genre_id` INT NOT NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS `production_companies` (\
            \`id` INT NOT NULL,\
            \`logo_path` VARCHAR(120) NULL,\
            \`name` VARCHAR(45) NULL,\
            \`origin_country` VARCHAR(45) NULL,\
           \ PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS `movie_production_companies` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NULL,\
            \`company_id` INT NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS `production_countries` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`iso_3166_1` VARCHAR(45) NULL,\
            \`name` VARCHAR(45) NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS `movie_production_countries` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NOT NULL,\
            \`country_id` INT NOT NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS spoken_languages` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`english_name` VARCHAR(45) NULL,\
            \`iso_639_1` VARCHAR(45) NULL,\
            \`name` VARCHAR(45) NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS `movie_spoken_languages` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NOT NULL,\
            \`language_id` VARCHAR(45) NOT NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS `collection` (\
            \`id` INT NOT NULL,\
            \`collection_name` VARCHAR(45) NULL,\
            \`collection_poster_path` VARCHAR(45) NULL,\
            \`collection_backdrop_path` VARCHAR(45) NULL,\
            \PRIMARY KEY (`id`))"    
        execute_ conn "CREATE TABLE IF NOT EXISTS `movie_collection` (\
            \`id` INT NOT NULL AUTO_INCREMENT,\
            \`movie_id` INT NOT NULL,\
            \`collection_id` INT NOT NULL,\
            \PRIMARY KEY (`id`))"

        return conn

-- search adult movie
getAdultMovie conn adult = do
    results <- queryNamed conn "SELECT * FROM movie WHERE adult=:adult" [":adult" := adult]
    return results
-- search by name
getMovieByName conn name = do
    results <- queryNamed conn "SELECT * FROM movie WHERE name LIKE '%:name%'" [":name" := name]
    return results
-- search by id
getMovieById conn id = do
    results <- queryNamed conn "SELECT * FROM movie WHERE id=:id" [":id" := id]
    return results

instance ToRow Movie where
    toRow (Movie adult_ backdrop_path_ budget_ homepage_ id_)
        = toRow (adult_, backdrop_path_, budget_, homepage_, id_)

createMovieInfo :: Connection -> MovieInfo -> IO ()
createMovieInfo conn movieInfo = do
    let movie = Movie {
        adult_ = adult movieInfo,
        backdrop_path_ = backdrop_path movieInfo,
        budget_ = budget movieInfo,
        homepage_ = homepage movieInfo,
        id_ = mid movieInfo
    }

    execute conn "INSERT INTO movie VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)" movie

    let genre_list = genres movieInfo
    let movie_id__ = mid movieInfo
    mapM_ (createMovie2Genre conn movie_id__) genre_list

    
createMovie2Genre :: Connection -> Int -> Genre -> IO ()
createMovie2Genre conn movie_id__ genre = do

    execute conn "INSERT INTO movie_genre (movie_id, genre_id) VALUES (?,?)" (movie_id__, (genre_id genre))

saveMovieInfo :: Connection -> [MovieInfo] -> IO ()
saveMovieInfo conn = mapM_ (createMovieInfo conn)

