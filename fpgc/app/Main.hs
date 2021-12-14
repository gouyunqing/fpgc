module Main where

import System.IO
import Type
import Fetch
import Parse
import Database

main :: IO ()
main = do
    putStrLn "---------------------------------"
    putStrLn "  Welcome to the Movie info app  "
    putStrLn "  (1) Download Movie info by id  "
    putStrLn "  (2) get Movie By Id            "
    putStrLn "  (3) Quit                       "
    putStrLn "---------------------------------"
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            putStrLn "Please enter the movie id"
            movie_id_read <- readLn :: IO Int
            print "Downloading..."
            json <- download movie_id_read
            print "Parsing..."
            case (parseMovieInfo json) of
                Left err -> print err
                    main
                Right recs -> do
                    print "Saving on DB..."
                    createMovieInfo conn (records recs)
                    print "Saved!"
                    main
                    
        3 -> print "Hope you've enjoyed using the app!"
        otherwise -> print "Invalid option"
