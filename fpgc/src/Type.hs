type Title = String
type Year = Int
type ID = String
type DType = String
type Poster = String

data MovieInfo = MovieInfo {
        title :: Title,
        year :: Year,
        id :: ID,
        dType :: DType,
        poster :: Poster
    } deriving (Show)
