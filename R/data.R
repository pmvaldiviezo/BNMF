#' Alternative code to download the dataset from the official site
#' \itemize{
#'   \item UserID:      numeric
#'   \item MovieID:     numeric
#'   \item Rating:      rating, numeric in a scale from 1-5
#'   \item Timestamp:   date
#' }

loadData<-function()
{
    #download dataset from Movielens
    url <- "http://files.grouplens.org/datasets/movielens/ml-latest-small.zip"
    outDir<-"../data/"
    download.file(url,"ml-latest.zip")
    dat <- unzip( "ml-latest.zip", exdir=outDir)
    #file .csv
    ratings.dat <- read.table(file=paste(outDir,"ml-latest-small/ratings.csv",sep =""), skip=1, sep=",")
    #file .dat
    #ratings.dat <- read.table(file=paste(outDir,"ratings.dat",sep=""), skip=1, sep=":")
    assign("ratings.dat", ratings.dat, envir = .GlobalEnv)
    
}

