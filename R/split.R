#' Compute a Random partitioning from input data
#' Function to determine a random partition of the input datset
#' @param pctSplit training percentage 
#' @param ratings.dat dataset  (format dataframe)

splitData <- function(pctSplit,ratings.dat) {
  ratings.dat<-ratings.dat[,1:3]
  colnames(ratings.dat)<-c("UserID", "MovieID", "Rating")
  ratings.dat$Class <- ifelse(ratings.dat$Rating>=4, yes="Rl", no="NRl") #0.8 normalized rating
  nrow(ratings.dat)
  ntrain <- nrow(ratings.dat)*pctSplit
  set.seed(161)
  index_train<-sample(1:nrow(ratings.dat),size = ntrain)
  train<-ratings.dat[index_train,]
  test<-ratings.dat[-index_train,]
  Us.trn <- unique(train[,"MovieID"])
  Us.tst <- unique(test[,"MovieID"])
  Us.itr <- data.frame(MovieID = intersect(Us.trn, Us.tst))
  train <- merge(x = train, y = Us.itr, by = "MovieID", all.y = TRUE)
  test <- merge(x = test, y = Us.itr, by = "MovieID", all.y = TRUE)
  Us.trn <- unique(train[,"UserID"])
  Us.tst <- unique(test[,"UserID"])
  Us.itr <- data.frame(UserID = intersect(Us.trn, Us.tst))
  train <- merge(x = train, y = Us.itr, by = "UserID", all.y = TRUE)
  test <- merge(x = test, y = Us.itr, by = "UserID", all.y = TRUE)
  train <- merge(x=ratings.dat, y=train, by=c("UserID","MovieID"), all.y=TRUE, )
  test <- merge(x=ratings.dat, y=test, by=c("UserID","MovieID"), all.y=TRUE, )
  train<-train[,1:4]
  test<-test[,1:4]
  colnames(train)<-c("UserID", "MovieID", "Rating","Class")
  colnames(test)<-c("UserID", "MovieID", "Rating","Class")
  assign("ratings.trn", train, envir = .GlobalEnv)
  assign("ratings.tst", test, envir = .GlobalEnv)  
  #   write.table(ratings.trn, file = "ratings.trn.csv", sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
  #   write.table(ratings.tst, file = "ratings.tst.csv", sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
}
