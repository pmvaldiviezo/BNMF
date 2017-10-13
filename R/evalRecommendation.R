#' Compute Precision and Recall
#' Functions to evaluate recommendations 
#' @param predictions       matrix of predictions
#' @param vectPredictions   vector TopN of recommendations
#' @param ratings.tst       dataframe of test ratings 
#' @return Precision, Recall and F1 for each TopN of recommendations

PrecisionRecall<-function(predictions,vectPredictions,ratings.tst){
  nmsCol <- colnames(R);
  nmsRow <- rownames(R);
  movies <- nmsCol
  users <- nmsRow
  colnames(predictions) <- nmsCol
  rownames(predictions) <- nmsRow
  predictions <- t(predictions)
  totPredictions <- nrow(predictions) 
  users <- colnames(predictions)
  preMatrix(totPredictions, users, predictions) 
  evalModel2(ratings.tst,totPredictions, vectPredictions, length(users), us,mv,pb, RcXrating=FALSE)
  precision=recall=F1=NULL
  for (np in vectPredictions) {
    df.userMovie <- df.userMovieTst
    colnames(df.userMovie)[colnames(df.userMovie) == paste("rc",np, sep="")] <- 'Recommendation'
    indicTst <- conf.matrix(df.userMovie)
    ####Fmeasure
    f1<-2*((indicTst$Prc *indicTst$Rec)/(indicTst$Prc +indicTst$Rec))
    ####Precision/Recall
    tmp<-indicTst$Prc
    precision<-c(tmp,precision)
    tmp<-indicTst$Rec
    recall<-c(tmp,recall)
    tmp<-f1
    F1<-c(tmp,F1)
    #write.table(x = paste(c(np,indicTst$Prc, indicTst$Rec,f1), collapse=";"), file = "metricsBNMF.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  }
  print(data.frame(TopN=vectPredictions,precision,recall,F1))
}
preMatrix <- function(totPredictions, users, predict) { # predict <- predictions
  library(lda)
  usrPrd <- top.topic.words(t(predict), totPredictions)
  colnames(usrPrd) <- users
  
  probab <- sapply(X = 1:ncol(predict), FUN = function(x) predict[usrPrd[1:totPredictions,x],x] )
  colnames(probab) <- users
  rownames(probab) <- NULL
  
  us <<- rep(users, each=totPredictions)
  mv <<- as.vector(usrPrd)    
  pb <<- as.vector(probab) 
}
evalModel2 <- function(ratings.tst,totPredictions=126, vectPredictions=c(5,10,15,20), nUsers, us,mv,pb, RcXrating=FALSE) {
  
  if (RcXrating==FALSE) {
    rc <- NULL
    for (iRc in vectPredictions) {
      
      tmp <- rep(c(rep("Rc",iRc),rep("NRc",(totPredictions-iRc))), nUsers)
      rc <- cbind(rc, tmp)
    }
    
    colnames(rc) <- paste("rc", vectPredictions, sep="")
    df.predictions <- data.frame(UserID=us, MovieID=mv, Probability=pb, rc, stringsAsFactors=FALSE)
  } 
  
  df.userMovieTst <<- merge(df.predictions, ratings.tst, by=c("UserID","MovieID"), all.y=TRUE ) 
  df.userMovieTst <<- na.omit(df.userMovieTst)
  write.table(df.predictions[,1:3], file = "predictions.csv", sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

conf.matrix <- function(df.userMovie) {
  tbl <- NULL
  fRecommendation <- factor(df.userMovie$Recommendation, levels=c("Rc","NRc"), labels=c("Rc","NRc"))
  fClass <- factor(df.userMovie$Class, levels=c("Rl","NRl"), labels=c("Rl","NRl"))
  tbl <- table(fRecommendation, fClass)
  
  Prc <- tbl[1,1] / (tbl[1,1]+tbl[1,2])
  Rec <- tbl[1,1] / (tbl[1,1]+tbl[2,1])
  TPR <- Rec
  FPR <- tbl[1,2] / (tbl[1,2]+tbl[2,2])
  
  lst.indic <- list(tbl=tbl, Prc=Prc, Rec=Rec, TPR=TPR, FPR=FPR)
  return (lst.indic)
}

