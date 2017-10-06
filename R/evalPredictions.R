#' Evaluation of Predictions
#' @param R.tst   matrix of test 
#' @param pred    matrix of predictions
#' @return error  \code{e}

mae<-function(R.tst, pred)
{
  M <- ncol(R.tst)  #Number items
  N <- nrow(R.tst) #Number users
  nTest<-length(R.tst[R.tst>0])
  e = 0
  for (i in 1:nrow(R.tst)) {
    for (j in 1:ncol(R.tst)) {
      if(R.tst[i,j]>0){
        e = e + abs(R.tst[i,j] - pred[i,j])
      }
    }
  }
  if (e < 0.001) {
    print("e inferior")
    
  }
  e<-e/nTest
  return(e)
  
}
