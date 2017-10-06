#' Predictions 
#' Compute the prediction the tastes of users:  pu,i
#' @param k    number of groups (latent factors) 
#' @param au.k matrix associated to users
#' @param bk.i matrix associated to items
#' @param R    ratings matrix
#' @return matrix  \code{outP}  \code{outR} 

computeMatrixPredictions<-
  function(k,au.k,bk.i,R){
    #Non-normalized rating (scale 0-1)
    p<-au.k %*% bk.i    
    #Tansformation the Predictions Matrix: Normalized rating (scale 1-5)
    r<-matrix(0,nrow(R),ncol(R))
    max.R<-max(R) #5 if scale is 1-5
    for (i in 1:nrow(R)) {
      for (j in 1:ncol(R)) {
        r[i,j]<-min(max.R*p[i,j]+1,max.R)
      }
    }
    return ( list(outP=p, outR=r) )
  }
