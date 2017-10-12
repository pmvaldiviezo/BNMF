#' Main function which invocate others functions for the run of the algorithm
#' @param iter      number of iterations
#' @param R         matrix of ratings
#' @param k         number of latent factors
#' @param alpha     for learning the algorithm
#' @param eta     f or learning the algorithm
#' @return matrix  \code{outR}   \code{au.k}   \code{bk.i}

BNMF<-
  function(iter,R,k,alpha,eta){
    ##Initialization of the model
    M <- ncol(R)  #Number items
    N <- nrow(R)
    parameters<-initializeModel(k,R) 
    lambda<-parameters$l
    xs <- "Iteration: " 
    cat(xs, " ...", sep="")
    
    for (step in 1:iter) {
      #####print iterations#####
      xs <- paste(xs, step, ", ", sep="")
      cat("\r")
      cat(xs, ".....", sep="")      
      ##########################
      new.e.positive<-computeNewEpsilon(parameters$m.pos,k,lambda,N,M,eta)
      new.e.negative<-computeNewEpsilon(parameters$m.neg,k,lambda,N,M,eta)
      new.gamma<-computeNewGamma(k,lambda,N,M,alpha) 
      sum.gam<-computeSumGamma(new.gamma)
      #######Compute the Output Matrices au.k and bk.i
      output<-computeOuput(k,new.gamma,new.e.positive, new.e.negative,N,M)   
      #Predictions of the ratings
      predicctions<-computeMatrixPredictions(k,output$a,output$b,R)
      lambda<-computeLearningParameters(parameters$m.pos,parameters$m.neg,new.gamma,sum.gam,new.e.positive, new.e.negative,N,M)
     
    }
    cat("\r")
    cat(xs, sep="")
    return(list(pred=predicctions$outR,au.k=output$a,bk.i=output$b))
    
  }
.onLoad <- function(libname=.libPaths(), pkgname="BNMF")

.onAttach <- function(libname=.libPaths(), pkgname="BNMF")
  cat("BNMF: A non negative matrix factorizationbased on a Bayesian probabilistic model.\n");



