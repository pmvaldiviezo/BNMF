#' Functions of BNMF model.
#' Main functions used to the learning of parameters of the model
#' @param m.pos   positive matrix 
#' @param m.neg   negative matrix
#' @param gamma   gamma matrix
#' @param sum.gam Summation of gammas
#' @param e.pos   positive epsilons matrix
#' @param e.neg   negative epsilons matrix
#' @param N       number of users
#' @param M       number of items
#' @return matrix  \code{lambda} 

computeLearningParameters<-
  function(m.pos,m.neg,gamma,sum.gam,e.pos, e.neg,N,M){
    #Compute of Digamma Function   
    di<-computeDigammaGamma(gamma)
    
    #Digamma functions to positive and negative epsilons
    digammas<-computeDigammaEpsilons(e.pos, e.neg)
    
    #Summation epsilons and digamms
    sum.digammas<-computeSumEps(e.pos,e.neg,sum.gam)
    
    #Compute prime lambda 
    a <-list()
    factor<-matrix(0,N,M)
    for (d in 1:k) {
      a[[d]]<- computeFactor(factor,m.pos,m.neg,d,di,sum.digammas$sumdig.e,sum.digammas$sumdig.gam,digammas$dig.e.pos,digammas$dig.e.neg)
    }
    #Compute lambda
    
    b<-list()
    factor2<-matrix(0,N,M)
    for (d in 1:k) {
      b[[d]]<- computeFactor2(factor2,d,a)
    }
    lambda<-b
    return(lambda)
  }

#Sum of Gammas
computeSumGamma<-
  function(gamma)
  {
    sum.gam<-apply(gamma,1,sum)
    return(sum.gam)
  }

#Compute Digamma Functions 
computeDigammaGamma<-
  function(gamma){  
  di<-digamma(gamma)
  di<-as.matrix(di)
  return(di)
}

#Compute Digamma function for positive and negative epsilons
computeDigammaEpsilons<-
  function(e.positive,e.negative){
  dig.e.post<-digamma(e.positive)
  dig.e.neg<-digamma(e.negative)
  digammas.e.pos<-dig.e.post
  digammas.e.neg<-dig.e.neg
  return(list(dig.e.pos=digammas.e.pos, dig.e.neg=digammas.e.neg))
}
#Summation of epsilons and digamms
computeSumEps<-
  function(e.positive,e.negative,sum.gam){
  sum.epsilons<-e.positive + e.negative
  dig.sum.e<-digamma(sum.epsilons)
  dig.sum.gam<-digamma(sum.gam)
  return(list(sumdig.e=dig.sum.e, sumdig.gam=dig.sum.gam))
}
#Compute prime lambda 
computeFactor <- 
  function(factor,m.positive,m.negative,d,di,dig.sum.e,dig.sum.gam,dig.e.post,dig.e.neg)
{
  for (i in 1:nrow(factor)) {
    for (j in 1:ncol(factor)) {
      factor[i,j]<-exp(di[i,d]-dig.sum.gam[i]+as.numeric(m.positive[i,j])%*%(dig.e.post[d,j]-dig.sum.e[d,j])+as.numeric(m.negative[i,j])%*%(dig.e.neg[d,j]-dig.sum.e[d,j]))
      
    }
  }
  return (factor) #return the matrix of each factor to prime lambda
}
#Compute lambda 
computeFactor2 <- 
  function(factor2,d,a)
{
  for (i in 1:nrow(factor2)) {
    for (j in 1:ncol(factor2)) {
      sum.lambda<-0
      for (dd in 1:k) {
        sum.lambda<-sum.lambda+a[[dd]][i,j]
        
      }
      
      factor2[i,j]<-ifelse(sum.lambda>0,a[[d]][i,j]/sum.lambda,0)
    }
  }
  return (factor2)  #Return the matrix of each factor for lambda
}
#Compute New epsilon
computeNewEpsilon <- function(m.rating,k,b,N,M,eta)
{
  new.epsilon<-matrix(0,k,M)
  for (d in 1:k) {
    for (j in 1:M) {
      product.r.lambda<-as.numeric(m.rating[1:N,j])*b[[d]][1:N,j]
      sumprod<-eta+sum(product.r.lambda)
      new.epsilon[d,j]<-sumprod
    }
  } 
  return(new.epsilon)
}

#Compute New Gamma
computeNewGamma <- function(k,b,N,M,alpha)
{
  new.gamma<-matrix(0,N,k)
  for (i in 1:N) {
    for (d in 1:k) {
      sumfactor<-alpha+sum(b[[d]][i,1:M])
      new.gamma[i,d]<-sumfactor
    }
  }
  return(new.gamma)
}
