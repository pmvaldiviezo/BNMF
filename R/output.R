#' Output Matrices 
#' Compute the Output Matrices of the algorithm
#' @param k           number of groups (latent factors) 
#' @param gamma       gamma matrix
#' @param e.positive  positive epsilons matrix
#' @param e.negative  negative epsilons matrix
#' @param N           number of users
#' @param M           number of items
#' @return matrix  \code{a}   \code{b} 

computeOuput<-
  function(k,gamma,e.positive, e.negative,N,M)
  {
    #Compute Ouput au,k= N*K, Latent vector to users
    au.k<-matrix(0,N,k)  
    for (i in 1:N) {
      for (j in 1:k) {
        ##Matrix associated to users au,k= N*K
        au.k[i,j]<-gamma[i,j]/sum(gamma[i,])
      }
    }
    
  #Compute Ouput bk,i= K*M, Latent vector to items
    bk.i<-matrix(0,k,M) 
    # Matrix associated to items bk,i= K*M
    bk.i<-e.positive/(e.positive+e.negative)
    return(list(a=au.k, b=bk.i))
}
