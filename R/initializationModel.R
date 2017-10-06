#' Initialization of  parameters of the model.
#' 
#' Utilities of BNMF software required to initialize  parameters of the model.
#' @param k number of groups (latent factors)
#' @param R ratings matrix
#' @return matrix  \code{e.pos}   \code{e.neg}   \code{m.pos}   \code{m.neg}    \code{l}

initializeModel<-function (k,R){
   
    M <- ncol(R)  #Number items
    N <- nrow(R) #Number users
    #Positive Matrix Setting
    m.positive<-R
    m.positive[is.na(m.positive)]<-1
    m.positive[m.positive==0]<-1
    
    #Negative Matrix Setting
    m.negative<-R
    max.R<-max(R)
    for (i in 1:N) {
        for (j in 1:M) {
          #If there is a rating, it is replaced for maximum rating - the value the original rating
          #If there is no rating we assign 1
          ifelse(!is.na(R[i,j]) & as.numeric(R[i,j])>0,m.negative[i,j]<-max.R-as.numeric(R[i,j]), m.negative[i,j]<-1)
        }
    }
    #######Initialization of model parameters
    #Initialization of Epsilons є (i, k) with random values from a uniform distribution
    set.seed(1)
    epsilon<-matrix(runif(M*k,min=0,max=1),k*2,M)
    row.epsilon<-nrow(epsilon)
    #Positive Epsilons
    epsilon1<-epsilon[1:k,1:M] #Extracts values the epsilon (first three rows)
    #Negative Epsilons
    kk<-k+1
    epsilon2<-epsilon[kk:row.epsilon,1:M] #Extracts values from the last three rows

    #Random initialization of Gamma
    set.seed(1)
    gamma<-matrix(runif(N*k,min=0,max=1),N,k)
    sum.gam<-computeSumGamma(gamma)
    
    #######Compute the Output Matrices of algorithm
    output<-computeOuput(k,gamma,epsilon1,epsilon2,N,M)
    #Initial Predictions of the ratings
    predicctions<-computeMatrixPredictions(k,output$a,output$b,R)
    ##############Initial compute of parameters to be learned
    lambda<-computeLearningParameters(m.positive,m.negative,gamma,sum.gam,epsilon1,epsilon2,N,M)
    ##End of Initialization
    
    return ( list(e.pos=epsilon1,e.neg=epsilon2,m.pos=m.positive,m.neg=m.negative,l=lambda) )
}

