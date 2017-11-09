# BNMF: Bayesian probabilistic model of non-negative factorization for collaborative filtering
BNMF is an algorithm which compute the prediction the tastes of users in recommender systems based on factorizing the rating matrix
into two non negative matrices, one related to items and another one related to users. 

The input of the algorithm is a matrix of rating R, the number of gropus K, alpha and beta parameters for the learning algorithm.

 The package provides functions to load input data, and  to realizes the learning procedure of the BNMF algorithm.
 This  package provides the function BNMF to realize the dynamics (with the learned parameters).
 
 # Installation
 
First, download the package from https://github.com/pmvaldiviezo/BNMF/releases/tag/BNMF1.0

BNMF can be installed by running the R environment and typing:
 
    install.packages("BNMF_1.0.tar.gz", repos=NULL,type = "source")
 
 
 # Example
    library(BNMF)
    #loading dataset of training and testing 
    data(ratings)
    dim(R)
    #System setting parameters
    k<-6  #Number of groups (latent factors)
    alpha<-0.8 #Control of group overlap  
    eta<-5 #Evidence that a group of users likes an item
    iter<-10  #setting number of iterations
    output<-BNMF(iter,R,k,alpha,eta)
    #Matrix associated to users
    output$au.k
    #Matrix associated to items
    output$bk.i
    #Predictions of the ratings 
    predictions<-output$pred
    predictions

    ####Prediction Accuracy
    mae(R.tst,predictions)
    ####Precision/Recall
    vectPredictions <- c(5,10,20,40) 
    PrecisionRecall(predictions,vectPredictions,ratings.tst)

# Reference

Hernando, A., Bobadilla, J., & Ortega, F. (2016). A non negative matrix factorization for collaborative filtering recommender systems based on a Bayesian probabilistic model. Knowledge-Based Systems, 97, 188–202. http://doi.org/10.1016/j.knosys.2015.12.018
