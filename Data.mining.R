#-------------- Function for data mining using a range of methods --------------#

#notes: 
#       First, select best model out of a range of methods using cross validation 
#         (i.e. splitting data  into training and testing: 
#         Test each model and calculate the out of sample error for that training method.

#       Second, use Best predictive model in "bagging" procedure (Breiman 1996) 
#       Bagging, aka bootstrap aggregation, is a relatively simple way to increase
#         the power of a predictive statistical model by taking multiple random 
#         samples (with replacement) from the training data set and using each of
#         these samples to construct a separate model and separate predictions for
#         the testing dataset. These predictions are then averaged to create a
#         more accurate, final prediction value.
#         If samples are very similar, then bagging is redundant.
#         Sample size for the training data set should be 1/50th - 1/2 of the entire data set
#         The smaller the bagging samples, the more samples you will need to collect 
#         and the more models you will need to generate to create more stable predictions


library(caret)      #for all things data mining
library(doParallel) # for parellel processing

#take advantage of parallel processing by using multiple cores
# detect cores and set for parallel processing
myCores <- detectCores()
registerDoParallel(myCores)

# set a seed to insure reproducability
set.seed(30316)


#function
Data.mining<-function(data,Y,Y.name,Y.type,Prop.train,iterations,ALL.models,Pred.data)  
{  
  #A. Select best model
  
  #A.1. Select random training and testing data
  training_dp <- createDataPartition(Y, p = Prop.train, list = FALSE)
  training_partition <- data[training_dp, ]
  testing_partition <- data[-training_dp, ]
  
  # set response var as factor if appropriate
  yy=match(Y.name,colnames(training_partition))
  if(Y.type=="Factor")
  {
    training_partition[,yy] <- as.factor(training_partition[,yy])
    testing_partition[,yy] <- as.factor(testing_partition[,yy])
  }
  
  # create formula
  Formula=as.formula(paste(paste(colnames(training_partition)[yy],
                                 "~", collapse=NULL),"."))
  
  # create an empty numeric vector to calculate out of sample error
  outOfSampleError <- numeric()
  
  
  #A.2. Train and predict models and calculate accuracy
  
  # add some parameters for train control
  TC <- trainControl(method = "cv", number = 12, returnData=FALSE, 
                     returnResamp="none", savePredictions=FALSE, 
                     verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)
  
  #2.1  (Neural Net)
  nnet <- train(Formula, method="nnet", data=training_partition, trControl= TC)
  nnetPrediction <- predict(nnet, testing_partition)
  if(Y.type=="Factor")
  {
    nnetAccuracy <- sum(nnetPrediction == testing_partition[,yy]) / length(nnetPrediction)
    nnetOutOfSampleError <- c(outOfSampleError, 1-nnetAccuracy)
  }else
  {
    nnetAccuracy=sqrt(mean((testing_partition[,yy]-nnetPrediction)^2))
    nnetOutOfSampleError <- c(outOfSampleError, nnetAccuracy)
  }
  
  #2.2 rf (Random Forest)
  rf <- train(Formula, method="rf", data=training_partition, trControl= TC)
  rfPrediction <- predict(rf, testing_partition)
  if(Y.type=="Factor")
  {
    rfAccuracy <- sum(rfPrediction == testing_partition[,yy]) / length(rfPrediction)
    rfOutOfSampleError <- c(outOfSampleError, 1-rfAccuracy)
  }else
  {
    rfAccuracy=sqrt(mean((testing_partition[,yy]-rfPrediction)^2))
    rfOutOfSampleError <- c(outOfSampleError, rfAccuracy)
  }
  
  #2.3  gbm (Generalized Boosted Regression)
  gbm <- train(Formula, method="gbm", data=training_partition, trControl= TC)
  gbmPrediction <- predict(gbm, testing_partition)
  if(Y.type=="Factor")
  {
    gbmAccuracy <- sum(gbmPrediction == testing_partition[,yy]) / length(gbmPrediction)
    gbmOutOfSampleError <- c(outOfSampleError, 1-gbmAccuracy)
  }else
  {
    gbmAccuracy=sqrt(mean((testing_partition[,yy]-gbmPrediction)^2))
    gbmOutOfSampleError <- c(outOfSampleError, gbmAccuracy)
  }
  
  #2.4 svmLinear (Support Vector Machines Linear)
  svml <- train(Formula, method="svmLinear", data=training_partition, trControl= TC)
  svmlPrediction <- predict(svml, testing_partition)
  if(Y.type=="Factor")
  {
    svmlAccuracy <- sum(svmlPrediction == testing_partition[,yy]) / length(svmlPrediction)
    svmlOutOfSampleError <- c(outOfSampleError, 1-svmlAccuracy)    
  }else
  {
    svmlAccuracy=sqrt(mean((testing_partition[,yy]-svmlPrediction)^2))
    svmlOutOfSampleError <- c(outOfSampleError, svmlAccuracy)
  }
  
  if(ALL.models=="YES")
  {
    #2.5 bayesglm (Bayesian GLM)
    bayesglm <- train(Formula, method="bayesglm", data=training_partition, trControl= TC)
    bayesglmPrediction <- predict(bayesglm, testing_partition)
    if(Y.type=="Factor")
    {
      bayesglmAccuracy <- sum(bayesglmPrediction == testing_partition[,yy]) / length(bayesglmPrediction)
      bayesglmOutOfSampleError <- c(outOfSampleError, 1-bayesglmAccuracy)
    }else
    {
      bayesglmAccuracy=sqrt(mean((testing_partition[,yy]-bayesglmPrediction)^2))
      bayesglmOutOfSampleError <- c(outOfSampleError, bayesglmAccuracy)
    }
    
    #2.6 knn (K Nearest Neighbor)
    knn <- train(Formula, method="knn", data=training_partition, trControl= TC)
    knnPrediction <- predict(knn, testing_partition)
    if(Y.type=="Factor")
    {
      knnAccuracy <- sum(knnPrediction == testing_partition[,yy]) / length(knnPrediction)
      knnOutOfSampleError <- c(outOfSampleError, 1-knnAccuracy)
    }else
    {
      knnAccuracy=sqrt(mean((testing_partition[,yy]-knnPrediction)^2))
      knnOutOfSampleError <- c(outOfSampleError, knnAccuracy)
    }
    
    #2.7 nb (Naive Bayes)
    nb <- train(Formula, method="nb", data=training_partition, trControl= TC)
    nbPrediction <- predict(nb, testing_partition)
    if(Y.type=="Factor")
    {
      nbAccuracy <- sum(nbPrediction == testing_partition[,yy]) / length(nbPrediction)
      nbOutOfSampleError <- c(outOfSampleError, 1-nbAccuracy)
    }else
    {
      nbAccuracy=sqrt(mean((testing_partition[,yy]-nbPrediction)^2))
      nbOutOfSampleError <- c(outOfSampleError, nbAccuracy)
    }
    
    #2.8 rpart (Recursive Partitioning and Regression Trees)
    rpart <- train(Formula, method="rpart", data=training_partition, trControl= TC)
    rpartPrediction <- predict(rpart, testing_partition)
    if(Y.type=="Factor")
    {
      rpartAccuracy <- sum(rpartPrediction == testing_partition[,yy]) / length(rpartPrediction)
      rpartOutOfSampleError <- c(outOfSampleError, 1-rpartAccuracy)
    }else
    {
      rpartAccuracy=sqrt(mean((testing_partition[,yy]-rpartPrediction)^2))
      rpartOutOfSampleError <- c(outOfSampleError, rpartAccuracy)
    }
    
    #2.9 svmRadial (Support Vector Machines Radial)  
    svmr <- train(Formula, method="svmRadial", data=training_partition, trControl= TC)
    svmrPrediction <- predict(svmr, testing_partition)
    if(Y.type=="Factor")
    {
      svmrAccuracy <- sum(svmrPrediction == testing_partition[,yy]) / length(svmrPrediction)
      svmrOutOfSampleError <- c(outOfSampleError, 1-svmrAccuracy)
    }else
    {
      svmrAccuracy=sqrt(mean((testing_partition[,yy]-svmrPrediction)^2))
      svmrOutOfSampleError <- c(outOfSampleError, svmrAccuracy)
    }
    
    #2.10 treebag (Bagged Classification and Regression Trees)
    treebag <- train(Formula, method="treebag", data=training_partition, trControl= TC)
    treebagPrediction <- predict(treebag, testing_partition)
    if(Y.type=="Factor")
    {
      treebagAccuracy <- sum(treebagPrediction == testing_partition[,yy]) / length(treebagPrediction)
      treebagOutOfSampleError <- c(outOfSampleError, 1-treebagAccuracy)
    }else
    {
      treebagAccuracy=sqrt(mean((testing_partition[,yy]-treebagPrediction)^2))
      treebagOutOfSampleError <- c(outOfSampleError, treebagAccuracy)
    }
    
  }
  
  
  #A.3. Display values in table ranked by accuracy
  trainMethods <- c("Neural Net","Random Forest","Generalized Boosted Regression",
                    "Support Vector Machines Linear")
  accuracy <- c(nnetAccuracy,rfAccuracy,gbmAccuracy,svmlAccuracy)
  outOfSampleError <- c(nnetOutOfSampleError,rfOutOfSampleError,
                        gbmOutOfSampleError,svmlOutOfSampleError)
  
  if(ALL.models=="YES")
  {
    trainMethods <- c(trainMethods,"Bayesian GLM",  
                      "K Nearest Neighbor","Naive Bayes",  
                      "Recursive Partitioning and Regression Trees",
                      "Support Vector Machines Radial",
                      "Bagged Classification and Regression Trees")
    accuracy <- c(accuracy,bayesglmAccuracy,knnAccuracy,nbAccuracy,rpartAccuracy,
                  svmrAccuracy, treebagAccuracy)
    outOfSampleError=c(outOfSampleError,bayesglmOutOfSampleError,  
                       knnOutOfSampleError,nbOutOfSampleError, 
                       rpartOutOfSampleError,svmrOutOfSampleError,
                       treebagOutOfSampleError)
  }
  
  results <- data.frame(trainMethods, accuracy, outOfSampleError)
  results=results[order(results$accuracy),]
  if(!Y.type=="Factor") colnames(results)[match("accuracy",colnames(results))]="RMSE"
  All.models.table=results
  
  
  #A.4 Choose best model
  if(Y.type=="Factor") Best.mod=as.character(results$trainMethods[which(results$accuracy==max(results$accuracy))])
  if(!Y.type=="Factor")Best.mod=as.character(results$trainMethods[which(results$RMSE==min(results$RMSE))])
  
  This.method=ifelse(Best.mod=="Bagged Classification and Regression Trees","treebag", 
              ifelse(Best.mod=="Bayesian GLM","bayesglm",                           
              ifelse(Best.mod=="Generalized Boosted Regression","gbm",             
              ifelse(Best.mod=="K Nearest Neighbor","knn",                         
              ifelse(Best.mod=="Naive Bayes","nb",                                
              ifelse(Best.mod=="Neural Net","nnet",                                 
              ifelse(Best.mod=="Random Forest","rf",                              
              ifelse(Best.mod=="Recursive Partitioning and Regression Trees","rpart",
              ifelse(Best.mod=="Support Vector Machines Linear","svmLinear",             
              ifelse(Best.mod=="Support Vector Machines Radial","svmRadial",NA))))))))))

  #A.5 Confusion matrix for best model
  MODEL <- train(Formula, method=This.method, data=training_partition, trControl= TC)
  predictCrossVal <- predict(MODEL, testing_partition)
  ConfMat=confusionMatrix(testing_partition[,yy], predictCrossVal)
  
  #B. Use best model in bagging procedure
  Predictions=vector('list',length=iterations)
  for(i in 1:iterations)
  {  
    #B.1. Select random training and testing data
    training_dp <- createDataPartition(Y, p = Prop.train, list = FALSE)
    training_partition <- data[training_dp, ]
    testing_partition <- data[-training_dp, ]
    
    # set response var as factor if appropriate
    yy=match(Y.name,colnames(training_partition))
    if(Y.type=="Factor")
    {
      training_partition[,yy] <- as.factor(training_partition[,yy])
      testing_partition[,yy] <- as.factor(testing_partition[,yy])
    }
    
    #B.2 Fit model and make predictions
     model <- train(Formula, method=This.method, data=training_partition, trControl= TC)
    if(is.null(Pred.data))
    {
      Preds <- predict(model, testing_partition)
    }else
    {
      Preds <- predict(model, Pred.data)
    }
    
    #B.3 Store results
     if(Y.type=="Factor") Preds=as.character(Preds)
    Predictions[[i]]=Preds
  } 
  
    #B.4 Average bootstrap aggregations
  All.Preds=do.call(rbind,Predictions)
  
  return(list(All.models.table=All.models.table,Best.mod=MODEL,ConfMat=ConfMat,Predictions=All.Preds))
}  