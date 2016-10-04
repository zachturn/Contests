# Test a xgboost model using CV

library(xgboost)
library(Matrix)
library(e1071)

data <- read.csv("~/Git/Contests/DrivenData/WaterTable/train_collapsed.csv",sep="\t")
data <- subset(data,select=-c(id))

kfold1 <- function(data,response,k,...){

  # Create sparse matrix
  train <- sparse.model.matrix(status_group~.-1,data=data)
  response <- as.numeric(data$status_group)-1
  
  set.seed(1848)
  folds <- sample(1:k,nrow(train),replace=TRUE)

  error <- rep(0,k)
  pred_list <- list()
    for(i in 1:k){
      xgtrain <- xgb.DMatrix(data=train[folds!=i,],label=response[folds!=i])
      xgtest <- xgb.DMatrix(data=train[folds==i,],label=response[folds==i])
  
      model <- xgb.train(data=xgtrain,watchlist=list(train=xgtrain,validation=xgtest),...)
      preds <- matrix(predict(model,xgtest),ncol=3,byrow=TRUE)
      
      pred_list[[i]] <- preds
      
      pred_class <- apply(preds,MARGIN=1,FUN=which.max) - 1
      
      error[i] <- mean(getinfo(xgtest,"label")!=pred_class)
   }
  
  return(list(prob_matrix=pred_list,error=error))
}

test1 <- kfold1(data,response,k=5,eta=0.05,subsample=0.5,max.depth=15,nrounds=10,
              objective="multi:softprob",
              num_class=3,
              eval_metric="mlogloss",early_stopping_rounds=5,maximize=FALSE)

kfold2 <- function(data,response,k,...){
  
  set.seed(1848)
  folds <- sample(1:k,nrow(train),replace=TRUE)
  
  error <- rep(0,k)
  pred_list <- list()
  for(i in 1:k){
    xgtrain <- xgb.DMatrix(data=train[folds!=i,],label=response[folds!=i])
    xgtest <- xgb.DMatrix(data=train[folds==i,],label=response[folds==i])
    
    model <- xgb.train(data=xgtrain,watchlist=list(train=xgtrain,validation=xgtest),...)
    preds <- matrix(predict(model,xgtest),ncol=3,byrow=TRUE)
    
    nb <- naiveBayes(status_group~.,data=data[folds!=i,])
    nb_preds <- predict(nb,newdata=data[folds==i,],type="raw")
    
    avg_preds <- (preds+nb_preds)/2
    
    pred_class <- apply(avg_preds,MARGIN=1,FUN=which.max) - 1
    
    pred_list[[i]] <- avg_preds
    error[i] <- mean(getinfo(xgtest,"label")!=pred_class)
  }
  
  return(list(prob_matrix=pred_list,error=error))
}

test2 <- kfold2(data,response,k=5,eta=0.05,subsample=0.5,max.depth=15,nrounds=10,
                objective="multi:softprob",
                num_class=3,
                eval_metric="mlogloss",early_stopping_rounds=5,maximize=FALSE)

test <- naiveBayes(x=as.matrix(train),y=response)
preds <- predict(test,newdata=as.matrix(train),type="raw")
