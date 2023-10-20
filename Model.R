library(data.table)
library(tidyverse)
library(Rtsne)
library(ggplot2)
library(xgboost)
library(caret)

master_train<-fread("./FinalProject/volume/data/interim/master_train.csv")
master_test<-fread("./FinalProject/volume/data/interim/master_test.csv")
master<-fread("./FinalProject/volume/data/interim/master.csv")
example_sub<-fread("./FinalProject/volume/data/raw/example_sub (1).csv")

view(master)

#PCA on master file
reddit_num <- master$reddit_num
text <- master$text
id <- master$id

master$reddit_num <- NULL
master$text <- NULL
master$id <- 1:20755

j_data<-data.frame(lapply(master, jitter,factor=0.01))
pca<-prcomp(master)

pca_dt<-data.table(unclass(pca)$x)

tsne<-Rtsne(pca_dt,pca = F,perplexity=150)
tsne_dt<-data.table(tsne$Y)

tsne_dt

tsne_dt$id <- master$id
tsne_dt$reddit_num <- reddit_num

ggplot(tsne_dt[1:200],aes(x=V1,y=V2,col=reddit_num,label=id))+geom_text()


#Setting up XGBoost model

model_train <- tsne_dt[1:200]
model_train <- select(model_train, -id)
model_test <- tsne_dt[201:20755]
model_test <- select(model_test, -id)

y.train <- master_train$reddit_num
y.test <- master_test$reddit_num


model_train$reddit_num

dummies <- dummyVars(reddit_num~ ., data = model_train)
x.train<-predict(dummies, newdata = model_train)
x.test<-predict(dummies, newdata = model_test)

dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)

param <- list(  objective           = "multi:softprob",
                num_class           = 10,
                gamma               =0.02,
                booster             = "gbtree",
                eval_metric         = "mlogloss",
                eta                 = 0.005,
                max_depth           = 20,
                min_child_weight    = 1,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                tree_method = 'hist'
)

watchlist <- list( train = dtrain)

XGBm<-xgb.cv( params=param,nfold=5,nrounds=2500,missing=NA,data=dtrain,print_every_n=1)
XGBm<-xgb.train( params=param,nrounds=330,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=1)

pred <- predict(XGBm, newdata = dtest)

pred <- matrix(pred, nrow = 20555, ncol = 10, byrow = TRUE)

submission <- cbind(master_test$id, pred)
submission <- as.data.frame(submission)

colnames(submission) <- colnames(example_sub)

fwrite(submission,"./FinalProject/volume/data/processed/submission4.csv")

submission
