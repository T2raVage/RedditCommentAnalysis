rm(list=ls())
library(data.table)
library(tidyverse)
library(reshape2)

#reading in data
train<-fread("./FinalProject/volume/data/raw/training_data.csv")
test<-fread("./FinalProject/volume/data/raw/test_file.csv")
example_sub<-fread("./FinalProject/volume/data/raw/example_sub (1).csv")
train_emb<-fread("./FinalProject/volume/data/interim/train_emb.csv")
test_emb<-fread("./FinalProject/volume/data/interim/test_emb.csv")

#Adding index as column so we retain order after melting
train$identifier <- rownames(train)
train$identifier <- as.integer(train$identifier)

#Melting columns from raw train 
train <- melt(data = train, id.vars = c("id","identifier", "text"), measure.vars = c("subredditcars", "subredditCooking", 
                                                              "subredditMachineLearning", "subredditmagicTCG",
                                                              "subredditpolitics", "subredditReal_Estate", 
                                                              "subredditscience", "subredditStockMarket",
                                                              "subreddittravel", "subredditvideogames"))

#Cleaning data to get 'reddit_num' column 
train <-
  train %>%
    filter(value == 1) %>%
    arrange(identifier) %>%
    select(id, variable, text)

levels(train$variable)[1] <- 0
levels(train$variable)[2] <- 1
levels(train$variable)[3] <- 2
levels(train$variable)[4] <- 3
levels(train$variable)[5] <- 4
levels(train$variable)[6] <- 5
levels(train$variable)[7] <- 6
levels(train$variable)[8] <- 7
levels(train$variable)[9] <- 8
levels(train$variable)[10] <- 9

train$variable <- as.integer(as.character(train$variable))
train$reddit_num <- train$variable
train <- select(train, c("id", "text", "reddit_num"))
head(train)
view(train_emb)
#Binding train and train embeddings with cbind
master_train <- cbind(train, train_emb)

#Adding reddit_num to raw test and binding with test embeddings

test$reddit_num <- 11
test$reddit_num <- as.integer(test$reddit_num)

master_test <- cbind(test, test_emb)
view(master_train)

#Binding master train and master test to make master file 
master <- rbind(master_train, master_test)

view(master)

fwrite(master_train,"./FinalProject/volume/data/interim/master_train.csv")
fwrite(master_test,"./FinalProject/volume/data/interim/master_test.csv")
fwrite(master,"./FinalProject/volume/data/interim/master.csv")
