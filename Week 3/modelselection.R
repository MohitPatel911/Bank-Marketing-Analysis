library(readr)
library(ggplot2)
library(GGally)
library(caret) # models
library(corrplot) # correlation plots
library(DALEX) # explain models
library(DescTools) # plots
library(doParallel) # parellel processing
library(dplyr) # syntax
library(inspectdf) # data overview
library(readr) # quick load
library(sjPlot) # contingency tables
library(tabplot) # data overview
library(tictoc) # measure time
library(inspectdf) # data overview
library(readr) # quick load
library(randomForest)
library(GGally)
library(caret) # models
library(corrplot) # correlation plots
bank<-read_csv("C:/Users/sneha/Downloads/bank (1).csv")

bank<-bank[-16]
for (i in 1:nrow(bank)) {
  if (bank[i,16] == 'yes') {
    bank[i,16] <- 1
  }
  else {
    bank[i,16] <- 0
  }
}
bank$deposit<-as.numeric(bank$deposit)
typeof(deposit)

for (i in 1:nrow(bank)) {
  if (bank[i,5] == 'yes') {
    bank[i,5] <- 1
  }
  else {
    bank[i,5] <- 0
  }
}
bank$default<-as.numeric(bank$default)
typeof(default)  

for (i in 1:nrow(bank)) {
  if (bank[i,7] == 'yes') {
    bank[i,7] <- 1
  }
  else {
    bank[i,7] <- 0
  }
}
bank$housing<-as.numeric(bank$housing)
typeof(housing)

for (i in 1:nrow(bank)) {
  if (bank[i,8] == 'yes') {
    bank[i,8] <- 1
  }
  else {
    bank[i,8] <- 0
  }
}
bank$loan<-as.numeric(bank$loan)
typeof(loan)

# is.null(bank)
# is.na(bank)
bank<-bank[!(bank$education=="unknown"),]
bank<-bank[!(bank$job=="unknown"),]
prop.table(table(bank$education))
prop.table(table(bank$job))
attach(bank)
cor(age,balance)
cor(Loan,Deposit)

inspect1<-inspect_cor(bank)
show_plot(inspect1)
inspect2<-inspect_cat(bank)
show_plot(inspect2)

ggplot(
  bank %>%
    group_by(previous, deposit) %>%
    tally(),
  aes(previous, n, fill = deposit)) + geom_col() +theme_bw()

ggplot(
  bank %>%
    group_by(month, deposit) %>%
    tally(),
  aes(month, n, fill = deposit)) +
  geom_col() +
  theme_bw()

# select categorical variables
df_cat <- select_if(bank, is.character) %>% names()
# remove the response
response_ind <- match('deposit', df_cat)
df_cat <- df_cat[-response_ind]

job_list<-aggregate(bank[, 6], list(bank$job), mean)
colnames(job_list)[1]<-"Job"
colnames(job_list)[2]<-"Avg_Balance"
barplot(names.arg=job_list$Job,job_list$Avg_Balance,horiz= TRUE,
        las=1,col="#69b3a2", border = "Purple",xlab="Average Balance", 
        ylab="", main = "Average balance as per Job", cex.names = 0.55)
edu_list<-aggregate(bank[,6], list(bank$education),mean)
colnames(edu_list)[1]<-"Education"
colnames(edu_list)[2]<-"Avg_Balance"
barplot(names.arg=edu_list$Education,edu_list$Avg_Balance,horiz= TRUE,
        las=1,col="#69b3a2", border = "Purple",xlab="Average Balance", 
        ylab="", main = "Average balance as per Education", cex.names = 0.8)

################# Linear Reg ##############################
data<-bank
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))

train <- data[ind==1,]
test <- data[ind==2,]


# Linear Model
lm <- lm(deposit ~ age+balance+default+loan+housing+day+duration+campaign+pdays+previous,data=train)
plot(lm)


summary(lm)

p_lm <- predict(lm, newdata = test)
observed_lm <- test$deposit
head(p_lm)
head(observed_lm)
Sum_Square_Error <- sum((observed_lm - p_lm) ^ 2)
Sum_Square_Total <- sum((observed_lm - mean(observed_lm)) ^ 2)
r_squared <- 1 - (Sum_Square_Error/Sum_Square_Total)
r_squared
rms_error <- sqrt(mean((p_lm - observed_lm)^2))
rms_error

############### Random forest###############################

set.seed(123)

sampling1<-sample(2,nrow(bank),replace=T,prob = c(0.8,0.2))

train<-bank[sampling1==1,]
test<-bank[sampling1==2,]
random_forest <- randomForest((as.factor(deposit))~ balance+campaign+housing+duration+pdays+previous,data=train,importance=TRUE,proximity=TRUE)
plot(random_forest)
pred <- predict(random_forest,test) # Predictions
table(pred)


#Create confusion matrix
CrossTable(test$`deposit`, pred,dnn = c('actual default', 'predicted default'))



########################### SVM #######################
intrain <- createDataPartition(y = bank$deposit, p= 0.8, list = FALSE)
training <- bank[intrain,]
testing <- bank[-intrain,]
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(as.factor(deposit) ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear

test_pred <- predict(svm_Linear, newdata = testing)
test_pred
confusionMatrix(test_pred, as.factor(testing$deposit) )

########################### Xgboost###########################

set.seed(123)
trainIndex <- createDataPartition(bank$deposit,
                                  p = 0.8,
                                  list = FALSE)
dfTrain <- bank[ trainIndex,]
dfTest  <- bank[-trainIndex,]
parameterGrid <-  expand.grid(eta = 0.1, # shrinkage (learning rate)
                              colsample_bytree = c(0.5,0.7), # subsample ration of columns
                              max_depth = c(3,6), # max tree depth. model complexity
                              nrounds = 10, # boosting iterations
                              gamma = 1, # minimum loss reduction
                              subsample = 0.8, # ratio of the training instances
                              min_child_weight = 2) # minimum sum of instance weight

model_xgb <- train(as.factor(deposit)~balance+loan+housing+duration+campaign +pdays+previous,
                   data = train,
                   method = "xgbTree",
                   trControl = trainControl(),
                   tuneGrid=parameterGrid)
model_xgb
