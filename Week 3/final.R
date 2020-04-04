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
library(InformationValue)
library(car)
library(tidyverse)
library(My.stepwise)
library(leaps)
library(MASS)
library(StepReg)
library(stepwise)

bank<-read_csv("C:/Users/Khushboo/Downloads/College/data mining/project/bank.csv")
attach(bank)
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

############# week 2##########################
##################### logistics regression ####################
data<-bank
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]
logit <- glm(`deposit` ~ .,data = train, family =  "binomial")
summary(logit)
predicted <- predict(logit, test, type="response")
predicted
vif(logit)
optCutOff <- optimalCutoff(test$deposit, predicted)[1] 
optCutOff
misClassError(test$deposit, predicted, threshold = optCutOff)
plotROC(test$deposit, predicted)
Concordance(test$deposit, predicted)
confusionMatrix(test$deposit, predicted, threshold = optCutOff)
data.frame(
  RMSE = RMSE(predicted, test$deposit),
  R2 = R2(predicted, test$deposit)
)

###################### new Variable #######################
for (i in 1:nrow(bank)) {
  if (bank[i,8] == 1 || bank[i,7] == 1 ) {
    bank[i,17] <- 1
  }
  else {
    bank[i,17] <- 0
  }
}
colnames(bank)[17]<-"debt"
######################## Logistic Opt ##################################
set.seed(1234)
bank_demo <- bank[-c(7,8)]
View(bank_demo)
attach(bank_demo)
ind <- sample(2, nrow(bank_demo), replace = T, prob = c(0.8, 0.2))
train1 <- bank_demo[ind==1,]
test1 <- bank_demo[ind==2,]
debt.glm <- glm(deposit ~. , family = binomial, data = train1)

step(debt.glm, direction="both")
step.model <- debt.glm %>% stepAIC(trace = FALSE)
coef(step.model)
probabilities <- predict(step.model, test1, type = "response")
vif(debt.glm)
optCutOff <- optimalCutoff(test1$deposit, probabilities)[1] 
optCutOff
misClassError(test1$deposit, probabilities, threshold = optCutOff)
plotROC(test1$deposit, probabilities)
Concordance(test1$deposit, probabilities)
confusionMatrix(test1$deposit, probabilities, threshold = optCutOff)

model.null <- glm(`deposit` ~ 1, family = "binomial" ,data = train1)
step.BIC <- step(model.null, scope = list(lower = model.null, upper = debt.glm), direction = "both", k = log(nrow(train1)))
summary(step.BIC)
pred <- predict(step.BIC, test1, type = "response")
optCutOff1 <- optimalCutoff(test1$deposit, pred)[1]
confusionMatrix(test1$deposit, pred, threshold = optCutOff1)
data.frame(
  RMSE = RMSE(pred, test1$deposit),
  R2 = R2(pred, test1$deposit)
)
plotROC(test1$deposit, pred)
####################### XGBoost #############################
set.seed(123)
trainIndex <- createDataPartition(bank_demo$deposit,
                                  p = 0.8,
                                  list = FALSE)
dfTrain <- bank_demo[ trainIndex,]
dfTest  <- bank_demo[-trainIndex,]
parameterGrid <-  expand.grid(eta = 0.08, # shrinkage (learning rate)
                              colsample_bytree = c(0.5,0.7), # subsample ration of columns
                              max_depth = c(10,35), # max tree depth. model complexity
                              nrounds = 100, # boosting iterations
                              gamma = 1, # minimum loss reduction
                              subsample = 0.75, # ratio of the training instances
                              min_child_weight = 2) # minimum sum of instance weight

model_xgb <- train(as.factor(deposit)~.,
                   data = dfTrain,
                   method = "xgbTree",
                   trControl = trainControl(),
                   tuneGrid=parameterGrid)
model_xgb
impVar <- varImp(model_xgb)
impVar