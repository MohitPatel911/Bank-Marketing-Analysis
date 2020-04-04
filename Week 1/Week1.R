library(readr)
library(ggplot2)
install.packages("GGally")
install.packages("dplyr")
library(dplyr)
library(GGally)
install.packages("inspectdf")
library(inspectdf)
install.packages("sjPlot")
library(sjPlot)
#data<-read_csv("C:/Users/sneha/Downloads/bank (1).csv")
summary(bank)  
is.null(bank)
is.na(bank)
summary(bank)

clean1<-bank[-16]
attach(clean1)
typeof(deposit)
clean2<-clean1[c(5,7,8,16)]
clean1[17]<-as.numeric(housing)
colnames(clean1)[17]<-"Housing"
clean1[18]<-as.numeric(default)
colnames(clean1)[18]<-"Default"
clean1[19]<-as.numeric(loan)
colnames(clean1)[19]<-"Loan"
clean1[20]<-as.numeric(deposit)
colnames(clean1)[20]<-"Deposit"
prop.table(table(clean1$V18))
View(clean1)
typeof(housing)

for (i in 1:nrow(clean1)) {
  if (clean1[i,16] =="yes") {
    clean1[i,16] <- 1
  }
  else {
    clean1[i,16] <- 0
  }
}
attach(clean1)
cor(age,balance)
cor(Loan,Deposit)
cor(Housing, Loan)
cor(campaign,Deposit)
cor(duration,Deposit)
cor(age, Deposit)


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

ggplot(
  bank %>%
    group_by(job, deposit) %>%
    tally(),
  aes(month, n, fill = deposit)) +
  geom_col() +
  theme_bw()

ggplot(
  bank %>%
    group_by(education, deposit) %>%
    tally(),
  aes(month, n, fill = deposit)) +
  geom_col() +
  theme_bw()

# select categorical variables
df_cat <- select_if(bank, is.character) %>% names()
# remove the response
response_ind <- match('deposit', df_cat)
df_cat <- df_cat[-response_ind]

# plot categorical variables
for (i in df_cat) {
  print(i)
  
  print(
    sjp.xtab(bank$deposit,
             bank[[i]],
             margin = "row",
             bar.pos = "stack",
             axis.titles = "deposit",
             legend.title = i)
  )
}


