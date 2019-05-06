#install.packages("randomForest")
#install.packages("")
library(randomForest)
library(party)
rm(list=ls())
dev.off()
dir()
setwd("C:\\Users\\shree\\OneDrive\\Documents\\Quarter 2\\Intermediate Anaytics\\Assignment 4\\Dataset")
mydata2 <-read.csv("C:\\Users\\shree\\OneDrive\\Documents\\Quarter 2\\Intermediate Anaytics\\Assignment 4\\Dataset\\income_employment_full_information.csv.csv")
View(mydata2)
table <- summary(mydata2)
write.csv(table,"table.csv")
str(mydata2)

make_number <- function(var) {
  clean <- var
  clean <- gsub(",", "", clean)
  clean <- gsub("*", "", clean)
  clean <- gsub("#", "", clean)
  return(as.numeric(clean))
}

#mydata2$Short_name <- make_number(mydata2$Short_name)
mydata2$H_MEAN <- make_number(mydata2$H_MEAN)
mydata2$A_MEAN <- make_number(mydata2$A_MEAN)
mydata2$MEAN_PRSE <- make_number(mydata2$MEAN_PRSE)
mydata2$H_MEDIAN <- make_number(mydata2$H_MEDIAN)
mydata2$A_MEDIAN <- make_number(mydata2$A_MEDIAN)
mydata2$TOT_EMP <- make_number(mydata2$TOT_EMP)
mydata2$AREA_NAME <- make_number(mydata2$AREA_NAME)
mydata2$OCC_TITLE <- make_number(mydata2$OCC_TITLE)
mydata2$Short_title <- make_number(mydata2$Short_title)

str(mydata2)
summary(mydata2)


#Droping Coloumns
mydata2$ï..AREA <- NULL
mydata2$AREA_NAME <- NULL
mydata2$OCC_TITLE <- NULL
mydata2$Short_title <- NULL


#Data Cleaning
mydata2 <- na.omit(mydata2)

table1 <- summary(mydata2)
write.csv(table1,"table1.csv")


income.rf <- randomForest(Short_name ~ ., data=mydata2, ntree=800,keep.forest=FALSE, importance=TRUE)
print(income.rf)

table2 <- importance(income.rf)
write.csv(table2,"table2.csv")
table3 <- importance(income.rf, type=1)
write.csv(table3,"table3.csv")
mydata2

#plot(randomForest(Short_name ~ ., data=mydata2, ntree=800,keep.forest=FALSE, importance=TRUE), log="y")
dev.off()
boxplot(mydata2$A_MEAN~mydata2$Short_name)

layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(income.rf, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(income.rf$err.rate),col=1:4,cex=0.8,fill=1:4)



#Minar Assignment
#First Function
sample_data <- USArrests
sample_data
y <- USArrests[,1]
y
x <- USArrests[,-1]
x
m <- lm(y ~ as.matrix(x))
m
#Prediction
yhat1 <- predict(m)
yhat1
USArrests$yhat1 <- yhat1
head(USArrests)
res <- resid(m)
res
#RMSE
RMSE = function(m, o){ 
  sqrt(mean((m - o)^2))
}

RMSE_lm <-RMSE(yhat1,y)
RMSE_lm

#Second Function
mean_murder <- sum(USArrests$Murder)/length(USArrests$Murder)
mean_murder
sd_murder <- sqrt(sum((USArrests$Murder-mean_murder)^2/(length(USArrests$Murder)-1)))
sd_murder
cv = function(sd, mean){
  sd/mean
}
final_cv <- cv(sd_murder,mean_murder)
final_cv

#CV in percentage
final_cv <- final_cv *100
final_cv

#Third Function
a <- USArrests$Murder
sd = function(a, mean){
  sqrt(sum((a-mean)^2/(length(a)-1)))
}
final_sd <- sd(a,mean_murder)
final_sd 

