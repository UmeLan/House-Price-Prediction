# Read data
df <- read.csv('kc_house_data_withoutOL.csv')
df$id <- NULL
df$zipcode<-NULL
df$long<-NULL
df$lat<-NULL
df$yr_built<-NULL
#data explore
plot(x=df$date, y=df$price, main='Compare Price against Date',type="p", xlab='Date',ylab='Price')
df$date<-NULL
#correlation between price and other variables
library(GGally)
library(ggplot2)
plot1<-ggpairs(data=df, columns=c(1:3,5,6),
               mapping = aes(color = "black"),
               axisLabels="show")
plot1
plot2<-ggpairs(data=df, columns=c(1,4,7,11:12),
               mapping = aes(color = "black"),
               axisLabels="show")
plot2
  #exclude sqft_above, sqft_basement becasue sqft_living = sqft_above+sqft_basement
df$sqft_above<-NULL
df$sqft_basement<-NULL
plot3<-ggpairs(data=df, columns=c(1,8:11),
               mapping = aes(color = "black"),
               axisLabels="show")
plot3
plot4<-ggpairs(data=df, columns=c(1,12:14),
               mapping = aes(color = "black"),
               axisLabels="show")
plot4
# adjust data type
df$waterfront <- as.factor(df$waterfront)
df$price<-as.numeric(df$price)
#year_renovated =0 if no renovated, equals 1 if the house has been renovated no matter the year
df$yr_renovated <- ifelse(df$yr_renovated==0,0,1)
# Seperate 70% of data as training data, 30% as test data
set.seed(12345)
inTrain <- sample(nrow(df), 0.7*nrow(df))
dftrain <- df[inTrain,]
dftest <- df[-inTrain,]
#linear regression
fit1 <- lm(price~., dftrain)
fit1
summary(fit1)
predict_lm <- predict(fit1, newdata = dftest)
Actual <- dftest$price
Metrics <- c("AE","RMSE","MAE")
N1 <-mean(Actual-predict_lm)
N2 <-sqrt(mean((Actual-predict_lm)^2))
N3 <-mean(abs(Actual-predict_lm))
Values <- c(N1,N2,N3)
table1 <- data.frame(Metrics, Values)
table1
plot(Actual,predict_lm,col="blue", main="Actual vs Predicted",pch=1,cex=0.9,
     type="p",xlab="Actual",ylab = "Predicted")
abline(lm(Actual~predict_lm))
#lasso
library(glmnet)
names(df)
set.seed(12345)
X = model.matrix(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+
                   Age+yr_renovated+sqft_living15+sqft_lot15,dftrain)[,-1]
intrain2<- sample(nrow(X), nrow(X)*0.7)
train<-X[intrain2,]
test<-X[-intrain2,]
Y = dftrain$price
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(X,Y,alpha = 1,lambda = grid)
plot(lasso.mod)
cv.out=cv.glmnet(X,Y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
a = model.matrix(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+
                   Age+yr_renovated+sqft_living15+sqft_lot15,dftest)[,-1]
b=dftest$price
out=glmnet(a,b,alpha=1,lambda = grid)
lasso.coef=predict(out, type="coefficients", s=bestlam)[1:14,]
lasso.coef
lasso_pred=predict(lasso.mod,s=bestlam,newx = a)
Metrics <- c("AE","RMSE","MAE")
N13 <-mean(Actual-lasso_pred)
N14 <-sqrt(mean((Actual-lasso_pred)^2))
N15 <-mean(abs(Actual-lasso_pred))
Values <- c(N13,N14,N15)
table5 <- data.frame(Metrics, Values)
table5
 #exlude sqft_lot due to lasso result
df$sqft_lot<-NULL
dftrain$sqft_lot<-NULL
dftest$sqft_lot<-NULL
#regression tree
library(tree)
fit2 <- tree(price~., dftrain)
plot(fit2)
text(fit2,pretty = 1)
summary(fit2)
fit2.cv <- cv.tree(fit2)
fit2.cv
plot(fit2.cv$size,fit2.cv$dev,type = "b")
# best = 7 shown on the plot
fit2_prune <- prune.tree(fit2, best = 7)
plot(fit2_prune)
text(fit2_prune,pretty = 1)
predict_tree <- predict(fit2_prune,dftest)
Metrics <- c("AE","RMSE","MAE")
N7 <-mean(dftest$price-predict_tree)
N8 <-sqrt(mean((predict_tree-dftest$price)^2))
N9 <-mean(abs(dftest$price-predict_tree))
Values <- c(N7,N8,N9)
table3 <- data.frame(Metrics, Values)
table3
#Random Forest
library(randomForest)
random.fit<-randomForest(price~.,data=dftrain,importance=TRUE, mtry=8,ntree=100)
summary(random.fit)
random.predict <- predict(random.fit, newdata = dftest)
random.fit
N10 <-mean(dftest$price-random.predict)
N11 <-sqrt(mean((dftest$price-random.predict)^2))
N12 <-mean(abs(dftest$price-random.predict))
Values <- c(N10,N11,N12)
Metrics <- c('AE','RMSE','MAE')
table4 <- data.frame(Metrics, Values)
table4
importance(random.fit)
varImpPlot(random.fit)
#nerual network
library(neuralnet)
  #sale the data to [0,1] here
df_nn<-df
df_nn$price<-(df_nn$price-min(df_nn$price))/(max(df_nn$price)-min(df_nn$price))
df_nn$bedrooms<-(df_nn$bedrooms-min(df_nn$bedrooms))/(max(df_nn$bedrooms)-min(df_nn$bedrooms))
df_nn$bathrooms<-(df_nn$bathrooms-min(df_nn$bathrooms))/(max(df_nn$bathrooms)-min(df_nn$bathrooms))
df_nn$sqft_living<-(df_nn$sqft_living-min(df_nn$sqft_living))/(max(df_nn$sqft_living)-min(df_nn$sqft_living))
df_nn$floors<-(df_nn$floors-min(df_nn$floors))/(max(df_nn$floors)-min(df_nn$floors))
df_nn$waterfront <- ifelse(df_nn$waterfront==0,0,1)
df_nn$waterfront<-(df_nn$waterfront-min(df_nn$waterfront))/(max(df_nn$waterfront)-min(df_nn$waterfront))
df_nn$view<-(df_nn$view-min(df_nn$view))/(max(df_nn$view)-min(df_nn$view))
df_nn$condition<-(df_nn$condition-min(df_nn$condition))/(max(df_nn$condition)-min(df_nn$condition))
df_nn$grade<-(df_nn$grade-min(df_nn$grade))/(max(df_nn$grade)-min(df_nn$grade))
df_nn$Age<-(df_nn$Age-min(df_nn$Age))/(max(df_nn$Age)-min(df_nn$Age))
df_nn$yr_renovated<-(df_nn$yr_renovated-min(df_nn$yr_renovated))/(max(df_nn$yr_renovated)-min(df_nn$yr_renovated))
df_nn$sqft_living15<-(df_nn$sqft_living15-min(df_nn$sqft_living15))/(max(df_nn$sqft_living15)-min(df_nn$sqft_living15))
df_nn$sqft_lot15<-(df_nn$sqft_lot15-min(df_nn$sqft_lot15))/(max(df_nn$sqft_lot15)-min(df_nn$sqft_lot15))
 #partion dataset
set.seed(12345)
inTrain_nn <- sample(nrow(df_nn), 0.7*nrow(df_nn))
df_nnTrain <- df_nn[inTrain_nn,]
df_nnTest <- df_nn[-inTrain_nn,]

nn <- neuralnet(price~bedrooms+bathrooms+
                  sqft_living+floors+waterfront+view+
                  condition+grade+Age+yr_renovated+
                  sqft_living15+sqft_lot15, 
                data = df_nnTrain, hidden = 0,stepmax = 1e6,
                err.fct = 'sse',linear.output = TRUE)
plot(nn,margin=0.1,uniform=TRUE)
 #calculate test rmse
predict_nn <- compute(nn, df_nnTest[,2:13])$net.result
actual_nn <- df_nnTest$price
RMSE.nn <- sqrt(sum((predict_nn-actual_nn)^2)/nrow(df_nnTest))
RMSE.nn
#used scaled data to run linear model
scale_linear <- lm(price~., df_nnTrain)
scale_linear.pred <- predict(scale_linear, newdata = df_nnTest)
Actual_lm_nn <- df_nnTest$price
RMSE_lm <-sqrt(mean((Actual_lm_nn-scale_linear.pred)^2))
RMSE_lm
