
###Final Project###

#Installing the following packages
install.packages("lubridate")
library(lubridate)

install.packages("dplyr")
library (dplyr)

install.packages("corrplot")
library(corrplot)

install.packages("ggplot2")
library(ggplot2)

install.packages("ggpubr")
library(ggpubr)

install.packages("car")
library(car)

install.packages("carData")
library(carData)

install.packages("tidyverse")
library(tidyverse)

library(RColorBrewer)

install.packages("MASS")
library (MASS)

install.packages("leaps")
library(leaps)

install.packages("glmnet")
library(glmnet)

install.packages("Metrics")
library(Metrics)

library(broom)

#Importing the data set
path<-"C:\\Users\\dsouz\\Desktop\\SolarPrediction.csv"
solar<-read.csv(path, header = TRUE, sep = ',')
solar
str(solar)

#Renaming the solar dataset
solar1<-solar

#Converting UNIXTime into datetime format to check if the variable can be used for future analysis.
as_datetime(solar1$UNIXTime)

#Adding the datetime variable to the solar1 dataset
solar1$datetime<-as_datetime(solar1$UNIXTime)

#Rearranging the datetime variable in the dataset. Calling this dataset 'solar2'
solar2 <- solar1[, c(1, 12, 2, 3, 4, 5, 6, 7,8, 9, 10, 11)]

#Created solar3 for future analysis with the variable time.
solar3 <- lubridate::with_tz(solar2$datetime, "HST")


#Choosing select columns to work with and prepare a model. Calling the dataset 'solar4'
solar4<-solar2 %>% select(Radiation, Temperature, Pressure, Humidity, WindDirection.Degrees., Speed)


#Exploratory Data analysis

summary(solar4)
str(solar4)

#Histogram and Boxplot
hist(solar4$Radiation, main = "Solar Radiation")
boxplot(solar4$Radiation, main = "Solar Radiation")
par(mfrow = c(2,2))

#Scatterplot Matrix
car::scatterplotMatrix(solar4, spread = FALSE, smoother.args = list(lty = 2), main = "Scatter Plot Matrix")

#Used the cor() fucntion to produce a correlation matrix with the numeric variables of 'solar2' dataset.
cors<-cor(solar4, use = 'pairwise')
cors

#Produced a plot of the correlation matrix
corrplot(cors, type = 'upper', col = brewer.pal(n=8, name = "RdYlBu"))

#The one with the strongest correlation with Radiation
car::scatterplot(Radiation ~ Temperature, data = solar4)

#The one with the lowest corrrelation with Radiation
car::scatterplot(Radiation ~ Speed, data = solar4)

#Using 5 continuous variables, to fit a regression model
fit<-lm(formula = Radiation ~ Temperature + Pressure + Humidity + WindDirection.Degrees. + Speed, data = solar4)
summary(fit)


#Checked AIC and BIC for future analysis
AIC(fit)
BIC(fit)

#Using the plot() function to plot the regression model. 4 graphs produced.
plot(fit)
par(mfrow = c(2,2))
plot(fit)
dev.off()

#Checking for multicollinearity in the model
vif(fit)

#Splitting the data
#Tried splitting the data using another method (using the Himsc package)
solar_split = split(solar4, cut(solar4$Radiation, 2))

solar_low <- as.data.frame(solar_split[[1]])
solar_high <- as.data.frame(solar_split[[2]])


#Attempted different combinations of predictor variablesto fit a better model
fit2<-lm(formula = Radiation ~ Temperature + Pressure + Humidity + Speed, data = solar4)
fit2<-lm(formula = Radiation ~ Temperature + WindDirection.Degrees. + Speed, data = solar4)
fit2<-lm(formula = Radiation ~ Temperature + Pressure + Humidity + WindDirection.Degrees., data = solar4)
fit2<-lm(formula = Radiation ~ Temperature + Pressure + Humidity, data = solar4)
fit2<-lm(formula = Radiation ~ Temperature + Pressure, data = solar4)
fit2<-lm(formula = Radiation ~ Temperature + Pressure + WindDirection.Degrees.+ Speed, data = solar4)

#Conducted an ANOVA test to check if a 5 or 4 predictor model is more suited
anova(fit, fit2)
AIC(fit, fit2)
BIC(fit, fit2)

#Attempting the forward, backward and step wise feature selection (used 2 types of code for stepwise).
model_step <- step(lm(Radiation ~ Temperature + Pressure + Humidity + WindDirection.Degrees. + Speed, data = solar4), direction = 'both')
summary (model_step)
 #OR
stepAIC(fit, direction = "both")
step_summary<-stepAIC(fit, direction = "both")
summary(step_summary)

stepAIC(fit, direction = "forward")
fwd_summary<-stepAIC(fit, direction = "forward")
summary(fwd_summary)

stepAIC(fit, direction = "backward")
bkwd_summary<-stepAIC(fit, direction = "backward")
summary(bkwd_summary)



#Attempting to correct any issues that have been discovered in the model (especially skewness). checking if the changes improved the model after transformation.

#Viewing the distribution of the response variable
hist(solar4$Radiation)

#Using powerTransform function to determine the transformation.
summary(powerTransform(solar4$Radiation))

#Attempted the Log transformation.
lm_log.model = lm(log1p(Radiation) ~ log1p(Temperature) + log1p(Pressure) + log1p(Humidity) + log1p(WindDirection.Degrees.) + log1p(Speed), data = solar4)
summary(lm_log.model)

#Applying transformation by taking the square root of the response variable and reviewing the results.
solar5<-solar4
solar5$Radiation_sqrt<-sqrt(solar5$Radiation)

solar5<-solar5 %>% select(Radiation_sqrt, Temperature, Pressure, Humidity, WindDirection.Degrees., Speed)

#Checking for improvements in the solar4 data and if the distribution has started to appear more normal.
hist(solar5$Radiation_sqrt)
boxplot(solar5$Radiation_sqrt)
par(mfrow = c(2,2))
dev.off()

summary(solar4$Radiation)
summary(solar5$Radiation_sqrt)

#Tried fitting a new model after applying the square root transformation. The R-Squared value increasedS
fitnew<-lm(formula = Radiation_sqrt ~ Temperature + Pressure + Humidity + WindDirection.Degrees. + Speed, data = solar5)
summary(fitnew)

fit3 <-lm(formula = Radiation_sqrt ~ Temperature + Pressure + Humidity + WindDirection.Degrees., data = solar5)
summary(fit3)

#checking for multicollinearity.
vif(fitnew)

#Best Subset Regression
leaps<-regsubsets(Radiation ~ Temperature + Pressure + Humidity + WindDirection.Degrees. + Speed, data =solar4, nbest =5)
summary(leaps)
plot(leaps, scale = 'adjr2')
coef(leaps, 21)

#Testing the best subset regression method on the transformed response variable 'Radiation_sqrt'.
leaps2<-regsubsets(Radiation_sqrt ~ Temperature + Pressure + Humidity + WindDirection.Degrees. + Speed, data =solar5, nbest =5)
summary(leaps2)
plot(leaps2, scale = 'adjr2')
coef(leaps2, 21)

#Tried working with the time variable
solar2$Time<- as.numeric(solar2$Time)

#Testing the best subset regression method on the transformed response variable 'Radiation_sqrt' with 4 predictors
leaps3<-regsubsets(Radiation_sqrt ~ Temperature + Pressure + Humidity + WindDirection.Degrees., data =solar5, nbest =5)
summary(leaps3)
plot(leaps3, scale = 'adjr2')
#######################################################################################################################
#Implementing Regularization on both non-transformed and transformed variable.
data <- solar4

data2 <- solar5 #with transformed variable

#Splitting the data into Train and test Datasets
set.seed(123)
trainIndex<-sample(x = nrow(data), size = nrow(data) * 0.7)
train<-data[trainIndex,]
test<-data[-trainIndex,]
#################################################
trainIndex2<-sample(x = nrow(data2), size = nrow(data2) * 0.7)
train2<-data2[trainIndex2,]
test2<-data2[-trainIndex2,]


train_x <- model.matrix(Radiation ~., train)[,-1]
test_x <- model.matrix(Radiation ~., test)[,-1]
#######################################################
train_x2 <- model.matrix(Radiation_sqrt ~., train2)[,-1]
test_x2 <- model.matrix(Radiation_sqrt ~., test2)[,-1]


train_y <- train$Radiation
test_y <- test$Radiation
#######################################################
train_y2 <- train2$Radiation_sqrt
test_y2 <- test2$Radiation_sqrt


#Finding the best value of lambda
#Find the best lambda using cross-validation
set.seed(123)
cv.lasso <- cv.glmnet(train_x, train_y, nfolds = 10)
plot(cv.lasso)
dev.off()
########################################################
cv.lasso2 <- cv.glmnet(train_x2, train_y2, nfolds = 10)
plot(cv.lasso2)


#optimal value of lambda; minimizes the prediction error
# lambda min - minimizes out of sample loss
# lambda 1se - largest value of lambda within 1 standard error of lambda min
log (cv.lasso$lambda.min)
log (cv.lasso$lambda.1se)
########################
log (cv.lasso2$lambda.min)
log (cv.lasso2$lambda.1se)


cv.lasso$lambda.min
0.6588425
cv.lasso$lambda.1se
9.783602
########################
cv.lasso2$lambda.min
cv.lasso2$lambda.1se


#Fit the final model on the training data using lambda.min
#alpha = 1 for Lasso (L2)
model.min_l <- glmnet(train_x, train_y, alpha = 1, lambda =cv.lasso$lambda.min)
model.min_l
coef(model.min_l)
###############################################################################
model.min_l2 <- glmnet(train_x2, train_y2, alpha = 1, lambda =cv.lasso2$lambda.min)
model.min_l2
coef(model.min_l2)


#Fit the final model on the training data using lambda.1se
#alpha = 1 for Lasso (L2)
model.1se_l <- glmnet(train_x, train_y, alpha = 1, lambda =cv.lasso$lambda.1se)
model.1se_l
coef(model.1se_l)
###############################################################################
model.1se_l2 <- glmnet(train_x2, train_y2, alpha = 1, lambda =cv.lasso2$lambda.1se)
model.1se_l2
coef(model.1se_l2)


#Train set predictions for lasso
pred.train <- predict(model.1se_l, newx = train_x)
train.rmse <- rmse(train_y, pred.train)
train.rmse
##################################################
pred.train2 <- predict(model.1se_l2, newx = train_x2)
train.rmse2 <- rmse(train_y2, pred.train2)
train.rmse2


#Test set predictions for lasso
pred.test <- predict(model.1se_l, newx = test_x)
test.rmse <- rmse(test_y, pred.test)
test.rmse
###################################################
pred.test2 <- predict(model.1se_l2, newx = test_x2)
test.rmse2 <- rmse(test_y2, pred.test2)
test.rmse2


#######################################################################################################################
# RIDGE REGRESSION
cv.ridge <- cv.glmnet(train_x, train_y, alpha = 0, nfolds = 10)
plot(cv.ridge)
##############################################################
cv.ridge2 <- cv.glmnet(train_x2, train_y2, alpha = 0, nfolds = 10)
plot(cv.ridge2)


log (cv.ridge$lambda.min)
log (cv.ridge$lambda.1se)
#####################################
log (cv.ridge2$lambda.min)
log (cv.ridge2$lambda.1se)


cv.ridge$lambda.min
23.13322
cv.ridge$lambda.1se
40.42596
#######################################
cv.ridge2$lambda.min
cv.ridge2$lambda.1se


#Fit the final model on the training data using lambda.min
#alpha = 0 for Ridge (L1)
model.min_r <- glmnet(train_x, train_y, alpha = 0, lambda =cv.ridge$lambda.min)
model.min_r
coef(model.min_r)
###############################################################################
model.min_r2 <- glmnet(train_x2, train_y2, alpha = 0, lambda =cv.ridge2$lambda.min)
model.min_r2
coef(model.min_r2)


#Fit the final model on the training data using lambda.1se
#alpha = 0 for Ridge (L1)
model.1se_r <- glmnet(train_x, train_y, alpha = 0, lambda =cv.ridge$lambda.1se)
model.1se_r
coef(model.1se_r)
###############################################################################
model.1se_r2 <- glmnet(train_x2, train_y2, alpha = 0, lambda =cv.ridge2$lambda.1se)
model.1se_r2
coef(model.1se_r2)


#Train set predictions for Ridge
pred.train_r <- predict(model.1se_r, newx = train_x)
train.rmse_r <- rmse(train_y, pred.train_r)
train.rmse_r
####################################################
pred.train_r2 <- predict(model.1se_r2, newx = train_x2)
train.rmse_r2 <- rmse(train_y2, pred.train_r2)
train.rmse_r2


#Test set Predictions for Ridge
pred.test_r <- predict(model.1se_r, newx = test_x)
test.rmse_r <- rmse(test_y, pred.test_r)
test.rmse_r
#####################################################
pred.test_r2 <- predict(model.1se_r2, newx = test_x2)
test.rmse_r2 <- rmse(test_y2, pred.test_r2)
test.rmse_r2


##########################################################################################################
#Displaying coefficients of ols model with no regularization
ols <- lm(Radiation ~., data = train)
coef(ols)
##########################################################################################################
ols2 <- lm(Radiation_sqrt ~., data = train2)
coef (ols2)

#View RMSE of full model (ols) and its train and test set
pred.ols_data<-predict(ols, new = data)
rmse(data$Radiation, pred.ols_data)


pred.ols_train<-predict(ols, new = train)
rmse(train$Radiation, pred.ols_train)


pred.ols_test <-predict(ols, new = test)
rmse(test$Radiation, pred.ols_test)
#######################################################################################
pred.ols_data2<-predict(ols2, new = data2)
rmse(data2$Radiation_sqrt, pred.ols_data2)

pred.ols_train2<-predict(ols2, new = train2)
rmse(train2$Radiation_sqrt, pred.ols_train2)

pred.ols_test2 <-predict(ols2, new = test2)
rmse(test2$Radiation_sqrt, pred.ols_test2)

##########################################################################################################
#Rough work to attempt time series

#renaming solar2 as solarnew
solarnew<-solar2

#Tried to split the datetime variable into 'date' and 'time' (this didn't quite work)
tidyr::separate(solarnew, datetime, c("date", "time"), sep = " ", remove = FALSE)

#Tried implementing time series for radiation
radiationtimeseries <- ts(solarnew$Radiation, frequency=12, start=c(2016,2))
radiationtimeseries2 <- ts(solarnew$Radiation, frequency=12, start=c(2016))
plot(radiationtimeseries2)

#Finalized on this model
radiationtimeseries2 <- ts(solarnew$radiation, frequency = 365, start = c(2016,2), end = c(2017,1))
plot(radiationtimeseries2)

#Splitting the "datetime" column usind the POSIXct () fucntion (to attempt the time series with it)
solarnew$time <- format(as.POSIXct(solarnew$datetime,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
solarnew$date <- format(as.POSIXct(solarnew$datetime,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")

##########################################################################################################
##Final attempt at time series
#Importing the data set
path<-"SolarPrediction.csv"
solar<-read.csv(path, header = TRUE, sep = ',')
solar
str(solar)


install.packages("xts")                 
library("xts")

#Converting UNIXTime into datetime format
solar$UNIXTime <- as_datetime(solar$UNIXTime)

# Convert data frame to time series
data_ts <- xts(solar$Radiation, solar$UNIXTime)

# Print time series
data_ts                                      

#check the data if it is in a time series format
class(data_ts)

#This is the start of the time series
start_1 <- start(data_ts)

#This is the end of the time series
end_1 <- end(data_ts)

#to check the summary of time series
summary(data_ts)

#Plot the time series
plot(data_ts)
abline(reg=lm(data_ts~time(data_ts)))

dat_ts <- ts(data_ts, start = c(2016, 9), end = c(2017, 4), frequency = 12)
plot(dat_ts)


