##############################################
##    ASSIGNMENT 3                      ######
#Introduction to Practical Data Sciences######
##    Exam Number B187939               ######
##     Part 3: Regression                ######
##############################################

#install needed packages
install.packages("data.table")
install.packages("ggpubr")
install.packages('csvread')
install.packages('ggplot2')

#calling the needed libraries:
library('data.table')
library("ggpubr")
library("dplyr")
library(tidyverse)
library("ggplot2")


# read the weather happy csv file
weather_happy <- read.csv(file = 'weather_happy.csv')
weather_happy


################################
##Process to split the data:  ##
################################


n1 <- nrow(weather_happy)

#set number of testing set (20% of total data)
ntest <- round(0.2*n1)
ntest

#set number of training  set (80% of total data)
ntrain <- n1 - ntest
ntrain

# Split the data into two sets: training and test sets
train_rows <- sample(1:n1, ntrain)
train_rows

wh_train <- weather_happy[train_rows,]
wh_train

wh_test <- weather_happy[-train_rows,]


# Build the model using the training set (80% of the patients)
model <- lm(Mean_Rating ~ Tmax + Tmin  + AF + Rain,
            data = weather_happy
            )


# Summarize the model
summary(model)


#Predict using training set:
predict(model)


#Measurements of model prediction error using training set:

rmse_train <- sqrt(mean((predict(model,wh_train) - wh_train$Mean_Rating)^2))
rmse_train
#0.08217


mae_train <- mean(abs(predict(model, wh_train) - wh_train$Mean_Rating))
mae_train
#0.060

##Create a explo_var df that comprises of the covariates for ease of use later
explo_var <- wh_train$Tmax + wh_train$Tmin +wh_train$AF+ wh_train$Rain


###############################
##Plot using training set ####
###############################

plot(explo_var, wh_train$Mean_Rating,
     xlab="Different variables",
     ylab="Average Happy Rating",main="Average Happy Rating vs different covariates",
     col=2)
abline(lm(wh_train$Mean_Rating ~ explo_var), col = 'blue')

# no linear relationship


fitted <- predict(model)
resid <- residuals(model)


hist(resid, xlab= 'Residuals', ylab='Frequency', main = ' Histogram of redisuals of training model')

df2 <- data.frame(
  resid_train = residuals(model),
  pred_train = predict(model))

#Use ggplot: predicted values vs abs residuals
plot_train <- ggplot(df2, aes(pred_train, abs(resid_train))) +
  geom_point() +
  geom_smooth()

plot_train +  ggtitle("Predicted Values from training set vs absolute values of residuals") +
  xlab("Predicted Values using train set") + ylab("Absolute values of residuals")


###
#create predicted df that made of predicted values and covariates
predicted_df <- data.frame(Meanrating_pred = predict(model, wh_train), 
                           covariates = wh_train$Tmax + wh_train$Tmin +wh_train$AF+ wh_train$Rain)



# this is the predicted line of multiple linear regression
plot_mul <- ggplot(data = predicted_df, aes(x = covariates, y = Meanrating_pred)) + 
  geom_point(color='red') +
  geom_line(color='blue',data = predicted_df, aes(x=covariates, y=Meanrating_pred))

plot_mul +  ggtitle("Plot of Covariates vs Average Happy Rating using training set") +
  xlab("Covariables") + ylab("Mean Happy Rating")




#################################################
#####           Use test set              #######
#################################################

#rmse, mae for test set using model from training data but with test set (out of sample evaluation)
rmse <- sqrt(mean((predict(model, wh_test) - wh_test$Mean_Rating)^2))
rmse
#0.0514

mae <- mean(abs(predict(model, wh_test) - wh_test$Mean_Rating))
mae
#0.0471


###########################
##    Make predictions  ###
###########################

#set random variables:
set.seed(123)

predict_test <- predict(model, newdata = wh_test, type = 'response')
predict_test


##################################################
## Evaluating prediction error with test set  ####
##################################################

###Another way to measure the regression model performance: find R2, RMSE and MAE
predictions <- model %>% predict(wh_test)

data.frame( R2 = R2(predictions, wh_test$Mean_Rating),
            RMSE = RMSE(predictions, wh_test$Mean_Rating),
            MAE = MAE(predictions, wh_test$Mean_Rating))

#find prediction error rate from test set:
prediction_error_rate <- RMSE(predictions, wh_test$Mean_Rating/mean(wh_test$Mean_Rating))
prediction_error_rate
#6.47


#find prediction error rate from test set:
prediction_error_rate <- RMSE(predictions, wh_test$Mean_Rating)/mean(wh_test$Mean_Rating)
prediction_error_rate
# 0.007 quite low

cor(wh_test$Mean_Rating, predict_test)
# -0.23

########################################
####     PLOT  with test set       #####
########################################

#find residuals between observed and predicted in test set:
residuals_test <- wh_test$Mean_Rating - predict_test 

#create subset of predict values using test set with prediction interval:
pred_test_int <- predict(model, newdata = wh_test, interval = "prediction")

#bind all together in 1 dataframe to make plot
mydata <- cbind(wh_test, pred_test_int, residuals_test)
View(mydata)


#subset of covariates in test set:
covariates_test=wh_test$Tmax + wh_test$Tmin + wh_test$AF + wh_test$Rain

#create plot of covariates vs VAS at 12 months from test set:
p <- ggplot(mydata, aes(covariates_test, wh_test$Mean_Rating)) +
  geom_point() +
  stat_smooth(method = lm)

p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  ggtitle("Plot of Covariates vs Average Happy Rating using testing set with Prediction Interval in red") +
  xlab('Different variables') + ylab('Average Happy Rating') + theme_classic()

#create plot of predicted values vs observed values:
plottest <- ggplot(mydata, aes(x = fit, y= Mean_Rating)) +
  geom_point(color='red') +
  stat_smooth(method = 'lm', color = 'blue') +
  ggtitle("Plot of Predicted values vs Actual Avg Happy Rating using testing set") +
  xlab("Predicted Values for Avg Happy Rating") + ylab("Actual Avg Happy Rating") + theme_classic()

plottest
# it can be seen there is no apparent linear relatioship here since the line is almost horizontal.



####################################
##Use Random Forest regression   ###
####################################

install.packages('randomForest')
library(randomForest)


set.seed(42)

# Set model
model_forest <- randomForest(Mean_Rating ~ Tmax + Tmin  + AF + Rain,
                      data = wh_train, replace=T,ntree=100)

# Find measurement metrics

#Importance
imp<-importance(model_forest)
vars<-dimnames(imp)[[1]]
imp<-data.frame(vars=vars,imp=as.numeric(imp[,1]))
imp<-imp[order(imp$imp,decreasing=T),]
par(mfrow=c(1,2))
varImpPlot(model_forest,main='Variable Importance Plot: Base Model')
plot(model_forest,main='Error vs No. of trees plot: Base Model')


# Tmin has most important feature although The difference in importance in diff 
#variables is minimal.



#################################################
### PLOT Forest Regression using training set####
##################################################

pred_forest_train<-predict(object=model_forest,newdata=wh_train)

actual_train<-wh_train$Mean_Rating

result_train<-data.frame(actual_train=actual_train,predicted=pred_forest_train)
paste('Function Call: ', model_forest$call)

paste('Mean Squared error: ',mean(model_forest$mse))
paste('Root Mean Squared error: ',mean(sqrt(model_forest$mse)))


ggplot(result_train)+
  geom_point(aes(x=actual_train,y=pred_forest_train,color=pred_forest_train-actual_train),alpha=0.7)+
  ggtitle('Plotting Error')

###############################################################
## Plot Forest Regression Using test set #######################
##############################################################


pred_forest_test<-predict(object=model_forest,newdata=wh_test)

actual<-wh_test$Mean_Rating

result<-data.frame(actual_test=actual,predicted_test=pred_forest_test)
paste('Function Call: ', model_forest$call)

paste('Mean Squared error: ',mean(model_forest$mse))
paste('Root Mean Squared error: ',mean(sqrt(model_forest$mse)))


#############################################
### PLOT Forest Regression using test set###
###########################################
ggplot(result)+
  geom_point(aes(x=actual_test,y=pred_forest_test,color=pred_forest_test-actual),alpha=0.7)+
  ggtitle('Plotting Error')


##No correlation seen here either


model_forest_Tmin <- randomForest(Mean_Rating ~ Tmin,
                             data = wh_train, replace=T,ntree=100)

pred_forest_train_tmin<-predict(object=model_forest_Tmin,newdata=wh_train)

actual_train_tmin<-wh_train$Mean_Rating

result_train_tmin<-data.frame(actual_train_tmin=actual_train_tmin,predicted_tmin=pred_forest_train_tmin)

paste('Mean Squared error: ',mean(model_forest_Tmin$mse))
paste('Root Mean Squared error: ',mean(sqrt(model_forest_Tmin$mse)))

ggplot(result_train_tmin)+
  geom_point(aes(x=actual_train_tmin,y=pred_forest_train_tmin,color=pred_forest_train_tmin-actual_train_tmin),alpha=0.7)+
  ggtitle('Plotting Error')
# if Tmin alone it shows some sort of relationship here





        