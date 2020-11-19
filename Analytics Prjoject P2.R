rm(list = ls()) #clears GE
unemp <- read.csv("https://raw.githubusercontent.com/BenAGelman/Project-II-Econ-494-F20/main/Analytics%20Project%20Data%20part%202.csv") #downloads data
dim(unemp) #pritns dimensions of df
summary(unemp) #provides six number summary, checking for NA's
View(unemp) #prints table view of data
mean(unemp$Unemployment_Rate) #prints average unemployment rate
sd(unemp$Unemployment_Rate) #pritns standard deviation of unemployment
var(unemp$Unemployment_Rate) #variance of unemployment rate

unemp$GDP..billions.2 <-unemp$GDP..billions.^2 #Quadratic transformation
unemp$GDP..billions.3 <- unemp$GDP..billions.^3 #cubic tranformation 
unemp$Median_Household_Income2<-unemp$Median_Household_Income^2 #Quadratic transformation
unemp$Median_Household_Income3<-unemp$Median_Household_Income^3 #cubic transformation
unemp$ln_median_household_income<-log(unemp$Median_Household_Income) #logarithmic transformation



p<- .7 #70% of data to train/build models
obs_count<-dim(unemp) [1] #counts observations
training_size<-floor(p*obs_count) #number of observations to be selected for training partition
training_size #returns training_size

set.seed(1234) #makes partition reproducible
train_ind<-sample(obs_count, size = training_size) #creates vector of randomly assinged row numbers 
View(train_ind) #returns chart of random data from 1-35, the sample size
Training<-unemp[train_ind,] #pulls random rows for testing
Testing<-unemp[-train_ind,]#pulls random rows for testing
dim(Training) #returns training dimension
dim(Testing) #returns testing dimensions 


M1<- lm(Unemployment_Rate ~ GDP..billions., Training) #creates model 
summary(M1) #six number summary of M1

PRED_1_IN<- predict(M1, Training) #generates predictions on in sample training 
View(PRED_1_IN)
PRED_1_OUT<-predict(M1,Testing) #generates prediction on the out of sample testing 
View(PRED_1_OUT)

RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Unemployment_Rate)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$Unemployment_Rate)^2)/length(PRED_1_OUT)) #computes out-of-sample                      

RMSE_1_IN #In Sample Error
RMSE_1_OUT # Out of Sample error

#Plotting Model 1 in 2d against both data Partitions
x_grid <- seq(0,1500,100) #creates a grid of x axis value labels 
predictions<- predict(M1, list(GDP..billions.=x_grid))
plot(Training$Unemployment_Rate ~ Training$GDP..billions., col='blue',
  xlab = "GDP (Billions)",
  ylab= "Unemployment Rate",
  main = "GDP vs. Unemployment Rate")
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Unemployment_Rate ~ Testing$GDP..billions., col='red', pch=3)

#Model 2 
M2 <- lm(Unemployment_Rate ~ GDP..billions. + GDP..billions.2, Training)
summary(M2)


#Predictions
PRED_2_IN<- predict(M2, Training) #generates predictions on in sample training 
View(PRED_2_IN)
PRED_2_OUT<-predict(M2,Testing) #generates prediction on the out of sample testing 
View(PRED_2_OUT)

RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$Unemployment_Rate)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$Unemployment_Rate)^2)/length(PRED_2_OUT)) #computes out-of-sample                      

RMSE_2_IN #In Sample Error
RMSE_2_OUT # Out of Sample error

# Plotting Model 2 
x_grid <- seq(0,1500,100) #creates a grid of x axis value labels 
predictions2<- predict(M2, list(GDP..billions.=x_grid, GDP..billions.2=x_grid^2))
plot(Training$Unemployment_Rate ~ Training$GDP..billions., col='blue',
     xlab = "GDP (Billions)",
     ylab= "Unemployment Rate",
     main = "Squared GDP vs. Unemployment Rate")
lines(x_grid, predictions2, col='green', lwd=3)
points(Testing$Unemployment_Rate ~ Testing$GDP..billions., col='red', pch=3)

#Model 3 
M3 <- lm(Unemployment_Rate ~ Median_Household_Income + Median_Household_Income2, Training)
summary(M3) #generates summary diagnostic output 
View(M3)
#generating predictions on training data for model 3

PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)

#Generating Predictions on Test Data for benchmarking
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$Unemployment_Rate)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$Unemployment_Rate)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #In Sample Error
RMSE_3_OUT #Out of sample error

#Plotting Model 3
x_grid <- seq(40000,100000,100) #creates a grid of x axis value labels 
predictions3<- predict(M3, list(Median_Household_Income=x_grid, Median_Household_Income2=x_grid^2))
plot(Training$Unemployment_Rate ~ Training$Median_Household_Income, col='blue',
     xlab = "Median Household Income",
     ylab= "Unemployment Rate",
     main = "Median Household Income vs. Unemployment Rate")
lines(x_grid, predictions3, col='green', lwd=3)
points(Testing$Unemployment_Rate ~ Testing$Median_Household_Income, col='red', pch=3)

#Model 4
M4 <- lm(Unemployment_Rate ~ Median_Household_Income + Median_Household_Income2 + Median_Household_Income3, Training) 
summary(M4) #generates summary diagnostic output

#generating predictions on training data for model 4

PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)

#Generating Predictions on Test Data for benchmarking
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$Unemployment_Rate)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$Unemployment_Rate)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #In Sample Error
RMSE_4_OUT #Out of sample error

#Plotting Model 4
x_grid <- seq(40000,100000,100) #creates a grid of x axis value labels 
predictions4<- predict(M4, list(Median_Household_Income=x_grid, Median_Household_Income2=x_grid^2, Median_Household_Income3=x_grid^3))
plot(Training$Unemployment_Rate ~ Training$Median_Household_Income, col='blue',
     xlab = "Median Household Income",
     ylab= "Unemployment Rate",
     main = "Median Household Income vs. Unemployment Rate")
lines(x_grid, predictions4, col='green', lwd=3)
points(Testing$Unemployment_Rate ~ Testing$Median_Household_Income, col='red', pch=3)
#Plotting Model 5
M5 <- lm(Unemployment_Rate ~ ln_median_household_income, Training)
summary(M5) #generates summary diagnostic output


PRED_5_IN <- predict(M5, Training) #generate predictions on the (in-sample) training data
View(PRED_5_IN)

#Generating Predictions on Test Data for benchmarking
PRED_5_OUT <- predict(M5, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_5_IN<-sqrt(sum((PRED_5_IN-Training$Unemployment_Rate)^2)/length(PRED_5_IN))  #computes in-sample error
RMSE_5_OUT<-sqrt(sum((PRED_5_OUT-Testing$Unemployment_Rate)^2)/length(PRED_5_OUT)) #computes out-of-sample 5
RMSE_5_IN #In Sample Error
RMSE_5_OUT #Out of sample error

#Plotting Model 5
x_grid <- seq(40000,100000,100) #creates a grid of x axis value labels 
predictions5 <- predict(M5, list(ln_median_household_income=log(x_grid)))
plot(Training$Unemployment_Rate ~ Training$Median_Household_Income, col='blue',
     xlab = "Natural Log of Median Household Income",
     ylab= "Unemployment Rate",
     main = "Median Household Income (Logarithmic) vs. Unemployment Rate")
lines(x_grid, predictions5, col='green', lwd=3)
points(Testing$Unemployment_Rate ~ Testing$Median_Household_Income, col='red', pch=3)

#RMSE all together
RMSE_1_IN
RMSE_2_IN
RMSE_3_IN
RMSE_4_IN
RMSE_5_IN

RMSE_1_OUT
RMSE_2_OUT
RMSE_3_OUT
RMSE_4_OUT
RMSE_5_OUT
