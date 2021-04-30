##import library
library(readxl)
library(caret)
df <- read_excel("C:/Users/Abu Tareq Rony/Downloads/GT Train.xlsx") #read excel data
head(df)
summary(df) #summary statistics
any(is.na(df))#check null value in dataset

set.seed(42)
#Train test split data
sample <- createDataPartition(y = df$Total_Emissions, p = 0.8, list = F) #split ratio 80:20
train <- df[ sample,] #train data
test  <- df[-sample,] #test data
summary(train)
summary(test)
#Multiple Linear Regression with Basic Parameter Tuning
Model1<-lm(Total_Emissions~Ambient_temperature+Ambient_pressure+Ambient_humidity+Air_filter_difference_pressure+Gas_turbine_exhaust_pressure+Turbine_inlet_temperature+Turbine_after_temperature+Turbine_energy_yield+Compressor_discharge_pressure,data = train)
summary(Model1)

# Make predictions
predictions <- predict(Model1, newdata = (test))
predictions

# (a) Prediction error, RMSE
RMSE(predictions, test$Total_Emissions)
# (b) R-square
R2(predictions, test$Total_Emissions)
#(c)coefficients
Model1$coef


library(ggplot2) #Linear regression plot using ggplot packages
ggplot(data = df, aes(x =(Ambient_temperature+Ambient_pressure+Ambient_humidity+Air_filter_difference_pressure+Gas_turbine_exhaust_pressure+Turbine_inlet_temperature+Turbine_after_temperature+Turbine_energy_yield+Compressor_discharge_pressure) , y =Total_Emissions)) +
  geom_point()
##Result:The first step in interpreting the multiple regression analysis is to examine the F-statistic and the associated p-value, at the bottom of model summary.
##In our example, it can be seen that p-value of the F-statistic is < 2.2e-16, which is highly significant. This means that, at least, one of the predictor variables is significantly related to the outcome variable.
##A solution is to adjust the R2 by taking into account the number of predictor variables.


#CART model
library(rpart)
# Build the model
Model2 <- rpart(Total_Emissions~Ambient_temperature+Ambient_pressure+Ambient_humidity+Air_filter_difference_pressure+Gas_turbine_exhaust_pressure+Turbine_inlet_temperature+Turbine_after_temperature+Turbine_energy_yield+Compressor_discharge_pressure, data = train)
summary(Model2)
# Plot the trees
par(xpd = NA) # Avoid clipping the text in some device
plot(Model2)
text(Model2, digits = 3) #add text in plot
##this full tree including all predictor appears to be very complex and can be difficult to interpret in the situation where you have a large data sets with multiple predictors.

# Make predictions on the test data
predictions <- predict(Model2, newdata = (test))
predictions

# (a) Prediction error, RMSE
RMSE(predictions, test$Total_Emissions)
# (b) R-square
R2(predictions, test$Total_Emissions)



##Boosting
require(gbm)
require(MASS)
#GBRT (Gradient Boosted Regression Trees) model
Model3<-gbm(Total_Emissions~Ambient_temperature+Ambient_pressure+Ambient_humidity+Air_filter_difference_pressure+Gas_turbine_exhaust_pressure+Turbine_inlet_temperature+Turbine_after_temperature+Turbine_energy_yield+Compressor_discharge_pressure, data = train,distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
summary(Model3) 
#Summary gives a table of Variable Importance and a plot of Variable Importance
# Make predictions on the test data
predictions <- predict(Model3, newdata = (test))
predictions
# the plot illustrates that the CV error is still decreasing at 10,000 trees.
# (a) Prediction error, RMSE
RMSE(predictions, test$Total_Emissions)
# (b) R-square
R2(predictions, test$Total_Emissions)

##Here, we see that the minimum CV RMSE is 4.991009 (this means on average our model is about 4.991009 off from the actual)

#step wise regression
Model <- lm(Total_Emissions~Ambient_temperature+Ambient_pressure+Ambient_humidity+Air_filter_difference_pressure+Gas_turbine_exhaust_pressure+Turbine_inlet_temperature+Turbine_after_temperature+Turbine_energy_yield+Compressor_discharge_pressure, data = train)
# Step wise regression model
Model4 <- stepAIC(Model, direction = "both", 
                      trace = FALSE)#stepAIC() [MASS package], which choose the best model by AIC
summary(Model4)
plot(Model4)

# Make predictions
predictions <- predict(Model4, newdata = (test))
predictions

# (a) Prediction error, RMSE
RMSE(predictions, test$Total_Emissions)
# (b) R-square
R2(predictions, test$Total_Emissions)
Model4$coef

#find the subset of variables in the data set resulting in the best performing model, that is a model that lowers prediction error.

#ridge regression
library(glmnet) #import library
varmtx <- model.matrix(Ambient_temperature~.-1, data=df)
response <- df$Total_Emissions
ridge <- glmnet(scale(varmtx), response, alpha=0)
#Build model
Model5 <- cv.glmnet(varmtx, response, alpha=0)
plot(ridge, xvar = "lambda", label=T)
abline(v=Model5$lambda.min, col = "red", lty=2) #Draw line
abline(v=Model5$lambda.1se, col="blue", lty=2)
##As a result, we can see that our model has improved significantly as the R-Square value has been increased.


#lasso regression. 
lasso <- glmnet(scale(varmtx), response, alpha=1)
# Cross validation to find the optimal lambda penalization
Model6 <- cv.glmnet(varmtx, response, alpha=1,newx=2)
summary(Model6)
plot(Model6)
#As we can see that, both the mse and the value of R-square for our model has been increased. Therefore, lasso model is predicting good.

#Polynomial regression
Model7<-lm(Total_Emissions ~ poly(Ambient_temperature, 2, raw = TRUE), data = train)
summary(Model7)

# Make predictions
predictions <- predict(Model7, newdata = (test))
predictions

# (a) Prediction error, RMSE
RMSE(predictions, test$Total_Emissions)
# (b) R-square
R2(predictions, test$Total_Emissions)
#(c)coefficients
Model7$coef
##Quadratic regression has a higher R-square value than simple linear regression.


#Simple linear Regression
Model8<-lm(Total_Emissions~Ambient_temperature,data = train)
summary(Model8)
# Make predictions
predictions <- predict(Model8, newdata = (test))
predictions

# (a) Prediction error, RMSE
RMSE(predictions, test$Total_Emissions)
# (b) R-square
R2(predictions, test$Total_Emissions)
#(c)coefficients
Model8$coef
#p-value of the F-statistic is < 2.2e-16, which is highly significant. 
