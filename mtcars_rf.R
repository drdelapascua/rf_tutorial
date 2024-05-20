#############
##
## Analysis of mtcars data using random forest
## Tuning and optimizing a RF model to predict mpg using the other variables
## Mark A. Hayes, USFWS
## 6/28/2023
## 
##
#############

#rm(list=ls()) # Clear the global environment as needed


## Libraries

library(randomForest)  

# Load the mtcars dataset
data(mtcars)

# View the first 6 rows of the dataset
head(mtcars, 6)

# Print the summary of the dataset
summary(mtcars)
dim(mtcars)

## Use the random forest algorithm to predict mpg using the other variables
## and then tune and optimize the RF model. 

# Create a training and testing set
set.seed(123)
train_index <- sample(nrow(mtcars), 0.75 * nrow(mtcars))
train <- mtcars[train_index, ]
test <- mtcars[-train_index, ]


## Tuning using the key hyperparameters

# hyperparameters to try
# ntree = 500, 1000, 1500. This is the number of trees in the forest
# mtry = square root of number of variables in the model. Controls the number of variables to randomly sample as candidates at each split. 
# max_depth = 5, 10, 15. Command controls the maximum depth of the decision trees in the random forest.
# min_samples_split = 5. Controls the minimum number of samples required to split an internal node in a decision tree.
# min_samples_leaf = 1

## RF model 1
rf1 <- randomForest(mpg ~ ., data = train, ntree = 500, mtry = 3,
        max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf1

# Make predictions on the test set
predictions <- predict(rf1, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$mpg)^2)
print(rmse) # 4.10

## RF model 2
rf2 <- randomForest(mpg ~ ., data = train, ntree = 1000, mtry = 3,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf2

# Make predictions on the test set
predictions <- predict(rf2, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$mpg)^2)
print(rmse) # 4.39

## RF model 3
rf3 <- randomForest(mpg ~ ., data = train, ntree = 1500, mtry = 3,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf3

# Make predictions on the test set
predictions <- predict(rf3, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$mpg)^2)
print(rmse) # 4.27

## Using mtree = 1000

## RF model 4, mtry - 5
rf4 <- randomForest(mpg ~ ., data = train, ntree = 1000, mtry = 5,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf4

# Make predictions on the test set
predictions <- predict(rf4, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$mpg)^2)
print(rmse) # 4.18

## RF model 5, mtry = 10
rf5 <- randomForest(mpg ~ ., data = train, ntree = 1000, mtry = 10,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf5

# Make predictions on the test set
predictions <- predict(rf5, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$mpg)^2)
print(rmse) # 4.04

## Use mtry = 5

## max_depth = 5, 10, 15

## RF model 6, max_depth = 10
rf6 <- randomForest(mpg ~ ., data = train, ntree = 1000, mtry = 5,
                    max_depth = 10, min_samples_split = 5, min_samples_leaf = 1)
rf6

# Make predictions on the test set
predictions <- predict(rf6, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$mpg)^2)
print(rmse) # 3.99

## RF model 7, max_depth = 15
rf7 <- randomForest(mpg ~ ., data = train, ntree = 1000, mtry = 5,
                    max_depth = 15, min_samples_split = 5, min_samples_leaf = 1)
rf7

# Make predictions on the test set
predictions <- predict(rf7, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$mpg)^2)
print(rmse) # 3.80

## max_depth of 15 is best

## Model 7 is the best model.

# Plot the variable importance for the best model, Model 7, using node purity

varImpPlot(rf7, sort = TRUE, main = "Variable Importance Plot")

# Print varImpPlot

dev.print(tiff, "varImpPlot_model7.tiff", height=4, width=6, units='in', res=300)

# Plot the variable importance for the best model, Model 7, using Gini index

# Calculate the mean decrease in gini index for each variable
importance <- importance(rf7, type = 1)

# Plot the variable importance plot
varImpPlot(importance) # This is not working. Not sure why.

# Print varImpPlotGini

dev.print(tiff, "varImpPlotGini_model7.tiff", height=4, width=6, units='in', res=300)



## Also, make sure that each hyperparameter setting makes sense and we understand what it's doing. 

## Before saving set the file path for saving code:
getwd()
setwd("C:/temp/DFP_2023/code")

## Then Quit Session

# End
