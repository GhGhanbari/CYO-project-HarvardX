
# Reading the data set and applying required packages

maindata <-read.csv("water_potability.csv")

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")


library(dplyr)
library(caret)
library(ggplot2)
library(tidyverse)




# Data analysis
#We explore data set to see its variable names and variable classes.

head(maindata)

#Variable names

names(maindata)

str(maindata)


#Exploring if there are any missing values
sapply(maindata, function(x) sum(is.na(x)))



#Dropping missing values

maindata <- na.omit(maindata)

# Changing Potability columns values from 0 and 1 to "potable", and "non-potable" and making them as factor
data <- maindata %>% mutate(Potability=as.factor(ifelse(Potability==1, "potable","non-potable")))


summary(data)

#Creating train and test data
set.seed(2007)
test_index <- createDataPartition(y = data$Potability, times = 1, p = 0.2, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]

# Data visualization


#Bar plot of the data set to see the count of potable and non-potable classes
data %>% ggplot(aes(Potability)) +geom_bar(width = 0.8,color="white") +ggtitle("Bar plot of potable and non-potable classes")


# histogram of "ph" variable
train %>% ggplot(aes(ph,fill=Potability)) + geom_histogram(bins=30, color="white") +
  ggtitle("Histogram of PH attribute for potable and non-potable classes")


#Hardness variable distribution:
# histogram of "Hardness" variable
train %>% ggplot(aes(Hardness)) + geom_histogram(bins=30,color="white")+ facet_grid(~ Potability) +
  ggtitle("Histogram of Hardness attribute for potable and non-potable classes")


#Solids variable distribution
# histogram of "Solids" variable
train %>% ggplot(aes(Solids,fill=Potability)) + geom_histogram(bins=30,color="white") +
  ggtitle("Histogram of Solids attribute for potable and non-potable classes")


#Chloramines variable distribution:
# histogram of "Chloramines" variable
train %>% ggplot(aes(Chloramines,fill=Potability)) + geom_histogram(bins=30,color="white") + 
  ggtitle("Histogram of Chloramines attribute for potable and non-potable classes")


#Sulfate variable distribution:
# histogram of "Sulfate" variable
train %>% ggplot(aes(Sulfate,fill=Potability)) + geom_histogram(bins=30, color="white") + 
  ggtitle("Histogram of Sulfate attribute for potable and non-potable classes")


#Conductivity variable distribution:
# histogram of "Conductivity" variable
train %>% ggplot(aes(Conductivity,fill=Potability)) + geom_histogram(bins=30, color="white") +
  ggtitle("Histogram of Conductivity attribute for potable and non-potable classes")


#Organic_carbon variable distribution:
# histogram of "Organic_carbon" variable
train %>% ggplot(aes(Organic_carbon,fill=Potability)) + geom_histogram(bins=30, color="white") +
  ggtitle("Histogram of Organic_carbon attribute for potable and non-potable classes")


#Trihalomethanes variable distribution:
# histogram of "Trihalomethanes" variable
train %>% ggplot(aes(Trihalomethanes,fill=Potability)) + geom_histogram(bins=30, color="white") +
  ggtitle("Histogram of Trihalomethanes attribute for potable and non-potable classes")


#Turbidity variable distribution:
# histogram of "Turbidity" variable
train %>% ggplot(aes(Turbidity,fill=Potability)) + geom_histogram(bins=30, color="white") +
  ggtitle("Histogram of Turbidity attribute for potable and non-potable classes")



# Finding correlation between numeric variables
cor(data[,1:ncol(data)-1])

#If we consider 0.7 as a cutoff, there are no strong correlation between the variables.

# Models
#We will train 3 models which are Bayesian Generalized Linear, K-Nearest Neighbours, and Random Forest.


# Bayesian Generalized Linear Model
fit_logr <- train(Potability ~ .,data = train, method = "bayesglm")

predictions_logr <- predict(fit_logr, test)


ConfM_bglm <- confusionMatrix(predictions_logr,test$Potability, positive = "potable")
ConfM_bglm

# putting sensitivity, specificity, and accuracy of the models in a tibble
results <- tibble(Method = "Bayesian GLM", Accuracy = ConfM_bglm$overall['Accuracy'],
                  Sensitivity=ConfM_bglm$byClass['Sensitivity'] ,Specifity=ConfM_bglm$byClass['Specificity'])


# K-Nearest Neighbours model

fit_knn <- train(Potability ~ ., data = train, method = "knn", preProcess=c('knnImpute'))

predictions_knn <-  predict(fit_knn, test)

conM_knn <- confusionMatrix(predictions_knn, test$Potability, positive = "potable")
conM_knn

# putting sensitivity, specificity, and accuracy of the models in a tibble
results <- bind_rows(results, tibble(Method = "K-Nearest Neighbours", Accuracy = conM_knn$overall['Accuracy'], 
                                     Sensitivity=conM_knn$byClass['Sensitivity'] ,Specifity=conM_knn$byClass['Specificity']))



#Random Forest model
fit_rf <- train(Potability ~ ., data = train, method = "rf")

predictions_rf <-  predict(fit_rf, test)

confM_rf <- confusionMatrix(predictions_rf,test$Potability, positive = "potable")
confM_rf

# putting sensitivity, specificity, and accuracy of the models in a tibble
results <- bind_rows(results, tibble(Method = "Random Forest", Accuracy = confM_rf$overall['Accuracy'],
                                     Sensitivity=confM_rf$byClass['Sensitivity'] ,Specifity=confM_rf$byClass['Specificity']))

# Results
#In the following table, I have provided the results obtained by 3 models in the previous section.


# Table of models results
results <-results %>% arrange(Accuracy)
results %>% knitr::kable("pipe")




