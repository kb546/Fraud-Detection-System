options(scipen = 999)

#install.packages("tidyverse")
library("tidyverse")

#Step 0: EDA (Explanatory Data Analysis) and cleaning.

#Combine valuePerSecond and ScannedLineItemsPerSecond to get averageValueofScannedLineItemsPerSecond
df$averageValueofScannedLineItemsPerSecond = df$valuePerSecond / df$scannedLineItemsPerSecond
df$averageValueofScannedLineItemsPerSecond[1:10]
#check for missing values
colSums(is.na(df))

#Treat Fraud and TrustLevel as categorical Values
df$trustLevel = as.factor(df$trustLevel)
df$fraud = as.factor(df$fraud)


# STEP 1: Divide the data into training and test sets
set.seed(42) # we are setting the seed for the sake of reproducibility.
n = dim(df)[1] #Number of Observations
train = sample(1:n, 0.7*n, replace=FALSE) #The indices of customers that will go to training set
test = setdiff(1:n, train) #The indices of customers that will go to the test set

df_train = df[train, ] # create the training dataset
df_test = df[test, ] # create the test dataset


# STEP 2: Build the model on training set only. Also, do all the modification
# to option the best possible model.

fit = glm(fraud ~ trustLevel +  totalScanTimeInSeconds + grandTotal  + lineItemVoids + scansWithoutRegistration + 
            scannedLineItemsPerSecond + valuePerSecond + lineItemVoidsPerPosition + averageValueofScannedLineItemsPerSecond,
            data=df_train, family='binomial')

summary(fit)

# STEP 3: Make prediction using the fitted/trained model on test set
test_pred_prob = predict(fit, df_test, type='response') # finding p for all test observations
test_pred = round(test_pred_prob)

train_pred = round(fit$fitted.values, 0)

# STEP 4: Obtain training and test errors

table(df_train$fraud, train_pred)
table(df_test$fraud, test_pred)

prop.table(table(df_train$fraud, train_pred))
prop.table(table(df_test$fraud, test_pred))

#Accuracy with all Attributes -> 0.934397163 + 0.046099291 = 0.980496454 = 98%
#Accuracy without the insignificant attributes -> 98%
#Accuracy without GrandTotal and valuePerSecond -> 0.932624113 + 0.044326241 = 0.976950354 = 97%

