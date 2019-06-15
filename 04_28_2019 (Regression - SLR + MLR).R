# Regression 
# Topics
# 1) Simple Linear Regression
# 2) Multiple Linear Regression

# corrplot is the alternate for pairs.panels found in lattice package

library(ISLR)
library(psych)
A = data.frame(Credit)
str(A)
head(A)
pairs.panels(A)


# Since pairs.panels is only applicable for numeric data so, we 1st seperate our numeric data
# To separate numeric data we use lapply command
numcols = lapply(A,is.numeric)
numcols

# Output is in list form to covert that into the vector,
# We use unlist command
numcols = unlist(lapply(A,is.numeric))
numcols

B = A[,numcols] # Here, We give only numeric coloums
pairs.panels(B) # Now it will gives co-relations between numeric features
cor(B)

# Sampling
# It will randomly select few rows from the whole dataset.
# For creating models & make predictions on that.
# trd & tsd contains all values which are present in sf Dataset.
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,] # Contains 70% values from the sf
tsd = A[sf == 2,] # Contains 30% values from the sf

# ?replace command

#--------------------------------------------------------------------------------
# Now start for creating our models
# Start to train our datasets on the basis of important features
# Models for Income
model1_Inc = lm(Income ~ Limit,data=trd) 
plot(model1_Inc)
summary(model1_Inc)
# Multiple R-squared is 66%
# Here Income can predicted based on Limit
# ?OUTPUT

model2_Inc = lm(Income ~ Rating,data=trd)
summary(model2_Inc)

model3_Inc = lm(Income ~ Balance,data=trd)
summary(model3_Inc)

#--------------------------------------------------------------------------------
# Models for Limit
model1_Limit = lm(Limit ~ Balance,data=trd)
summary(model1_Limit)

model2_Limit = lm(Limit ~ Rating,data=trd)
summary(model2_Limit)
# Multiple R-squared is 99%

model3_Limit = lm(Limit ~ Income,data=trd)
summary(model3_Limit)

#--------------------------------------------------------------------------------
# Models for Rating
model1_Rating = lm(Rating ~ Balance,data=trd)
summary(model1_Rating)
# Multiple R-squared is 76%

#--------------------------------------------------------------------------------
# Models for Balance
model1_Bal = lm(Balance ~ Income,data=trd)
summary(model1_Bal)

model2_Bal = lm(Balance ~ Limit,data=trd)
summary(model2_Bal)
# Multiple R-squared is 75%

model3_Bal = lm(Balance ~ Rating,data=trd)
summary(model3_Bal)
# Multiple R-squared is 76%
# So, here we can conclude that,
# For predicting balance, Rating is the good predictor. 

#---------------------------------------------------------------------------------------------------------------------------------------
# Now start making predictions for Testing datadets

# For Income
pred_Inc = predict(model1_Inc,tsd) # Limit
# pred_Inc: Predict values for given Income on the basis of Limit for Testing Data.

# For Rating
pred_Rating = predict(model1_Rating,tsd) # Balance
pred_Rating

# For Balance
pred_Bal = predict(model3_Bal,tsd) # Rating

# For Limit 
pred_Lim = predict(model2_Limit,tsd) # Rating

# Now start Comparing with actual values using cbind function.

# For Income
CB = cbind(tsd$Limit,pred_Inc,tsd$Income)
head(CB)

# For Balance
CB2 = cbind(tsd$Rating,pred_Bal,tsd$Balance)
head(CB2)

# For Limit
CB3 = cbind(tsd$Rating,pred_Lim,tsd$Limit)
head(CB3)

# For Rating
CB4 = cbind(tsd$Balance,pred_Rating,tsd$Rating)
head(CB4)

# Here,
# pred_Rating = Y(cap)
# tsd$Rating = Actual Y
# Y(cap) - Actual Y = Error
#---------------------------------------------------------------------

# 2) Multiple Linear Regression

# Model of Multiple Regression Model
# Here we add all other features which is important to predict Income using + sign.
# To give all features we use .(dot)
# To give all features except some particular feature,
# We use .-(Except this coloum/feature)

# Model 4 train the Income values on the basis of Limit, Balance & Rating.
model4_Inc = lm(Income~Balance+Rating+Limit,data = trd)
summary(model4_Inc)
# Now, for MLR, we consider Adjusted R-squared
# Adjusted R-squared is 81%

# It will give predicted values for Income on the basis of Limit, Balance & Rating on Testing Data.
pred_Inc = predict(model4_Inc,tsd) # Limit, Rating, Balance
head(pred_Inc)

CBM = cbind(tsd$Limit,pred_Inc,tsd$Income)
head(CBM)



