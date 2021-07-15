all <- data.frame(cbind(y,x))

# Split data numerically :
train <- all[1:round(0.75*nrow(all),0),]; dim(train) # Training set
test <- all[(round(0.75*nrow(all),0)+1):nrow(all),]; dim(test) # Testing set

#Split train and test sets into the explanatory and response variables
xtrain = data.frame(train[,-1]);
ytrain =  train[,1]; 
xtest = data.frame(test[,-1]); 
ytes = test[,1]; 

#Linear Regression Model
model <- lm(
    train.y ~.,
    data = train.x
)
sum <- summary(model)

#Using xtest to predict y and finding MSE
ypred <- predict(model, xtest)
test.mse <- sum((ypred- ytest)^2)/nrow(test)

# Comparing the yprediction and ytest values
comparison <- cbind(ytest, ypred)
