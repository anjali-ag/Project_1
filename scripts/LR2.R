urlfile="https://raw.githubusercontent.com/nychealth/covid-vaccine-data/main/doses/doses-by-day.csv"
nycdata<-read.csv(url(urlfile))

# This is to convert the dates from String to Date format in order to use it for the graphs
dates=as.Date(nycdata[,1])
dates[length(dates)-1]

data = read.csv("//cloud/project/nyc_data.csv")

# Take col 1 as an example

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

#Plotting the predicted values agains the truth values
plot(dates[162:211],test.y,col="red",xlab="DATE",ylab="Number of daily vaccinations",type="l")
lines(dates[162:211],preds.prob, col="blue")
