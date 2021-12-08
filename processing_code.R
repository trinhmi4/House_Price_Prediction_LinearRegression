# loading data and taking sample
data <- read.csv('housing.csv', fileEncoding="UTF-8-BOM")
set.seed(1004865513)
rows <- sample(1:nrow(data), 1000, replace = FALSE)
mydata <- data[rows,]

# divide data into training data and validation data with 50/50
rows <- sample(1:nrow(mydata), 500, replace = FALSE)
validdata <- mydata[rows,]
trainindex <- setdiff(1:1000, rows)
traindata <- mydata[trainindex,]
summary(validdata)
summary(traindata)

# first we will look at the full model and determine whether transformations are necessary to correct any
# model violations:
full <- lm(median_house_value ~ ., data=traindata[,-1])
summary(full)

# check conditions to see if residual plots can tell us what is wrong with model
pairs(traindata[,-1])
par(mfrow=c(1,2))
plot(traindata$median_house_value~fitted(full), main="Y v Fitted", xlab="Fitted", ylab="Median house value")
abline(a = 0, b = 1)
lines(lowess(traindata$median_house_value~fitted(full)), col="blue")

# curve in y vs fitted plot so I use transformation
full_adj <- lm(I(sqrt(median_house_value)) ~ ., data=traindata[,-1])
summary(full_adj)
#check if condition 1 is satisfied
plot(sqrt(traindata$median_house_value)~fitted(full_adj), main="Square Root of Y v Fitted", xlab="Fitted", ylab="Sqrt Median house value")
abline(a = 0, b = 1)
lines(lowess(sqrt(traindata$median_house_value)~fitted(full_adj)), col="red")

# Improve a lot after transformation although there are still points far from the identity line,
# but this might be due to outliers. Now we can look at residual plot
# in this plot we notice there is one point irrelevant from others, and 
# few outliers not in [-2,2]
par(mfrow=c(4,4))
par(mar = rep(2, 4))
plot(rstandard(full_adj)~fitted(full_adj), xlab="fitted", ylab="Standard Residuals")
for(i in c(2:13)){
  par(mar = rep(2, 4))
  plot(rstandard(full_adj)~traindata[,i], xlab=names(traindata)[i], ylab=" Standard Residuals")
}
qqnorm(rstandard(full))
qqline(rstandard(full))
# We can see non constant error variance in few plots, 
# also there are outliers/ high leverage points
# the end of qq plot is also abnormal

# check multicollinearity
library(car)
vif(full_adj)
# longitude, latitude, total_rooms, total_bedrooms, population, households,
# near_bay, near_ocean, oneh_ocean, inland very high vif
summary(full_adj)
income <- lm(median_income ~ ., data=traindata[,-c(1,10)])
summary(income)
#total rooms has smallest p value, lets try take it out of the model
full_adj <- lm(I(sqrt(median_house_value)) ~ ., data=traindata[,-c(1,5)])
summary(full_adj)
# look at population multicollinearity
pop <- lm(population ~ ., data=traindata[,-c(1,10, 5)])
summary(pop)
# remove households
full_adj <- lm(I(sqrt(median_house_value)) ~ ., data=traindata[,-c(1,5,8)])
summary(full_adj)
# look at near ocean
ocean <- lm(near_ocean ~ ., data=traindata[,-c(1,10, 5, 8)])
summary(ocean)
# remove oneh ocean
full_adj <- lm(I(sqrt(median_house_value)) ~ ., data=traindata[,-c(1,5,8, 13)])
summary(full_adj)
# not good
# try remove near bay
full_adj <- lm(I(sqrt(median_house_value)) ~ ., data=traindata[,-c(1,5,8,11)])
summary(full_adj)
# not good
# try remove inland
full_adj <- lm(I(sqrt(median_house_value)) ~ ., data=traindata[,-c(1,5,8,14)])
summary(full_adj)
# still exhibits relationship between price and all variables as well as single variables
par(mfrow=c(4,4))
par(mar = rep(2, 4))
plot(rstandard(full_adj)~fitted(full_adj), xlab="fitted", ylab="Standard Residuals")
for(i in c(2:4,6,7,9:13)){
  par(mar = rep(2, 4))
  plot(rstandard(full_adj)~traindata[,i], xlab=names(traindata)[i], ylab=" Standard Residuals")
}
qqnorm(rstandard(full))
qqline(rstandard(full))
#still exists non constant error variance


# look at leverage point
h <- hatvalues(full_adj)
hcut <- 2*length(coefficients(full_adj))/nrow(traindata)
w1 <- which(h > hcut)
w1

# look at outliers
r <- rstandard(full_adj)
w2 <- which(r >=2 | r <= -2)

# there is no bad leverage point
# w2 <- r[which(h > hcut)]
w1 %in% w2

# influential point
d <- cooks.distance(full_adj)
dcut <- qf(0.5, length(coef(full_adj)), full_adj$df.residual)
w3 <- which(d > dcut)
w3
# none influential point according to cook distance

dffit <- dffits(full_adj)
cutfit <- 2*sqrt(length(coef(full_adj))/nrow(traindata))
w4 <- which(abs(dffit) > cutfit)
w4

dfbeta <- dfbetas(full_adj)
cutb <- 2/sqrt(nrow(traindata))
w5 <- which(abs(dfbeta[,1]) > cutb | abs(dfbeta[,2]) > cutb | abs(dfbeta[,3]) > cutb | abs(dfbeta[,4]) > cutb)
w5

# remove outliers and influential points
full_adj <- lm(I(sqrt(median_house_value)) ~ ., data=traindata[-c(w1,w4, w5),-c(1,5,8,14)])
summary(full_adj)
# increase adjusted R square
# use validation data
validmodel <- lm(I(sqrt(median_house_value)) ~ ., data=validdata[,-c(1,5,8,14)])
summary(validmodel)


