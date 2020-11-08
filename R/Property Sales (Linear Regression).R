setwd("C:/Users/Kyriakos/Desktop") #import dataset
sales = read.csv("property-sales.csv")

#Studying data

str(sales) #show data
head(sales)
summary(sales)
hist(sales$SalePrice,main="Property Sale Prices",xlab="Sale price",col="red")
hist(log(sales$SalePrice),main="Property Sale Prices",xlab="Sale price",col="red")
hist(sqrt(sales$SalePrice),main="Property Sale Prices",xlab="Sale price",col="red")
boxplot(sales$SalePrice,main="Property Sale Prices")
hist(sales$GrLivArea,main="GrLivArea",col="red")
boxplot(sales$GrLivArea,main="GrLivArea")
hist(sales$LotArea,main="GrLivArea",col="red")
boxplot(sales$LotArea,main="Lot Area")
hist(sales$OverallQual,main="OQual",col="red")
boxplot(sales$OverallQual,main="OQual")
hist(sales$OverallCond,main="OCond",col="red")
boxplot(sales$OverallCond,main="OCond")
hist(sales$YearBuilt,main="YearBuilt",col="red")
boxplot(sales$YearBuilt,main="YearBuilt")
hist(sales$FullBath,main="Fullbath",col="red")
boxplot(sales$FullBath,main="FullBath")
hist(sales$HalfBath,main="HalfBath",col="red")
boxplot(sales$HalfBath,main="Halfbath")
hist(sales$BedroomAbvGr,main="BedRoomAbv",col="red")
boxplot(sales$BedroomAbvGr,main="Bedroomabv")
hist(sales$KitchenAbvGr,main="KitchenAbv",col="red")
boxplot(sales$KitchenAbvGr,main="KitchenAbv")
hist(sales$GarageArea,main="GarageArea",col="red")
boxplot(sales$GarageArea,main="GarageArea")

plot(sales$Fireplace,col="red")

#Numerical
salesnum = sales[,-c(1,3,4,8,14,15,17)]#Removing categorical variables
library("corrplot")
M = cor(salesnum)
corrplot(M)
corrplot.mixed(M) #heatplot with value
attach(salesnum)
par(mfrow=c(1, 1))
plot(OverallQual,SalePrice,main="SalePrice ~ OverallQuality")
plot(GrLivArea,SalePrice,main = "SalePrice ~ GrLivArea")
plot(YearBuilt,SalePrice,main = "SalePrice ~ YearBuilt")
plot(FullBath,SalePrice, main = "SalePrice ~ Fullbath")
plot(GarageArea,SalePrice,main = "SalePrice ~ GarageArea")
plot(GrLivArea,FullBath,main = "GrLivAre ~ Fullbath")
plot(YearBuilt,GrLivArea,main = "GrLivAre ~ yearbuilt")
scatter.smooth(x=FullBath+GrLivArea+GarageArea+OverallQual, y=SalePrice)
par(mfrow=c(1, 2))

#All(Including categorical)
detach(salesnum)
attach(sales)
par(mfrow=c(1, 4))
plot(Fireplace,MSZoning,main="FP-Mszoning",xlab="FP",ylab="MsZoning")
plot(Fireplace,LotArea,main="FP-Lotarea",xlab="FP",ylab="lotarea")
plot(Fireplace,MSZoning,main="FP-bldgtype",xlab="FP",ylab="bldtgtype")
plot(Fireplace,HouseStyle,main="FP-housestyle",xlab="FP",ylab="housestyle")
plot(Fireplace,OverallQual,main="FirePlace-overallqual",xlab="FP",ylab="overallquality")
plot(Fireplace,OverallCond,main="FirePlace-overallcondition",xlab="FP",ylab="overallcondition")
plot(Fireplace,YearBuilt,main="FP-yearbuilt",xlab="FP",ylab="yearbuilt")
plot(Fireplace,CentralAir,main="FirePlace-centralair",xlab="FP",ylab="centralair")
plot(Fireplace,GrLivArea,main="FirePlace-grlivarea",xlab="FP",ylab="grLivarea")
plot(Fireplace,FullBath,main="FP-fullbath",xlab="FP",ylab="fullbath")
plot(Fireplace,HalfBath,main="FP-halfbath",xlab="FP",ylab="halfbath")
plot(Fireplace,BedroomAbvGr,main="FirePlace-bedroomabv",xlab="FP",ylab="bedroomabvgrade")
plot(Fireplace,KitchenAbvGr,main="FP-kitchenabv",xlab="FP",ylab="kitchenabvgrade")
plot(Fireplace,KitchenQual,main="FP-kitchenqual",xlab="FP",ylab="kitchenqual")
plot(Fireplace,GarageArea,main="FP-garagearea",xlab="FP",ylab="garagearea")
plot(Fireplace,SaleCondition,main="FP-salecondition",xlab="FP",ylab="salecondition")
plot(Fireplace,SalePrice,main="FirePlace-salepirce",xlab="FP",ylab="saleprice")
plot(CentralAir,SalePrice,main="centralair-salepirce",xlab="centralair",ylab="saleprice")
plot(MSZoning,YearBuilt,main="MSZoning-YearBuilt",xlab="MSZoning",ylab="YearBuilt")
plot(SaleCondition,YearBuilt,main="MSZoning-YearBuilt",xlab="MSZoning",ylab="YearBuilt")
#lotarea,grlivarea,bedroomabv,garagearea,saleprice,overalqual(possible features)




#Building linear models

#Train/Test Split
set.seed(126)
trainingRowIndex = sample(1:nrow(sales), 0.8*nrow(sales))
trainingData = sales[trainingRowIndex, ]                           
testData = sales[-trainingRowIndex, ]

#Building models
salemod1 = lm(log(SalePrice) ~ ., data = trainingData)#default model
summary(salemod1)
plot(salemod1)
salemod2 = lm(log(SalePrice) ~ MSZoning+LotArea+OverallQual+GrLivArea+OverallCond+YearBuilt+CentralAir+HalfBath+KitchenQual+Fireplace+GarageArea,data=trainingData)#default model
summary(salemod2)
plot(salemod2)
salemod3 = lm(log(SalePrice) ~ MSZoning+LotArea+OverallQual+GrLivArea+OverallCond+YearBuilt+CentralAir+KitchenQual+Fireplace+GarageArea,data=trainingData)#default model
summary(salemod3)
plot(salemod3)
salemod4 = lm(log(SalePrice) ~ MSZoning+LotArea+GrLivArea+OverallCond+YearBuilt+CentralAir+KitchenQual+Fireplace+GarageArea
              +I(LotArea^2)+I(OverallQual^2)+I(GrLivArea^2)+I(YearBuilt^2)+I(GarageArea^2),data=trainingData)# adding squares to check for non-linearity
summary(salemod4)
plot(salemod4)

#cross validating
library(psych)
library(caret)
data_ctrl <- trainControl(method = "cv", number = 10)

set.seed(126)
salemod2 = train(log(SalePrice) ~ MSZoning+LotArea+OverallQual+GrLivArea+OverallCond+YearBuilt+CentralAir+HalfBath+KitchenQual+Fireplace+GarageArea,
                 data = trainingData,                        
                 trControl = data_ctrl,              # folds
                 method = "lm")       
salemod2

set.seed(126)
salemod3 = train(log(SalePrice) ~ MSZoning+LotArea+OverallQual+GrLivArea+OverallCond+YearBuilt+CentralAir+KitchenQual+Fireplace+GarageArea,   # model to fit
                 data = trainingData,                        
                 trControl = data_ctrl,              # folds
                 method = "lm")                      # specifying regression model )
salemod3

set.seed(126)
salemod4 = train(log(SalePrice) ~ MSZoning+LotArea+GrLivArea+OverallCond+YearBuilt+CentralAir+KitchenQual+Fireplace+GarageArea
                 +I(LotArea^2)+I(OverallQual^2)+I(GrLivArea^2)+I(YearBuilt^2)+I(GarageArea^2),   # model to fit
                 data = trainingData,                        
                 trControl = data_ctrl,              # folds
                 method = "lm") #best model
salemod4
#predicting
salePred <- predict(salemod4, testData)
actuals_preds = data.frame(cbind(actuals=testData$SalePrice, predicteds=salePred))
correlation_accuracy = cor(actuals_preds)
head(actuals_preds)
rmse <- sqrt(sum((exp(salePred) - testData$SalePrice)^2)/length(testData$SalePrice))
c(RMSE = rmse, R2=summary(salemod4)$r.squared)

#Testing non-linear models
nlin1=lm(SalePrice ~ poly(LotArea, degree=2, raw=TRUE) +
           poly(OverallQual, degree=2, raw=TRUE) +
           poly(OverallCond, degree=2, raw=TRUE) +
           poly(GrLivArea, degree=2, raw=TRUE)+
           poly(FullBath, degree=2,raw=TRUE)+
           poly(GarageArea, degree=2,raw=TRUE),data=trainingData)

nlin1=lm(SalePrice ~ poly(LotArea, degree=2, raw=TRUE) +
           poly(OverallQual, degree=2, raw=TRUE) +
           poly(OverallCond, degree=2, raw=TRUE) +
           poly(GrLivArea, degree=2, raw=TRUE)+
           poly(FullBath, degree=2,raw=TRUE),data=trainingData)

#predicting non-linear
set.seed(126)
nlin1 = train(SalePrice ~ poly(LotArea, degree=2, raw=TRUE) +
                poly(OverallQual, degree=2, raw=TRUE) +
                poly(OverallCond, degree=2, raw=TRUE) +
                poly(GrLivArea, degree=2, raw=TRUE)+
                poly(FullBath, degree=2,raw=TRUE)+
                poly(GarageArea, degree=2,raw=TRUE),
              data = trainingData,                        
              trControl = data_ctrl,              # folds
              method = "lm")
set.seed(126)
nlin2 = train(SalePrice ~ poly(LotArea, degree=2, raw=TRUE) +
                poly(OverallQual, degree=2, raw=TRUE) +
                poly(OverallCond, degree=2, raw=TRUE) +
                poly(GrLivArea, degree=2, raw=TRUE)+
                poly(FullBath, degree=2,raw=TRUE),
              data = trainingData,                        
              trControl = data_ctrl,              # folds
              method = "lm")
#nlin predic
nlinPred <- predict(nlin1, testData)
actuals_preds = data.frame(cbind(actuals=testData$SalePrice, predicteds=salePred))
correlation_accuracy = cor(actuals_preds)
head(actuals_preds)
rmse <- sqrt(sum((nlinPred - testData$SalePrice)^2)/length(testData$SalePrice))
c(RMSE = rmse, R2=summary(nlin1)$r.squared)
#Classification
attach(sales)

#lotarea,grlivarea,bedroomabv,garagearea,saleprice,overalqual

fireplace.no=subset(sales,Fireplace=="N")
fireplace.yes=subset(sales,Fireplace=="Y")
plot(fireplace.no$SalePrice,fireplace.no$GrLivArea,col="blue")
points(fireplace.yes$SalePrice,fireplace.yes$GrLivArea,col="red")
par(mfrow=c(1, 4))
plot(Fireplace,OverallQual)
plot(Fireplace,SalePrice)
plot(Fireplace,GrLivArea)

#models

set.seed(123)
testindex=sample(nrow(sales),0.3*nrow(sales))
testData=sales[testindex,]
trainingData=sales[-testindex,]

#testmodel
testmod=glm(Fireplace ~ MSZoning+LotArea+SalePrice+BldgType+HouseStyle+OverallCond+YearBuilt+CentralAir+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+GarageArea+SaleCondition+GrLivArea+OverallQual,family=binomial,data=trainingData)

#model1
logreg1=glm(Fireplace ~ LotArea+SalePrice+OverallCond+YearBuilt+CentralAir+BedroomAbvGr+KitchenAbvGr+GrLivArea+OverallQual,family=binomial,data=trainingData)
testprob=predict(logreg1,testData,type="response")
testpred=rep("N",438)
testpred[testprob>0.48]="Y"
confmatrix=table(testpred,testData$Fireplace)
confmatrix[2,2]/(confmatrix[1,2]+confmatrix[2,2])
confmatrix[2,1]/(confmatrix[1,1]+confmatrix[2,1]) #false positive rate
(confmatrix[1,2]+confmatrix[2,1])/438 #overall misclassification error
precision = confmatrix[2,2]/(confmatrix[2,1]+confmatrix[2,2])
recall = confmatrix[2,2]/(confmatrix[1,2]+confmatrix[2,2])
print(precision)
print(recall)

#model2
logreg2=glm(Fireplace ~ LotArea+SalePrice+YearBuilt+CentralAir+BedroomAbvGr+KitchenAbvGr+GrLivArea+OverallCond,family=binomial,data=trainingData)
testprob=predict(logreg2,testData,type="response")
testpred=rep("N",438)
testpred[testprob>0.5]="Y"
confmatrix=table(testpred,testData$Fireplace)
confmatrix[2,2]/(confmatrix[1,2]+confmatrix[2,2])
confmatrix[2,1]/(confmatrix[1,1]+confmatrix[2,1]) #false positive rate
(confmatrix[1,2]+confmatrix[2,1])/438 #overall misclassification error
precision = confmatrix[2,2]/(confmatrix[2,1]+confmatrix[2,2])
recall = confmatrix[2,2]/(confmatrix[1,2]+confmatrix[2,2])
print(precision)
print(recall)

##Dataset: House Prices - https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data