rm(list = ls())


library(dplyr)

redWine <- read.csv("winequality-red.csv", sep=";", header = T) #1599
View(head(redWine))

whiteWine <- read.csv("winequality-white.csv", sep=";", header = T) #4898

str(whiteWine)

#missing
colSums(is.na(redWine)) #all 0
colSums(is.na(whiteWine)) #all 0

barplot(table(whiteWine$quality)) ###unbalanced data
unique(whiteWine$quality)

boxplot(whiteWine$quality)

whitecor <- cor(whiteWine)

#correlation between density and residual sugar is high
#correlation between density and alcohol is high

names(whiteWine) <- gsub(' ', '', names(whiteWine))

#train and test
set.seed(54)
indexes = sample(1:nrow(whiteWine), size = 0.8*nrow(whiteWine), replace = F)

train <- whiteWine[indexes,]
test <- whiteWine[-indexes,]

#linear model Iteration1

white.lm <- lm(quality ~., data = train)
summary(white.lm)

library(car)
white.vif <- vif(white.lm)
#Highest VIF value of density
#Removing density since highly correlated with other factors and also have high VIF

#Iteration-2
white.lm <- lm(quality ~.-density, data = train)
summary(white.lm)
white.vif <- vif(white.lm)





white.step <- step(white.lm, scope="~.", direction="both")
summary(white.step)

white.step$anova
white.step$coefficients

white.lm <- lm(quality ~. -density-citricacid - chlorides , data = train)
white.lm$coefficients


dev.off()
plot(white.lm, pch=16, which=1)
abline(0, 0)
#Adjusted R-squared:  0.272 


test$predcited <- round(predict(white.lm, test))


check.lm <- as.matrix(table(test$quality, test$predcited))
is <- do.call(intersect, unname(dimnames(check.lm)))
check.acc <- sum(check.lm[cbind(is, is)])/sum(check.lm) ##0.4867347

#Accuracy of linear model == 0.4867347



#Random Forest
library(randomForest)

white.rf <- randomForest(quality ~ . , train  ,  ntree = 200, mtry = 4, nodesize = floor(0.05*nrow(train)))

View(white.rf$importance) 
#Predictor importance: alcohol  >volatileacidity >density>freesulfurdioxide >chlorides>
#                      residualsugar>totalsulfurdioxide >citricacid>pH  >fixedacidity >susulphates

test.rf <- test
test.rf$predcited <- NULL

test.rf$predcited <- round(predict(white.rf , test.rf))

check.rf <- as.matrix(table(test.rf$quality, test.rf$predcited))
is.RF <- do.call(intersect, unname(dimnames(check.rf)))
check.rf.acc <- sum(check.rf[cbind(is.RF, is.RF)])/sum(check.rf) ##0.5346939

#Accuracy of random forest == 0.5346939
