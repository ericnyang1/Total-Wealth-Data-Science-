setwd("")
set.seed(456)

library(ISLR)
library(faraway)
library(ggplot2)
library(MASS)
library(splines)
library(glmnet)

data_tr<-read.table("data_tr.txt", header = TRUE, sep= "\t", dec= ".")[,-1]
dt <- data_tr
summary(dt$tw)

family_size <- as.matrix(dt[,17])
dt$pov_line <- as.matrix(rep(NA,length(family_size)))


#Feature Engineering: 
#Poverty Line: Legally accepted threshold to be considered 'in poverty' based on HHS.gov poverty guidelines. First person 6,620 and 2,260 for each additional person in household
#Distance from Poverty Line: Difference between income and poverty line
#Dummy Poverty: 1 = in poverty, 0 = not in poverty


for (i in 1:length(dt$pov_line)) {
  if 
  (family_size[i] > 1) {
    dt$pov_line[i] <- 6280 + (family_size[i]-1)*2140
  }
  else{
   dt$pov_line[i] <-  6280
  }
}

dt$povdis <- dt$inc - dt$pov_line 

dt$poverty <- as.matrix(rep(NA,length(dt$pov_line)))

for (i in 1:length(dt$povdis)) {
  if 
  (dt$povdis[i] < 0) {
    dt$poverty[i] <- 1
  }
  else{
    dt$poverty[i] <-  0
  }
}


#Check for outliar data 

dt$tw <- eval(substitute(dt$tw),eval(data_tr))
na1 <- sum(is.na(dt$tw))
m1 <- mean(dt$tw, na.rm = T)

par(mfrow=c(2,2),oma=c(0,0,3,0))

#Box plot of total wealth

boxplot(dt$tw, main="With outliers")

#Histogram of total wealth
hist(dt$tw, main="With outliers", xlab=NA, ylab=NA)

#Identify outlines 
outlier <- boxplot.stats(dt$tw)$out
mo <- mean(outlier)
dt$tw <- ifelse(dt$tw %in% outlier, NA, dt$tw)


#Box plot of total wealth without Outlier
boxplot(dt$tw, main="Without outliers")

#Histogram of total wealth without Outlier
hist(dt$tw, main="Without outliers", xlab=NA, ylab=NA)
title("Outlier Check", outer=TRUE)

na2 <- sum(is.na(dt$tw))

cat("Outliers identified:", na2 - na1, "n")
cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(dt$tw))*100, 1), "n")
cat("Mean of the outliers:", round(mo, 2), "n")

m2 <- mean(dt$tw, na.rm = T)
cat("Mean without removing outliers:", round(m1, 2), "n")
cat("Mean if we remove outliers:", round(m2, 2), "n")

# Remove outlier data
data <- na.omit(dt)


#Plot histograms and access skewness for each variable
par(mfrow=c(2,2))
lapply(X=c("tw","ira","e401","nifa","inc","hmort","hval","hequity","educ","male","twoearn","nohs","hs",
           "smcol","col", "age","fsize", "marr", "povdis", "poverty"), FUN=function(s)
             hist(data[, s], main=paste("Histogram of", s)))


#Standardize continuous independent variables
colnames(data)
X.cont <- data[,c(2,4,5,6,7,8,9,16,17,19,20)]
p <- ncol(X.cont)
n <- nrow(X.cont)
xmn <- scale(X.cont, center = FALSE, scale = rep(sqrt(n - 1), p)) 
data[,c(2,4,5,6,7,8,9,16,17,19,20)] <- xmn[,1:11]
summary(data)

dev.off()

#Correlation matrix: Choose starting predictors what aren't correlated to eliminate multicollinertity 
pairs(data[,-1])
round(cor(data[,-1]),2)

#Starting predictors: hequity, col , age, ira, e401k, twoearn, nifa, poverty
full <- lm(tw ~ ., data=data)
null <- lm(tw ~ 1, data=data)

#Backwards elimation variable section
stepAIC(full , direction= "backward", scope=list(lower=null, upper=full))
backward <- lm(formula = tw ~ ira + e401 + nifa + inc + hmort + hval + twoearn + hs + smcol + age + fsize + poverty, data = data)
round(vif(backward),1)

#Variable Selection:Forward selection
stepAIC(null, direction='forward', scope=list(lower=null, upper=full))
forward <- lm(formula = tw ~ hequity + nifa + ira + e401 + povdis + age +  twoearn + hmort + nohs + poverty, data = data)
round(vif(forward),1)


#Stepwise selection variable section
stepAIC(full, direction="both", scope=list(lower=null, upper=full))
stepwise <- lm(formula = tw ~ ira + e401 + nifa + inc + hmort + hval + twoearn + hs + smcol + age + fsize + poverty, data = data)
round(vif(stepwise),1)

extractAIC(backward)
extractAIC(forward) 
extractAIC(stepwise)

#Forward regression has the lowest AIC.Use variables: hequity , nifa , ira , e401 , povdis , age ,  twoearn , hmort , nohs , poverty
dev.off()
summary(forward)


#Identify and remove outliar via Cook's Distance
par(mfrow=c(2,2))
plot(forward)

dev.off()

cooksd <- cooks.distance(forward)
plot(cooksd, pch=1, cex=.5, main="Influential Observations by Cooks distance")  
abline(h = 4/nrow(data), col="red")  

influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(data)))])
data <- data[-influential,]

#Rerun regression
forward <-  lm(formula = tw ~ hequity + nifa + ira + e401 + povdis + age + twoearn + hmort + nohs + poverty, data = data)
summary(forward)

#Drop poverty and nohs
#Model 1 Simple Regression
model.1 <-  lm(formula = tw ~ hequity + nifa + ira + e401 + povdis + age + twoearn + hmort, data = data)
summary(model.1)
par(mfrow=c(2,2))
plot(model.1)

#Plot simple regression to analyze relationship for non-linear transformation
par(mfrow=c(2,3))

plot(cbind(data$hequity, data$tw), main = "Scatter Plot: tw ~ hequity",
     xlab = "hequity", ylab = "tw",
     pch = 20, frame = FALSE)
fitted.1 <- predict(lm(tw ~ hequity, data=data))
lines(data$hequity, fitted.1, col="red", lwd=2)

plot(data$nifa, data$tw, main = "Scatter Plot: tw ~ nifa",
     xlab = "nifa", ylab = "tw",
     pch = 20, frame = FALSE)
fitted.2 <- predict(lm(tw ~ nifa, data=data))
    lines(cbind(data$nifa, fitted.2), col="red", lwd=2)
  
plot(cbind(data$ira, data$tw), main = "Scatter Plot: tw ~ ira",
     xlab = "ira", ylab = "tw",
     pch = 20, frame = FALSE)
fitted.3 <- predict(lm(tw ~ ira, data=data))
     lines(cbind(data$ira, fitted.3), col="red", lwd=2)

plot(cbind(data$povdis, data$tw), main = "Scatter Plot: tw ~ povdis",
     xlab = "povdis", ylab = "tw",
     pch = 20, frame = FALSE)
fitted.4 <- predict(lm(tw ~ povdis, data=data))
     lines(cbind(data$povdis, fitted.4), col="red", lwd=2)

plot(data$age, data$tw, main = "Scatter Plot: tw ~ age",
     xlab = "age", ylab = "tw",
     pch = 20, frame = FALSE)
fitted.5 <- predict(lm(tw ~ age, data=data))
lines(cbind(data$age, fitted.5), col="red", lwd=2)

plot(data$hmort, data$tw, main = "Scatter Plot: tw ~ hmort",
     xlab = "hmort", ylab = "tw",
     pch = 20, frame = FALSE)
fitted.6 <- predict(lm(tw ~ hmort, data=data))
lines(cbind(data$hmort, fitted.6), col="red", lwd=2)


#Cross Validation on povdis
y <- data$tw 
n <-length(y)
k <- 10
ii <-sample(rep(1:k, length= n))
p <- 10
pol_cv <-matrix(data=NA,nrow=length(y),ncol=p)
mse_poly <-rep(NA, p)
for(s in 1:p) { 
  for(j in 1:k){
    hold <- (ii == j)
    train <- (ii != j)
    pol <-lm(formula = y[train] ~ hequity + nifa + ira + e401 + poly(povdis,s) + age + twoearn + hmort, data=data[train,])
    pol_cv[hold,s] <-predict(pol, newdata=data[hold,])
  }
  mse_poly[s] <-mean((y - pol_cv[,s])^2)
}
plot(mse_poly, main="CV: Degree of polynominals of povdis")

#CV suggests poly(povdis,4)

#Model 2 Polynomial Transformation Regression 
model.2 <- lm(tw ~poly(povdis,4) + hequity + nifa + ira + e401 +age + twoearn + hmort, data=data)

poly_povdis4 <- poly(data$povdis,4)
summary(model.2)

#Cross Fold of hmort Degrees of Freedom Natural Splines
y <- data$tw 
n <-length(y)
k <- 10
ii <-sample(rep(1:k, length= n))
p <- 10
df_cv <-matrix(data=NA,nrow=length(y),ncol=p)
mse_df <-rep(NA, p)

for(s in 1:p) { 
  for(j in 1:k) {
    hold <- (ii == j)
    train <- (ii != j)
    ns <-lm(formula = y[train] ~ hequity + nifa + ira + e401 + poly(povdis,4) + age + twoearn + ns(hmort,s),data=data[train,])
    df_cv[hold,s] <-predict(ns, newdata=data[hold,])
  }
  mse_df[s] <-mean((y - df_cv[,s])^2)
}
plot(mse_df, main="CV: Degree of Freedom Natural Cubic Spline hmort")
summary(mse_df)

#CV suggests ns(hmort,2)

#Model 3 GAM 
model.3 <- lm(formula = tw ~ hequity + nifa + ira + e401 + poly(povdis,4) + age + twoearn + ns(hmort,2),data=data)
ns_hmort2 <- ns(data$hmort,2)
summary(model.3)


#Cross Fold of age Degrees of Freedom Natural Splines
y <- data$tw 
n <-length(y)
k <- 10
ii <-sample(rep(1:k, length= n))
p <- 10
df_cv <-matrix(data=NA,nrow=length(y),ncol=p)
mse_df <-rep(NA, p)

for(s in 1:p) { 
  for(j in 1:k) {
    hold <- (ii == j)
    train <- (ii != j)
    ns <-lm(formula = y[train] ~ hequity + nifa + ira + e401 + poly(povdis,4) + ns(age,s) + twoearn + ns(hmort,2),data=data[train,])
    df_cv[hold,s] <-predict(ns, newdata=data[hold,])
  }
  mse_df[s] <-mean((y - df_cv[,s])^2)
}
plot(mse_df, main="CV: Degree of Freedom Natural Cubic Spline age")


#CV suggests ns(age,1)

#Model 4 GAM
model.4 <- lm(formula = tw ~ hequity + nifa + ira + e401 + poly(povdis,4) + ns(age,1) + twoearn + ns(hmort,2),data=data)
ns_age <- ns(data$age,1)
summary(model.4)



#Create New covariate Matrix
data2 <- data.frame(data$tw,data$hequity,data$nifa,data$ira,data$e401,data$povdis,poly_povdis4,data$twoearn,data$hmort,ns_hmort2,data$age,ns_age)

#Rename columns for easy reference
names(data2)[1] <- "tw"
names(data2)[2] <- "hequity"
names(data2)[3] <- "nifa"
names(data2)[4] <- "ira"
names(data2)[5] <- "e401"
names(data2)[6] <- "povdis"
names(data2)[11] <- "twoearn"
names(data2)[12] <- "hmort"
names(data2)[15] <- "age"


#Stepwise Model Selection Methods
null <- lm(tw ~ 1, data=data2)
full <- lm(tw ~., data=data2)

#Model 5 Forward Selection
stepAIC(null, direction='forward', scope=list(lower=null, upper=full))
model.5 <- lm(formula = tw ~ hequity + nifa + ira + e401 + povdis + X1.2 + 
                twoearn + X1.1 + X3 + X2 + X4 + hmort, data = data2)
#Model 6 Backwards Selection
stepAIC(full , direction= "backward", scope=list(lower=null, upper=full))
model.6 <- lm(formula = tw ~ hequity + nifa + ira + e401 + povdis + X2 + 
                X3 + X4 + twoearn + hmort + X1.1 + age, data = data2)

#Model 7 Forward & Backward Selection
stepAIC(full, direction="both", scope=list(lower=null, upper=full))
model.7 <- lm(formula = tw ~ hequity + nifa + ira + e401 + povdis + X2 + 
                X3 + X4 + twoearn + hmort + X1.1 + age, data = data2)


#CV Lambda Ridge Regression & Lasso
lambdas.rr <-exp(seq(-1, 10, length = 50))
y <- model.matrix(~.,data=data2)[,2]
X <-model.matrix(~.,data=data2)[,-c(1,2)]

lasso_cv <-cv.glmnet(x = X, y = y, lambda = lambdas.rr,alpha = 1)
lasso_cv$lambda.min
lasso_pred <-predict(lasso_cv, newx=X, s = lasso_cv$lambda.min )

ridge_cv <-cv.glmnet(x = X, y = y, lambda = lambdas.rr,alpha = 0)
ridge_cv$lambda.min
ridge_pred <-predict(ridge_cv, newx=X, s = ridge_cv$lambda.min )


#CV Final Model Selection
n <- length(data2)
k <- 10
y <- data2$tw
ii <- sample(rep(1:k, length= n))
pr1 <- pr2 <-  pr3 <- pr4 <- pr5 <- pr6 <- pr7 <- pr.lasso <- pr.ridge <- numeric(length(y))

for (j in 1:k){
  hold <- (ii == j)
  train <- (ii != j)
  
  model.1 <- lm(formula = y[train] ~ hequity + nifa + ira + e401 + povdis + age + twoearn + hmort, data = data2[train,])
  model.2 <- lm(formula = y[train] ~ poly(povdis,4) + hequity + nifa + ira + e401 +age + twoearn + hmort, data=data[train,])
  model.3 <- lm(formula = y[train] ~ hequity + nifa + ira + e401 + poly(povdis,4) + age + twoearn + ns(hmort,2),data = data2[train,])
  model.4 <- lm(formula = y[train] ~ hequity + nifa + ira + e401 + poly(povdis,4) + ns(age,1) + twoearn + ns(hmort,3),data = data2[train,])
  model.5 <- lm(formula = y[train] ~ hequity + nifa + ira + e401 + povdis + X1.2 +  twoearn + X1.1 + X3 + X2 + X4 + hmort, data = data2[train,])
  model.6 <- lm(formula = y[train] ~ hequity + nifa + ira + e401 + povdis + X2 +  X3 + X4 + twoearn + hmort + X1.1 + age, data = data2[train,])
  model.7 <- lm(formula = y[train] ~ hequity + nifa + ira + e401 + povdis + X2 + X3 + X4 + twoearn + hmort + X1.1 + age, data = data2[train,])

  pr1[hold] <- predict(model.1, newdata= data2[hold,]) 
  pr2[hold] <- predict(model.2, newdata= data2[hold,])
  pr3[hold] <- predict(model.3, newdata= data2[hold,])
  pr4[hold]<- predict(model.4, newdata= data2[hold,])
  pr5[hold]<- predict(model.5, newdata= data2[hold,])
  pr6[hold]<- predict(model.6, newdata= data2[hold,])
  pr7[hold]<- predict(model.7, newdata= data2[hold,])
  
  xx.tr <- X[train,]
  y.tr <-  y[train]
  xx.te <- X[hold,]
  ridge.cv <-cv.glmnet(x=as.matrix(xx.tr), y=y.tr,lambda = lambdas.rr, nfolds=k, alpha=0)
  lasso.cv <-cv.glmnet(x=as.matrix(xx.tr), y=y.tr, lambda = lambdas.rr, nfolds=k, alpha=1)
  
  pr.lasso[hold] <-predict(lasso.cv, s = lasso_cv$lambda.min, newx=xx.te)
  pr.ridge[hold] <-predict(ridge.cv ,s = ridge_cv$lambda.min, newx=xx.te)
}

mse1 <- mean((pr1 - y)^2)
mse2<- mean((pr2 - y)^2)
mse3 <- mean((pr3 - y)^2)
mse4 <- mean((pr4 - y)^2)
mse5 <- mean((pr5 - y)^2)
mse6 <- mean((pr6 - y)^2)
mse7 <- mean((pr7 - y)^2)
mse.las <- mean((pr.lasso-y)^2)
mse.ridge <- mean ((pr.ridge-y)^2)

MSE <- c(mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse.las,mse.ridge)
which.min(MSE) 

#GAM Model with poly(povdis,4) had the lowest MSE
summary(model.4)


#Prediction
data_te<-read.table("data_for_prediction.txt", header = TRUE, sep= "\t", dec= ".")[,-1]

family_size <- as.matrix(data_te[,16])
data_te$pov_line <- as.matrix(rep(NA,length(family_size)))
for (i in 1:length(data_te$pov_line)) {
  if 
  (family_size[i] > 1) {
    data_te$pov_line[i] <- 6280 + (family_size[i]-1)*2140
  }
  else{
    data_te$pov_line[i] <-  6280
  }
}

data_te$povdis <- data_te$inc - data_te$pov_line 
data_te$poverty <- as.matrix(rep(NA,length(data_te$pov_line)))

for (i in 1:length(data_te$povdis)) {
  if 
  (data_te$povdis[i] < 0) {
    data_te$poverty[i] <- 1
  }
  else{
    data_te$poverty[i] <-  0
  }
}

colnames(data_te)
X.cont <- data_te[,c(1,2,3,4,5,6,7,8,9,10,11)]
p <- ncol(X.cont)
n <- nrow(X.cont)
xmn <- scale(X.cont, center = FALSE, scale = rep(sqrt(n - 1), p)) 
data_te[,c(1,2,3,4,5,6,7,8,9,10,11)] <- xmn[,1:11]

my_model <- lm(tw ~ poly(povdis,4) + hequity + nifa + ira + e401 +age + twoearn + hmort, data=data2)

my_predictions <- predict(my_model, newx = as.matrix(data_te[,c(2:20)]))
write.table(my_predictions, file ='my_predictions.txt')