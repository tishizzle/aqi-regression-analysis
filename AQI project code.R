#Loading the dataset
data_AQI = read.csv("C:\\Users\\Ritika Patel\\OneDrive\\Desktop\\MIT sem 2\\RA an MA Lab\\project\\maindata.csv")
head(data_AQI)

#Structure of Dataset
str(data_AQI)

#checked for missing values :
colSums(is.na(data_AQI))
nrow(data_AQI)
ncol(data_AQI)

#Fixing names
names(data_AQI) <- make.names(names(data_AQI))
names(data_AQI)

head(data_AQI)


#Fixing date for better understanding
data_AQI$Date <- as.Date(data_AQI$Date, format="%d-%m-%Y")

head(data_AQI)

#Removing columns 
data20 <- data_AQI
data20$City <- NULL
data20$AQI_Bucket <- NULL
data20$Xylene <- NULL

#Removing rows where AQI is missing
data21 <- data20[!is.na(data20$AQI), ]
nrow(data21)
ncol(data21)
head(data21)


#Fill remaining missing values
library(zoo) 

for(col in names(data21)){ #for loop for each column one by one
  if(is.numeric(data21[[col]])){ #only numeric columns 
    data21[[col]] <- na.locf(data21[[col]], na.rm = FALSE) #fill using previous values
    data21[[col]][is.na(data21[[col]])] <- mean(data21[[col]], na.rm = TRUE) #fill remaining using mean like top ones .. as we cannot use locf
  }
}

#Checking
colSums(is.na(data21))

#Dropping date for regression : 
data21$Date <- NULL

#Final dataset
head(data21)



#Building first model :
model_full <- lm(AQI ~ ., data = data21)
summary(model_full)

#Refined model
#removing NO and O3
model_refined <- lm(AQI ~ PM2.5 + PM10 + NO2 + NOx + NH3 + CO + SO2 + Benzene + Toluene, data = data21)
summary(model_refined)

anova(model_refined)


#checking multicollinearity 
library(car)
vif(model_refined)

#CONDITIONAL NUMBER
#Design matrix
X <- model.matrix(model_refined)[,-1]

#Eigen values
eigen_vals <- eigen(t(X) %*% X)$values

#Condition number
k <- sqrt(max(eigen_vals) / min(eigen_vals))
k
#k = 240.9285 thus  moderate multicollinearity





# Subset selection : 
library(leaps)

subset_model <- regsubsets(AQI ~ ., data = data21, nvmax = 11)
subset_summary <- summary(subset_model)

subset_summary$adjr2
subset_summary$bic

plot(subset_summary$adjr2, type="b", xlab="No. of Variables", ylab="Adjusted R2")
plot(subset_summary$bic, type="b", xlab="No. of Variables", ylab="BIC")

#Select best model:
#Highest Adjusted R² 
#Lowest BIC

model_subset <- lm(AQI ~ PM2.5 + PM10 + NO2 + NOx + NH3 + CO + Benzene + Toluene, data = data21)
summary(model_subset)



#step wise regression
library(MASS)

step_model <- stepAIC(model_full, direction = "both")
summary(step_model)
#Stepwise regression selects variables based on AIC
#Compare with subset model



#residual analysis :
#Standardized residuals:
res <- rstandard(model_subset)
plot(res, type="p", main="Standardized Residuals")
abline(h=0, col="red")

#qq plot
qqnorm(res)
qqline(res, col="red")


#normality test 
shapiro.test(res)
ks.test(res, "pnorm", mean(res), sd(res))


#Transformation
model_log <- lm(log(AQI) ~ PM2.5 + PM10 + NO2 + NOx + NH3 + CO + Benzene + Toluene, data = data21)
summary(model_log)



res_log <- rstandard(model_log)
qqnorm(res_log)
qqline(res_log)

shapiro.test(res_log)



#box cox
library(MASS)

boxcox(model_subset)


lambda <- 1.3

AQI_box <- (data21$AQI^lambda - 1)/lambda

model_box <- lm(AQI_box ~ PM2.5 + PM10 + NO2 + NOx + NH3 + CO + Benzene + Toluene, data=data21)

summary(model_box)

res_box <- rstandard(model_box)
qqnorm(res_box)
qqline(res_box)
shapiro.test(res_box)







#LASSO
library(glmnet)


# X = predictors (matrix form required)
X1 <- as.matrix(data21[, -which(names(data21) == "AQI")])

# Y = response
y1 <- data21$AQI

set.seed(123)

cv_lasso1 <- cv.glmnet(X1, y1, alpha = 1)  # alpha = 1 → LASSO
cv_lasso1


#Find best lambda
cv_lasso1$lambda.min
cv_lasso1$lambda.1se

#get coefficients
lasso_model1 <- glmnet(X1, y1, alpha = 1, lambda = cv_lasso1$lambda.min)

coef(lasso_model1)



lasso_model_1se <- glmnet(X1, y1, alpha = 1, lambda = cv_lasso1$lambda.1se)
summary(lasso_model_1se)
coef(lasso_model_1se)



#PCA
#Standardize predictors
X_scaled <- scale(X1)

#apply PCA
pca_model <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
summary(pca_model)

#Extract Pcs
PC <- pca_model$x[, 1:7]

#run regression
data_pca <- data.frame(AQI = y1, PC)

model_pcr <- lm(AQI ~ ., data = data_pca)
summary(model_pcr)
