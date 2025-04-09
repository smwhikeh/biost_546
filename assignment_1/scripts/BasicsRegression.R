# Contents:
# * Use of data frames
# * Generating random datasets
# * Plotting (ggplot2)
# * Linear regression

# Please update R!
rm(list = ls())

library(rgl)
library(ggplot2)
library(ISLR2)


###########################################################
############### Basic Data Manipulation ###################
###########################################################

## Basic matrix manipulation (homogeneous data)
x = matrix(data = c(1,2,3,4), nrow=2, ncol=2)
x

x = matrix(c(1,2,3,4),2,2)
sqrt(x)
x+1

# (Random) vectors
vec = rnorm(50,1,2)
vec_nomean = vec-mean(vec)

mean(vec_nomean)
vec_nomean_sd1 = vec_nomean/sd(vec_nomean)

sd(vec_nomean_sd1)
mean(vec_nomean_sd1)

# Define random matrix from a random vector
x = matrix(vec,25,2)
dim(x)

head(x)

# Normalizing matrix
x_scaled = scale(x)
sd(x_scaled[,2])
mean(x_scaled[,2])

# Adding columns (or rows)
dim(x)
x = cbind(x,rnorm(25))
dim(x)

cor(x[,1], x[,2])

colMeans(x) # Mean of every column
var(x)      # (Co)Variance matrix of the data matrix
var(x[,1])  # Variance of single column

cor(x)       # Correlation matrix of the data

plot(x[,1], x[,2])

# Basic dataframe manipulation (non-homogeneous data)
comp.data <- data.frame(V2 = c('A','B','A'),
                        V3 =  c(1,2.5,1.6)) 
comp.data
class(comp.data)
str(comp.data)

class(comp.data$V3) # Type of data
comp.data$V2 = as.factor(comp.data$V2)

str(comp.data)

comp.data[,1]
colnames(comp.data)
colnames(comp.data) = c("Class","Qty")
comp.data

###########################################################
######### Generate synthetic data for Regression ##########
###########################################################

set.seed(0)
x = matrix(runif(200, 0, 5),100,2) # Implicit assumption?
# Y = beta_0 + X1 * beta_1 + X2 * beta_2 + epsilon

y = 2.5+2*x[,1]+4*x[,2]+rnorm(100,mean = 0, sd = 0.1)

lm.yx = lm(y ~ x)
summary(lm.yx)

# Add non-linear covariates to the data matrix
x = cbind(x,x[,1]^2)
#x
#summary(lm(y ~ x))

# Add non-linear covariates to the data matrix
y = 2 + x[,1]*3 + x[,2]*(-1) + (x[,1]^2)*(-3) + rnorm(100,mean = 0, sd = 0.1)
summary(lm(y ~ x))


###########################################################
######################### Plots ###########################
###########################################################
plot(x[,1],y)   # Outcome vs covariate 1
plot(x[,2],y)   # Outcome vs covariate 2
plot3d(x[,1],x[,2],y)  # Outcome vs (covariate 1,covariate 2). Note: implemented in library rgl

# Alternative: ggplot2 (https://posit.co/wp-content/uploads/2022/10/data-visualization-1.pdf)
# Outcome vs covariate 1

ggplot()+
  geom_point(aes(x = x[,1], y = y), size = 2, col = 'red')+
  labs(title = "Training set", x ="X1", y = "Y")

# Outcome (color) vs (covariate 1,covariate 2)
ggplot()+
  geom_point(aes(x = x[,1], y = x[,2], col = y), size = 2)+
  labs(title = "Training set", x ="X1", y = "X2")

## Outcome (dot color and size) vs (covariate 1,covariate 2)
ggplot()+
  geom_point(aes(x = x[,1], y = x[,2], col = y, size = y))+
  labs(title = "Training set", x ="X1", y = "X2")

## Add multiple layers
ggplot()+
  geom_point(aes(x = x[,1], y = y))+
  geom_smooth(aes(x = x[,1], y = y))


# Different approach to using ggplot2
df = data.frame(x,y)
colnames(df)

ggplot(aes(x = X1, y = X2, col = y), data = df)+
  geom_point(size=2)

ggplot(aes(x = X1, y = y), data = df)+
  geom_point(size=1)+
  geom_smooth()


###########################################################
#################### Regression ###########################
###########################################################


# Multiple Linear Regression
head(Boston) # In library ISLR2 -- ?Boston
str(Boston)
colnames(Boston)

lm.fit <- lm(medv ~ lstat + age , data = Boston)
names(lm.fit)  # What's in lm.fit?

lm.fit$coefficients # Extract coefficients (approach 1)
coef(lm.fit)   # Extract coefficients (approach 2)

coef(lm.fit)[2]


summary(lm.fit) # 
names(summary(lm.fit)) # What's in Summary?
summary(lm.fit)$sigma

# R documentation summary?
?summary.lm

# How do I fit a model without intercept? 
lm.fit <- lm(medv ~ -1 +lstat + age , data = Boston)
summary(lm.fit)

# Confidence intervals for the coefficients
confint(lm.fit)

# Simple prediction
medv_hat = predict(lm.fit) # Prediction for data in training set

# Predictions for new observations
predict(lm.fit, data.frame(lstat = c(5, 10, 15), age = c(30,50,70)))

# Confidence intervals and predict() prediction intervals for the prediction
# predict(lm.fit , data.frame(lstat = c(5, 10, 15), age = c(30,50,70)),
#           interval = "confidence")

#predict(lm.fit , data.frame(lstat = c(5, 10, 15), age = c(30,50,70)),
#           interval = "prediction")



# Plots for linear models: lm medv ~ lstat
lm.fit1 = lm(medv ~ lstat, data = Boston)

ggplot()+
  geom_point(aes(x = Boston$lstat, y = Boston$medv), size = 2)+
  geom_abline(slope = lm.fit1$coefficients[2], intercept = lm.fit1$coefficients[1], col = 'red')

# Plots for linear models by generating many "new observations"
new_obs = data.frame(lstat = seq(from = 0, to = 40, by = 1), age = 40)
pred = predict(lm.fit1, new_obs)

ggplot()+
  geom_point(aes(x = Boston$lstat, y = Boston$medv))+
  geom_line(mapping=aes(x = new_obs$lstat, y=pred), size=1, col='red')

# model with all covariates
lm.fit <- lm(medv ~ ., data = Boston)
summary (lm.fit)

# model with all but one covariate
lm.fit1 <- lm(medv ~ . - age , data = Boston)
summary(lm.fit1)


# Model with interaction
lm.fit_int = lm(medv ~ lstat + age + lstat:age , data = Boston)
summary(lm.fit_int)
model.matrix(lm.fit_int)

# Alternative: summary(lm(medv ~ lstat * age , data = Boston))


# Non-linear Transformations of the Predictors (^ is a special character: needs wrapping)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

# Non-linear Transformations of the Predictors
summary(lm(medv ~ log(rm), data = Boston))


# Polynomial transformations of the Predictors
lm.fit5 <- lm(medv ~ poly(lstat , 5), data = Boston)
model.matrix(lm.fit5)
summary(lm.fit5)

# Make plot for polynomial model by generating many "new observations"
new_obs = data.frame(lstat = seq(from = 0, to = 40, by = 1))
pred = predict(lm.fit5, new_obs)

ggplot()+
  geom_point(aes(x = Boston$lstat, y = Boston$medv))+
  geom_line(mapping=aes(x = new_obs$lstat, y=pred), size=1, col='red')


# Qualitative Predictors
head(Carseats) #library(ISLR2). ?Carseats
str(Carseats)

str(Carseats$ShelveLoc)
Carseats$ShelveLoc

contrasts(Carseats$ShelveLoc)
contrasts(Carseats$US)

# You can change factor encoding as follows
# ShelveLoc2 = factor(Carseats$ShelveLoc, levels = c("Good", "Bad", "Medium"))
# contrasts(ShelveLoc2)


# Qualitative Predictors with interactions
summary(lm(Sales ~ ShelveLoc, data = Carseats))

# beta_0 + beta_1*ShelveLocGood + beta_2*ShelveLocMedium
# IF ShelveLoc = Bad    THEN beta_0 
# IF ShelveLoc = Good   THEN beta_0 + beta_1
# IF ShelveLoc = Medium THEN beta_0 + beta_2


summary(lm(Sales ~ ShelveLoc +  Price,
               data = Carseats))

# beta_0 + beta_1*ShelveLocGood + beta_2*ShelveLocMedium + beta_3*Price
# IF ShelveLoc = Bad    THEN  beta_0                   + beta_3*Price
# IF ShelveLoc = Good   THEN  beta_0 + beta_1          + beta_3*Price
# IF ShelveLoc = Medium THEN  beta_0          + beta_2 + beta_3*Price

summary(lm(Sales ~ ShelveLoc + Price + US,
           data = Carseats))

# beta_0 + beta_1*ShelveLocGood + beta_2*ShelveLocMedium + beta_3*Price + beta_4*USYes
# IF ShelveLoc = Bad   , US = No:    beta_0 +                 + beta_3*Price
# IF ShelveLoc = Good  , US = No:    beta_0 + beta_1          + beta_3*Price
# IF ShelveLoc = Medium, US = No:    beta_0 + beta_2          + beta_3*Price
# IF ShelveLoc = Bad   , US = Yes:   beta_0          + beta_4 + beta_3*Price
# IF ShelveLoc = Good  , US = Yes:   beta_0 + beta_1 + beta_4 + beta_3*Price
# IF ShelveLoc = Medium, US = Yes:   beta_0 + beta_2 + beta_4 + beta_3*Price

summary(lm(Sales ~ ShelveLoc + US + Price + ShelveLoc:Price,
           data = Carseats))

# beta_0 + beta_1*ShelveLocGood + beta_2*ShelveLocMedium + beta_3*Price + beta_4*USYes + 
#          beta_5*ShelveLocGood*Price + beta_6*ShelveLocMedium*Price
# IF ShelveLoc = Bad   , US = No:    beta_0 +                 + beta_3*Price
# IF ShelveLoc = Good  , US = No:    beta_0 + beta_1          + (beta_3+beta_5)*Price
# IF ShelveLoc = Medium, US = No:    beta_0 + beta_2          + (beta_3+beta_6)*Price
# IF ShelveLoc = Bad   , US = Yes:   beta_0          + beta_4 + beta_3*Price
# IF ShelveLoc = Good  , US = Yes:   beta_0 + beta_1 + beta_4 + (beta_3+beta_5)*Price
# IF ShelveLoc = Medium, US = Yes:   beta_0 + beta_2 + beta_4 + (beta_3+beta_6)*Price