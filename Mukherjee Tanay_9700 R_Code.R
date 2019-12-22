# Author: Tanay Mukherjee
# EMPL ID: 23987468
# Subject: Final Project for STA 9700

#----------Chapter 1 and 2: Linear Regression with 2 variables--------------

# Load the data
pedestrian <- read.csv("C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9700 - Modern Regression Analaysis\\Project\\ped_sample.csv")

# Show first 5 rows of the dataset
head(pedestrian, n=5)

# Variables in the dataset
names(pedestrian)

# Creating scatter plot for the given data set
library(ggplot2)
ggplot(pedestrian, aes(x=pedestrian$Towards.Manhattan,y=pedestrian$Pedestrians)) + geom_point(size=1, alpha=0.6) +
  xlab("Pedestrian crossing towards Manhattan") + 
  ylab("Total count of pedestrians") 

ggplot(pedestrian, aes(x=pedestrian$Towards.Brooklyn,y=pedestrian$Pedestrians)) + geom_point(size=1, alpha=0.6) +
  xlab("Pedestrian crossing towards Brooklyn") + 
  ylab("Total count of pedestrians")  


# The model fit from the dataset
model<-lm(pedestrian$Pedestrians~pedestrian$Towards.Manhattan)
plot(model)

model<-lm(pedestrian$Pedestrians~pedestrian$Towards.Brooklyn)
plot(model)

# Checking outliers and heteroscedacity
library(car)
outlier(pedestrian$Towards.Manhattan)

# The co-relation between variables
library(dplyr)
library(GGally)
ped_corr <- pedestrian %>% select(Towards.Manhattan,Towards.Brooklyn, Pedestrians)
ggcorr(ped_corr, label = TRUE)

# Scatter plot between both the regressor variables - x1 and x2
ggplot(pedestrian, aes(x=pedestrian$Towards.Manhattan,y=pedestrian$Towards.Brooklyn)) + geom_point(size=1, alpha=0.6) +
  geom_smooth(method = 'lm', se = FALSE) + xlab("Pedestrian crossing towards Manhattan") + 
  ylab("Pedestrian crossing towards Brooklyn")

# Assign values of the columns to the necessary variables
x = pedestrian$Towards.Manhattan
y = pedestrian$Pedestrians
z = pedestrian$Towards.Brooklyn

# Computation of sample slope, intercept and residuals
lm(y~z)
summary(lm(y~z))

# Check the normal distribution of the values of daily pedestrians
hist(y, prob=TRUE, ylim=c(0,.06), breaks=5) +
  curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)

# T- test for two tails sample for both x1 and x2
t.test(x, y, va.equal = TRUE)
t.test(z, y, va.equal = TRUE)

# F -test
res.ftest <- var.test(pedestrian$Pedestrians ~ pedestrian$Towards.Manhattan, data = pedestrian)
res.ftest

# Check the variance influence
var.test(pedestrian$Pedestrians ~ pedestrian$Towards.Manhattan, data = pedestrian)


# partial coeff
# Assign values of the columns to the necessary variables
x = pedestrian$Towards.Manhattan
y = pedestrian$Pedestrians
z = pedestrian$Towards.Brooklyn
lm(y~x)
summary(lm(y~x))
summary(lm(y~x))$r.squared 

summary(lm(y~x))$adj.r.squared

lm(y~x+z)
summary(lm(y~x+z))

help(summary.lm)

library(dplyr)
ped_pair <- pedestrian %>% select(3:5)

# Check for multi-collinearity
pairs(ped_pair)
ped <- corr(ped_pair)
ped

cor(ped_pair)

# Compute eigen values
eigen(cor(ped_pair))$values

# The upper and the lower bound for the fitted model
model <- lm(pedestrian$Pedestrians~pedestrian$Towards.Manhattan, data=pedestrian)
model
P_conf <- predict(model, pedestrian, interval = "confidence")
P_conf %>% head()
P_pred <- predict(model, pedestrian, interval = "prediction")
P_pred %>% head()


# Plot the partial coefficients
reg_Ped_Man <- lm(pedestrian$Pedestrians~pedestrian$Towards.Manhattan, data = pedestrian)
reg_Ped_Man_res <- residuals(reg_Ped_Man)
reg_man_brook <- lm(pedestrian$Towards.Brooklyn~pedestrian$Pedestrians, data = pedestrian)
reg_man_brook_res <- residuals(reg_man_brook)
summary(lm(reg_Ped_Man_res~reg_man_brook_res))

# Plot is against the dummy variable
ggplot(pedestrian, aes(x=pedestrian$Towards.Manhattan,y=pedestrian$Towards.Brooklyn)) + geom_point(size=1, alpha=0.6) +
  geom_smooth(method = 'lm', se = FALSE) + xlab("Pedestrian crossing towards Manhattan") + 
  ylab("Pedestrian crossing towards Brooklyn") + facet_grid(~.x2_dummy)



#----------Chapter 3: Matrix Methods--------------

# Load the file again
pedestrian <- read.csv("C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9700 - Modern Regression Analaysis\\Homework_9700\\ped_hat.csv")

Y = pedestrian[,3]
Xmat = cbind(rep(1,8),pedestrian[,1], pedestrian[,2])

Y
Xmat

b_hat <- solve(t(Xmat) %*% Xmat) %*% t(Xmat) %*% Y
b_hat

Xmat %*% b_hat

# Compute Hat matrix
HatMat <- Xmat %*% solve(t(Xmat) %*% Xmat) %*% t(Xmat)
HatMat %*% Y

max(diag(HatMat))

x <- pedestrian$Total.Pedestrians
y <- pedestrian$temperature
z <- pedestrian$Towards.Manhattan

lm(y~x)
lm(y~z)
lm(x~y)
lm(x~z)
lm(z~x)
lm(z~y)

summary(lm(z~x+y))


Y = pedestrian[,2]
Xmat = cbind(rep(1,8),pedestrian[,1])

Xmat

# Compyte b-hat matrix
b_hat <- solve(t(Xmat) %*% Xmat) %*% t(Xmat) %*% Y
b_hat

max(diag(b_hat))


#----------Chapter 4: Polynomial Regression--------------

# Load the data
pedestrian <- read.csv("C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9700 - Modern Regression Analaysis\\Project\\ped_sample.csv")

# Show first 5 rows of the dataset
head(pedestrian, n=5)

pedestrian %>% select(2,3,4,16) %>% head(n=10)

# Variables in the dataset
names(pedestrian)

# Assign values of the columns to the necessary variables
x = pedestrian$Towards.Manhattan
y = pedestrian$Pedestrians
z = pedestrian$Towards.Brooklyn

library(dplyr)
pedestrian <- pedestrian %>% mutate(square_x = Towards.Manhattan*Towards.Manhattan)

x_sq <- pedestrian$square_x

library(GGally)
pedestrian <- pedestrian %>% select(Pedestrians,Towards.Manhattan,Towards.Brooklyn,temperature,interaction_term)
ggpairs(pedestrian)

# Creating scatter plot for the given data set
library(ggplot2)
ggplot(pedestrian, aes(x=x,y=y)) + geom_point(size=1, alpha=0.6) +
  geom_smooth(method = 'lm', se = FALSE) + xlab("Pedestrian crossing towards Manhattan") + 
  ylab("Total count of pedestrians") 

model<-lm(pedestrian$Pedestrians~pedestrian$Towards.Manhattan)
summary(model)

# calculate the sqaured value for the regressor variable
model_2 <- lm(pedestrian$Pedestrians~pedestrian$Towards.Manhattan + x_sq)
summary(model_2)


# Plot the graph for polynomial regression
ggplot(pedestrian, aes(x=x,y=y)) + geom_point(size=1, alpha=0.6) +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 9, raw=TRUE),colour="red") +
  xlab("Pedestrian crossing towards Manhattan") + ylab("Total count of pedestrians")

ggplot(pedestrian, aes(x=x,y=y)) + geom_point(size=1, alpha=0.6) +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 14),colour="red") +
  xlab("Pedestrian crossing towards Manhattan") + ylab("Total count of pedestrians")

# Anova Test for F values
par(mfrow=c(2,2))
plot(model_2)

anova(model, model_2)
pairs(pedestrian %>% select(c(3,4,5,13)))

mydata <- pedestrian %>% tbl_df()
(chi2 <- chisq.test(table(mydata$Towards.Manhattan, mydata$Pedestrians), p = rep(1/12, 12)))

chisq.test(table(x, y), correct = FALSE)

# pairs(Pro2)

ggplot(pedestrian, aes(x=x,y=y)) + geom_point(size=1, alpha=0.6) +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, c(2,3), raw=TRUE),colour="red") +
  xlab("Pedestrian crossing towards Manhattan") + ylab("Total count of pedestrians")

x = pedestrian$temperature

ggplot(pedestrian, aes(x=x,y=y)) + geom_point(size=1, alpha=0.6) +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 3, raw=TRUE),colour="red") +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 4, raw=TRUE),colour="red") +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 5, raw=TRUE),colour="red") +
  xlab("Temperature") + ylab("Total count of pedestrians")

# Ref Link: https://rpubs.com/mdlama/spring2017-lab6supp1

# Identify dummy and interaction variable and plot it
model_3 <- lm(pedestrian$Pedestrians ~ pedestrian$Towards.Manhattan + pedestrian$temperature)
summary(model_3)

model_4 <- lm(pedestrian$Towards.Manhattan ~ pedestrian$x1_dummy + pedestrian$x2_dummy )
summary(model_4)

model_dummy <- lm(pedestrian$Pedestrians ~ pedestrian$Towards.Manhattan + pedestrian$Towards.Brooklyn + pedestrian$temperature + x*pedestrian$x1_dummy)
summary(model_dummy)
par(mfrow=c(2,2))
plot(model_dummy)

library(jtools) # for summ()
library(interactions)
library(ggplot2)
summ(model_dummy)

interact_plot(pedestrian, pred = pedestrian$Pedestrians, modx = pedestrian$Towards.Manhattan)

# estimate the model with a binary interaction term
bi_model <- lm(pedestrian$Pedestrians ~ pedestrian$temperature + x*pedestrian$x1_dummy, data = pedestrian)

interact_plot(model_dummy, pred = pedestrian$Pedestrians, modx = pedestrian$x1_dummy, data = pedestrian)

pedestrian.factor <- factor(x1_dummy)

contrasts(pedestrian$x1_dummy)

# print a robust summary of the coefficients
coeftest(bi_model, vcov. = vcovHC, type = "HC1")

pedestrian <- pedestrian %>% mutate(interaction_term = pedestrian$Towards.Manhattan * pedestrian$x1_dummy)

pedestrian %>% select(3,4,13,16,17) %>% head(n=5)

pedestrian %>% ggparis ()


#----------Chapter 5: Model Selection--------------

library(dplyr)
library(tidyverse)
library(caret)
library(leaps)
library(tidyr)
library(ISLR)

# Assign values of the columns to the necessary variables
x = pedestrian$Towards.Manhattan
y = pedestrian$Pedestrians
z = pedestrian$Towards.Brooklyn

# select (pedestrian,-c(pedestrian$sample,pedestrian$x1_dummy,pedestrian$x2_dummy))

# 1.1
pairs(pedestrian %>% select(c(3,4,5,6,7,8)))

# 1.2
pairs(pedestrian %>% select(c(3,4,5,6,7,8)), log = "xy")


pedestrian <- na.omit(pedestrian)
str(pedestrian)

pedestrian <- rename(pedestrian, Manhattan = Towards.Manhattan)
pedestrian <- rename(pedestrian, Brooklyn = Towards.Brooklyn)
pedestrian <- rename(pedestrian, WS = weather_summary)


pedestrian_leaps <- pedestrian %>% select(3,4,5,6,7,8)

names(pedestrian_leaps)

leaps <- regsubsets(pedestrian_leaps$Brooklyn~., data=pedestrian_leaps,nvmax = 6)

reg.summary <- summary(leaps)
names(reg.summary)

data.frame(RSQ = which.max(reg.summary$rsq), 
           RSS = which.max(reg.summary$rss), AdjR2 = which.max(reg.summary$adjr2),
           BIC = which.min(reg.summary$bic), CP = which.min(reg.summary$cp))

plot(reg.summary$rsq, xlab = "Number of variables", ylab = "R square", type = "l")
points(2, reg.summary$rsq[2], col="red", cex = 2, pch=20)

plot(reg.summary$rss, xlab = "Number of variables", ylab = "R square", type = "l")
points(1, reg.summary$rss[1], col="red", cex = 2, pch=20)

#Adjust R-square
which.max(reg.summary$adjr2)
plot(reg.summary$rss, xlab = "Number of variables", ylab = "Adjusted R square", type = "l")
points(2, reg.summary$adjr2[2], col="red", cex = 2, pch=20)

# BIC and CP
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(2, reg.summary$bic[2], col="red", cex = 2, pch=20)

which.min(reg.summary$cp)
plot(reg.summary$cp, xlab = "Number of variables", ylab = "CP", type = "l")
points(3, reg.summary$cp[3], col="red", cex = 2, pch=20)


# subset model
subset_model <- regsubsets(pedestrian_leaps$Brooklyn~., data=pedestrian_leaps, nvmax = 5)
summary(subset_model)

pedestrian_regfit <- pedestrian %>% select(3,4,5,7,8)
names(pedestrian_regfit)

#Period notation regression "Brooklyn" against all other variables in the dataset
regfit <- regsubsets(pedestrian_regfit$Brooklyn~., data=pedestrian_regfit, nbest=, method = "exhaustive")

#Period notation regression "Brooklyn" against all other variables in the dataset plus all interactions
# regfit <- regsubsets(pedestrian_regfit$Brooklyn~.^2, data=pedestrian_regfit, nbest=, method = "exhaustive")


summary(regfit)

library(car)

subsets(regfit,statistic = "rsq")
subsets(regfit,statistic = "rss")
subsets(regfit,statistic = "adjr2")
subsets(regfit,statistic = "bic")
subsets(regfit,statistic = "cp")

model_summ <- lm(pedestrian$Pedestrians ~ pedestrian$Towards.Manhattan + pedestrian$temperature)
summary(model_summ)

reg.summary$rsq
reg.summary$rss
reg.summary$adjr2
reg.summary$cp
reg.summary$bic

pedestrian_regfit <- pedestrian %>% select(3,4,5,6,7,8)

regfit <- regsubsets(pedestrian_regfit$Brooklyn~., data=pedestrian_regfit, nbest=, method = "exhaustive")

plot(regfit, scale = "r2")
plot(regfit, scale = "adjr2")
plot(regfit, scale = "bic")
plot(regfit, scale = "Cp")

coef(regfit, 4) #default bic

backward_model <- lm(pedestrian_leaps$Pedestrians ~., data = pedestrian_leaps)
summary(backward_model)

forward_model <- lm(pedestrian_leaps$Pedestrians ~., data = pedestrian_leaps)
summary(forward_model)

step(backward_model, direction = "backward")
step(forward_model, direction = "forward")
step(forward_model, direction = "forward", scope = formula(backward_model))

forward_model <- lm(Pedestrians ~ Manhattan, data = pedestrian_leaps)


plot(forward_model, which = 6)

plot(forward_model)

# par(mfrow=c(2,2))
# par(mfrow=c(1,1))
plot(forward_model)




library(car)
vif <- lm(Pedestrians ~., data = pedestrian_leaps)
summary(vif)

# checking multicolinearity for independent variables.
vif(vif)


forward_model <- lm(Pedestrians ~ Manhattan, data = pedestrian_leaps)
summary(forward_model)

forward_model <- lm(Pedestrians ~ Manhattan + Brooklyn, data = pedestrian_leaps)
summary(forward_model)
step(forward_model, direction = "forward", scope = formula(backward_model))

# All models and the best fit model
library(olsrr)
all_model <- lm(Pedestrians ~., data = pedestrian_leaps)
ols_step_best_subset(all_model)
summary_values <- ols_step_all_possible(all_model)
names(summary_values)
final_ans <- summary_values %>% select(3,4,5,7,8,9)
final_ans

plot(summary_values)
names(pedestrian_leaps)
cookd <- lm(Pedestrians ~ Manhattan + Brooklyn + temperature + precipitation, data = pedestrian_leaps )
plot(cookd,6)

summary_values[16,]

final_model <- lm(Pedestrians ~ Towards.Manhattan + Towards.Brooklyn + temperature, data=pedestrian_leaps)
plot(final_model,4)


vif(final_model)

ols_step_forward_p(all_model)


#----------Chapter 6: Special Topic - Logistic Regression--------------

summary(pedestrian)


sapply(pedestrian, sd)

xtabs(~Towards.Manhattan + Towards.Brooklyn, data = pedestrian)

mylogit <- glm(x2_dummy ~ Towards.Manhattan + Towards.Brooklyn + weather_dummy, data = pedestrian, family = "binomial")
summary(mylogit)

confint(mylogit)
confint.default(mylogit)

library(aod)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 1:4)

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

exp(coef(mylogit))

newdata1 <- with(pedestrian, data.frame(Towards.Manhattan = mean(Towards.Manhattan), Towards.Brooklyn = mean(Towards.Brooklyn), weather_dummy = factor(1:4)))
newdata1

newdata1$weather_dummy <- as.numeric(newdata1$weather_dummy)
str(newdata1)
pedestrian$weather_dummy <- as.numeric(pedestrian$weather_dummy)
str(pedestrian)

newdata1$weather_dummy <- predict(mylogit, newdata = newdata1, type = "response")
newdata1


newdata2 <- with(pedestrian, data.frame(Towards.Manhattan = rep(seq(from = 200, to = 800, length.out = 100),
                                                                4), Towards.Brooklyn = mean(Towards.Brooklyn), weather_dummy = rep(1:1, each = 100)))


newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link", se = TRUE))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

head(newdata3)

library(ggplot2)

# Logistic Regression
ggplot(newdata3, aes(x = Towards.Manhattan, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) +
  geom_line(aes(colour = weather_dummy), size = 1)

## End of the project ##
