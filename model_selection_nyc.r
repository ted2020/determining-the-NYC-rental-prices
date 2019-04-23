
library(dplyr)

# R2 explained: only 1% of variation in the dependent variable is explained by the model
    # prefer adj R2
# SE represents the average distance of observed values from the regression(fitted) line. smaller the better. 
# SE = k, so on average, observed values are k centimeters away from the fitted line.



# check for multicollinearity
            # multicollinearity is the degree of correlation between regressors in a model
            # high degree of multicollinearity inflates teh variance of the estimator
        # VIF(Bhat1)=1/(1-R2)     (Variance Inflation Factor)
                    # how much var(Bhat1) is inflated due to correlation with other independent variables
            # there is VIF for each coefficient
            # larger VIFs indicate a greater degree of multicollinearity
# As a rule of thumb, a variable whose VIF values are greater than 10 may merit further investigation
# if VIF > 4, investigate
# if VIF > 10, act
# Tolerance, defined as 1/VIF, is used by many researchers to check on the degree of collinearity.
# A tolerance value lower than 0.1 is comparable to a VIF of 10. 
#It means that the variable could be considered as a linear combination of other independent variables.



# check for heteroskedasticity


# AIC
    # comparing the fitness of the model
    # the goal is not overfitting
    # smaller the better
        # AIC = n(ln(SSR/n)) + 2(k + 1)
    #AIC will over estimate the optimal degree of polynomial
# BIC
    # same idea as AIC
        # BIC = n(ln(SSR/n)) + (k+1)(ln(n))
    # minimizing BIC yields a consistent estimate of the degree of the polynomial

# t-stat
#If the absolute value of the t-value is greater than the critical value, you reject the null hypothesis.
    # critical values = table value
        # t value  >  critical      >>>>  reject the null (i.e. Ho: B1 = 0) (i.e Ha: B1 != 0)



#Correlation
    #As a rule of thumb, for absolute value of cor:
#0.00-0.19: very weak
#0.20-0.39: weak
#0.40-0.59: moderate 
#0.60-0.79: strong
#0.80-1.00: very strong.
    # keep only weakly correlated items



# RMSE
        # measure the goodness of fit
            # estimates the standard deviation of error term
                # RMSE = sqrt(SSR/(n-k-1)) 


# confounding
    # how much of dependent variable is explained between the independent variables
    # variance in the dependent can be explained by either(any) variable
    # confounding variable influences both the other independent variables and the dependent variable,
    # which causes a spurious association
    # confounding variable can cause an increase in variance and introduce bias


# check for interaction terms



# check for polynomial terms


# check for omitted variable bias
    # if model is misspecified
        # to specify a model
            # first, identify the research question and variable of interest
            # second, create a baseline model: use economic theory and judgement
                # baseline should include variables that have an effect on and correlated with the dependent
            # third, identify the questionable variables
                # add and remove those variable and observe the dependent
            
# check for bad controls
    # including more independent variable is Not always good
    # independent variables shouldnt be a function of dependent variable
    
# check for joint significant hypothesis

# # eigenvalues analysis
    # if all eigenvalues are about the same magnitude, no multicollinearity
    # if bigger than 100, there is a problem

fulldata <- read.csv("clean.csv")

head(fulldata)

dim(fulldata)

library(olsrr)


fit <- lm(A3 ~ A8+A12+A13+A19+A22+A24+A26+A30+A38, fulldata)
#fit
fit_ols <- ols_step_all_possible(fit)
fit_ols

best_aic <- arrange(fit_ols,aic)
best_aic[1:5,]
best_sbic <- arrange(fit_ols,sbic)
best_sbic[1:5,]

summary(fit)

pickthebest <- fit_ols %>% filter(n==4)
head(pickthebest)
# this provides the best sbic:
# car free commute + median household income + unemployment rate + income diversity ratio

# second best option is: 
# car free commute + mean travel time to work + median household income + unemployment rate + income diversity ratio

# now look for omitted variable bias, bad control, interaction terms, and possible polynomial regressions 

ols_vif_tol(fit)

TSS = sum((fulldata$A3 - mean(fulldata$A3))^2)
TSS
ESS = sum((fit$fitted.values - mean(fulldata$A3))^2)
ESS
RSS = sum((fulldata$A3 - fit$fitted.values)^2)
RSS
RSS <- c(crossprod(fit$residuals))
RSS
MSE <- RSS / length(fit$residuals)
MSE
RMSE <- sqrt(MSE)
RMSE
R2 <- ESS/TSS
R2
R2 <- 1-(RSS/TSS)
R2

plot(fulldata$A3,fit$residuals)
abline(0,0)

cor(fulldata[,4:47])

cov(fulldata[,4:47])

fit2 <- lm(A3 ~ A8+A12+A13+A19+A22+A24+A27+A30+A38+A10+A16+A36, fulldata)
#fit
fit_ols2 <- ols_step_all_possible(fit2)
fit_ols2

best_aic <- arrange(fit_ols2,aic)
best_aic[1:5,]
best_sbic <- arrange(fit_ols2,sbic)
best_sbic[1:5,]

ols_vif_tol(fit2)

summary(fit2)

fit3 <- lm(A3 ~ (A7+A8+A10)^5, fulldata)
#fit
fit_ols3 <- ols_step_all_possible(fit3)
fit_ols3

best_aic <- arrange(fit_ols3,aic)
best_aic[1:5,]
best_sbic <- arrange(fit_ols3,sbic)
best_sbic[1:5,]

summary(fit3)

ols_vif_tol(fit3)

# check for possible 2nd and 3rd degree for polynomial regression

fit4 <- lm(A3 ~ A13+A30+I(A13^3), fulldata)
#fit
fit_ols4 <- ols_step_all_possible(fit4)
head(fit_ols4)

best_aic <- arrange(fit_ols4,aic)
best_aic[1:5,]
best_sbic <- arrange(fit_ols4,sbic)
best_sbic[1:5,]

summary(fit4)

ols_vif_tol(fit4)

TSS = sum((fulldata$A3 - mean(fulldata$A3))^2)
TSS
ESS = sum((fit4$fitted.values - mean(fulldata$A3))^2)
ESS
RSS = sum((fulldata$A3 - fit4$fitted.values)^2)
RSS
RSS <- c(crossprod(fit4$residuals))
RSS
MSE <- RSS / length(fit4$residuals)
MSE
RMSE <- sqrt(MSE)
RMSE
R2 <- ESS/TSS
R2
R2 <- 1-(RSS/TSS)
R2

plot(fulldata$A3,fit4$residuals)
abline(0,0)

# fixing heteroskedasticity

fit5 <- lm(log(A3) ~ log(A13)+A30+A8+A26, fulldata)
#fit
fit_ols5 <- ols_step_all_possible(fit5)
head(fit_ols5)

plot(fulldata$A3,fit5$residuals)
abline(0,0)

ols_vif_tol(fit5)

logA3 <- log(fulldata$A3) 

TSS = sum((logA3 - mean(logA3))^2)
TSS
ESS = sum((fit5$fitted.values - mean(logA3))^2)
ESS
RSS = sum((logA3 - fit5$fitted.values)^2)
RSS
MSE <- RSS / length(fit5$residuals)
MSE
RMSE <- sqrt(MSE)
RMSE
R2 <- ESS/TSS
R2



logA3 <- log(fulldata$A3) 
mean(logA3)

plot(logA3,fit5$residuals)
abline(0,0)

logA13 <- log(fulldata$A13)
logA16 <- log(fulldata$A16)

fit6 <- lm(log(A3) ~ logA13+logA16+A23+A27+A30, fulldata)
#fit
fit_ols6 <- ols_step_all_possible(fit6)
head(fit_ols6)

best_aic <- arrange(fit_ols6,aic)
best_aic[1:5,]
best_sbic <- arrange(fit_ols6,sbic)
best_sbic[1:5,]

ols_vif_tol(fit6)

logA3 <- log(fulldata$A3) 

TSS = sum((logA3 - mean(logA3))^2)
TSS
ESS = sum((fit6$fitted.values - mean(logA3))^2)
ESS
RSS = sum((logA3 - fit6$fitted.values)^2)
RSS
MSE <- RSS / length(fit6$residuals)
MSE
RMSE <- sqrt(MSE)
RMSE
R2 <- ESS/TSS
R2

summary(fit6)

# checkign for confounding
# If the sum of ESS in simple regressions is higher than the ESS in multiple regression
# it means the predictors are correlated.
# Much of the variance in A3 is confounded between A13 and A16 and others
# The variance in A3 could be explained by either A13 or A16
# is counted twice if the sums of squares for A13 and A16 are simply added.

fit.for_confounding <- lm(log(A3) ~ logA13+logA16, fulldata)
ESS.for_confounding = sum((fit6$fitted.values - mean(logA3))^2)

# leave out logA16
fit6.logA13 <- lm(log(A3) ~ logA13, fulldata)
ess.logA13 <- sum((fit6.logA13$fitted.values - mean(logA3))^2)
# now leave out logA13
fit6.logA16 <- lm(log(A3) ~ logA16, fulldata)
ess.logA16 <- sum((fit6.logA16$fitted.values - mean(logA3))^2)
ifelse((sum(ess.logA13+ess.logA16)>ESS.for_confounding),"confounding","no confounding")

logA13.unique <- ESS-ess.logA16
logA13.unique
logA16.unique <- ESS-ess.logA13
logA16.unique

# leave out A27
fit6.A23 <- lm(log(A3) ~ A23, fulldata)
ess.A23 <- sum((fit6.A23$fitted.values - mean(logA3))^2)
# now leave out A23
fit6.A27 <- lm(log(A3) ~ A27, fulldata)
ess.A27 <- sum((fit6.A27$fitted.values - mean(logA3))^2)
ifelse((sum(ess.A23+ess.A27)>ESS),"confounding","no confounding")

A23.unique <- ESS - ess.A27
A23.unique
A27.unique <- ESS - ess.A23
A27.unique

# leave out A30
fit6.A23 <- lm(log(A3) ~ A23, fulldata)
ess.A23 <- sum((fit6.A23$fitted.values - mean(logA3))^2)
# now leave out A23
fit6.A30 <- lm(log(A3) ~ A30, fulldata)
ess.A30 <- sum((fit6.A30$fitted.values - mean(logA3))^2)
ifelse((sum(ess.A23+ess.A30)>ESS),"confounding","no confounding")

A23.unique_2 <- ESS - ess.A30
A23.unique_2
A30.unique <- ESS - ess.A23
A30.unique


logA13.logA16.confounded <- ESS - logA13.unique - logA16.unique
logA13.logA16.confounded

fit7 <- lm(log(A3) ~ logA13+logA16+A22+A26+A29, fulldata)
# possible additionals: A42*A43, A25, A26, 
#fit
fit_ols7 <- ols_step_all_possible(fit7)
head(fit_ols7)

best_aic <- arrange(fit_ols7,aic)
best_aic[1:5,]
best_sbic <- arrange(fit_ols7,sbic)
best_sbic[1:5,]

ols_vif_tol(fit7)

logA3 <- log(fulldata$A3) 

TSS = sum((logA3 - mean(logA3))^2)
TSS
ESS = sum((fit7$fitted.values - mean(logA3))^2)
ESS
RSS = sum((logA3 - fit7$fitted.values)^2)
RSS
MSE <- RSS / length(fit7$residuals)
MSE
RMSE <- sqrt(MSE)
RMSE
R2 <- ESS/TSS
R2

summary(fit7)

plot(logA3,fit7$residuals, xlab = "dependent", ylab = "residuals", main = "residuals plot")
abline(0,0)

# joint hypothesis for gender variables

# looking for joint significance of variables A46 tru A50 (race variables)
fit8_unrestricted <- lm(log(A3) ~ logA13+logA16+A22+A26+A29+A46+A47+A48+A49+A50, fulldata)
fit8_restricted <- lm(log(A3) ~ logA13+logA16+A22+A26+A29, fulldata)

anov_unr <- anova(fit8_unrestricted)
anov_unr
anov_r <- anova(fit8_restricted)
anov_r

SSEu <- anov_unr[10, 2]
SSEu
SSEr <- anov_r[6,2]
SSEr

N <- nrow(fulldata) #Number of observations in dataset
K <- 11 # 11 Betas in the unrestricted model (10 independent + B0)
J <- 5 #Because Ho has five restrictions (A46 tru A50)

fcritical <- qf(.95,K,J)
paste0( "f critical is ", fcritical)
fval <- ((SSEr-SSEu)/J) / (SSEu/(N-K-1))  # calculated f-stat
fval


ifelse((fval>fcritical),"jointly significant","not jointly significant")

pval <- 1-pf(fval, J, N-K) # p value of the test


anov_r_ur <- anova(fit8_restricted,fit8_unrestricted)
anov_r_ur

MSR <- anov_r_ur$RSS[2]/anov_r_ur$Res.Df[2]
MSR
Fstatpartial <- anov_r_ur$'Sum of Sq'[2]/MSR
Fstatpartial

library(lmtest)
library(SciViews)
likelihoodtest <- lrtest(fit8_unrestricted,fit8_restricted)
likelihoodtest


G2 <- (-2)*(ln(likelihoodtest$LogLik[2]/likelihoodtest$LogLik[1]))
G2
# if G2 is too large, reject the restricted model

summary(fit8_unrestricted)

library(robustbase)
library(nlme)

gls <- gls(log(A3) ~ log(A13)+log(A16)+A22+A26+A29, fulldata)
summary(gls)
