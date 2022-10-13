
#X1: PRECIP
#X2: EDUC
#X3: NONWHITE
#X4: POOR
#X5: NOX
#X6: SO2
#Y: MORTALITY
----
  ---
  title: "Final Project STA 108"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#### Step 2: have a glimpse of the data
```{r}
dat <- read.csv("mortality.csv")
dat <- dat[1:7]
names(dat) <- c("X1", "X2", "X3", "X4", "X5", "X6", "Y")
library(kader)
dat$X3 <- kader:::cuberoot(dat$X3)
dat$X4 <- kader:::cuberoot(dat$X4)
dat$X5 <- log(dat$X5)
dat$X6 <- log(dat$X6)
head(dat) # have a brief look
plot(dat) # matrix plot of the data
cor(dat) # correlation matrix
library(kader)
fit <- lm(Y~X1+X2+X3+X4+X5+X6,data = dat); fit
anova(fit) # the ANOVA table
summary(fit) # show the standard errors
```
#### Step 3: Do the diagnostics
```{r}
fit1 = lm(dat$Y~X1,data =dat)
plot(fit1$fitted.values,fit1$residuals)
qqnorm(fit1$res); qqline(fit1$res)
fit2 = lm(dat$Y~X2,data =dat)
qqnorm(fit2$res); qqline(fit2$res)
fit4 = lm(dat$Y~X4,data =dat)
qqnorm(fit4$res); qqline(fit4$res)
plot(lm(Y~X1, data = dat))
plot(lm(Y~X2, data = dat))
plot(lm(Y~X3, data = dat))
plot(dat$Y, fit$fitted.values, xlab = "Y", ylab = "Fitted Y") # plot the observed Y values against the fitted Y-values
# plot the residuals against the independent variables; six graphs

par(mfrow=c(2,3))
plot(dat$X1, fit$res, xlab = 'X1', ylab = 'Residuals'); abline(h = 0)
plot(dat$X2, fit$res, xlab = 'X2', ylab = 'Residuals'); abline(h = 0)
plot(dat$X3, fit$res, xlab = 'X3', ylab = 'Residuals'); abline(h = 0)
plot(dat$X4, fit$res, xlab = 'X4', ylab = 'Residuals'); abline(h = 0)
plot(dat$X5, fit$res, xlab = 'X5', ylab = 'Residuals'); abline(h = 0)
plot(dat$X6, fit$res, xlab = 'X6', ylab = 'Residuals'); abline(h = 0)
par(mfrow=c(1,1))
# histogram of residuals 
hist(fit$res)
# normal probability plot of residuals
qqnorm(fit$res); qqline(fit$res)
```


#### Step 4: Transform the data
```{r}
# The regression is linear, so there is no need to do the transformation.
```
#### Step 5: Exclude the unnecessary variables
```{r}
model_full=lm(Y~., data=dat)
step(model_full, direction="both") # Backward Stepwise Regreession

model_ini=lm(Y~1, data=dat)
step(model_ini, direction="both", scope=list(lower=model_ini, upper=model_full)) # Forward Stepwise Regreession

# They return the same model: Y ~ X1 + X2 + X3 + X6
```


plot(dat) # matrix plot of the data
cor(dat) # correlation matrix
fit = lm(y ~ x1 + x2 + x3, data = dat) #fit the regression
summary(fit)
anova(fit) #anova table
X = cbind(1, dat$X1,dat$X2,dat$X3,dat$X4,dat$X5,dat$X6); X
b = solve(t(X) %*% X) %*% t(X) %*% (dat$y)
b


