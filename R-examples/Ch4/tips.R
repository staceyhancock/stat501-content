## Two-Way Classification in R - Example

## Example 16.15 in Utts and Heckard, 5th ed.
## Study from Rind and Bordia, 1996
# Research question: Do happy faces increase your tips?
# One female and one male restaurant server at a Philadelphia restaurant drew
# happy faces on the checks of randomly chosen dining parties.
# In all they drew happy faces on 45 checks (22 for female, 23 for male),
# and did not draw happy faces on 44 other checks (23 for female, 21 for male).
# The sequence for drawing the happy face or not
# was randomized in advance.

## In class exercises:
# 1. What are the observational units in this data set?
# 2. What variables were measured on each observational unit? Types?
# 3. How would you organize this data set in a spreadsheet? 
#    What are the rows? Columns?

# Load required libraries
library(tidyverse)
library(pracma)  # eye
library(MASS)  # ginv
library(nlme)  # gls

############
##### Data Import
############
## Load tips data set from course webpage
tips <- read.table("http://www.math.montana.edu/shancock/data/happyface.txt", header=TRUE)
View(tips)

## Recode Message factor so that "None" is the reference category
## (R will by default use the first category in alphabetical order as reference)
tips$Message <- relevel(factor(tips$Message), ref = "None")


############
##### EDA
############
## Boxplots
tips %>% 
  ggplot(aes(x = Sex, y = TipPct, fill = Message)) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               position = position_dodge(0.8)) +
  labs(x = "Sex", y = "Tip Percentage",
       title = "Tip Percentage vs Message and Sex")

# Group means
tips.grp <- tips %>%
  group_by(Sex, Message) %>%
  summarise(Grp_Means = mean(TipPct))
tips.grp

# Interaction plot (plot means and connect dots)
tips.grp %>%
  ggplot(aes(x = Sex, y = Grp_Means, color = Message)) +
  geom_line(aes(group = Message)) +
  geom_point() +
  labs(x = "Sex", y = "Mean Tip Percentage",
       title = "Interaction Plot of Mean Tip Percentage")


## Summary statistics

# Univariate
summary(tips)

# By group
tips %>% 
  group_by(Sex, Message) %>% 
  summarise(
    mean = mean(TipPct),
    sds = sd(TipPct),
    n = length(TipPct))

############
##### Modeling - Ordinary Least Squares
############

## Fit two-way classification model with interaction
mod <- lm(TipPct ~ 1 + Sex + Message + Sex:Message, data=tips)
# Equivalent to:
mod <- lm(TipPct ~ Sex*Message, data=tips)
summary(mod)

## Check calculations
y <- tips$TipPct
X <- model.matrix(mod)

betahat <- solve(t(X) %*% X) %*% t(X) %*% y
P <- X %*% solve(t(X) %*% X) %*% t(X)
sse <- t(y - X%*%betahat) %*% (y - X%*%betahat)
sigma2hat <- as.vector(sse/(89-4))
sqrt(sigma2hat) # Should match "Residual standard error"

## Variance-covariance matrix of betahat
Vhat.betahat <- sigma2hat * solve(t(X)%*%X)
# matches
sigma2hat * summary(mod)$cov.unscaled
# Standard errors are sqrts of diagonal
sqrt(diag(Vhat.betahat))


# R-squared = fraction of variability in y explained by the model?
sst <- sum( (y - mean(y)) ^2)
1 - sse/sst

# Overall F-test:
# H0: E(Y) = beta_0
# Ha: E(Y) = beta_0 + beta_1*SexMale 
#            + beta_2*MessageHappyFace + beta_3*SexMale:MessageHappyFace
# Note: SSE under H0 = SST (no predictors)
Fstat <- ((sst - sse)/(4-1))/(sigma2hat)
# p-value
pf(Fstat, 3, 85, lower.tail=FALSE)

# Null model
mod0 <- lm(TipPct ~ 1, data=tips)
summary(mod0)
sse0 <- sum((y - mod0$fitted)^2)

##
### Inference for beta_2 + beta_3
## # Expected difference between tip % with happy face
## # vs no happy face for males
C <- matrix(c(0,0,1,1),ncol = 1, nrow = 4)
# Estimate
est <- as.vector(t(C)%*%betahat)
# Check
sum(coef(mod)[c(3,4)])
tips.grp[3,3] - tips.grp[4,3]

# Standard error of estimate
se <- as.vector(sqrt(t(C)%*%Vhat.betahat%*%C))

# F-statistic for H0: beta_2 + beta_3 = 0
Fstat2 <- (t(est) %*%  est)/(se^2)
# p-value
pf(Fstat2, 1, 85, lower.tail=FALSE)

# t-statistic
tstat <- est/se
# Note same as
-sqrt(Fstat2)
# p-value
2*pt(tstat, 85)

## Confidence interval
est + c(-1,1) * qt(.975, 85) * se

##
### Non-full rank design matrix - over-parameterization
### Effects model: E(y_{ijk}) = mu + alpha_i + beta_j + gamma_ij
##
F <- as.numeric(tips$Sex == "Female")
M <- as.numeric(tips$Sex == "Male")
N <- as.numeric(tips$Message == "None")
H <- as.numeric(tips$Message == "HappyFace")

X.e <- cbind(
  rep(1, length(y)),  # mu
  F,  # alpha_1
  M,  # alpha_2
  N,  # beta_1
  H,  # beta_2
  F*N,  # gamma_11
  F*H,  # gamma_12
  M*N,  # gamma_21
  M*H   # gamma_22
)

round(svd(X.e)$d,5)  # Rank = 4

# Fit model
betahat.e <- ginv(t(X.e) %*% X.e) %*% t(X.e) %*% y
sse.e <- t(y - X.e%*%betahat.e) %*% (y - X.e%*%betahat.e)
sigma2hat.e <- as.vector(sse.e/(89-4))

# But beta is not estimable!
# Try another generalized inverse
z <- runif(9)  # Pick arbitrary 9x1 vector
betahat.e2 <- betahat.e + 
  (eye(9) - ginv(t(X.e) %*% X.e) %*% t(X.e) %*% X.e) %*%z
# Different "estimate"
# But same sse:
t(y - X.e%*%betahat.e2) %*% (y - X.e%*%betahat.e2)
sse.e

## Inference only dependent on sse is still valid:
# R-squared is the same as the other parameterization
sst <- sum( (y - mean(y)) ^2)
1 - sse.e/sst
1 - sse/sst

# Overall F-test is the same as the other parameterization
F.e <- ((sst - sse.e)/(4-1))/(sigma2hat.e)
F.e
Fstat
# p-value
pf(F.e, 3, 85, lower.tail=FALSE)
pf(Fstat, 3, 85, lower.tail=FALSE)
