################
### BINOMIAL ###
################
# n = 10, p = .4, y = 2
x=seq(0,12,1)
plot(x, dbinom(x,10,.4), type = "h")
dbinom(2,10,.4)
x=seq(0,2,1)
sum(dbinom(x,10,.4))
pbinom(2,10,.4) #Why is this the same as the previous prob?
qbinom(.15,10,.4)
rbinom(100,10,.4)

#################
### GEOMETRIC ###
#################
# p = .4, y = 3
x=seq(0,15,1)
par(mfrow=c(1,2))
plot(x, dgeom(x,.4), type = "h",xlim=c(0,15))
plot(x+1, dgeom(x,.4), type = "h",xlim=c(0,15)) #What is the difference between these two plots? Why?
dgeom(2,.4)
x=seq(0,2,1)
sum(dgeom(x,.4))
pgeom(2,.4) #Why is this the same as the previous prob?
qgeom(.78,.4)
rgeom(100,.4)+1

#########################
### NEGATIVE BINOMIAL ###
#########################
# r = 1, p = .4, y = 3
#Why are these these values and plots the same as those found for the geometric distribution?
x=seq(0,15,1)
par(mfrow=c(1,2))
plot(x, dnbinom(x,1,.4), type = "h",xlim=c(0,15))
plot(x+1, dnbinom(x,1,.4), type = "h",xlim=c(0,15)) #What is the difference between these two plots? Why?
dnbinom(2,1,.4)
x=seq(0,2,1)
sum(dnbinom(x,1,.4))
pnbinom(2,1,.4) #Why is this the same as the previous prob?
qnbinom(.78,1,.4)
rnbinom(100,1,.4)+1 #Why are these the only values that are different from those found for the geometric distribution?


######################
### HYPERGEOMETRIC ###
######################
# M = 2, N-M = 8, N = 10, n = 3, y = 1
par(mfrow=c(1,1))
x=seq(0,10,1)
plot(x, dhyper(x,2,8,3), type = "h") #Why does the pmf zero out where it does?
dhyper(1,2,8,3)
x=seq(0,1,1)
sum(dhyper(x,2,8,3))
phyper(1,2,8,3) #Why is this the same as the previous prob?
qhyper(.93,2,8,3)
rhyper(100,2,8,3)

###############
### POISSON ###
###############
# lambda = 3, y = 1
x=seq(0,20,1)
plot(x, dpois(x,3), type = "h")
dpois(1,3)
x=seq(0,1,1)
sum(dpois(x,3))
ppois(1,3) #Why is this the same as the previous prob?
qpois(.19,3)
rpois(100,3)


###############
### UNIFORM ###
###############
# a = 0, b = 10, y=1
curve(dunif(x,0,10),xlim=c(-2,12))
dunif(1,0,10)
punif(1,0,10)
qunif(.10,0,10)
runif(100,0,10)


##############
### NORMAL ###
##############
# mu = 0, sigma^2 = 25, y=1
curve(dnorm(x,0,5),xlim=c(-20,20))
dnorm(1,0,5)
pnorm(1,0,5)
qnorm(.5792597,0,5)
rnorm(100,0,5)


#############
### GAMMA ###
#############
# alpha = 1, beta = 2, y=3
#par(mfrow=c(2,2))
curve(dgamma(x,1,1/2),xlim=c(-5,30))
pgamma(3,1,1/2)
qgamma(.5792597,1,1/2)
rgamma(100,1,1/2) 

###################
### EXPONENTIAL ###
###################
# beta = 2, y=3
#Why are these these values and the plot the same as those found for the gamma distribution?
curve(dexp(x,1/2),xlim=c(-5,30))
pexp(3,1/2)
qexp(.5792597,1/2)
rexp(100,1/2) #Why are these the only values that are different from those found for the gamma distribution?

##################
### CHI-SQUARE ###
##################
# df = 2, y=3
#Why are these these values and the plot the same as those found for the gamma distribution?
curve(dchisq(x,2),xlim=c(-5,30))
pchisq(3,2)
qchisq(.5792597,2)
rchisq(100,2)#Why are these the only values that are different from those found for the gamma distribution?


############
### BETA ###
############
# alpha = 1, beta = 2, y=.3
curve(dbeta(x,1,2), xlim=c(-1,2))
pbeta(.3,1,2) #Why are we looking for P(Y<=.3) instead of P(Y<=3) like in the other examples?
qbeta(.51,1,2)
rbeta(100,1,2) 


##############
### CAUCHY ###
##############
# theta = 2, y=3
curve(dcauchy(x,2), xlim=c(-6,10))
pcauchy(3,2)
qcauchy(.75,2)
rcauchy(100,2) 


#################
### LOGNORMAL ###
#################
# mu=1, sigma^2=25, y=3
curve(dlnorm(x,2,5), xlim=c(0,10))
plnorm(3,2,5)
qlnorm(.75,2,5)
lnormal<-rlnorm(1000,2,5) 
hist(log(lnormal),nclass=12,freq=FALSE) #How would you describe the distribution of these transformed values? Why?
curve(dnorm(x,2,5),add=TRUE) #Compare to a normal distribution with mu = 2 and sigma^2=25 



