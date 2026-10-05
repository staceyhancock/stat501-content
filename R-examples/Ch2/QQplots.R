# Quantile-quantile plot demo - 10/5/26
samp1 <- c(1, 6, 7, 10, 11, 15)
samp2 <- c(200, 210, 250, 270)

quantile(samp1)
quantile(samp2)

plot(quantile(samp1) ~ quantile(samp2))

# Try the next few lines several times varying the sample size.
# Since we are generating from a normal distribution,
# the qq-plot of the data vs theoretical normal quantiles
# should look like an approximate straight line.
randnorm <- rnorm(50)
qqnorm(randnorm)
qqline(randnorm)

# Try these with varying sample sizes.
# Now that we are generating from an exponential distribution
# (right-skewed), the qq-plot of the data vs theoretical
# normal quantiles should look like a C-curve above the line.
randexp <- rexp(50)
qqplot(randnorm, randexp)

