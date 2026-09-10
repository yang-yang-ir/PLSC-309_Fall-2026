#### PLSC 309 R Lab Session
## Date: 09-10-2026
## Sampling Distribution

# remove all existing lists
rm(list=ls())

# Set random seed
set.seed(123) # For the purpose of reproducibility

p_true <- 0.40 # true parameter (the estimand in theory)

n <- 10000 # sample size

# Simulate n=1000 voters: 1 = support, 0 = not support
support <- rbinom(n, size = 1, prob = p_true) #random binomial distribution (rbinom)

# Look at the first six simulated voters
head(support)

# Calculate the sample mean
mean(support)

# Now we would like to repeat the same process for 1000 times
R <- 1000 # 1000 draws (random sampling)

estimates <- replicate(
  R,
  mean(rbinom(n, size = 1, prob = p_true))
)

head(estimates) # the first six sample means

mean(estimates) # expected value of the sample mean

# Let's use a histogram to plot the distribution of sample means
hist(
  estimates,
  breaks = 150,
  main = "Sampling Distribution of the Sample Mean",
  xlab = "Estimated Support",
  cex.main = 0.8, cex.lab = 0.8, # customize label sizes
)

abline(v = p_true, lwd = 2, col="red")
abline(v = mean(estimates), lwd = 2, col="blue")



