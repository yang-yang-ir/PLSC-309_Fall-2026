#### PLSC 309 R Lab Session
## Date: 09-10-2026
## Confidence Interval

# Set a random seed
set.seed(1234)

# True population support
p_true <- 0.40

# Sample size
n <- 1000

# Number of repeated samples
R <- 100

# Store results
estimate <- numeric(R)
lower <- numeric(R)
upper <- numeric(R)

# Repeated sampling
for (r in 1:R) {
  
  # Draw a random sample
  support <- rbinom(n, size = 1, prob = p_true)
  
  # Point estimate
  estimate[r] <- mean(support)
  
  # Standard error
  se <- sqrt(estimate[r] * (1 - estimate[r]) / n)
  
  # 95% confidence interval
  lower[r] <- estimate[r] - 1.96 * se
  upper[r] <- estimate[r] + 1.96 * se
}

# Does each CI contain the true value?
covered <- lower <= p_true & upper >= p_true


# Show all the 100 confidence intervals
show <- 1:100

cols <- ifelse(covered[show], "black", "red")

plot(
  estimate[show], show,
  xlim = range(c(lower[show], upper[show], p_true)),
  pch = 19,
  col = cols,
  xlab = "Candidate Support",
  ylab = "Sample",
  main = "95% Confidence Intervals from Repeated Samples",
  cex.main = 0.9,
  cex.lab = 0.8,
  cex.axis = 0.7
)

segments(
  lower[show], show,
  upper[show], show,
  col = cols
)

abline(v = p_true, lty = 2, lwd = 2)

