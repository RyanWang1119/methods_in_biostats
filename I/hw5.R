p_value <- pt(q = 10, df = 15, lower.tail = FALSE)
p_value

power.t.test(n = 100, delta = 0.01, sd = 0.04, sig.level = 0.05, type = "one.sample",
             alternative = "one.sided")$power
pnorm(-0.855, lower.tail = F)

library(stats)
lambda_true <- 0.2
n <- 10
nsim <- 10000
alpha <- 0.05

set.seed(112)

lower_bounds <- numeric(nsim)
upper_bounds <- numeric(nsim)


#### Exact approach ####
for (i in 1:nsim) {
  sample <- rexp(n, rate = lambda_true)

  y_bar <- mean(sample)

  # CI
  lower_bounds[i] <-  qchisq(alpha/2, 2*n) / (2*n*y_bar)
  upper_bounds[i] <-  qchisq(1 - alpha/2, 2*n) / (2*n*y_bar)

}


coverage <- mean((lower_bounds < lambda_true) & (lambda_true < upper_bounds))
cat("Coverage of 95% confidence intervals (Exact approach):", coverage * 100, "%\n")

#### CLT approach ####
for (i in 1:nsim) {
  sample <- rexp(n, rate = lambda_true)

  y_bar <- mean(sample)

  # CI
  lower_bounds[i] <-  (qnorm(alpha/2)+sqrt(n)) / (y_bar*sqrt(n))
  upper_bounds[i] <-  (qnorm(1-alpha/2)+sqrt(n)) / (y_bar*sqrt(n))

}


coverage <- mean((lower_bounds < lambda_true) & (lambda_true < upper_bounds))
cat("Coverage of 95% confidence intervals (CLT approach):", coverage * 100, "%\n")

