###### Q1 ######

## b
binom_likelihood <- function(k, n, p) {
  dbinom(k, size = n, prob = p)
}  # likelihood

prob_H <- seq(0, 1, by = 0.01)
binom_likelihood_value <- binom_likelihood(k=7, n=10, p=prob_H)

max_binom_likelihood <- max(binom_likelihood_value)
normalized_binom_likelihood <- binom_likelihood_value / max_binom_likelihood  # normalize the likelihood

plot(prob_H, normalized_binom_likelihood, type = "l",
     xlab = "Probability of Heads", ylab = "Normalized Likelihood",
     main = "Likelihood of 7 Heads in 10 Flips")

abline(h = max(normalized_binom_likelihood), col = "blue")
abline(v = 0.5, col = "red")
abline(v = 0.7, col = "green")
points(0.7, 1, col = "darkgreen", pch = 19)

# The likelihood does not suggest that the coin is fair.


## e
n_binom_likelihood <- function(k, n, p) {
  dnbinom(k, size = n, prob = p)
}

prob_T <- seq(0.01, 1, by = 0.01)
n_binom_likelihood_value <- n_binom_likelihood(k=3, n=10-3, p=prob_T)

max_n_binom_likelihood <- max(n_binom_likelihood_value)
normalized_n_binom_likelihood <- n_binom_likelihood_value / max_n_binom_likelihood  # normalize the likelihood

plot(prob_T, normalized_n_binom_likelihood, type = "l",
     xlab = "1- Probability of Tails", ylab = "Normalized Likelihood",
     main = "Likelihood of 3 Tails in 10 Flips")

abline(h = max(normalized_n_binom_likelihood), col = "blue")
abline(v = 0.5, col = "red")
abline(v = 0.7, col = "green")
points(0.7, 1, col = "darkgreen", pch = 19)

# The likelihood does not suggest that the coin is fair.

## f
p_lt_10 <- sum(dnbinom(x=0:(9-3), size = 3, prob = 0.5))
p_ge_10 <- 1-p_lt_10


###### Q3 ######

## a
set.seed(929)
a_sample <- rexp(n=1000, rate=1)
a_mean <- mean(a_sample)
a_variance <- var(a_sample)
identical(a_variance, sum((a_sample-a_mean)^2)/(1000-1))

# S

## b
cumulative_mean <- cumsum(a_sample)/(1:1000)
plot(1:1000, cumulative_mean, type="l", ylab = "Cumulative Mean")
abline(h=1)

# Since the law of large number states that the sample mean is consistent
