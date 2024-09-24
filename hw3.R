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


pbinom(6, 10, 0.5, lower.tail = F)
pnbinom(q=3, size = 10, prob=0.5)
dnbinom(x=0, size = 3, prob=0.5)


## e
n_binom_likelihood <- function(k, n, p) {
  dnbinom(k, size = n, prob = p)
}

prob_T <- seq(0.01, 1, by = 0.01)
n_binom_likelihood_value <- n_binom_likelihood(k=10-3, n=3, p=prob_T)

max_n_binom_likelihood <- max(n_binom_likelihood_value)
normalized_n_binom_likelihood <- n_binom_likelihood_value / max_n_binom_likelihood  # normalize the likelihood

plot(prob_T, normalized_n_binom_likelihood, type = "l",
     xlab = "Probability of Tails", ylab = "Normalized Likelihood",
     main = "Likelihood of 3 Tails in 10 Flips")

abline(h = max(normalized_n_binom_likelihood), col = "blue")
abline(v = 0.5, col = "red")
abline(v = 0.3, col = "green")
points(0.3, 1, col = "darkgreen", pch = 19)



## f
p_lt_10 <- sum(dnbinom(x=0:(9-3), size = 3, prob = 0.5))
p_ge_10 <- 1-p_lt_10
