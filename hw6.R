
##### Q3 #####
calculate_log_odds <- function(x1, n1, x2, n2) {
  odd1 <- x1 / (n1 - x1)
  odd2 <- x2 / (n2 - x2)
  log_OR <- log(odd1/odd2)
  se <- sqrt(1/x1 + 1/(n1 - x1) + 1/x2 + 1/(n2 - x2))

  ci_lower <- log_OR - 1.96 * se
  ci_upper <- log_OR + 1.96 * se

  return(list(log_OR = log_OR, ci_lower = ci_lower, ci_upper = ci_upper))
}


simulate_coverage <- function(p1, p2, n1, n2, n_sim = 1000) {
  true_log_OR <- log((p1 / (1 - p1)) / (p2 / (1 - p2)))
  n <- 0

  for (i in 1:n_sim) {
    x1 <- rbinom(1, n1, p1)
    x2 <- rbinom(1, n2, p2)

    if (x1 == 0 || x1 == n1 || x2 == 0 || x2 == n2) {
      next
    }
    calculated_cover <- calculate_log_odds(x1, n1, x2, n2)

    if (calculated_cover$ci_lower <= true_log_OR && true_log_OR <= calculated_cover$ci_upper) {
      n <- n + 1
    }
  }
  coverage <- n / n_sim
  return(coverage)
}

combinations <- list(
  list(p1 = 0.1, p2 = 0.1, n1 = 100, n2 = 100),
  list(p1 = 0.1, p2 = 0.5, n1 = 100, n2 = 100),
  list(p1 = 0.1, p2 = 0.9, n1 = 100, n2 = 100),
  list(p1 = 0.5, p2 = 0.5, n1 = 100, n2 = 100),
  list(p1 = 0.5, p2 = 0.9, n1 = 100, n2 = 100),
  list(p1 = 0.9, p2 = 0.9, n1 = 100, n2 = 100)
)

result <- sapply(combinations, function(params) {
  simulate_coverage(params$p1, params$p2, params$n1, params$n2)
})

result_df <- data.frame(
  p1 = sapply(combinations, function(params) params$p1),
  p2 = sapply(combinations, function(params) params$p2),
  n1 = sapply(combinations, function(params) params$n1),
  n2 = sapply(combinations, function(params) params$n2),
  coverage = result)
print(result_df)

# No matter how the success probabilities of the two groups differ, the coverage of the confidence interval
# of the sample log odds ratios is close to the nominal 95% level.

##### Q4 #####

##### conditional test: Fisher's exact.
dat_tbl <- matrix(c(30,10,10,30), byrow = TRUE, ncol = 2)
fisher.test(dat_tbl, alternative = "greater", conf.level = 0.9)

##### unconditional test:
chisq.test(dat_tbl)  # Chi-squared test.
1-pnorm(4.47)  # Score test p -value.
1-pnorm(5.16)  # Wald's test p -value.

BarnardTest(dat_tbl, method="z-pooled", alternative = "greater")  # Barnard Test

##### Q5 #####
dat_tbl <- matrix(c(18,2,4,16), byrow = TRUE, ncol = 2)
fisher.test(dat_tbl, alternative = "two.sided")

qnorm(0.9)
