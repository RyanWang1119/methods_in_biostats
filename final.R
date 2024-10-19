binorm <- function(mu1, mu2, sigma, rho, n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rho*x1 + sqrt(1 - rho^2)*x2
  y1 <- mu1 + sigma*x1
  y2 <- mu2 + sigma*x3
  ymat <- cbind(y1, y2)
  return(ymat)
}

y <- binorm(130, 130, 25, 0.35, 20)
y



mu1 <- 130
mu2 <- 105
sigma <- 25
rho <- 0.4
n <- 20
alpha <- 0.025
nsim <- 10000


paired_t_test <- function(data) {
  t.test(data[, 1], data[, 2], paired = T, alternative = "greater", conf.level = 1 - alpha)
}

two_sample_t_test <- function(data) {
  t.test(data[, 1], data[, 2], paired = F, alternative = "greater", conf.level = 1 - alpha, var.equal = T)
}

#### Simulation ####
w_simulation <- function(mu1, mu2, sigma, rho, n, alpha) {
  reject_paired <- 0
  coverage_paired <- 0
  reject_two_sample <- 0
  coverage_two_sample <- 0

for (i in 1:10000) {
    data <- binorm(mu1, mu2, sigma, rho, n)  # bivariate normal samples

    # Paired
    paired_test <- paired_t_test(data)
    if (paired_test$p.value < alpha) {
      reject_paired <- reject_paired + 1  # reject if p_value less than alpha
    }
    if (paired_test$conf.int[1] < (mu1-mu2) & paired_test$conf.int[2] > (mu1-mu2)) {
      coverage_paired <- coverage_paired + 1  # CI cover the true mean
    }

    # Two-sample
    two_sample_test <- two_sample_t_test(data)
    if (two_sample_test$p.value < alpha) {
      reject_two_sample <- reject_two_sample + 1
    }
    if (two_sample_test$conf.int[1] < (mu1-mu2) & two_sample_test$conf.int[2] > (mu1-mu2)) {
      coverage_two_sample <- coverage_two_sample + 1
    }
  }
  df <- data.frame(
    paired = c(reject_paired, coverage_paired),
    two_Sample = c(reject_two_sample, coverage_two_sample),
    row.names = c("Number of Rejection", "Number of Coverage")
  )

  return(df)
}

library(dplyr)

# Scenario (i)
# H_0: mu1 = mu2
# truth : mu1 = mu2 = 130, null is true.
# type I error = rate of falsely rejecting the null = proportion of rejection
s_i<- w_simulation(mu1=130, mu2=130, sigma=25, rho=0.4, n=20, alpha=0.025)
print(s_i)

type_1_error_i <- data.frame(
  (s_i[1,]/10000), row.names = "Type 1 Error")

coverage_i <- data.frame(
  (s_i[2,]/10000), row.names = "Coverage")

scenario_i <- bind_rows(s_i, type_1_error_i, coverage_i)
print(scenario_i)


# Scenario (i)
# H_0: mu1 = mu2
# truth : mu1 = 130 != mu2 = 105, null is false.
# Power = rate of correctly rejecting the null = proportion of rejection
s_ii<- w_simulation(mu1=130, mu2=105, sigma=25, rho=0.4, n=20, alpha=0.025)
print(s_ii)

power_ii <- data.frame(
  (s_ii[1,]/10000), row.names = "Power")

coverage_ii <- data.frame(
  (s_ii[2,]/10000), row.names = "Coverage")

scenario_ii <- bind_rows(s_ii, power_ii, coverage_ii)
print(scenario_ii)
