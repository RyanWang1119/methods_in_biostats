###### Q3 ######

# a
set.seed(929)
a_sample <- rexp(n=1000, rate=1)
a_mean <- mean(a_sample)
a_variance <- var(a_sample)
identical(a_variance, sum((a_sample-a_mean)^2)/(1000-1))

# b
cumulative_mean <- cumsum(a_sample)/(1:1000)
plot(1:1000, cumulative_mean, type="l", ylab = "Cumulative Mean")
abline(h=1)
