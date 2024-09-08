library(ggplot2)

set.seed(98)
x_i <- runif(1000, min = 0, max = 1)  # simulate from the Unif[0,1]
y_i <- sqrt(-5*log(1-x_i))  # apply the inverse function to get Weibull distributed data

plot(y_i, ylab = "Values")  # simulated data
eq_1 <- quantile(y_i, seq(0.01, 0.99, length = 100))  # quantile of the simulated data

simulated_data <- data.frame(Quantile = seq(0.01, 0.99, length = 100), Value = eq_1)

theoretical_data <- data.frame(Quantile = seq(0.01, 0.99, length = 100),
                               Value = qweibull(p = seq(0.01, 0.99, length = 100), shape = 2, scale = sqrt(5)))

ggplot() +
  geom_line(data = simulated_data, aes(x = Quantile, y = Value), color = "blue", linetype = "dashed", size =1) +
  geom_line(data = theoretical_data, aes(x = Quantile, y = Value), color = "red", linetype = "solid", size =1) +
  labs(x = "Quantiles", y = "Values")

