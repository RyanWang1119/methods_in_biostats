p_hat <- 140/348
p <- 4/12

z_test <- (p_hat - p)/sqrt(p*(1-p)/348)
p_value <- pnorm(z_test, lower.tail = F)

e <- c(z_test, p_value)
names(e) <- c("test statistics", "p value")
print(e)



