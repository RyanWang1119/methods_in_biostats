p_hat <- 140/348
p <- 4/12

z_test <- (p_hat - p)/sqrt(p*(1-p)/348)
p_value <- pnorm(z_test, lower.tail = F)

e <- c(z_test, p_value)
names(e) <- c("test statistics", "p value")
print(e)


set.seed(1116)
sim <- 10000

sample_size <- list(small=10, big=500)  # two sample sizes

dist <- list(
  thick = function(n){rt(n, df=3)},
  skewed = function(n){rchisq(n, df=1)},
  normal = function(n){rnorm(n, mean = 0, sd=1)}
  )  # three distributions

result <- list()

for (i in names(sample_size)){
  n <- sample_size[[i]]
  for (j in names(dist)){
    distribution <- dist[[j]]

    p_v <- replicate(sim,{
      x<-distribution(n)  # sample x
      y<-distribution(n)  # sample y
      t.test(x,y, var.equal = T)$p.value
      })

    prop_less <- mean(p_v<=0.05)
    result[[paste(i, j)]] <- prop_less
  }
}

result_table <- data.frame(
  Proportion = unlist(result)
)
print(result_table)

