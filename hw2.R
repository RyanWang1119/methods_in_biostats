CMMS <- data.frame(score = seq(from=5, to=30, by=5),
                           no = c(0, 0, 3, 9, 16, 18),
                           yes = c(2, 1, 4, 5, 3, 1))  # raw data

cutoff <- c(1, 2, 3, 4, 5)
specificity <- c(1, 2, 3, 4, 5)
sensitivity <- c(1, 2, 3, 4, 5)

for (i in c(1:6)){
  cutoff[i] <- i*5
  specificity[i] <- sum(CMMS[(i+1):6,2])/sum(CMMS[1:6,2])
  sensitivity[i] <- sum(CMMS[1:i,3])/sum(CMMS[1:6,3])

}  # calculate the specificity and sensitivity

result_table <- data.frame(cutoff, sensitivity,
                           specificity,
                           one_minus_specificity = 1- specificity)

result_table[c(6,6), c("specificity", "one_minus_specificity")] <- list(0,1)

cutoff0 <- data.frame(
  cutoff = 0,
  sensitivity = 0,
  specificity = 1.00,
  one_minus_specificity = 0
)  # manually adding a row

result_table <- rbind(cutoff0, result_table)
result_table

## b
library(ggplot2)
ggplot(data = result_table, aes(x= one_minus_specificity, y= sensitivity)) +
  geom_line() + labs(x = "1 - Specificity (False Positive)",
                     y = "Sensitivity (True Positive)",
                     title = "ROC Curve for the Test")

## c

