# birthday
n_student <- c(1:364)

for (i in n_student){
  prob_[i] <- 1-prod(365:(365-i))/(365^i)
  prob_
}


x=1:100
lambda=20
plot(x,dpois(x,lambda),type="p",pch=19,
     col="blue",lwd=3, xlab="Number of patients",
     ylab="Probability",cex.lab=1.5,cex.axis=1.5,
     col.axis="blue")

py=rep(0,100)

set.seed(29)
for (i in 1:100)
{py[i] <- sum(rpois(10000, lambda=20)==i)/10000 }
lines(py, col = "red", lwd=3)
