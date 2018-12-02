

## Slide 5

library(MASS)
head(cats)


## Slide 6

boxplot(cats$Hwt ~ cats$Sex, 
        main = "Male and Female Cat Heart Weights")

## Slide 7

plot(density(cats$Hwt[cats$Sex == "F"]), col = "red", 
     xlim = c(4, 18), main = "Male and Female Cat Heart Weights")
lines(density(cats$Hwt[cats$Sex == "M"]), col = "blue")

## Slide 8

girlcats <- cats$Sex == "F"
t.test(cats$Hwt[girlcats], cats$Hwt[!girlcats])



## Slide 14

girlcats <- cats$Sex == "F"
Dhat     <- mean(cats$Hwt[girlcats])-mean(cats$Hwt[!girlcats])
nf   <- sum(girlcats); nm <- sum(!girlcats)
P    <- 10000
sample_diffs <- rep(NA, P)

for (i in 1:P) {

  ###################
  ## Add Code here ##
  ###################
  
}

pval <- mean(abs(sample_diffs) >= abs(Dhat))
pval





## Slide 15

girlcats <- cats$Sex == "F"
Dhat     <- mean(cats$Hwt[girlcats])-mean(cats$Hwt[!girlcats])
nf   <- sum(girlcats); nm <- sum(!girlcats)
P    <- 10000
sample_diffs <- rep(NA, P)

for (i in 1:P) {
  
  perm_data <- cats$Hwt[sample(1:(nf+nm))]
  meanf     <- mean(perm_data[1:nf])
  meanm     <- mean(perm_data[-(1:nf)])
  sample_diffs[i] <- meanf - meanm
}

pval <- mean(abs(sample_diffs) >= abs(Dhat))
pval





