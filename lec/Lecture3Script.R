

####################################
######  Begin Document 
####################################


## Slide 5 -------------------------------
## Check yourself
# part 1
sum(iris[iris$Species=="versicolor","Petal.Width"]<=1.2)

# part 2
mean(iris[iris$Species=="setosa","Petal.Width"])

# part 3
tapply(iris$Petal.Width,iris$Species,mean)

# part 4
table(iris[iris$Sepal.Width>3,"Species"])

# part 5
iris$Versicolor <- ifelse(iris$Species=="versicolor",1,0)  


## Slide 6 -------------------------------

# plot 1
hist(iris$Sepal.Width, xlab = "Iris Sepal Width", main = "Iris Dataset")

# plot 2
plot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)
legend("topright",legend=unique(iris$Species),col = 1:length(unique(iris$Species)), pch =1)

# Or plot 2
plot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)
legend("topright",legend=unique(iris$Species),fill = 1:length(unique(iris$Species)))

# plot 3
boxplot(iris$Sepal.Length~iris$Species,xlab = "Iris Species", xlab = "Iris Petal Width", main = "Iris Dataset")




## Slide 21 -------------------------------

setwd("~/Desktop/Data")
Grocery <- read.table("Kutner_6_9.txt", header=T)
head(Grocery)

# Construct design matrix
X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3)

## Slide 22 -------------------------------

beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% Grocery$Y
round(t(beta_hat), 2)


## Slide 24 -------------------------------

lm0 <- lm(Y ~ X1 + X2 + X3, data = Grocery)
lm0


## Slide 26 -------------------------------

lm0 <- lm(Y ~ X1 + X2 + X3, data = Grocery)
residuals(lm0)[1:5] 
fitted(lm0)[1:5]

## Slide 27 -------------------------------

summary(lm0)


## Slide 30 -------------------------------

A <- cbind(c(3,-2,7),c(-2,12,9),c(8,-16,5))
qr(A)$rank

## Slides 31-32 -------------------------------

# Define X_4
X4 <- ifelse(Grocery$X3==1,0,1)

# Define redundant design matrix 
X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3,X4)
head(X)

# Rank, Determinant, Inverse 
qr(t(X) %*% X)$rank
det(t(X) %*% X)
solve((t(X) %*% X))

# Linear model
lm(Y~X1+X2+X3+X4,data=Grocery)

# Define design matrix 
X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3)
head(X)

# Rank, Determinant, Inverse 
qr(t(X) %*% X)$rank
det(t(X) %*% X)
solve((t(X) %*% X))

# Linear model
lm(Y~X1+X2+X3,data=Grocery)

######################################################
# Bootstrap
######################################################


## Slide 38 -------------------------------

set.seed(1)
mu = 0

n <- 100

vec <- rnorm(n, mean = mu)

head(vec)

mean(vec)


## Slide 39 -------------------------------


B <- 1000
estimates <- vector(length = B)
for (b in 1:B) {
  new_sample <- sample(vec, size = n, replace = TRUE)
  estimates[b] <- mean(new_sample)
}
head(estimates)


## Slide 42 -------------------------------


var(estimates)


## Slide 43 -------------------------------
# Extended examples

L <- 2*mean(vec)-quantile(estimates,.975);L
U <- 2*mean(vec)-quantile(estimates,.025);U


## Slide 46 -------------------------------
# Extended examples

L <- quantile(estimates,.025);L 
U <- quantile(estimates,.975);U

