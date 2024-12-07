```{r}

k <- 3 # Choose how many basis functions are associated to X

true_coefficients <- runif(k, min = 0.5, max = 5)

sigma_Z <- 1
sigma_U <- 1
sigma_X <- 1
sigma_Y <- 1
n <- 10000

epsilon_Z <- rnorm(n, mean = 0, sd = sigma_Z)
U <- rnorm(n, mean = 0, sd = sigma_U)
epsilon_X <- rnorm(n, mean = 0, sd = sigma_X)
epsilon_Y <- rnorm(n, mean = 0, sd = sigma_Y)

Z <- epsilon_Z
X <- Z^3 + U + epsilon_X

Y <- rowSums(sapply(1:k, function(i) true_coefficients[i] * X^i)) + U + epsilon_Y

data <- data.frame(Z = Z, X = X, Y = Y)

X <- data$X
Y <- data$Y
Z <- data$Z

A <- matrix(0, nrow = k, ncol = k)
b <- numeric(k)

for (i in 1:k) {
  for (j in 1:k) {
    A[i, j] <- sum(Z^i * X^j)
  }
  b[i] <- sum(Z^i * Y)
}

params <- solve(A, b)

cat("Estimated coefficients:\n")
print(params)

cat("True coefficients:\n")
print(true_coefficients)

```
