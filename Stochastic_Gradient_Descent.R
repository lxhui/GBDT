rm(list = ls())
x1 = rnorm(200, 0.1, 1)
x2 = rnorm(200, 0.5, 1)
x3 = rnorm(200, 1, 1)
X_F = matrix(c(x1, x2, x3), 200, 3, byrow = FALSE)
beta = c(0.1, 0.6, 0.7)
dim(beta) = c(3, 1)
Y_F = X_F %*% beta + rnorm(200, 0, 0.1)
lambda = 1e-04
epsilion = 1e-10
beta_hat = matrix(0, 3, 1)
j = 1
while (1) {
    message(j)
    j = j + 1
    for (i in 1:200) {
        Y = Y_F[i, , drop = FALSE]
        X = X_F[i, , drop = FALSE]
        grad = (t(X) %*% X %*% beta_hat - t(X) %*% Y)/200
        if (norm(grad, type = "F") < epsilion) {
            break
        } else {
            beta_hat = beta_hat - lambda * grad
        }
    }
    if (i < 200) {
        break
    }
}

