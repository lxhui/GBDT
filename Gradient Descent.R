rm(list = ls())
x1 = rnorm(200, 0.1, 1)
x2 = rnorm(200, 0.5, 1)
x3 = rnorm(200, 1, 1)
X = matrix(c(x1, x2, x3), 200, 3, byrow = FALSE)
beta = c(0.3, 0.5, 0.7)
dim(beta) = c(3, 1)
Y = X %*% beta + rnorm(200, 0, 0.1)
lambda = 1e-04
epsilion = 1e-10
beta_hat_old = matrix(0, 3, 1)
i = 1
while (1) {
    message(i)
    i = i + 1
    J_old = norm(Y - X %*% beta_hat_old, type = "F")^2/(2 * 200)
    grad = (t(X) %*% X %*% beta_hat_old - t(X) %*% Y)/200
    if (norm(grad, type = "F") < epsilion) {
        break
    } else {
        beta_hat_new = beta_hat_old - lambda * grad
        J_new = norm(Y - X %*% beta_hat_new, type = "F")^2/(2 * 200)
        if (abs(J_new - J_old) < epsilion || norm(beta_hat_new - beta_hat_old, type = "F") < 
            epsilion) {
            beta_hat = beta_hat_new
            print(beta_hat)
            break
        } else {
            beta_hat_old = beta_hat_new
        }
    }
}
