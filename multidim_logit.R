library("rstan")
library('Metrics')
rstan_options(auto_write = TRUE)

set.seed(10)
n <- 1000
x0 <- rep(x = 1, times = n)
x1 <- rchisq(n, df = 3)
x2 <- rnorm(n, mean = 1, sd = 2)
x3 <- rexp(n, 1)
eps <- rlogis(n)

true_coefs <- c(2, 3, -4, -6.5) 
X <- cbind(x0, x1, x2, x3) 
y <- as.numeric((X %*% true_coefs + eps > 0))
mean(y)

data <- list(n = n, k=length(true_coefs), X = X, y = y)

model <- stan(file = '/Users/sasaatlasov/Documents/R/logit_matrix.stan',
              data=data,
              iter=10000, 
              chains=1,
              warmup=1000)
summary(model)

get_data <- function(n=1000){
  x0 <- rep(x = 1, times = n)
  x1 <- rchisq(n, df = 3)
  x2 <- rnorm(n, mean = 1, sd = 2)
  x3 <- rexp(n, 1)
  eps <- rlogis(n)
  true_coefs <- c(2, 3, -4, -6.5)
  X <- cbind(x0, x1, x2, x3)
  y <- as.numeric((X %*% true_coefs + eps > 0))
  data <- list(n = n, k=length(true_coefs), X = X, y = y)
  return(data)
}

simulation <- function(file_path, iterations=100){
  pb <- txtProgressBar(min = 0, max = iterations, style = 3)
  means <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                      beta3 = numeric(0))
  true_coefs_table <- data.frame(beta0 = rep(2, iterations), beta1 = rep(3, iterations),
                                 beta2 = rep(-4, iterations), beta3 = rep(-6.5, iterations))
  for(i in 1:iterations){
    data <- get_data()
    model <- stan(file = file_path,
                  data=data,
                  iter=4000, 
                  chains=1, seed=10)
    means[i, ] <- c(get_posterior_mean(model))
    setTxtProgressBar(pb, i)
  }
  MAPE <- c(t(matrix(colMeans(abs((true_coefs_table - means) / true_coefs_table)))))
  RMSE <- c(t(matrix(colMeans(sqrt((true_coefs_table - means) ^ 2)))))
  close(pb)
  return(matrix(c(MAPE, RMSE), nrow = 2))
}

mape_results <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                           beta3 = numeric(0))
rmse_results <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                           beta3 = numeric(0))

z <- simulation(file_path = '/Users/sasaatlasov/Documents/R/logit_matrix.stan', 
                iterations = 100)
mape_results <- rbind(mape_results, z[1, ])
rmse_results <- rbind(rmse_results, z[2, ])

colnames(rmse_results) <- c('beta0', 'beta1', 'beta2', 'beta3')
colnames(mape_results) <- c('beta0', 'beta1', 'beta2', 'beta3')

mape_results <- cbind(Metric='MAPE',Prior=c('Normal'), mape_results)
rmse_results <- cbind(Metric='RMSE', Prior=c('Normal'), rmse_results)

mape_results
rmse_results

res <- rbind(mape_results, rmse_results)
res

