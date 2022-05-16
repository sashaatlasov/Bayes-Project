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
data_example <- list(n = n, y = y, x1=x1, x2=x2, x3=x3)

model_1 <- stan_model(file = '/Users/sasaatlasov/Documents/R/Logit/logit_normal_diff.stan')
fit <- sampling(model_1, data=data_example,
                iter=2000, 
                chains=1,
                warmup=600)
summary(fit)

get_data <- function(n=1000){
  x0 <- rep(x = 1, times = n)
  x1 <- rchisq(n, df = 3)
  x2 <- rnorm(n, mean = 1, sd = 2)
  x3 <- rexp(n, 1)
  eps <- rlogis(n)
  true_coefs <- c(2, 3, -4, -6.5)
  X <- cbind(x0, x1, x2, x3)
  y <- as.numeric((X %*% true_coefs + eps > 0))
  data <- list(n = n, y = y, x1 = x1, x2 = x2, x3 = x3)
  return(data)
}

simulation <- function(file_path, iterations=100, n_objects=1000){
  pb <- txtProgressBar(min = 0, max = iterations, style = 3)
  means <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                      beta3 = numeric(0))
  coeffs <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                       beta3 = numeric(0))
  true_coefs_table <- data.frame(beta0 = rep(2, iterations), beta1 = rep(3, iterations),
                                 beta2 = rep(-4, iterations), beta3 = rep(-6.5, iterations))
  for(i in 1:iterations){
    data <- get_data(n=n_objects)
    data_ml <- data.frame(cbind(data$y, 
                                data$x1,
                                data$x2,
                                data$x3))
    colnames(data_ml) <- c('y', 'x1', 'x2', 'x3')
    data_ml <- data.frame(data_ml)
    
    m <- stan_model(file=file_path)
    model <- sampling(m, data=data,
                      iter=2000, 
                      chains=1, seed=10)
    model_2 <- glm(formula = y ~ ., family = binomial, data=data.frame(data_ml))
    
    coeffs[i, ] <- coefficients(model_2)
    means[i, ] <- c(get_posterior_mean(model))
    setTxtProgressBar(pb, i)
  }
  MAPE <- c(t(matrix(colMeans(abs((true_coefs_table - means) / true_coefs_table)))))
  RMSE <- c(t(matrix(sqrt(colMeans((true_coefs_table - means) ^ 2)))))
  
  MAPE_ml <- c(t(matrix(colMeans(abs((true_coefs_table - coeffs) / true_coefs_table)))))
  RMSE_ml <- c(t(matrix(sqrt(colMeans((true_coefs_table - coeffs) ^ 2)))))
  close(pb)
  return(list(bayes=matrix(c(MAPE, RMSE), nrow = 2), ml=matrix(c(MAPE_ml, RMSE_ml), nrow = 2)))
}

mape_results <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                           beta3 = numeric(0))
rmse_results <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                           beta3 = numeric(0))

mape_results_ml <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                           beta3 = numeric(0))
rmse_results_ml <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                           beta3 = numeric(0))

for(i in seq(50, 300, by=10)){
  simulations <- simulation(file_path = '/Users/sasaatlasov/Documents/R/Logit/logit_normal_diff.stan',                 iterations = 100, n_objects=i)
  mape_results <- rbind(mape_results, simulations$bayes[1, ])
  rmse_results <- rbind(rmse_results, simulations$bayes[2, ])
  
  mape_results_ml <- rbind(mape_results_ml, simulations$ml[1, ])
  rmse_results_ml <- rbind(rmse_results_ml, simulations$ml[2, ])
}

rmse_results_ml
rmse_results

colnames(rmse_results) <- c('beta0', 'beta1', 'beta2', 'beta3')
colnames(mape_results) <- c('beta0', 'beta1', 'beta2', 'beta3')

write.csv(mape_results, file='/Users/sasaatlasov/Documents/Jupyter/Байес/mape_100.csv')
write.csv(rmse_results, file='/Users/sasaatlasov/Documents/Jupyter/Байес/rmse_100.csv')
write.csv(mape_results_ml, file='/Users/sasaatlasov/Documents/Jupyter/Байес/mape_100_ml.csv')
write.csv(rmse_results_ml, file='/Users/sasaatlasov/Documents/Jupyter/Байес/rmse_100_ml.csv')
