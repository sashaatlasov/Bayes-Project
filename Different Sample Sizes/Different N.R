library("rstan")
library('Metrics')
rstan_options(auto_write = TRUE)

set.seed(10000)
n <- 1000

x0 <- rep(x = 1, times = n)
x1 <- rnorm(n, mean=0.8, sd=1)
x2 <- rnorm(n, mean=1/2, sd=1)
eps <- rlogis(n, loc=0, scale=1)
true_coefs <- c(1, -3, 2)
X <- cbind(x0, x1, x2)
y <- as.numeric((X %*% true_coefs + eps > 0))
mean(y)
data_example <- list(n = n, y = y, x1=x1, x2=x2)

model_1 <- stan_model(file = '/Users/sasaatlasov/Documents/R/Logit/logit_normal_diff.stan')
fit <- sampling(model_1, data=data_example,
                iter=2000, 
                chains=1,
                warmup=600, verbose=FALSE)
summary(fit)

get_data <- function(n=1000){
  x0 <- rep(x = 1, times = n)
  x1 <- rnorm(n, mean=0.8, sd=1)
  x2 <- rnorm(n, mean=1/2, sd=1)
  eps <- rlogis(n, loc=0, scale=1)
  true_coefs <- c(1, -3, 2)
  X <- cbind(x0, x1, x2)
  y <- as.numeric((X %*% true_coefs + eps > 0))
  data <- list(n = n, y = y, x1 = x1, x2 = x2)
  return(data)
}

simulation <- function(file_path, iterations=100, n_objects=1000){
  pb <- txtProgressBar(min = 0, max = iterations, style = 3)
  means <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0))
  coeffs <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0))
  true_coefs_table <- data.frame(beta0 = rep(1, iterations), beta1 = rep(-3, iterations),
                                 beta2 = rep(2, iterations))
  for(i in 1:iterations){
    data <- get_data(n=n_objects)
    data_ml <- data.frame(cbind(data$y, 
                                data$x1,
                                data$x2))
    colnames(data_ml) <- c('y', 'x1', 'x2')
    data_ml <- data.frame(data_ml)
    
    m <- stan_model(file=file_path)
    model <- sampling(m, data=data,
                      iter=2000, open_progress=FALSE, verbose=FALSE,
                      chains=1, seed=10)
    model_2 <- glm(formula = y ~ ., family = binomial(link = "logit"), 
                   data=data.frame(data_ml), control = glm.control(maxit = 100))
    
    coeffs[i, ] <- coefficients(model_2)
    means[i, ] <- c(get_posterior_mean(model))
    setTxtProgressBar(pb, i)
  }
  MAPE <- matrix(colMeans(abs((true_coefs_table - means) / true_coefs_table)))
  RMSE <- matrix(sqrt(colMeans((true_coefs_table - means) ^ 2)))
  
  MAPE_ml <- t(matrix(colMeans(abs((true_coefs_table - coeffs) / true_coefs_table))))
  RMSE_ml <- t(matrix(sqrt(colMeans((true_coefs_table - coeffs) ^ 2))))
  close(pb)
  return(list(bayes=matrix(c(MAPE, RMSE), nrow = 2), ml=matrix(c(MAPE_ml, RMSE_ml), nrow = 2)))
}

mape_results <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0))
rmse_results <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0))

mape_results_ml <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0))
rmse_results_ml <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0))

for(i in seq(50, 300, by=10)){
  simulations <- simulation(file_path = '/Users/sasaatlasov/Documents/R/Logit/logit_normal_diff.stan',  iterations = 100, n_objects=i)
  mape_results <- rbind(mape_results, simulations$bayes[1, ])
  rmse_results <- rbind(rmse_results, simulations$bayes[2, ])
  
  mape_results_ml <- rbind(mape_results_ml, simulations$ml[1, ])
  rmse_results_ml <- rbind(rmse_results_ml, simulations$ml[2, ])
}
rmse_results_ml
mape_results_ml
rmse_results
mape_results

colnames(rmse_results) <- c('beta0', 'beta1', 'beta2')
colnames(mape_results) <- c('beta0', 'beta1', 'beta2')
colnames(rmse_results_ml) <- c('beta0', 'beta1', 'beta2')
colnames(mape_results_ml) <- c('beta0', 'beta1', 'beta2')

write.csv(mape_results, file='/Users/sasaatlasov/Documents/Jupyter/Байес/mape_5.csv')
write.csv(rmse_results, file='/Users/sasaatlasov/Documents/Jupyter/Байес/rmse_5.csv')
write.csv(mape_results_ml, file='/Users/sasaatlasov/Documents/Jupyter/Байес/mape_5_ml.csv')
write.csv(rmse_results_ml, file='/Users/sasaatlasov/Documents/Jupyter/Байес/rmse_5_ml.csv')

