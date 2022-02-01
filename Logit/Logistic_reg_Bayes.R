library("rstan")
library('Metrics')
rstan_options(auto_write = TRUE)

# Сначала попробуем оценить одну логистическую регрессию с априорными нормальными 
# распределениями для всех коэффициентов - N(2, 7).

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
data_example <- list(n = n, y = y, x0 = x0, x1 = x1, x2 = x2, x3 = x3)

model_1 <- stan(file = '/Users/sasaatlasov/Documents/R/logit_normal.stan',
                data=data_example,
                iter=4000, 
                chains=1,
                warmup=600)
summary(model_1)
plot(model_1)

# Создадим вспомогательные функции для генерации данных и сохранения симуляций
get_data <- function(n=1000){
  x0 <- rep(x = 1, times = n)
  x1 <- rchisq(n, df = 3)
  x2 <- rnorm(n, mean = 1, sd = 2)
  x3 <- rexp(n, 1)
  eps <- rlogis(n)
  true_coefs <- c(2, 3, -4, -6.5)
  X <- cbind(x0, x1, x2, x3)
  y <- as.numeric((X %*% true_coefs + eps > 0))
  data <- list(n = n, y = y, x0 = x0, x1 = x1, x2 = x2, x3 = x3)
  return(data)
}

# Заранее создадим две таблицы: для MAPE и RMSE
mape_results <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                           beta3 = numeric(0))
rmse_results <- data.frame(beta0 = numeric(0), beta1 = numeric(0), beta2 = numeric(0), 
                           beta3 = numeric(0))

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

# Проведем по 100 сиуляций на каждое априорное распределение и сохраним результаты
z <- simulation(file_path = '/Users/sasaatlasov/Documents/R/logit_normal.stan', 
                iterations = 100)
mape_results <- rbind(mape_results, z[1, ])
rmse_results <- rbind(rmse_results, z[2, ])

x <- simulation(file_path = '/Users/sasaatlasov/Documents/R/logit_logistic.stan', 
                iterations = 100)
mape_results <- rbind(mape_results, x[1, ])
rmse_results <- rbind(rmse_results, x[2, ])

c <- simulation(file_path = '/Users/sasaatlasov/Documents/R/logit_uniform.stan', 
                iterations = 100)
mape_results <- rbind(mape_results, c[1, ])
rmse_results <- rbind(rmse_results, c[2, ])

v <- simulation(file_path = '/Users/sasaatlasov/Documents/R/logit_student.stan', 
                iterations = 100)
mape_results <- rbind(mape_results, v[1, ])
rmse_results <- rbind(rmse_results, v[2, ])
mape_results <- mape_results * 100

colnames(rmse_results) <- c('beta0', 'beta1', 'beta2', 'beta3')
colnames(mape_results) <- c('beta0', 'beta1', 'beta2', 'beta3')
mape_results <- cbind(Prior=c('Normal', 'Logistic', 'Uniform', 'Student'), mape_results)
rmse_results <- cbind(Prior=c('Normal', 'Logistic', 'Uniform', 'Student'), rmse_results)

mape_results
rmse_results





