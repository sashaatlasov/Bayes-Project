#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
library("rstan")
rstan_options(auto_write = TRUE)

# Генерируем выборку из 
#нормального распределения
set.seed(123)                                              # для воспроизводимости
n <- 1000                                                  # объем выборки                      
mu <- 1                                                    # параметры распределения
sigma <- 2
x <- rlogis(n = n, location = mu, scale = sigma)                   # реализация выборки
# Формируем данные для модели
data <- list(x = x,                                        # real x[n]
             n = length(x))                                # int<lower=0> n

# Генерируем выборку из апостериорного
# распределения параметров mu и sigma
model <- stan(file = "/Users/sasaatlasov/Documents/R/normal.stan",
              data = data,                                 # входные данные
              chains = 1,                                  # количество выборок из апостериорного распределения
              iter = 2000)                                 # удвоенный объем выборки из
                                                           # апостериорного распределения
posterior <- extract(model)
posterior$mu                                               # выборка из апостериорного распределения mu
hist(posterior$mu, breaks = 15, main=paste('Example'))
posterior$sigma                                            # выборка из апостериорного распределения sigma
hist(posterior$sigma, breaks = 15, main=paste('Example'))

# Оценим параметр mu (с sigma по аналогии)
mean(posterior$mu)                                         # как математическое ожидание апостериорного распределения mu
median(posterior$mu)                                       # как медиану апостериорного распределения mu
mean(x)                                                    # при помощи метода максимального правдоподобия
# Описательные статистики по
# апостериорному распределению параметров
summary(model)


model_2 <- stan(file = "/Users/sasaatlasov/Documents/R/normal_log.stan",
              data = data,                                
              chains = 1,                                 
              iter = 2000)                                 
posterior_2 <- extract(model_2)
posterior_2$mu                                               
hist(posterior_2$mu, breaks = 15, main=paste('Logistic'))
posterior_2$sigma                                            
hist(posterior_2$sigma, breaks = 15, main=paste('Logistic'))
mean(posterior_2$mu)                                        
median(posterior_2$mu)                                       
mean(x)                                                 
summary(model_2)


model_3 <- stan(file = "/Users/sasaatlasov/Documents/R/normal_2.stan",
                data = data,                                
                chains = 1,                                 
                iter = 2000)                                 
posterior_3 <- extract(model_3)
posterior_3$mu                                               
hist(posterior_3$mu, breaks = 15, main=paste('Normal'))
posterior_3$sigma                                            
hist(posterior_3$sigma, breaks = 15, main=paste('Normal'))
mean(posterior_3$mu)                                        
median(posterior_3$mu)                                       
mean(x)                                                 
summary(model_3)



# Задания
# 1. Повторить этот пример для логистического распределения
#    Подсказка: достаточно заменить функцию normal_lpdf
# 2. Заменить априорное распределение параметров на нормальное
#    с нулевым математическим ожиданием и дисперсией, равной 30
#    Подсказка: досточно заменить функцию uniform_lpdf
# 3. Повторить пример для оценивания параметров двумерного
#    нормального распределения
#    Подсказка: https://mc-stan.org/docs/2_27/functions-reference/multivariate-normal-distribution.html