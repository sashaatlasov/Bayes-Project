data {
  int<lower=0> n;         
  vector[n] x1; 
  vector[n] x2;
  vector[n] x3;
  int<lower=0,upper=1> y[n];            
}


parameters {
  real beta0;
  real beta1;
  real beta2;
  real beta3;
}

model {
  beta0 ~ normal(2, 7); 
  beta1 ~ normal(2, 7); 
  beta2 ~ normal(2, 7); 
  beta3 ~ normal(2, 7); 	
  y ~ bernoulli_logit(beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3);
}                                        
