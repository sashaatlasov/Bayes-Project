data {
  int<lower=0> n;
  int<lower=0,upper=1> y[n];
  vector[n] x1;	
  vector[n] x2;	
  vector[n] x3;	
}

parameters {
  real beta1;
  real beta2;
  real beta3;
  real beta4;
}

model {
  beta1 ~ normal(2, 5);
  beta2 ~ normal(3, 5);
  beta3 ~ normal(-4, 5);
  beta4 ~ normal(-6.5, 5);
  y ~ bernoulli_logit(beta1 + beta2 * x1 + beta3 * x2 + beta4 * x3);
}
