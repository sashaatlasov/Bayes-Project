data {
  int<lower=0> n;         
  int<lower=0> k;
  matrix[n, k] X;	
  int<lower=0,upper=1> y[n];            
}


parameters {
  vector[k] beta;
}

model {
  target += uniform_lpdf(beta | -8, 8);
  target += bernoulli_logit_lpmf(y | X * beta);
}    