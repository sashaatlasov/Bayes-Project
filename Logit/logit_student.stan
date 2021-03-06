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
  target += student_t_lpdf(beta | 20, 0, 7);
  target += bernoulli_logit_lpmf(y | X * beta);
}    