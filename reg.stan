
data{
  int N; //sample size
  int M; //number of independent variables(including bias)
  vector[N] y; //dependent variable
  matrix[N,M] x; //independent variables
}
parameters{
  vector[M] beta; //coefficient of independent variables
  real<lower=0> sigma;
}
model{
  beta ~ normal(0,100);
  sigma ~ cauchy(0,5);
  y ~ normal(x*beta,sigma);
}
