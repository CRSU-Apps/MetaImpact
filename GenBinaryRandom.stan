// Generate new study based on existing meta-analysis of binary data under random-effects model //

// Input data for model
data {
  int<lower=1> Num;                 // # of studies in current meta-analysis
  int<lower=0> rc[Num];             // # of events in control group of each study
  int<lower=0> nc[Num];             // # of people in control group of each study
  int<lower=0> rt[Num];             // # of events in treatment group of each study
  int<lower=0> nt[Num];             // # of people in treatment group of each study
  int<lower=1> sizeNew;             // size of new study
  real<lower=0,upper=1> pcNew;      // event rate in control group of new study 
}

// The (incl. transformed) parameters accepted by the model. 
parameters {
  real mu[Num];                     // transformed baseline effect for each study
  real delta[Num];                  // transformed effect for each study
  real d;                           // common-effect on transformed scale
  real<lower=0> sigma;              // between-study SD                       NEED TO CHECK WHETHER WANT THIS (means I can't have exactly 0)
  real deltaNew;                    // study effect for new study on transformed scale
}

transformed parameters {
  // common effect
  real OR = exp(d);  
  // event rates transformed to be on logit scale
  real<lower=0,upper=1> pc[Num] = inv_logit(mu);
  real<lower=0,upper=1> pt[Num];
  for (i in 1:Num) pt[i] = inv_logit(mu[i] + delta[i]);
           
}

model {
  // Likelihood
  rc ~ binomial(nc, pc);
  rt ~ binomial(nt, pt);
  // Priors
  mu ~ normal(0,100);    // Vague priors
  delta ~ normal(d, sigma);                 // random-effects model with common effect
  d ~ normal(0, 1000);
  sigma ~ cauchy(0,0.5);
  deltaNew ~ normal(d, sigma);
}

generated quantities {
  // Values of new study based on from meta-analysis
  real ORNew = exp(deltaNew);
  real<lower=0,upper=1> ptNew = ((pcNew / (1 - pcNew)) * ORNew) / (1 + (pcNew / (1 - pcNew)) * ORNew);
  int<lower=0> rcNew = binomial_rng(sizeNew, pcNew);
  int<lower=0> rtNew = binomial_rng(sizeNew, ptNew);
}

