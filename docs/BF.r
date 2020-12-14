halfCauchyPrior <- function(x,scale,bin_width) {
  # if (x<bin_width/2) {
  #   return(0)
  # } else {
    return((pcauchy(x+bin_width/2,scale=scale)-
                pcauchy(x-bin_width/2,scale=scale)))
  # }
}

BF <- function (t,N,scale=0.65,bin_width=0.001) {
  
  likelihood_h0 <- dt(t,N-1);
  
  # likelihood_h1 <- 0;
  # normalizing_factor <- 0;
  # 
  # for (mu in seq(-300,300,bin_width)) {
  #   likelihood_h1 <- likelihood_h1+ 
  #     dt(t-mu,N-1)*(dcauchy(mu,scale))
  #   normalizing_factor <- normalizing_factor+dcauchy(mu,scale);
  # }
  # likelihood_h1 <- likelihood_h1/normalizing_factor;
  
  h1_samples <- c();
  
  for (mu in seq(1,100)) {
    mu <- rcauchy(1,scale=scale);
    h1_samples <- c(h1_samples,
                    dt(t-mu,N-1))
  };
  likelihood_h1 = mean(h1_samples);
  
  return(likelihood_h1/likelihood_h0)
    
}

