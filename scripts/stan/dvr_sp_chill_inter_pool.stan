// Stan model based off of Dan and Lizzie from Buds repo
// Started 9 October 2017 by Cat
///// Notes below by Dan and Lizzie with adjustments by Cat for rethinking paper
// Stan model following Meetup 2016-03-28
// Now with chilling levels as dummy variables for each level, two levels
// Including species as intercept
// Duration of Vegetative Risk as a function of species as modeled group level factors, and temperature, photoperiod, and chilling as unmodeled factors (experimental manipulation)

// Note by Lizzie: This model includes pooled intercepts
// Adding non-centered parameterization (NCP or ncp) on b_inter_sp to start 


data {
  int<lower=0> N;
  int<lower=0> n_sp;
  int<lower=1, upper=n_sp> sp[N];
  vector[N] lday;
  vector[N] warm;
  vector[N] photo;
  vector[N] chill1; 
  vector[N] chill2;
}

transformed data { 	      // 9 interaction terms
  vector[N] inter_wp;         // vector[N] inter_wp  = warm .* photo;
  vector[N] inter_wc1;           
  vector[N] inter_wc2;           
  vector[N] inter_pc1;           
  vector[N] inter_pc2;           

  inter_wp    = warm .* photo; 
  inter_wc1    = warm .* chill1;  
  inter_wc2    = warm .* chill2;  
  inter_pc1    = photo .* chill1;  
  inter_pc2    = photo .* chill2;

}

parameters {
  vector[n_sp] a_sp;
  vector[n_sp] b_warm;
  vector[n_sp] b_photo;
  vector[n_sp] b_chill1;
  vector[n_sp] b_chill2;

  vector[n_sp] b_inter_wp_ncp;
  vector[n_sp] b_inter_wc1_ncp;
  vector[n_sp] b_inter_wc2_ncp;
  vector[n_sp] b_inter_pc1_ncp;
  vector[n_sp] b_inter_pc2_ncp;

  real mu_a; 
  real mu_b_warm; 
  real mu_b_chill1;
  real mu_b_chill2;
  real mu_b_photo;

  real mu_b_inter_wp;
  real mu_b_inter_wc1;
  real mu_b_inter_wc2;
  real mu_b_inter_pc1;
  real mu_b_inter_pc2;
  real<lower=0> sigma_b_warm;
  real<lower=0> sigma_b_photo;
  real<lower=0> sigma_b_chill1;
  real<lower=0> sigma_b_chill2;

  real<lower=0> sigma_a;

  real<lower=0> sigma_b_inter_wp;
  real<lower=0> sigma_b_inter_wc1;
  real<lower=0> sigma_b_inter_wc2;
  real<lower=0> sigma_b_inter_pc1;
  real<lower=0> sigma_b_inter_pc2;
    
  real<lower=0> sigma_y; 
  }


transformed parameters { // Vectorize: Won't save time probably here (no scalar x vector)
  vector[n_sp] b_inter_wp;
  vector[n_sp] b_inter_wc1;
  vector[n_sp] b_inter_wc2;
  vector[n_sp] b_inter_pc1;
  vector[n_sp] b_inter_pc2;
  vector[N] y_hat; // Note to self: all these declarations must happen together!

  b_inter_wp = mu_b_inter_wp + sigma_b_inter_wp*b_inter_wp_ncp;
  b_inter_wc1 = mu_b_inter_wc1 + sigma_b_inter_wc1*b_inter_wc1_ncp;
  b_inter_wc2 = mu_b_inter_wc2 + sigma_b_inter_wc2*b_inter_wc2_ncp;
  b_inter_pc1 = mu_b_inter_pc1 + sigma_b_inter_pc1*b_inter_pc1_ncp;
  b_inter_pc2 = mu_b_inter_pc2 + sigma_b_inter_pc2*b_inter_pc2_ncp;
		
	for(i in 1:N){
		y_hat[i] = a_sp[sp[i]] + 
		b_warm[sp[i]] * warm[i] + 
		b_photo[sp[i]] * photo[i] + 
		b_chill1[sp[i]] * chill1[i] + 
		b_chill2[sp[i]] * chill2[i] +
		b_inter_wp[sp[i]] * inter_wp[i] +
		b_inter_wc1[sp[i]] * inter_wc1[i] +
		b_inter_wc2[sp[i]] * inter_wc2[i] +
		b_inter_pc1[sp[i]] * inter_pc1[i] +
		b_inter_pc2[sp[i]] * inter_pc2[i]
		;
				
		}
	
}

model {
	// Priors //
	mu_b_warm ~ normal(0, 15); // 100 = 3 months on either side. Narrow down to 35
	mu_b_photo ~ normal(0, 15);
	mu_b_chill1 ~ normal(0, 15);
	mu_b_chill2 ~ normal(0, 15);

/*	mu_b_inter_wp ~ normal(0, 35); // Delete because all in NCP now
	mu_b_inter_ws ~ normal(0, 35);
	mu_b_inter_ps ~ normal(0, 35);
	mu_b_inter_wc1 ~ normal(0, 35);	
	mu_b_inter_wc2 ~ normal(0, 35);	
	mu_b_inter_pc1 ~ normal(0, 35);	
	mu_b_inter_pc2 ~ normal(0, 35);	
	mu_b_inter_sc1 ~ normal(0, 35);	
	mu_b_inter_sc2 ~ normal(0, 35);	*/
	
	sigma_b_warm ~ normal(0, 10); // Start big at 10, go smaller if introduces problems
	sigma_b_photo ~ normal(0, 10); 
	sigma_b_chill1 ~ normal(0, 10);
	sigma_b_chill2 ~ normal(0, 10);

	sigma_b_inter_wp ~ normal(0, 10);
	sigma_b_inter_wc1 ~ normal(0, 10);	
	sigma_b_inter_wc2 ~ normal(0, 10);	
	sigma_b_inter_pc1 ~ normal(0, 10);	
	sigma_b_inter_pc2 ~ normal(0, 10);

	a_sp ~ normal(mu_a, sigma_a);  
	
	b_warm ~ normal(mu_b_warm, sigma_b_warm);
	b_photo ~ normal(mu_b_photo, sigma_b_photo);
	b_chill1 ~ normal(mu_b_chill1, sigma_b_chill1);
	b_chill2 ~ normal(mu_b_chill2, sigma_b_chill2);

/*	b_inter_wp ~ normal(mu_b_inter_wp, sigma_b_inter_wp); // Delete because all in NCP now
	b_inter_ws ~ normal(mu_b_inter_ws, sigma_b_inter_ws);
	b_inter_ps ~ normal(mu_b_inter_ps, sigma_b_inter_ps);
	b_inter_wc1 ~ normal(mu_b_inter_wc1, sigma_b_inter_wc1);		
	b_inter_wc2 ~ normal(mu_b_inter_wc2, sigma_b_inter_wc2);
	b_inter_pc1 ~ normal(mu_b_inter_pc1, sigma_b_inter_pc1);		
	b_inter_pc2 ~ normal(mu_b_inter_pc2, sigma_b_inter_pc2);
	b_inter_sc1 ~ normal(mu_b_inter_sc1, sigma_b_inter_sc1);		
	b_inter_sc2 ~ normal(mu_b_inter_sc2, sigma_b_inter_sc2); */
        b_inter_wp_ncp ~ normal(0, 15);
	b_inter_wc1_ncp ~ normal(0, 15);		
	b_inter_wc2_ncp ~ normal(0, 15);
	b_inter_pc1_ncp ~ normal(0, 15);		
	b_inter_pc2_ncp ~ normal(0, 15);

	
	dvr ~ normal(y_hat, sigma_y);

}

