data{
	//Day attributes
	int<lower=0> D; //number of fishing days (sampling frame)
	int<lower=0> G; //number of unique gear/angler types 
	int<lower=0> S; //number of river sections
	int<lower=0> H; //max number of angler effort counts within a sample day across entire sampling frame (max countnum)
	int<lower=0> P_n; //number of periods (number of states for our state variable); P_n can equal D or some other interval (e.g., weekly)
	vector<lower=0,upper=1>[D] w; //index denoting daytype, where 1=weekday, 2= weekend/holiday.  
	int<lower=0> period[D]; //index denoting period    
	vector<lower=0>[D] L; // total amount of available fishing hours per day (e.g., day length - sunrise to sunset)
	matrix<lower=0>[D,S] O; //index denoting fishery status, where 1=open, 0 = closed 
	//Vehicle index effort counts
	int<lower=0> V_n; //total number of individual vehicle index effort counts
	int<lower=0> day_V[V_n]; //index denoting the "day" for an individual vehicle index effort count
	int<lower=0> section_V[V_n]; //index denoting the "section" for an individual vehicle index effort count
	int<lower=0> countnum_V[V_n]; //index denoting the "count number" for an individual vehicle index effort count
	int<lower=0> V_I[V_n]; //number of vehicles enumerated during an individual index effort survey
	//Trailer index effort counts 
	int<lower=0> T_n; //total number of boat trailer index effort counts
	int<lower=0> day_T[T_n]; //index denoting the "day" for an individual boat trailer index effort count
	int<lower=0> section_T[T_n]; //index denoting the "section" for an individual boat trailer index effort count
	int<lower=0> countnum_T[T_n]; //index denoting the "count number" for an individual boat trailer index effort count
	int<lower=0> T_I[T_n]; //number of boat trailers enumerated during an individual index effort survey
	//Angler index effort counts
	int<lower=0> A_n; //total number of angler index effort counts             
	int<lower=0> day_A[A_n]; //index denoting the "day" for an individual angler index effort count     
	int<lower=0> gear_A[A_n]; //index denoting the "gear/angler type" for an individual angler index effort count (e.g., 1=bank and 2=boat)     
	int<lower=0> section_A[A_n]; //index denoting the "section" for an individual angler index effort count  
	int<lower=0> countnum_A[A_n]; //index denoting the "count number" for an individual angler index effort count
	int<lower=0> A_I[A_n]; //number of anglers enumerated during an individual index effort survey         
	//Census (tie-in) effort counts
	int<lower=0> E_n; //total number of angler census effort counts 
	int<lower=0> day_E[E_n]; //index denoting the "day" for an individual angler census effort count
	int<lower=0> gear_E[E_n]; //index denoting the "gear/angler type" for an individual angler census effort count (e.g., 1=bank and 2=boat) 
	int<lower=0> section_E[E_n]; //index denoting the "section" for an individual angler census effort count  
	int<lower=0> countnum_E[E_n]; //index denoting the "count number" for an individual angler census effort count
	int<lower=0> E_s[E_n]; //number of anglers enumerated during an individual census effort survey
	//Angler interviews
	int<lower=0> IntViews; //total number of angler interviews conducted across all surveys dates                                        	
	int<lower=0> day_IntViews[IntViews]; //index denoting the "day" for an individual angler interview                       	
	int<lower=0> gear_IntViews[IntViews]; //index denoting the "gear/angler type" for an individual angler interview (e.g., 1=bank and 2=boat)      						
	int<lower=0> section_IntViews[IntViews]; //index denoting the "section" for an individual angler interview    						
	int<lower=0> c[IntViews]; // total number of fish caught by an angler (group) collected from an individual angler interview           						
	vector<lower=0>[IntViews] h; // total number of hours fish by an angler (group) collected from an individual angler interview        						
	int<lower=0> V_A[IntViews]; // total number of vehicles by an individual angler/group brought to the fishery on a given survey date		
	int<lower=0> T_A[IntViews]; // total number of boat trailers by an individual angler/group brought to the fishery on a given survey date			
	int<lower=0> A_A[IntViews]; // total number of anglers in the group interviewed 		
	//hyper and hyperhyper parameters
	real prior_sigma_C; //the hyperhyper scale parameter in the hyperprior sigma_C ~ cauchy(0,prior_sigma_C); sigma_C is the SD hyperparameter in the prior eps_C ~ normal(0,sigma_C)
	real prior_sigma_E; //the hyperhyper scale parameter in the hyperprior sigma_E ~ cauchy(0,prior_sigma_E); sigma_E is the SD hyperparameter in the prior eps_E ~ normal(0,sigma_E)
	real prior_sigma_r_E; //the hyperhyper scale parameter in the hyperprior sigma_r_E ~ cauchy(0,prior_sigma_r_E); sigma_r_E is an intermediate variable for r_E, which is the alpha and beta hyperparameters in the prior on eps_E_H ~ gamma(r_E,r_E)
	real prior_B1; //the SD hyperparameter in the prior B1 ~ normal(0, prior_B1);
	real prior_sigma_r_C; //the hyper scale parameter in the prior sigma_r_C ~ cauchy(0, prior_sigma_r_C); sigma_r_C is an intermediate variable for r_C
	real prior_phi_C_prior; //the rate (alpha) and shape (beta) hyperparameters for the prior phi_C_prior ~ beta(prior_phi_C_prior,prior_phi_C_prior); sigma_r_C is an intermediate variable for phi_C
	real prior_phi_E_prior; //the rate (alpha) and shape (beta) hyperparameters for the prior phi_E_prior ~ beta(prior_phi_E_prior,prior_phi_E_prior); sigma_r_E is an intermediate variable for phi_E
	real prior_omega_C_0; // the SD hyperparameter in the prior omega_C_0[g,s] ~ normal(0,prior_omega_C_0); 
	real prior_omega_E_0; // the SD hyperparameter in the prior omega_E_0[g,s] ~ normal(0,prior_omega_E_0); 
	real prior_b; //the SD hyperparameter in the prior b[g] ~ lognormal(0,prior_b); 
	real prior_mu_C_mu; //the mean hyperparameter in the prior mu_C[g,s] ~ normal(prior_mu_C_mu, prior_mu_C_sd);
	real prior_mu_C_sd; //the SD hyperparameter in the prior mu_C[g,s] ~ normal(prior_mu_C_mu, prior_mu_C_sd);
	real prior_mu_E_mu; //the mean hyperparameter in the prior mu_E[g,s] ~ normal(prior_mu_E_mu, prior_mu_E_sd);
	real prior_mu_E_sd; //the SD hyperparameter in the prior mu_E[g,s] ~ normal(prior_mu_E_mu, prior_mu_E_sd);
}
transformed data{
	matrix<lower=0,upper=1>[G,S]p_TE; //proportion of section covered by tie in counts (serves as an expansion if p_TE != 1)
	for(g in 1:G){
		for(s in 1:S){
			p_TE[g,s] = 1; 
		}
	}
}
parameters{
	//Effort
	matrix[G,S] mu_E; //season-long effort intercept  
	real B1; //fixed effect accounting for the effect of day type on effort   
	real<lower=0> sigma_E; //effort process error standard deviation 
	real<lower=0> sigma_r_E; //prior on r_E
	real<lower=0,upper=1> phi_E_prior; //prior on a transformation of phi_E								    			
	matrix[P_n-1,G] eps_E[S]; //effort process errors  
	matrix[G,S] omega_E_0; //effort residual for initial time step (p=1)
	vector<lower=0,upper=1>[G] R_V; //true angler vehicles per angler
	vector<lower=0,upper=1>[G] R_T; //true angler trailers per angler
	vector<lower=0>[G] b; //bias in angler vehicles per angler from road counts of cars
	matrix<lower=0>[D,G] eps_E_H[S,H]; //gamma random variate accounting for overdispersion in the census effort counts due to within-day variability in angler pressure
	matrix<lower=0,upper=1>[G,S] p_I; //fixed proportion of angler effort observed in an index area
	//Catch rates
	matrix[G,S] mu_C; //season-long catch rate intercept                            							
	real<lower=0> sigma_C; //catch rate (CPUE) process error standard deviation 
	real<lower=0,upper=1> phi_C_prior; //Prior on a transformation of phi_C									    			
	real<lower=0> sigma_r_C; //Prior on r_C
	matrix[P_n-1,G] eps_C[S]; //CPUE process errors 
	matrix[G,S] omega_C_0; //CPUE residual for initial time step (p=1)                             							
}
transformed parameters{
	//Effort
	real<lower=-1,upper=1> phi_E; //auto-regressive (AR), mean-reverting lag-1 coefficient for effort 
	matrix[P_n,G] omega_E[S]; //residual in effort 
	matrix<lower=0>[D,G] lambda_E_S[S]; //mean daily effort
	matrix<lower=0>[D,G] lambda_E_S_I[S,H]; //mean hourly effort
	real<lower=0> r_E; //over-dispersion parameter accounting for within day variability in effort
	//Catch rates
	real<lower=-1,upper=1> phi_C; //auto-regressive (AR), mean-reverting lag-1 coefficient for CPUE 
	real<lower=0> r_C; //over-dispersion parameter accounting for among angler (group) variability in CPUE
	matrix[P_n,G] omega_C[S]; //Residual in CPUE
	matrix<lower=0>[D,G] lambda_C_S[S]; //mean daily CPUE

	r_E = 1 / square(sigma_r_E);
	r_C = 1 / square(sigma_r_C);
	phi_C = (phi_C_prior * 2)-1;
	phi_E = (phi_E_prior * 2)-1;
	for(g in 1:G){
		for(s in 1:S){
			omega_C[s][1,g] = omega_C_0[g,s];
			omega_E[s][1,g] = omega_E_0[g,s];
		}
		for(p in 2:P_n){ 
			for(s in 1:S){
				omega_C[s][p,g] = phi_C * omega_C[s][p-1,g] + eps_C[s][p-1,g]; 
				omega_E[s][p,g] = phi_E * omega_E[s][p-1,g] + eps_E[s][p-1,g]; 
			}													
		}
		for(d in 1:D){       
			for(s in 1:S){	
				lambda_C_S[s][d,g] = exp(mu_C[g,s] + omega_C[s][period[d],g]) * O[d,s];
				lambda_E_S[s][d,g] = exp(mu_E[g,s] + omega_E[s][period[d],g] + B1 * w[d])* O[d,s];
				for(i in 1:H){
					lambda_E_S_I[s,i][d,g] = lambda_E_S[s][d,g] * eps_E_H[s,i][d,g];									
				}
			}
		}								    
	}	
}
model{
	//Hyperpriors (effort hyperparameters)
	sigma_E ~ cauchy(0,prior_sigma_E);                                						
  	phi_E_prior ~ beta(prior_phi_E_prior,prior_phi_E_prior);
	sigma_r_E ~ cauchy(0,prior_sigma_r_E);
	B1 ~ normal(0,prior_B1);
	//Hyperpriors (CPUE hyperparameters)
	sigma_C ~ cauchy(0,prior_sigma_C);                     						
  	phi_C_prior ~ beta(prior_phi_C_prior,prior_phi_C_prior);
	sigma_r_C ~ cauchy(0, prior_sigma_r_C);
	//Priors 
	for(g in 1:G){
		for(p in 2:P_n){ 
			for(s in 1:S){
				eps_C[s][p-1,g] ~ normal(0,sigma_C);
				eps_E[s][p-1,g] ~ normal(0,sigma_E);
			}
		}
		for(d in 1:D){
			for(s in 1:S){  					  
				for(i in 1:H){
					eps_E_H[s,i][d,g] ~ gamma(r_E,r_E); 
				}
			}
		}
		for(s in 1:S){
			omega_C_0[g,s] ~ normal(0,prior_omega_C_0); 
			omega_E_0[g,s] ~ normal(0,prior_omega_E_0); 
			mu_C[g,s] ~ normal(prior_mu_C_mu, prior_mu_C_sd);
			mu_E[g,s] ~ normal(prior_mu_E_mu,prior_mu_E_sd);
			p_I[g,s] ~ beta(0.5,0.5);
		}
		R_V[g] ~ beta(0.5,0.5); //Note: leaving constant among days AND sections...may need to tweak; can make beta because is "true" angler cars or angler trailers per angler!
		R_T[g] ~ beta(0.5,0.5); //Note: leaving constant among days AND sections...may need to tweak; can make beta because is "true" angler cars or angler trailers per angler!
		b[g] ~ lognormal(0,prior_b); //Note: leaving constant among days AND sections...may need to tweak could go as low as 0.25 for sigma
	}
	//Likelihoods
	//Index effort counts - vehicles
	for(i in 1:V_n){
		V_I[i] ~ poisson((lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],1] * p_TE[1,section_V[i]] * R_V[1] +
						 lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],2] * p_TE[2,section_V[i]] * R_V[2]) * b[1]);
	}
	//Index effort counts - trailers
	for(i in 1:T_n){
		T_I[i] ~ poisson((lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],1] * p_TE[1,section_T[i]] * R_T[1] +
						 lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],2] * p_TE[2,section_T[i]] * R_T[2]) * b[2]); 
	}
	//Index effort counts - anglers
	for(i in 1:A_n){
		A_I[i] ~ poisson(lambda_E_S_I[section_A[i],countnum_A[i]][day_A[i],gear_A[i]] * p_TE[gear_A[i],section_A[i]] * p_I[gear_A[i],section_A[i]]);
	}
	//Census (tie-in) effort counts - anglers
	for(e in 1:E_n){
		E_s[e] ~ poisson(lambda_E_S_I[section_E[e],countnum_E[e]][day_E[e],gear_E[e]] * p_TE[gear_E[e],section_E[e]]);				
	}
	//Angler interviews 
	for(a in 1:IntViews){
		//catch
		c[a] ~ neg_binomial_2(lambda_C_S[section_IntViews[a]][day_IntViews[a], gear_IntViews[a]] * h[a] , r_C);
		//vehicles
		V_A[a] ~ binomial(A_A[a], R_V[gear_IntViews[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
		//trailers
		T_A[a] ~ binomial(A_A[a], R_T[gear_IntViews[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
	}												
}
generated quantities{ 
	matrix<lower=0>[D,G] lambda_Ctot_S[S]; //total daily catch
	matrix<lower=0>[D,G] C[S]; //realized total daily catch
	matrix<lower=0>[D,G] E[S]; //realized total daily effort
	real<lower=0> C_sum; //season-total catch
	real<lower=0> E_sum; //season-total effort
	vector[V_n + T_n + A_n + E_n + IntViews + IntViews + IntViews] log_lik;
	C_sum = 0;
	E_sum = 0;
	for(g in 1:G){
		for(d in 1:D){
			for(s in 1:S){
				lambda_Ctot_S[s][d,g] = lambda_E_S[s][d,g] * L[d] * lambda_C_S[s][d,g]; 
				C[s][d,g] = poisson_rng(lambda_Ctot_S[s][d,g]); 
				C_sum = C_sum + C[s][d,g];
				E[s][d,g] = lambda_E_S[s][d,g] * L[d]; 
				E_sum = E_sum + E[s][d,g];
			}                                                                                                              
		}
	}
	//point-wise log likelihood for LOO-IC
	//Index effort counts - vehicles
	for (i in 1:V_n){
		log_lik[i] = poisson_lpmf(V_I[i]|(lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],1] * p_TE[1,section_V[i]] * R_V[1] +
						 				  lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],2] * p_TE[2,section_V[i]] * R_V[2]) * b[1]);
	}
	//Index effort counts - trailers
	for(i in 1:T_n){
		log_lik[V_n +i] = poisson_lpmf(T_I[i]|(lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],1] * p_TE[1,section_T[i]] * R_T[1] +
						 lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],2] * p_TE[2,section_T[i]] * R_T[2]) * b[2]); 
	}
	//Index effort counts - anglers
	for(i in 1:A_n){ 
		log_lik[V_n + T_n + i] = poisson_lpmf(A_I[i]|lambda_E_S_I[section_A[i],countnum_A[i]][day_A[i],gear_A[i]] * p_TE[gear_A[i],section_A[i]] * p_I[gear_A[i],section_A[i]]);
	}
	//Census (tie-in) effort counts - anglers
	for(e in 1:E_n){
		log_lik[V_n + T_n + A_n + e] = poisson_lpmf(E_s[e]|lambda_E_S_I[section_E[e],countnum_E[e]][day_E[e],gear_E[e]] * p_TE[gear_E[e],section_E[e]]);				
	}
	//Angler interviews - catch (number of fish)
	for(a in 1:IntViews){
		log_lik[V_n + T_n + A_n + E_n + a] = neg_binomial_2_lpmf(c[a]|lambda_C_S[section_IntViews[a]][day_IntViews[a],gear_IntViews[a]] * h[a] , r_C);
	}
	//Angler interviews - number of vehicles
	for(a in 1:IntViews){
		log_lik[V_n + T_n + A_n + E_n + IntViews + a] = binomial_lpmf(V_A[a]|A_A[a], R_V[gear_IntViews[a]]);
	}
	//Angler interviews - number of trailers
	for(a in 1:IntViews){
		log_lik[V_n + T_n + A_n + E_n + IntViews + IntViews + a] = binomial_lpmf(T_A[a]|A_A[a], R_T[gear_IntViews[a]]);
	}												
}