data{
	int<lower=0> D;
	int<lower=0> G;
	int<lower=0> S;
	int<lower=0> H;
	int<lower=0> P_n; //*************** Period - vector just like week Wk_n changed to P_n (number of states for our state variable)
	//day attributes
	vector<lower=0,upper=1>[D] w;
	int<lower=0> period[D];        //***************
	vector<lower=0>[D] L;
	matrix<lower=0>[D,S] O; //open or closed?
	//Index counts
	//Vehicles
	int<lower=0> V_n;
	int<lower=0> day_V[V_n];
	//int<lower=0> gear_V[V_n];
	int<lower=0> section_V[V_n];
	int<lower=0> countnum_V[V_n];
	int<lower=0> V_I[V_n];
	//trailers
	int<lower=0> T_n;
	int<lower=0> day_T[T_n];
	//int<lower=0> gear_T[T_n];
	int<lower=0> section_T[T_n];
	int<lower=0> countnum_T[T_n];
	int<lower=0> T_I[T_n];
	//Angler counts
	int<lower=0> A_n;             //KB edit
	int<lower=0> day_A[A_n];      //KB edit
	int<lower=0> gear_A[A_n];     //KB edit
	int<lower=0> section_A[A_n];  //KB edit
	int<lower=0> countnum_A[A_n]; //KB edit
	int<lower=0> A_I[A_n];        //KB edit
	//Tie in counts
	int<lower=0> E_n;
	int<lower=0> day_E[E_n];
	int<lower=0> gear_E[E_n];
	int<lower=0> section_E[E_n];
	int<lower=0> countnum_E[E_n];
	int<lower=0> E_s[E_n];
	//Angler interviews
	int<lower=0> IntViews;                                //KB edit
	int<lower=0> day_IntViews[IntViews];                  //KB edit
	int<lower=0> gear_IntViews[IntViews];      						//KB edit
	int<lower=0> section_IntViews[IntViews];   						//KB edit
	int<lower=0> c[IntViews];//catch           						//KB edit
	vector<lower=0>[IntViews] h; //hours       						//KB edit
	int<lower=0> V_A[IntViews];////vehicles (in group) 		//KB edit
	int<lower=0> T_A[IntViews];////trailers (in group) 		//KB edit
	int<lower=0> A_A[IntViews];//angler count (in group)	//KB edit
	//priors
	real prior_sigma_C;
	real prior_sigma_E;
	real prior_sigma_E_H;
	real prior_B1;
	real prior_phi_NB_C;
	real prior_phi_C_prior;
	real prior_phi_E_prior;
	real prior_eps_C_0;
	real prior_eps_E_0;
	real prior_b;
	real prior_mu_C_mu_mu;
	real prior_mu_C_mu_sd;
	real prior_mu_E_mu_mu;
	real prior_mu_E_mu_sd;
}
transformed data{
	matrix<lower=0,upper=1>[G,S]p_TE;
	vector[G*S] Zero;
	Zero = rep_vector(0,G*S);
	for(g in 1:G){
		for(s in 1:S){
			p_TE[g,s] = 1;
		}
	}
}
parameters{
	//Effort
	matrix[G,S] mu_E_mu;  
	real B1;   
	real<lower=0> sigma_E; 
	cholesky_factor_corr[G*S] Lcorr_E;
	real<lower=0> sigma_E_H;
	real<lower=0,upper=1> phi_E_prior;								    			
	matrix[P_n-1,G*S] eps_E_eps;  //*************** used to be D-1 (instead of P_n -1)
	matrix[G,S] eps_E_0;
	vector<lower=0,upper=1>[G] R_V; //true angler vehicles per angler
	vector<lower=0,upper=1>[G] R_T; //true angler trailers per angler
	vector<lower=0>[G] b; //bias in angler vehicles per angler from road counts of cars
	matrix<lower=0>[D,G] eps_E_H[S,H];
	matrix<lower=0,upper=1>[G,S] p_I; //KB edit
	//Catch
	matrix[G,S] mu_C_mu;  
	//real B2;                                 							
	real<lower=0> sigma_C;
	cholesky_factor_corr[G*S] Lcorr_C;
	real<lower=0,upper=1> phi_C_prior;								    			
	real<lower=0> phi_NB_C_prior;   
	matrix[P_n-1,G*S] eps_C_eps;  //*************** used to be D-1 (instead of P_n -1)
	matrix[G,S] eps_C_0;                              							
}
transformed parameters{
	//effort
	real<lower=-1,upper=1> phi_E;
	matrix[P_n,G] eps_E[S]; //*************** P_n instead of D
	matrix<lower=0>[D,G] lambda_E_S[S];
	matrix<lower=0>[D,G] lambda_E_S_I[S,H];
	//catch
	real<lower=-1,upper=1> phi_C;
	real<lower=0> phi_NB_C;
	matrix[P_n,G] eps_C[S]; //*************** P_n instead of D
	matrix<lower=0>[D,G] lambda_C_S[S];

	phi_NB_C = 1 / square(phi_NB_C_prior);
	phi_C = (phi_C_prior * 2)-1;
	phi_E = (phi_E_prior * 2)-1;
	for(g in 1:G){
		for(s in 1:S){
			eps_C[s][1,g] = eps_C_0[g,s];
			eps_E[s][1,g] = eps_E_0[g,s];
		}
		for(p in 2:P_n){ //*************** p instead of d
			for(s in 1:S){
				eps_C[s][p,g] = phi_C * eps_C[s][p-1,g] + to_matrix(eps_C_eps[p-1,],G,S)[g,s]; //*************** p instead of d
				eps_E[s][p,g] = phi_E * eps_E[s][p-1,g] + to_matrix(eps_E_eps[p-1,],G,S)[g,s]; //*************** p instead of d
			}													
		}
		for(d in 1:D){       
			for(s in 1:S){	
				lambda_C_S[s][d,g] = exp(mu_C_mu[g,s] + eps_C[s][period[d],g]) * O[d,s];// + B2 * w[d]); //lambda_C[d,g] = exp(eps_C[d,g] + B2 * w[d]);//RW   //*************** period[] new
				lambda_E_S[s][d,g] = exp(mu_E_mu[g,s] + eps_E[s][period[d],g] + B1 * w[d])* O[d,s];   //*************** period[] new										
				for(i in 1:H){
					lambda_E_S_I[s,i][d,g] = lambda_E_S[s][d,g] * eps_E_H[s,i][d,g];									
				}
			}
		}								    
	}	
}
model{
	//Effort
	sigma_E ~ cauchy(0,prior_sigma_E); 
	Lcorr_E ~ lkj_corr_cholesky(1);                               						
  	phi_E_prior ~ beta(prior_phi_E_prior,prior_phi_E_prior);
	sigma_E_H ~ cauchy(0,prior_sigma_E_H);
	B1 ~ normal(0,prior_B1);
	//catch
	sigma_C ~ cauchy(0,prior_sigma_C);
    Lcorr_C ~ lkj_corr_cholesky(1);                               						
  	phi_C_prior ~ beta(prior_phi_C_prior,prior_phi_C_prior);
	phi_NB_C_prior ~ cauchy(0,prior_phi_NB_C); 
	for(p in 2:P_n){   //*************** p loop instead of d loop
		eps_C_eps[p-1,] ~ multi_normal_cholesky(Zero, diag_pre_multiply(rep_vector(sigma_C,G*S), Lcorr_C));
		eps_E_eps[p-1,] ~ multi_normal_cholesky(Zero, diag_pre_multiply(rep_vector(sigma_E,G*S), Lcorr_E));
	}
	for(g in 1:G){
		for(d in 1:D){
			for(s in 1:S){  					  
				for(i in 1:H){
					eps_E_H[s,i][d,g] ~ gamma(1/(sigma_E_H*sigma_E_H),1/(sigma_E_H*sigma_E_H)); 
				}
			}
		}
		for(s in 1:S){
			eps_C_0[g,s] ~ normal(0,prior_eps_C_0); 
			eps_E_0[g,s] ~ normal(0,prior_eps_E_0); 
			mu_C_mu[g,s] ~ normal(prior_mu_C_mu_mu, prior_mu_C_mu_sd);
			mu_E_mu[g,s] ~ normal(prior_mu_E_mu_mu,prior_mu_E_mu_sd);
		}
		R_V[g] ~ beta(0.5,0.5); //leaving constant among days AND sections...may need to tweak; can make beta because is "true" angler cars or angler trailers per angler!
		R_T[g] ~ beta(0.5,0.5); //leaving constant among days AND sections...may need to tweak; can make beta because is "true" angler cars or angler trailers per angler!
		b[g] ~ lognormal(0,prior_b); //leaving constant among days AND sections...may need to tweak could go as low as 0.25 for sigma
	}
	//index effort count likelihood
	for(i in 1:V_n){
		//I[i] ~ poisson(lambda_E_S_I[section_I[i],countnum_I[i]][day_I[i],gear_I[i]] * p_TE[gear_I[i],section_I[i]] * p_I[gear_I[i],section_I[i]]);
		V_I[i] ~ poisson((lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],1] * p_TE[1,section_V[i]] * R_V[1] +
						 lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],2] * p_TE[2,section_V[i]] * R_V[2]) * b[1]);  //Note: leaving ratio of cars per angler and bias constant among days since was invariant!
	}
	for(i in 1:T_n){
		T_I[i] ~ poisson((lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],1] * p_TE[1,section_T[i]] * R_T[1] +
						 lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],2] * p_TE[2,section_T[i]] * R_T[2]) * b[2]); 
	}
	for(i in 1:A_n){ //KB edit
		A_I[i] ~ poisson(lambda_E_S_I[section_A[i],countnum_A[i]][day_A[i],gear_A[i]] * p_TE[gear_A[i],section_A[i]] * p_I[gear_A[i],section_A[i]]);
	}
	//Tie in count Likelihood
	for(e in 1:E_n){
		E_s[e] ~ poisson(lambda_E_S_I[section_E[e],countnum_E[e]][day_E[e],gear_E[e]] * p_TE[gear_E[e],section_E[e]]);				
	}
	//Angler Likelihood
	for(a in 1:IntViews){
		c[a] ~ neg_binomial_2(lambda_C_S[section_IntViews[a]][day_IntViews[a], gear_IntViews[a]] * h[a] , phi_NB_C);
		V_A[a] ~ binomial(A_A[a], R_V[gear_IntViews[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
		T_A[a] ~ binomial(A_A[a], R_T[gear_IntViews[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
	}											
}
generated quantities{
  matrix[G*S,G*S] Omega_C;
  matrix[G*S,G*S] Omega_E;
	matrix<lower=0>[D,G] lambda_Ctot_S[S];
	matrix<lower=0>[D,G] C[S];
	matrix<lower=0>[D,G] E[S];
	real<lower=0> C_sum;
	real<lower=0> E_sum;
	vector[V_n + T_n + A_n + E_n + IntViews + IntViews + IntViews] log_lik; //KB edit
	Omega_C = multiply_lower_tri_self_transpose(Lcorr_C);
	Omega_E = multiply_lower_tri_self_transpose(Lcorr_E);
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
	//Index count likelihoods (trailer and vehicle)
	for (i in 1:V_n){
		log_lik[i] = poisson_lpmf(V_I[i]|(lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],1] * p_TE[1,section_V[i]] * R_V[1] +
						 				  lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],2] * p_TE[2,section_V[i]] * R_V[2]) * b[1]);
	}
	for(i in 1:T_n){
		log_lik[V_n +i] = poisson_lpmf(T_I[i]|(lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],1] * p_TE[1,section_T[i]] * R_T[1] +
						 lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],2] * p_TE[2,section_T[i]] * R_T[2]) * b[2]); 
	}
	for(i in 1:A_n){ //KB edit
		log_lik[V_n + T_n + i] = poisson_lpmf(A_I[i]|lambda_E_S_I[section_A[i],countnum_A[i]][day_A[i],gear_A[i]] * p_TE[gear_A[i],section_A[i]] * p_I[gear_A[i],section_A[i]]);
	}
	//Tie in count Likelihood
	for(e in 1:E_n){
		log_lik[V_n + T_n + A_n + e] = poisson_lpmf(E_s[e]|lambda_E_S_I[section_E[e],countnum_E[e]][day_E[e],gear_E[e]] * p_TE[gear_E[e],section_E[e]]);				
	}
	//Angler Likelihoods
	for(a in 1:IntViews){
		log_lik[V_n + T_n + A_n + E_n + a] = neg_binomial_2_lpmf(c[a]|lambda_C_S[section_IntViews[a]][day_IntViews[a],gear_IntViews[a]] * h[a] , phi_NB_C);
	}
	for(a in 1:IntViews){
		log_lik[V_n + T_n + A_n + E_n + IntViews + a] = binomial_lpmf(V_A[a]|A_A[a], R_V[gear_IntViews[a]]); //Note: leaving ratio of cars per angler constant among days since was invariant!
	}
	for(a in 1:IntViews){
		log_lik[V_n + T_n + A_n + E_n + IntViews + IntViews + a] = binomial_lpmf(T_A[a]|A_A[a], R_T[gear_IntViews[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
	}											
}