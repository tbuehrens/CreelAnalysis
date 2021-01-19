//2) closed times by section (matrix with day section, 0,1)
data{
	int<lower=0> D;
	int<lower=0> G;
	int<lower=0> S;
	int<lower=0> H;
	//int<lower=0> Wk_n;
	//day attributes
	vector<lower=0,upper=1>[D] w;
	int<lower=0> week[D];
	vector<lower=0>[D] L;
	matrix<lower=0>[D,S] O; //open or closed?
	//Index counts
	//Vehicles
	int<lower=0> V_n;
	int<lower=0> day_V[V_n];
	int<lower=0> gear_V[V_n];
	int<lower=0> section_V[V_n];
	int<lower=0> countnum_V[V_n];
	int<lower=0> V_I[V_n];
	//trailers
	int<lower=0> T_n;
	int<lower=0> day_T[T_n];
	int<lower=0> gear_T[T_n];
	int<lower=0> section_T[T_n];
	int<lower=0> countnum_T[T_n];
	int<lower=0> T_I[T_n];
	//Tie in counts
	int<lower=0> E_n;
	int<lower=0> day_E[E_n];
	int<lower=0> gear_E[E_n];
	int<lower=0> section_E[E_n];
	int<lower=0> countnum_E[E_n];
	int<lower=0> E_s[E_n];
	//Angler interviews
	int<lower=0> A;
	int<lower=0> day_A[A];
	int<lower=0> gear_A[A];
	int<lower=0> section_A[A];
	int<lower=0> c[A];//catch
	vector<lower=0>[A] h; //hours
	int<lower=0> V_A[A];////vehicles (in group) 
	int<lower=0> T_A[A];////trailers (in group) 
	int<lower=0> A_A[A];//angler count (in group)
}
transformed data{
	matrix<lower=0,upper=1>[G,S]p_TE;
	for(g in 1:G){
		for(s in 1:S){
			p_TE[g,s] = 1;
		}
	}
}
parameters{
	//Effort
	vector[G] mu_E_mu;  
	real B1;   
	real<lower=0> sigma_E; 
	real<lower=0> sigma_E_H;
	real<lower=0> sigma_P;	
	real<lower=0,upper=1> phi_E_prior;	
	real<lower=0,upper=1> phi_P_prior;								    			
	matrix[D-1,G] eps_E_eps;  
	//matrix[Wk_n-1,G] eps_E_eps;   
	vector[G] eps_E_0;
	matrix[G,S-1] eps_P_0;
	matrix[G,S-1] mu_P; 
	//matrix<lower=0,upper=1>[G,S] p_I;
	//matrix<lower=0,upper=1>[G,S] R; //true angler vehicles per angler
	//matrix<lower=0,upper=1>[G,S] b; //bias in angler vehicles per angler from road counts of cars
	vector<lower=0,upper=1>[G] R_V; //true angler vehicles per angler
	vector<lower=0,upper=1>[G] R_T; //true angler trailers per angler
	vector<lower=0>[G] b; //bias in angler vehicles per angler from road counts of cars
	matrix<lower=0>[D,G] eps_E_H[S,H];
	matrix[D-1,G] eps_P_eps[S-1];
	//matrix[Wk_n-1,G] eps_P_eps[S-1];
	//Catch
	matrix[G,S] mu_C_mu;  
	//real B2;                                 							
	real<lower=0> sigma_C;
	real<lower=0,upper=1> phi_C_prior;								    			
	real<lower=0> phi_NB_C_prior;   
	matrix[D-1,G] eps_C_eps[S];  
	//matrix[Wk_n-1,G] eps_C_eps;   
	matrix[G,S] eps_C_0;                              							
}
transformed parameters{
	//effort
	real<lower=-1,upper=1> phi_E;
	real<lower=-1,upper=1> phi_P;
	matrix[D,G] eps_E;
	//matrix[Wk_n,G] eps_E;
	matrix[D,G] nu[S-1];
	matrix[D,G] eps_P[S-1];
	matrix[D,G] sum_nu;
	//matrix[Wk_n,G] eps_P[S-1];
	matrix<lower=0>[D,G] lambda_E;
	matrix<lower=0,upper=1>[D,G] p_S[S];
	matrix<lower=0>[D,G] lambda_E_S[S];
	matrix<lower=0>[D,G] lambda_E_S_I[S,H];
	
	//catch
	real<lower=-1,upper=1> phi_C;
	real<lower=0> phi_NB_C;
	matrix[D,G] eps_C[S];
	//matrix[Wk_n,G] eps_C;
	matrix<lower=0>[D,G] lambda_C_S[S];

	phi_NB_C = 1 / square(phi_NB_C_prior);
	phi_C = (phi_C_prior * 2)-1;
	phi_E = (phi_E_prior * 2)-1;
	phi_P = (phi_P_prior * 2)-1;
	
	for(g in 1:G){
		eps_E[1,g] = eps_E_0[g];
		for(s in 1:S){
			eps_C[s][1,g] = eps_C_0[g,s];
		}
		for(s in 1:(S-1)){
			eps_P[s][1,g] = eps_P_0[g,s];
		}
		for(d in 2:D){
			eps_E[d,g] = phi_E * eps_E[d-1,g] + eps_E_eps[d-1,g]; //eps_E[d,g] = eps_E[d-1,g] + eps_E_eps[d-1,g];//RW
			for(s in 1:S){
				eps_C[s][d,g] = phi_C * eps_C[s][d-1,g] + eps_C_eps[s][d-1,g]; //eps_C[d,g] = eps_C[d-1,g] + eps_C_eps[d-1,g];//RW
			}	
			for(s in 1:(S-1)){
				eps_P[s][d,g] = phi_P * eps_P[s][d-1,g] + eps_P_eps[s][d-1,g];
			}													
		}
		//for(W in 2:Wk_n){
			//eps_C[W,g] = phi_C * eps_C[W-1,g] + eps_C_eps[W-1,g]; //eps_C[d,g] = eps_C[d-1,g] + eps_C_eps[d-1,g];//RW
			//eps_E[W,g] = phi_E * eps_E[W-1,g] + eps_E_eps[W-1,g]; //eps_E[d,g] = eps_E[d-1,g] + eps_E_eps[d-1,g];//RW	
			//for(s in 1:(S-1)){
			//	eps_P[s][W,g] = phi_P * eps_P[s][W-1,g] + eps_P_eps[s][W-1,g];
			//}													
		//}	
		for(d in 1:D){
			sum_nu[d,g] = 0;
			lambda_E[d,g] = exp(mu_E_mu[g] + eps_E[d,g] + B1 * w[d]); //lambda_C[d,g] = exp(eps_C[d,g] + B2 * w[d]);//RW         
			//lambda_E[d,g] = exp(mu_E_mu[g] + eps_E[week[d],g] + B1 * w[d]); //lambda_C[d,g] = exp(eps_C[d,g] + B2 * w[d]);//RW
			//lambda_C[d,g] = exp(mu_C_mu[g] + eps_C[week[d],g]);// + B2 * w[d]); //lambda_C[d,g] = exp(eps_C[d,g] + B2 * w[d]);//RW
			for(s in 1:(S-1)){	
			 	nu[s][d,g] = mu_P[g,s] + eps_P[s][d,g];
			 	sum_nu[d,g] += exp(nu[s][d,g]);
			}	
			for(s in 1:(S-1)){	
				p_S[s][d,g] = (exp(nu[s][d,g])/(sum_nu[d,g] + 1)) * O[d,s]; 
				//p_S[s][d,g] = exp(nu[s][d,g])/(sum(exp(nu[1:(S-1)][d,g])) + 1); Need to fix indexes so sum is across sections within a single day and gear
			}
			//p_S[S][d,g] = 1/(sum(exp(nu[1:(S-1)][d,g])) + 1); Need to fix indexes so sum across sections within a single dayday and gear see page 336 of Stan v 2.17 manual
			p_S[S][d,g] = (1/(sum_nu[d,g] + 1)) * O[d,S]; 
			for(s in 1:S){	
				lambda_C_S[s][d,g] = exp(mu_C_mu[g,s] + eps_C[s][d,g]) * O[d,s];// + B2 * w[d]); //lambda_C[d,g] = exp(eps_C[d,g] + B2 * w[d]);//RW
				lambda_E_S[s][d,g] = lambda_E[d,g] * p_S[s][d,g];										
				for(i in 1:H){
					lambda_E_S_I[s,i][d,g] = lambda_E_S[s][d,g] * eps_E_H[s,i][d,g];									
				}
			}
		}								    
	}	
}
model{
	//Effort
	mu_E_mu ~ normal(0,5);
	sigma_E ~ cauchy(0,2);   // normal(0,0.1)://                             						
  	phi_E_prior ~ beta(2,2);
	sigma_E_H ~ cauchy(0,2);//normal(0,0.01);
	phi_P_prior ~ beta(2,2);
	sigma_P ~ cauchy(0,2);//normal(0,0.1)
	B1 ~ normal(0,3);
	//catch
	sigma_C ~ cauchy(0,2);   // normal(0,0.1)://                             						
  	phi_C_prior ~ beta(2,2);
	phi_NB_C_prior ~ cauchy(0,2); //normal(0,0.2);
	//B2 ~ normal(0,1);
	for(g in 1:G){
		for(d in 2:D){
			eps_E_eps[d-1,g] ~ normal(0,sigma_E);
			for(s in 1:S){
				eps_C_eps[s][d-1,g] ~ normal(0,sigma_C);
			}
			for(s in 1:(S-1)){
				eps_P_eps[s][d-1,g] ~ normal(0,sigma_P);
			}
		}
/*		for(W in 2:Wk_n){
			eps_C_eps[W-1,g] ~ normal(0,sigma_C);
			eps_E_eps[W-1,g] ~ normal(0,sigma_E);
			for(s in 1:(S-1)){
				eps_P_eps[s][W-1,g] ~ normal(0,sigma_P);
			}
		}*/
		for(d in 1:D){
			for(s in 1:S){  					  
				for(i in 1:H){
					eps_E_H[s,i][d,g] ~ gamma(1/(sigma_E_H*sigma_E_H),1/(sigma_E_H*sigma_E_H)); 
				}
			}
		}
		for(s in 1:(S-1)){
			eps_P_0[g,s] ~ normal(0,2); //may need to tweak
			mu_P[g,s] ~ normal(0,2); //may need to tweak
		}
		for(s in 1:S){
			//p_I[g,s] ~ beta(0.5,0.5);
			//R[g,s] ~ beta(0.5,0.5); //leaving constant among days...may need to tweak; can make beta because is "true" angler cars or angler trailers per angler!
			//b[g,s] ~ lognormal(0,1); //leaving constant among days...may need to tweak could go as low as 0.25 for sigma
			eps_C_0[g,s] ~ normal(0,0.4); //eps_C_0[g] ~ normal(-2, 0.5);//RandomWalk
			mu_C_mu[g,s] ~ normal(-3, 1);
		}
		eps_E_0[g] ~ normal(0,1); //eps_E_0[g] ~ normal(-2, 0.5);//RandomWalk//Index count Likelihood
		R_V[g] ~ beta(0.5,0.5); //leaving constant among days AND sections...may need to tweak; can make beta because is "true" angler cars or angler trailers per angler!
		R_T[g] ~ beta(0.5,0.5); //leaving constant among days AND sections...may need to tweak; can make beta because is "true" angler cars or angler trailers per angler!
		b[g] ~ lognormal(0,1); //leaving constant among days AND sections...may need to tweak could go as low as 0.25 for sigma
		//b[g] ~ gamma(1E-06,1E-06);
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
	//Tie in count Likelihood
	for(e in 1:E_n){
		E_s[e] ~ poisson(lambda_E_S_I[section_E[e],countnum_E[e]][day_E[e],gear_E[e]] * p_TE[gear_E[e],section_E[e]]);				
	}
	//Angler Likelihood
	for(a in 1:A){
		c[a] ~ neg_binomial_2(lambda_C_S[section_A[a]][day_A[a],gear_A[a]] * h[a] , phi_NB_C);
		V_A[a] ~ binomial(A_A[a], R_V[gear_A[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
		T_A[a] ~ binomial(A_A[a], R_T[gear_A[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
	}												
}
generated quantities{
	matrix<lower=0>[D,G] C[S];
	matrix<lower=0>[D,G] E[S];
	real<lower=0> C_sum;
	real<lower=0> E_sum;
	vector[V_n + T_n + E_n + A + A + A] log_lik;
	C_sum = 0;
	E_sum = 0;
	for(g in 1:G){
		for(d in 1:D){
			for(s in 1:S){
				C[s][d,g] = poisson_rng(lambda_E_S[s][d,g] * L[d] * lambda_C_S[s][d,g]); 
				C_sum = C_sum + C[s][d,g];
				E[s][d,g] = poisson_rng(lambda_E_S[s][d,g] * L[d]); 
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
	//Tie in count Likelihood
	for(e in 1:E_n){
		log_lik[V_n + T_n + e] = poisson_lpmf(E_s[e]|lambda_E_S_I[section_E[e],countnum_E[e]][day_E[e],gear_E[e]] * p_TE[gear_E[e],section_E[e]]);				
	}
	//Angler Likelihoods
	for(a in 1: A){
		log_lik[V_n + T_n + E_n + a] = neg_binomial_2_lpmf(c[a]|lambda_C_S[section_A[a]][day_A[a],gear_A[a]] * h[a] , phi_NB_C);
	}
	for(a in 1: A){
		log_lik[V_n + T_n + E_n + A + a] = binomial_lpmf(V_A[a]|A_A[a], R_V[gear_A[a]]); //Note: leaving ratio of cars per angler constant among days since was invariant!
	}
	for(a in 1: A){
		log_lik[V_n + T_n + E_n + A + A + a] = binomial_lpmf(T_A[a]|A_A[a], R_T[gear_A[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
	}												
}