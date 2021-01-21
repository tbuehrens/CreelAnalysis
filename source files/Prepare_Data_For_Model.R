#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file prepares final dataset for creel (stan) model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Last minute data fixes
  standat$V_A[standat$V_A>standat$A_A]<-standat$A_A[standat$V_A>standat$A_A] #Number of vehicles for a group can't be greater than number of anglers in the group
      
# Create two new "standat" variables depending on specified time (strata) period (e.g., day vs. week) 
  if(model_period=="day"){
    standat$P_n<-nrow(all.Dates) # For now: enter "D" for day or length(unique(week)) for week
    standat$period<-c(1:nrow(all.Dates)) #week          # For now: enter "1:D" for day or "week" for week
  }else{
    if(model_period=="week"){
      standat$P_n<-length(unique(all.Dates$Weeknum_Rel)) 
      standat$period<-all.Dates$Weeknum_Rel
    }
  }

# Add priors to "standat"
    standat$value_cauchyDF_sigma_eps_C<-value_cauchyDF_sigma_eps_C   
    standat$value_cauchyDF_sigma_eps_E<-value_cauchyDF_sigma_eps_E       
    standat$value_cauchyDF_sigma_r_E<-value_cauchyDF_sigma_r_E     
    standat$value_cauchyDF_sigma_r_C<-value_cauchyDF_sigma_r_C    
    standat$value_normal_sigma_omega_C_0<-value_normal_sigma_omega_C_0         
    standat$value_normal_sigma_omega_E_0<-value_normal_sigma_omega_E_0
    standat$value_normal_sigma_B1<-value_normal_sigma_B1      
    standat$value_lognormal_sigma_b<-value_lognormal_sigma_b                    
    standat$value_normal_mu_mu_C<-value_normal_mu_mu_C   
    standat$value_normal_sigma_mu_C<-value_normal_sigma_mu_C   
    standat$value_normal_mu_mu_E<-value_normal_mu_mu_E   
    standat$value_normal_sigma_mu_E<-value_normal_sigma_mu_E   
    standat$value_betashape_phi_E_scaled<-value_betashape_phi_E_scaled 
    standat$value_betashape_phi_C_scaled<-value_betashape_phi_C_scaled 