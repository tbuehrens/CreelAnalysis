#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file prepares final dataset for creel (stan) model and captures model inputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  
# proportion tie-in expansion
  p_TI<-matrix(rep(1, standat$G * standat$S), nrow=standat$G, ncol=standat$S)
  standat$p_TI<-p_TI

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
    
# R session Info
  writeLines(capture.output(sessionInfo()), paste0(filepath_Run, "/info - sessionInfo.txt"))
  
# Capture standat object
  writeLines(capture.output(standat), paste0(filepath_Run, "/info - standat.txt"))
    
# Model summary
  mod.sum.elements<-
      c("Model Name"                   , as.character(creel_models$Model_Name[creel_models$Model_number == model_number])
      , "Model File"                   ,   model.file.name<-as.character(creel_models$Model_file_name[creel_models$Model_number == model_number])
      , "Model Period"                 , model_period
      , "Date Begin"                   , as.character(Date_Begin)
      , "Date End"                     , as.character(Date_End)
      , "Catch Group"                  , catch.group.of.interest
      , ""                             , c("")
      , "Chains"                       , n_chain
      , "Iterations"                   , n_iter
      , "Warmup"                       , n_warmup
      , "Thin Rate"                    , n_thin
      , "Adapt_Delta"                  , adapt_delta
      , "Max Tree Depth"               , max_treedepth
      , "Cores"                        , n_cores
      , ""                             , c("")
      , "value_cauchyDF_sigma_eps_C"   , value_cauchyDF_sigma_eps_C
      , "value_cauchyDF_sigma_eps_E"   , value_cauchyDF_sigma_eps_E
      , "value_cauchyDF_sigma_r_E"     , value_cauchyDF_sigma_r_E
      , "value_cauchyDF_sigma_r_C"     , value_cauchyDF_sigma_r_C
      , "value_normal_sigma_omega_C_0" , value_normal_sigma_omega_C_0 
      , "value_normal_sigma_omega_E_0" , value_normal_sigma_omega_E_0
      , "value_lognormal_sigma_b"      , value_lognormal_sigma_b
      , "value_normal_sigma_B1"        , value_normal_sigma_B1
      , "value_normal_mu_mu_C"         , round(value_normal_mu_mu_C, 3)
      , "value_normal_sigma_mu_C"      , round(value_normal_sigma_mu_C, 3) 
      , "value_normal_mu_mu_E"         , round(value_normal_mu_mu_E, 3) 
      , "value_normal_sigma_mu_E"      , round(value_normal_sigma_mu_E, 3)
      , "value_betashape_phi_E_scaled" , value_betashape_phi_E_scaled 
      , "value_betashape_phi_C_scaled" , value_betashape_phi_C_scaled 
  )
  mod.sum<-setNames(as.data.frame(matrix(mod.sum.elements, nrow=length(mod.sum.elements)/2, ncol=2, byrow=TRUE)), c("Argument", "Sim_Input"))
  writeLines(capture.output(mod.sum), paste0(filepath_Run, "/info - model setup.txt"))
    