 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file either (1) loads models results (via an .rds file) results , or (2) runs a new model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on "model_number" chosen, create objects of the "model.file.name" and the "model.name"
      model.file.name<-as.character(creel_models$Model_file_name[creel_models$Model_number == model_number])
      model.name<-as.character(creel_models$Model_Name[creel_models$Model_number == model_number])

# Load saved model results or...
if(model_source== "load_saved"){
  res_Stan <- readRDS(paste(wd_output_files, catch.group.of.interest, model.name, saved_model_date,"results_r_file.rds", sep="/")) #Load RDS (stan model output)
  res<-extract(res_Stan) #Extract posterior draws
#...Run new model
}else{
  
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
              
#Compile Model
  start.time<-Sys.time(); print(start.time)
  message(paste("Compiling stan model"))
  model<-stan_model(paste(wd_models, model.file.name, sep="/")) 
  
#Run model in stan using NUTS/HMC
  message(paste("Running stan model - track progress via Viewer Pane"))
  res_Stan<-
    sampling(
      object=model
      ,data=standat
      , chains = n_chain
      , cores=n_cores
      , iter=n_iter
      , thin=n_thin
      , warmup=n_warmup
      , include=T
      , control=list(adapt_delta=adapt_delta , max_treedepth=max_treedepth)
    )
  end.time<-Sys.time()
  model_duration<-print(paste("Elapsed Time = ",end.time-start.time,sep=""))
  print(elasped_time_by_chain<-get_elapsed_time(res_Stan))
  approx.model.runtime.minutes<-max(elasped_time_by_chain[,2])/60+max(elasped_time_by_chain[,1])/60
  
# Generate summaries of model inputs and outputs
  source(paste0(wd_source_files, "/08a_Save_TimeSeries_Model_Output_2019-04-10.R"))
}