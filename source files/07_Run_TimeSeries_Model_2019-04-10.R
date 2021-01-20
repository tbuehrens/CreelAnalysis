#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file either (1) loads output from a previously run time-series creel model, or (2) runs the time-series creel model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      model.file.name<-as.character(creel_models$Model_file_name[creel_models$Model_number == model_number])
      model.name<-as.character(creel_models$Model_Name[creel_models$Model_number == model_number])

# Import saved model results or run new model (based on answers above)
  if(model_source== "saved"){
    
#KB - this section might need updating (2021-01-19)
    
      #Load RDS (stan model output)
          res_Stan <- readRDS(paste(wd_output_files, catch.group.of.interest, model.name, saved_model_date,"/results_r_file.rds", sep="/"))

      #Extract posterior draws
          res<-extract(res_Stan)
    
}else{
  
    #Last minute data fixes
      #standat$V_A[standat$gear_A==2]<-standat$T_A[standat$gear_A==2]
      #standat$O<-matrix(rep(standat$O,standat$S),ncol=standat$S)
      standat$V_A[standat$V_A>standat$A_A]<-standat$A_A[standat$V_A>standat$A_A]
      
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
      model<-stan_model(paste(wd_models, model.file.name, sep="/")) 
      
    #Run model in using NUTS/HMC
      start.time<-Sys.time()
      print(start.time)
      res_Stan<-sampling(object=model,data=standat, chains = n.chain, cores=n.cores,iter=n.iter,thin=n.thin, warmup=n.warmup,include=T,control=list(adapt_delta=set.adapt_delta, max_treedepth=12))
      end.time<-Sys.time()
      model.run.time<-print(paste("Elapsed Time = ",end.time-start.time,sep=""))
      print(elasped_time_by_chain<-get_elapsed_time(res_Stan))
      approx.model.runtime.minutes<-max(elasped_time_by_chain[,2])/60+max(elasped_time_by_chain[,1])/60

}