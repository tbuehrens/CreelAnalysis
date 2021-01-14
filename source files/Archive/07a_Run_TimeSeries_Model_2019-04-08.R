#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file either (1) loads output from a previously run time-series creel model, or (2) runs the time-series creel model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      model.file.name<-as.character(creel.models$Model_file_name[creel.models$Model_number == model.number])
      model.name<-as.character(creel.models$Model_Name[creel.models$Model_number == model.number])

# Import saved model results or run new model (based on answers above)
if(model.source== "saved"){
      setwd(model.output.wd)
      setwd(catch.group.of.interest)
      setwd(model.name)
      setwd(model.date)
      
  #Load RDS (stan model output)    
      res_Stan <- readRDS("results_r_file.rds")
              
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
      standat$prior_sigma_C<-prior_sigma_C   
      standat$prior_sigma_E<-prior_sigma_E       
      standat$prior_sigma_r_E<-prior_sigma_r_E     
      standat$prior_sigma_r_C<-prior_sigma_r_C    
      standat$prior_omega_C_0<-prior_omega_C_0         
      standat$prior_omega_E_0<-prior_omega_E_0
      standat$prior_B1<-prior_B1      
      standat$prior_b<-prior_b                    
      standat$prior_mu_C_mu<-prior_mu_C_mu   
      standat$prior_mu_C_sd<-prior_mu_C_sd   
      standat$prior_mu_E_mu<-prior_mu_E_mu   
      standat$prior_mu_E_sd<-prior_mu_E_sd   
      standat$prior_phi_E_prior<-prior_phi_E_prior 
      standat$prior_phi_C_prior<-prior_phi_C_prior 
              
    #Compile Model
      model<-stan_model(model.file.name) 
      
    #Run model in using NUTS/HMC
      start.time<-Sys.time()
      print(start.time)
      res_Stan<-sampling(object=model,data=standat, chains = n.chain, cores=n.cores,iter=n.iter,thin=n.thin, warmup=n.warmup,include=T,control=list(adapt_delta=set.adapt_delta, max_treedepth=12))
      end.time<-Sys.time()
      model.run.time<-print(paste("Elapsed Time = ",end.time-start.time,sep=""))
      print(elasped_time_by_chain<-get_elapsed_time(res_Stan))
      approx.model.runtime.minutes<-max(elasped_time_by_chain[,2])/60+max(elasped_time_by_chain[,1])/60

}