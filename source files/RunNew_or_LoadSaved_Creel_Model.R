#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file either (1) loads models results (via an .rds file), or (2) runs a new model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on "model_number" chosen, create object of the "model.name"
    model.name<-as.character(creel_models$Model_Name[creel_models$Model_number == model_number])

# Load saved model results or...
if(model_source== "load_saved"){
  res_Stan <- readRDS(paste(filepath_Run, "results - res_Stan.rds", sep="/")) #Load RDS (stan model output)
  res<-extract(res_Stan)  #Extract posterior draws   
#...Run new model
}else{

#Compile Model
  start.time<-Sys.time(); print(start.time)
  message(paste("Compiling stan model"))
    model.file.name<-as.character(creel_models$Model_file_name[creel_models$Model_number == model_number])
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
      , init="0"
      , warmup=n_warmup
      , include=T
      , control=list(adapt_delta=adapt_delta , max_treedepth=max_treedepth)
    )
  end.time<-Sys.time()
  model_duration<-print(paste("Elapsed Time = ",end.time-start.time,sep=""))
  print(elasped_time_by_chain<-get_elapsed_time(res_Stan))
  approx.model.runtime.minutes<-max(elasped_time_by_chain[,2])/60+max(elasped_time_by_chain[,1])/60
  
#Save Model warnings
  print(warnings())
  model_warnings<-warnings() #KB note: this doesn't appear to be working (not sure why)
 
# # Model warnings #KB note: this doesn't appear to be working (not sure why)
#     if(length(model_warnings)>0){saved_model_warning<-model_warnings}else{saved_model_warning<-c("there were no model warnings")}
#     writeLines(capture.output(saved_model_warning), paste0(filepath_Run, "/info - model warnings.txt"))

# Save a file of the summary model results
  s.stan<-summary(res_Stan)
  write.csv(s.stan$summary, paste0(filepath_Run, "/results - res_Stan.csv"))
  
# Save "res_stan" object as .rds file   
  saveRDS(res_Stan, file=paste0(filepath_Run, "/results - res_Stan.rds"))

# Model run-time
  run.time<-c("Approx. Run Time", round(approx.model.runtime.minutes,2))
  mod_run.time<-setNames(as.data.frame(matrix(run.time, nrow=length(run.time)/2, ncol=2, byrow=TRUE)), c("Argument", "Time_minutes"))
  writeLines(capture.output(mod_run.time), paste0(filepath_Run, "/info - model run time.txt"))
  
#Extract posterior draws    
  res<-extract(res_Stan) 

# Calculate loo-IC
  loo_IC<-loo(res$log_lik) # see: https://rdrr.io/cran/loo/man/loo.html and https://cran.r-project.org/web/packages/loo/loo.pdf 
  writeLines(capture.output(loo_IC), paste0(filepath_Run, "/results -looIC.txt"))  
}

   
 