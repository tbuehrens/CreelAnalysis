#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file either (1) loads models results (via an .rds file), or (2) runs a new model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on "model_number" chosen, create object of the "model.name"
    model.name<-as.character(creel_models$Model_Name[creel_models$Model_number == model_number])

# Load saved model results or...
if(model_source== "load_saved"){
  res_Stan <- readRDS(paste(filepath_modeloutputs, "results_res_Stan.rds", sep="/")) #Load RDS (stan model output)
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
}
#Extract posterior draws    
  res<-extract(res_Stan) 
 