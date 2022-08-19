#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This code generates summaries files regarding the inputs and outputs for a new model run 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Model warnings #KB note: this doesn't appear to be working (not sure why)
#     if(length(model_warnings)>0){saved_model_warning<-model_warnings}else{saved_model_warning<-c("there were no model warnings")}
#     writeLines(capture.output(saved_model_warning), paste0(filepath_modeloutputs, "/info_Model_warnings_", Sys.Date(), ".txt"))

# R session Info
  writeLines(capture.output(sessionInfo()), paste0(filepath_modeloutputs, "/info_sessionInfo_", Sys.Date(), ".txt"))
    
# Model summary
  mod.sum.elements<-
      c("Model Name"                   , as.character(creel_models$Model_Name[creel_models$Model_number == model_number])
      , "Model File"                   , model.file.name
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
  writeLines(capture.output(mod.sum), paste0(filepath_modeloutputs, "/info_Model_setup_", Sys.Date(), ".txt"))

# Model run-time
  run.time<-c("Approx. Run Time", round(approx.model.runtime.minutes,2))
  mod_run.time<-setNames(as.data.frame(matrix(run.time, nrow=length(run.time)/2, ncol=2, byrow=TRUE)), c("Argument", "Time_minutes"))
  writeLines(capture.output(mod_run.time), paste0(filepath_modeloutputs, "/info_Model_Run_Time_", Sys.Date(), ".txt"))

# Save a file of the summary model results
  s.stan<-summary(res_Stan)
  write.csv(s.stan$summary, paste0(filepath_modeloutputs, "/results_res_Stan_summary_", Sys.Date(), ".csv"))
  
# Save "res_stan" object as .rds file   
  saveRDS(res_Stan, file=paste0(filepath_modeloutputs, "/results_res_Stan.rds"))

# Calculate loo-IC
  loo_IC<-loo(res$log_lik) # see: https://rdrr.io/cran/loo/man/loo.html and https://cran.r-project.org/web/packages/loo/loo.pdf 
  writeLines(capture.output(loo_IC), paste0(filepath_modeloutputs, "/results_model_looIC_", Sys.Date(), ".txt"))  
