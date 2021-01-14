#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If model_source== "new", this file saves the time-series creel model results 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Save Model Results
          #Set working Direcotry   
            if(file.access(wd_output_files)!=0){dir.create(wd_output_files, showWarnings=TRUE)}  # Test and then create the directory
            filepath_catchgroup<-paste(wd_output_files, catch.group.of.interest, sep="/")
            
            if(file.access(filepath_catchgroup)!=0){dir.create(filepath_catchgroup, showWarnings=TRUE)}  # Test and then create the directory
            filepath_model.name<-paste(filepath_catchgroup, model.name, sep="/")
            
            if(file.access(filepath_model.name)!=0){dir.create(filepath_model.name, showWarnings=TRUE)}  # Test and then create the directory
            new_model_run_date<-paste(Sys.Date(), sep="")
            filepath_date<-paste(filepath_model.name, new_model_run_date, sep="/")
            
            if(file.access(filepath_date)!=0){dir.create(filepath_date, showWarnings=TRUE)}  # Test and then create the directory
            #setwd(Directory)
            
        #Save "sessionInfo"
          writeLines(capture.output(sessionInfo()), paste0(filepath_date, "/sessionInfo.txt"))
            
        #STAN summary file
          s.stan<-summary(res_Stan)
          write.csv(s.stan$summary, paste(filepath_date, paste("results_stan", catch.group.of.interest, model.name, Sys.Date(), ".csv",sep="_"), sep="/"))
          saveRDS(res_Stan, file=paste0(filepath_date, "/results_r_file.rds"))

        #Extract posterior draws 
          res<-extract(res_Stan)  
          
        #Calculate loo-IC
          loo_IC<-loo(res$log_lik) # see: https://rdrr.io/cran/loo/man/loo.html and https://cran.r-project.org/web/packages/loo/loo.pdf 
          writeLines(capture.output(loo_IC), paste(filepath_date, paste("loo_IC", catch.group.of.interest, model.name, Sys.Date(), ".txt",sep="_"), sep="/"))  

        #Model summary
            mod.sum.elements<-c("Model", as.character(creel_models$Model_Name[creel_models$Model_number == model_number]), "Model Period", model_period
                                , "Date Begin", as.character(Date_Begin), "Date End", as.character(Date_End), "Catch Group", catch.group.of.interest
                                , "", c("")
                                ,"Chains",  n.chain, "Iterations", n.iter, "Warmup", n.warmup, "Thin Rate", n.thin, "Adapt_Delta", set.adapt_delta, "Max Tree Depth", max.tree, "Cores", n.cores
                                , "", c("")
                                , "value_cauchyDF_sigma_eps_C", value_cauchyDF_sigma_eps_C
                                , "value_cauchyDF_sigma_eps_E", value_cauchyDF_sigma_eps_E
                                , "value_cauchyDF_sigma_r_E", value_cauchyDF_sigma_r_E
                                , "value_cauchyDF_sigma_r_C", value_cauchyDF_sigma_r_C
                                , "value_normal_sigma_omega_C_0", value_normal_sigma_omega_C_0 
                                , "value_normal_sigma_omega_E_0",  value_normal_sigma_omega_E_0
                                , "value_lognormal_sigma_b", value_lognormal_sigma_b
                                , "value_normal_sigma_B1", value_normal_sigma_B1
                                , "value_normal_mu_mu_C", round(value_normal_mu_mu_C, 3)
                                , "value_normal_sigma_mu_C", round(value_normal_sigma_mu_C, 3) 
                                , "value_normal_mu_mu_E", round(value_normal_mu_mu_E, 3) 
                                , "value_normal_sigma_mu_E", round(value_normal_sigma_mu_E, 3)
                                , "value_betashape_phi_E_scaled", value_betashape_phi_E_scaled 
                                , "value_betashape_phi_C_scaled",  value_betashape_phi_C_scaled 
            )
            mod.sum<-setNames(as.data.frame(matrix(mod.sum.elements, nrow=length(mod.sum.elements)/2, ncol=2, byrow=TRUE)), c("Argument", "Sim_Input"))
            #write.table(mod.sum, file = paste("Model_set_up2", catch.group.of.interest, model.name, Sys.Date(), ".txt",sep="_"), row.names = F,  sep="\t", quote = F, col.names = T)
            writeLines(capture.output(mod.sum), paste(filepath_date, paste("Model_set_up", model.name, catch.group.of.interest, Sys.Date(), ".txt", sep="_"), sep="/"))
          
        #Model run-time
            run.time<-c("Approx. Run Time", round(approx.model.runtime.minutes,2))
            mod_run.time<-setNames(as.data.frame(matrix(run.time, nrow=length(run.time)/2, ncol=2, byrow=TRUE)), c("Argument", "Time_minutes"))
            writeLines(capture.output(mod_run.time), paste(filepath_date, paste("Model_Run_Time", model.name, catch.group.of.interest, Sys.Date(), ".txt",sep="_"), sep="/"))
            
        #Model warnings
            warnings<-warnings()
            if(length(warnings)>0){
              writeLines(capture.output(summary(warnings)), paste(filepath_date, paste("Model_warnings", model.name, catch.group.of.interest, Sys.Date(), ".txt",sep="_"), sep="/"))
              #write.csv(warnings, paste("Model_warnings", catch.group.of.interest, model.name, Sys.Date(), ".csv",sep="_"))
              
            }else{
              warnings<-"there were no model warnings"
              writeLines(capture.output(summary(warnings)), paste(filepath_date, paste("Model_warnings", model.name, catch.group.of.interest, Sys.Date(), ".txt",sep="_"), sep="/"))
              #write.csv(warnings, paste("Model_warnings", catch.group.of.interest, model.name, Sys.Date(), ".csv",sep="_"))
            }
          