#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If model.source== "new", this file saves the time-series creel model results 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Save Model Results
          #Set working Direcotry   
            if(file.access(model.output.wd)!=0){dir.create(model.output.wd, showWarnings=TRUE)}  # Test and then create the directory
            setwd(model.output.wd)
            
            if(file.access(catch.group.of.interest)!=0){dir.create(catch.group.of.interest, showWarnings=TRUE)}  # Test and then create the directory
            setwd(catch.group.of.interest)
            
            if(file.access(model.name)!=0){dir.create(model.name, showWarnings=TRUE)}  # Test and then create the directory
            setwd(model.name)
            
            Directory<-paste(Sys.Date(), sep="")
            if(file.access(Directory)!=0){dir.create(Directory, showWarnings=TRUE)}  # Test and then create the directory
            setwd(Directory)
            
        #STAN summary file
          s.stan<-summary(res_Stan)
          write.csv(s.stan$summary,paste("results_stan", catch.group.of.interest, model.name, Sys.Date(), ".csv",sep="_"))
          saveRDS(res_Stan,file="results_r_file.rds")
          #save(res,file="results_extract_r_file.rda")
        
        #Extract posterior draws 
          res<-extract(res_Stan)  
          
        #Calculate loo-IC
          loo_IC<-loo(res$log_lik) # see: https://rdrr.io/cran/loo/man/loo.html and https://cran.r-project.org/web/packages/loo/loo.pdf 
          capture.output(loo_IC, file = paste("loo_IC", catch.group.of.interest, model.name, Sys.Date(), ".txt",sep="_"))  

        #Model summary
            mod.sum.elements<-c("Model", as.character(creel.models$Model_Name[creel.models$Model_number == model.number]), "Model Period", model_period
                                , "", c("")
                                ,"Chains",  n.chain, "Iterations", n.iter, "Warmup", n.warmup, "Thin Rate", n.thin, "Adapt_Delta", set.adapt_delta, "Max Tree Depth", max.tree, "Cores", n.cores

            )
            mod.sum<-setNames(as.data.frame(matrix(mod.sum.elements, nrow=length(mod.sum.elements)/2, ncol=2, byrow=TRUE)), c("Argument", "Sim_Input"))
            #write.table(mod.sum, file = paste("Model_set_up2", catch.group.of.interest, model.name, Sys.Date(), ".txt",sep="_"), row.names = F,  sep="\t", quote = F, col.names = T)
            capture.output(mod.sum, file = paste("Model_set_up", model.name, catch.group.of.interest, Sys.Date(), ".txt",sep="_"))
          
        #Model run-time
            run.time<-c("Approx. Run Time", round(approx.model.runtime.minutes,2))
            mod_run.time<-setNames(as.data.frame(matrix(run.time, nrow=length(run.time)/2, ncol=2, byrow=TRUE)), c("Argument", "Time_minutes"))
            capture.output(mod_run.time, file = paste("Model_Run_Time", model.name, catch.group.of.interest, Sys.Date(), ".txt",sep="_"))
            
        #Model warnings
            warnings<-warnings()
            if(length(warnings)>0){
              capture.output(summary(warnings), file = paste("Model_warnings", model.name, catch.group.of.interest, Sys.Date(), ".txt",sep="_"))
              #write.csv(warnings, paste("Model_warnings", catch.group.of.interest, model.name, Sys.Date(), ".csv",sep="_"))
              
            }else{
              warnings<-"there were no model warnings"
              capture.output(summary(warnings), file = paste("Model_warnings", model.name, catch.group.of.interest, Sys.Date(), ".txt",sep="_"))
              #write.csv(warnings, paste("Model_warnings", catch.group.of.interest, model.name, Sys.Date(), ".csv",sep="_"))
            }
          