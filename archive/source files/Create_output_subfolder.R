#---------------------------------------------------------------------------------------- -    
# This file creates sub-folders to save model information, outputs, and summarized results
#---------------------------------------------------------------------------------------- - 
# outputs
  ifelse(!dir.exists(wd_outputs), {dir.create(wd_outputs); "Output sub-folder created"},"Output sub-folder exists already")
    
# catch group  
  filepath_catchgroup<-paste(wd_outputs, catch.group.of.interest, sep="/")
  ifelse(!dir.exists(filepath_catchgroup), {dir.create(filepath_catchgroup); "Catch group sub-folder created"},"Catch group sub-folder exists already")

# model run  
  filepath_Run<-paste(filepath_catchgroup, paste0("Run_", Model_Run), sep="/")
  ifelse(!dir.exists(filepath_Run), {dir.create(filepath_Run); "Model run sub-folder created"},"Model run sub-folder exists already")

# model information and outputs    
  filepath_modeloutputs<-paste(filepath_Run, "model_outputs", sep="/")
  ifelse(!dir.exists(filepath_modeloutputs), {dir.create(filepath_modeloutputs); "Model info sub-folder created"},"Model info sub-folder exists already")

# summarized estimates  
  filepath_modelestimates<-paste(filepath_Run, "summarized_estimates", sep="/")
  ifelse(!dir.exists(filepath_modelestimates), {dir.create(filepath_modelestimates); "Model estimates sub-folder created"},"Model info sub-folder exists already")
