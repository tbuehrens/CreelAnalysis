#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file analyzes roving-roving creel survey data using a Bayesian state-space model in STAN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE)) # clean workspace

#---------------------------------------------------------------------------------------------------------- -  
# (1) SPECIFY WORKING DIRECTORIES, SET SEED(S), AND LOAD R PACKAGES, FUNCTIONS, AND LOOK-UP TABLES (LUTs)----
#---------------------------------------------------------------------------------------------------------- -
# Specify working directory
      wd_LUT_files <-"lookup tables"  # Location of look-up tables (maybe could be merged with data files??)
      wd_data_files   <-"data"        # Location where data files are stored
      wd_source_files<-"source files" # Location of source file (code working "behind the scenes")
      wd_model_files  <-"models"      # Location of model files 
      wd_output_files<-"output" # Location of saved output (summary figures/tables and model results)

  # Load functions
    source(paste0(wd_source_files, "/01_Load_Functions.R"))
      
  # Load LUTs
    source(paste0(wd_source_files, "/01_Load_LUTs.R"))

# Load necessary packages (excluding BTSPAS -- see next section)
    package_list<-c("here", "plyr", "tidyverse", "lubridate", "devtools", "xlsx", "cowplot", "ggpubr", "chron", "suncalc", "rstan", "shinystan", "loo", "data.table", "RColorBrewer", "reshape2", "MASS", "timeDate")
    install_or_load_pack(package_list)    
 
#---------------------------------------------------------------------------------------------------------- -
# (3) IMPORT ALL CREEL SURVEY DATA AND FORMAT                                                            ####
#---------------------------------------------------------------------------------------------------------- - 
    # Import creel survey effort and interview .csv data files
        effort.dat<-read.csv(paste0(wd_data_files, "/03_Effort_dat - improved_tiein_entry - 2019_Skagit_creel_JSH_2-17-19.csv"), header=TRUE) #updated file name/version 3/4/2019 JSH
        interview.dat<-read.csv(paste0(wd_data_files, "/03_Interview-dat_hand-corr-times_2-28-2019.csv"), header=TRUE)
    
    # Quickly evaluate creel data files
        head(effort.dat)
        head(interview.dat)

    # Run source summary file 
      source(paste0(wd_source_files, "/03_Import_Creel_Data_and_Format_2019-04-08.R"))

#---------------------------------------------------------------------------------------------------------- -
# (4) DENOTE DATA OF INTEREST (i.e., data to extract and evaluate)                                       ####
#---------------------------------------------------------------------------------------------------------- - 
  # Extract by "YearGroup"?
      by.YearGroup<-"N" #Enter "Y" or "N" 
      unique(effort.dat$YearGroup) #YearGroup runs from May 1st - April 30th
      YearGroup.of.Interest<-c("2017-2018") 
      
  # Extract by "Season"? (Summer vs. Winter)
      by.Season<-"N" #Enter "Y" or "N" 
      unique(effort.dat$Season) #Summer (May 1st - Oct 31st), Winter (Nov. 1 - April 30)
      Season.of.Interest<-c("Winter")
  
  # Extract by "Year"?        
      by.Year<-"N" #Enter "Y" or "N" 
      unique(effort.dat$Year)
      Year.of.Interest<-c("2017") 
      
  # Extract by "StreamName"?
      by.StreamName<-"Y" #Enter "Y" or "N" 
      unique(effort.dat$StreamName)
      StreamName.of.Interest<-c("Skagit")
      
  # Extract by Date (Range)?
      by.Date<-"N" #Enter "Y" or "N" 
      #unique(effort.dat$Date)
      Begin.Date<-c("2016-05-01") #Format must be "yyyy-mm-dd"
      End.Date<-c("2017-03-31") #Format must be "yyyy-mm-dd"
      
  # Extract by Surveyor?
      by.Surveyor<-"N" #Enter "Y" or "N" 
      unique(effort.dat$Surveyor)
      Surveyor.of.Interest<-c("") 

#---------------------------------------------------------------------------------------------------------- -
# (5) EXTRACT DATA OF INTEREST AND CALCULATE FIELDS                                                      ----
#---------------------------------------------------------------------------------------------------------- -
  # Run source summary file    
    source(paste0(wd_source_files, "/05_Extract_Data_of_Interest_and_Calculate_Fields_2019-04-08.R"))  

#---------------------------------------------------------------------------------------------------------- -
# (6) SUMMARIZE EFFORT AND CATCH DATA FOR TIME-SERIES MODEL                                              ----
#---------------------------------------------------------------------------------------------------------- - 
  # Identify catch groupings of interest
      unique(sub.group.dat$Catch.Group[order(sub.group.dat$Catch.Group)])
      catch.group.of.interest<-c("SH_W_R") #ONLY SELECT ONE GROUPING AT A TIME
    
  # Identify first and last dates of fishery (i.e., over what continuous range of dates do we want to estimate effort and catch)
      range(sub.effort.dat$Date); #range of creel data dates (NOTE: this may not match the range of dates the fishery was open)
      Date_Begin<-min(sub.effort.dat$Date) #c("2018-04-01") #Define date as the first day for which you want to make an estimate for the fishery (not necessarily first survey date)
      Date_End<-  max(sub.effort.dat$Date) #c("2018-04-30")    
    
  # Identify dates when fishery was closed by section (i.e., there should not have been any effort or catch)
      # Total number of dates that at least one section of the river was closed
          total.closed.dates<-1
      
      # Final number of "sub-sections" we are breaking up the estimates by
          final.effort.section.xwalk
          
      # NOTE: if "total.closed.dates" >0, use the following format to enter closure dates and section, where :
            # the first column is the list of individual dates (by row) the fishery was closed date 
            # the number of additional columns equals the number of "final" sections based on "final.effort.section.xwalk" 
            # the enter the following values below each section:
            # Enter 1 if the section was open and enter 0 if the section was closed  
          
                                 #    Date   , Section-1, Section-2
          closed.Dates.Sections<-c("2019-02-11",     0,         0 )


  #Run source summary file 
    source(paste0(wd_source_files, "/06_Summarize_Effort_and_Catch_Data_for_TimeSeries_Model_2019-04-08.R"))

#---------------------------------------------------------------------------------------------------------- -
# (7) RUN/LOAD CREEL MODEL                                                                               ----
#---------------------------------------------------------------------------------------------------------- - 
  # Denote whether you want to run a "new" model or load "saved" results from a previous model run
      model_source<-c("new")  #enter either "saved" or "new"
      saved_model_date<-c("2019-05-03") #If loading a "saved" model (.RDS file), enter the date that data were exported (note: this date will be the folder name)     
          
  # Denote which creel model you want to run
      creel_models[,1:3] #model summary table
      model_number<-c(1)
      
  # Specify time period to stratify data by - day vs. week 
      model_period<-c("day") #enter "day" or "week"
      
  # Specify parameter values for model priors
      value_cauchyDF_sigma_eps_C = 1 # the hyperhyper scale (degrees of freedom) parameter in the hyperprior distribution sigma_eps_C; default = 1  
      value_cauchyDF_sigma_eps_E = 1 # the hyperhyper scale (degrees of freedom) parameter in the hyperprior distribution sigma_eps_E; default = 1  
      value_cauchyDF_sigma_r_E = 1  # the hyperhyper scale (degrees of freedom) parameter in the hyperprior distribution sigma_r_E; default = 1  
      value_cauchyDF_sigma_r_C = 1  # the hyperhyper scale (degrees of freedom) parameter in the hyperprior distribution sigma_r_C; default = 1  
      value_normal_sigma_omega_C_0 = 1  #the SD hyperparameter in the prior distribution omega_C_0; normal sd (log-space); default = 1   
      value_normal_sigma_omega_E_0 =  3 # the SD hyperparameter in the prior distribution omega_E_0; normal sd (log-space);; default = 3  
      value_lognormal_sigma_b = 1 # the SD hyperparameter in the prior distribution b; default = 1  
      value_normal_sigma_B1 = 5 # the SD hyperparameter in the prior distribution B1; default = 5  
      value_normal_mu_mu_C = log(0.02) # the mean hyperparameter in the prior distribution mu_C; median (log-space); default = 0.02 (was originally  0.05) 
      value_normal_sigma_mu_C = 1.5 # the SD hyperparameter in the prior distribution mu_C; normal sd (log-space); default = 1.5 (was originally 5)
      value_normal_mu_mu_E = log(15) # the mean hyperparameter in the prior distribution mu_E; median effort (log-space); default = 15 
      value_normal_sigma_mu_E = 2  # the SD hyperparameter in the prior distribution mu_E; normal sd (log-space); default = 2 (was originally 5) 
      value_betashape_phi_E_scaled = 1 # the rate (alpha) and shape (beta) hyperparameters in phi_E_scaled; default = 1 (i.e., beta(1,1) which is uniform), alternative beta(2,2) 
      value_betashape_phi_C_scaled = 1 # the rate (alpha) and shape (beta) hyperparameters in phi_C_scaled; default = 1 (i.e., beta(1,1) which is uniform), alternative beta(2,2)
    
  # Specific Stan model arguments
      n.chain<-4            # number of Markov chains. The default is 4.
      n.iter<-200        # number of iterations for each chain (including warmup). The default is 2000.
      n.cores<-4           #Number of cores to use when executing the chains in parallel. The defaults is 1.Stan manual recommends setting the mc.cores option to be as many processors as the hardware and RAM allow (up to the number of chains).
      n.warmup<-100       # number of warmup (aka burnin) iterations per chain.  The default is n.iter/2.  
      n.thin<-1          # the period for saving samples. The default is 1, which is usually the recommended value.
      set.adapt_delta<-0.90 # (originally 0.95) the target average proposal acceptance probability during Stan's adaptation period. Value between 0 and 1.  Increasing it will force Stan to take smaller steps (and thus run time will be longer). The default is 0.8.
      max.tree<-12
      
  # Run either (1) "new" time-series model or (2) load previously run model output
      source(paste0(wd_source_files, "/07_Run_TimeSeries_Model_2019-04-10.R"))

#---------------------------------------------------------------------------------------------------------- -
# (8) SUMMARIZE MODEL OUTPUTS AND SAVE                                                                   ----
#---------------------------------------------------------------------------------------------------------- - 
  # Create summary output files and save model results (only runs if "new" model) 
      if(model_source== "new"){source(paste0(wd_source_files, "/08a_Save_TimeSeries_Model_Output_2019-04-10.R"))}

  # Create plots and tables of results
      source(paste0(wd_source_files, "/08b_Summarize_Model_Output_Figures_and_Tables_2019-04-10.R")) 
      
  # Look at some of the model output       
      print(names(res)) #list with named components corresponding to the model parameters
      dim(res$lambda_E_S)

  # Evaluate model results with "Shinystan" 
      launch_shinystan(res_Stan)
    
