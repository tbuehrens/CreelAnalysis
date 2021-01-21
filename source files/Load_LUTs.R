#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file loads the look-up tables (LUTs) that are necessary to run analysis code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    effort_xwalk<-read.csv(file.path(wd_LUTs,effort_xwalk_filename)) #Effort Section table
    river_loc<-read.csv(file.path(wd_LUTs,river_loc_filename)) #River Location table
    creel_models<-read.csv(file.path(wd_LUTs,creel_models_filename))#Creel model file names and descriptions
