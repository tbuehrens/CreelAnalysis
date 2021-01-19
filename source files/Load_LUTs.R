#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file loads the look-up tables (LUTs) that are necessary to run analysis code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    effort_xwalk<-read.csv(paste0(wd_LUT_files, "/02_Crosswalk_Table_for_Index_TieIn_Sections_2019-01-10.csv"), header=TRUE) #Effort Section table
    river_loc<-read.csv(paste0(wd_LUT_files, "/02_River.Locations_2019-01-07.csv"), header=TRUE) #River Location table
    creel_models<-read.csv(paste0(wd_LUT_files, "/02_Creel_Models_2019-04-12.csv"), header=TRUE) #Creel model file names and descriptions
