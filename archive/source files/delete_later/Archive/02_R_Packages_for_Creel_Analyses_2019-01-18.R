#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file loads the R files that are necessary to run ALL creel analysis code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #NOTE: this files should be run before any other R source code files are run  


    if(!require(odbc))install.packages("odbc");   library("odbc") #needed to import data directly from Access database 
    #if(!require(pacman))install.packages("pacman");    library("pacman")
    if(!require(chron))install.packages("chron");library('chron')
    if(!require(suncalc))install.packages("suncalc");library('suncalc')
    if(!require(ProgGUIinR))install.packages("ProgGUIinR");library('ProgGUIinR')
    if(!require(plyr))install.packages("plyr");library('plyr')
    if(!require(tidyr))install.packages("tidyr");library('tidyr')
    if(!require(dplyr))install.packages("dplyr");library('dplyr')
    if(!require(rstan))install.packages("rstan");library('rstan')
    if(!require(shinystan))install.packages("shinystan");library('shinystan')
    if(!require(loo))install.packages("loo");library('loo')
    if(!require(data.table))install.packages("data.table");library('data.table')
    if(!require(RColorBrewer))install.packages("RColorBrewer");library("RColorBrewer")
    if(!require(reshape2))install.packages("reshape2"); library(reshape2)
    if(!require(lubridate))install.packages("lubridate"); library(lubridate)
    if(!require(MASS))install.packages("MASS"); library(MASS)

    #pacman ::p_load(jsonlite, RODBC, curl, lubridate)
    
  #Function to ask "not in"
    "%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 