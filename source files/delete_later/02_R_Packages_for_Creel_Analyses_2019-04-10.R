#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file loads the R packages and functions that are necessary to run ALL creel analysis code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #Load R packages
    if(!require(odbc))install.packages("odbc");   library("odbc") #needed to import data directly from Access database 
    if(!require(chron))install.packages("chron");library('chron')
    if(!require(suncalc))install.packages("suncalc");library('suncalc')
    #if(!require(ProgGUIinR))install.packages("ProgGUIinR");library('ProgGUIinR')
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
    if(!require(timeDate))install.packages("timeDate");library('timeDate')

  #Function to ask "not in"
    "%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 
    
  #Function "GetHolidays"
    GetHolidays <- function(x) { 
    years = as.POSIXlt(x)$year+1900 
    years = unique(years) 
    holidays <- NULL 
    for (y in years) { 
            holidays <- c(holidays, as.character(USNewYearsDay(y))) 
            holidays <- c(holidays, as.character(USMLKingsBirthday(y)))
            holidays <- c(holidays, as.character(USPresidentsDay(y)))
            holidays <- c(holidays, as.character(USMemorialDay(y))) 
            holidays <- c(holidays, as.character(USIndependenceDay(y))) 
            holidays <- c(holidays, as.character(USLaborDay(y))) 
            holidays <- c(holidays, as.character(USVeteransDay(y))) 
            holidays <- c(holidays, as.character(USThanksgivingDay(y))) 
            holidays <- c(holidays, as.character(timeDate(as.character(.nth.of.nday(y, 11, 5, 4))))) #Black Friday
            holidays <- c(holidays, as.character(USChristmasDay(y))) 
     } 
     holidays = as.Date(holidays,format="%Y-%m-%d") 
     #ans = x %in% holidays 
    return(holidays) 
} 
