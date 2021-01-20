#This file identifies holidays for specified years of interest (see "timeDate" package for list of potential holidays)

if(!require(timeDate))install.packages("timeDate");library('timeDate')

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


