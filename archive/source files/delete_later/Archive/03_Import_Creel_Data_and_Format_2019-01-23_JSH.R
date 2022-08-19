#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file imports raw Skagit River creel survey files and formats them so that they can then be formatted for time-series analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#NOTE: Must run the following files before the code below will work:

      # #---------------------------------------------------------------------------------------------------------- -  
      # # (2) LOAD R PACKAGES, FUNCTIONS, AND X-WALK TABLES                                                       ----
      # #---------------------------------------------------------------------------------------------------------- -


#---------------------------------------------------------------------------------------------------------- -
# (3A) IMPORT CREEL DATA                                                                                  ####
#---------------------------------------------------------------------------------------------------------- -
  #bring in data from .csv files  
    # effort.dat<-read.csv("03_Effort_dat - 2018_Skagit_creel_2019-01-10.csv", header=TRUE)
    # head(effort.dat)
    # interview.dat<-read.csv("03_Interview_dat - 2018_Skagit_creel_2019-01-10.csv", header=TRUE)
    # head(interview.dat)
    
  #rename "interview.dat" file as "group.dat"
    group.dat<-interview.dat
    head(group.dat)
 
  # # Select data source - data can be imported directly from creel database OR from a previously data base import that were saved as a .Rdata (working space) file
  #     creel.data.source<-c("Saved")     #Enter either "Direct" or "Saved"
  #     data.export.date<-c("2018-12-06") #If loading a "Saved" .Rdata file, enter the date that data exported (note: this date will be saved in the .Rdata file name)  
  #   
  # # Set working directory where data base exports (i.e,. ".Rdata" files) are saved           
  #     setwd("~/BentleyKT/Projects/Creel surveys/Analysis/Source files")
  #     
  # # Define the Path to Access database where source data resides
  #     dbPath <- "S:/FP/Reg5/Database/Tributary_Creel/TRIBUTARY_CREEL_BACK_END.accdb"    
  #   
  # # Import creel data 
  #     if(creel.data.source == "Direct"){
  #         # Set significant digits to 14 for lat/lon under the options function
  #           options(digits=14)
  # 
  #         # Pull new list values from Access
  #           con <- odbcConnectAccess2007(dbPath) #Open connection to Access database
  #           effort_summary = sqlQuery(con, as.is = TRUE, "SELECT * FROM qryEffortSummary;");
  #           group_summary = sqlQuery(con, as.is = TRUE, "SELECT * FROM qryGroupSummary")
  #           gearfish_summary = sqlQuery(con, as.is = TRUE, "SELECT * FROM qryGearFishSummary;")
  #           close(con) #Close connection to Access database
  #       
  #         #Rename data frames (DFs)
  #           effort.dat<-as.data.frame(apply(effort_summary, 2, function(x) gsub("^$|^ $", NA, x)))
  #           group.dat<-as.data.frame(apply(group_summary, 2, function(x) gsub("^$|^ $", NA, x)))
  #           gearfish.dat<-as.data.frame(apply(gearfish_summary, 2, function(x) gsub("^$|^ $", NA, x)))
  #       
  #         #Save work space
  #           save.image(file=paste("03_Creel.database.Export.From_", as.Date(Sys.time()),".Rdata",sep=""))
  #     } else{
  #         #Load work space data
  #           load(paste("03_Creel.database.Export.From_", data.export.date,".Rdata", sep=""))
  #     }

#------------------------------------------------------------------------------------------------------------      


#------------------------------------------------------------------------------------------------------------    
    
#---------------------------------------------------------------------------------------------------------- -   
# (3B) FORMAT DATA                                                                                         ####
#---------------------------------------------------------------------------------------------------------- -
  # Examine data for rows with <NAs> 
      effort.dat[is.na(effort.dat$Stream)==TRUE,]
      effort.dat<-effort.dat[is.na(effort.dat$Stream)==FALSE,]
      
      interview.dat[is.na(interview.dat$Stream)==TRUE,]
      interview.dat<-effort.dat[is.na(interview.dat$Stream)==FALSE,]
      head(interview.dat)
      
                                            # group.dat[is.na(group.dat$Stream)==TRUE,]
                                            # group.dat<-group.dat[is.na(group.dat$Stream)==FALSE,]
                                            # 
                                            # gearfish.dat[is.na(gearfish.dat$Stream)==TRUE,]
                                            # gearfish.dat<-gearfish.dat[is.na(gearfish.dat$Stream)==FALSE,]
    
  # Define YearGroups 
        #This uses day-of-year (j.date) which needs to be looked up 
        #Note: Jan.1st = 1, Feb. 1= 32, etc.
        #Leap Year will change j.date, so be aware of this if +/- 1 day matters
    
  # j.date "YearGroup" begins and ends
      YearBegin<-121 #121 = May 1st (in a non-leap year)
    
  # Define seasons (summer vs. winter)
      summerBegin<-121 #121 = May 1st (in a non-leap year)
      summerEnd<-304 #304 = Oct. 31st (in a non-leap year)
      winterBegin<-305 #305 = Nov. 1st (in a non-leap year)
      winterEnd<-120 #120 = Apr 30th (in a non-leap year)
    
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # format "effort.dat" data
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    #Remove data rows with no "EffortID" (i.e., EffortID is blank) - these rows are merely a product of the query and only consist of header data... 
    #...(i.e., no effort count data).  For example, now "supplemental" interview data will not have effort counts (and thus no EffortID)
    #NOTE: even after removing these rows there are still data rows with no effort count data and/or times - these are due to either:
        # (1) No effort count conducted (i.e., missing both times and counts) - (e.g., this happened with many of the Klickitat surveys)
        # (2) A sub-set of rows from #1 should have information listed in the "NoSurveyCode" column explaining why effort count data were not collected
        # (3) Effort data exist, but Start and End times were not recorded
        # (4) An unknown reason - these need to be rectified before analyzing the data
      # nrow(effort.dat[is.na(effort.dat$EffortID)==TRUE,])
      # effort.dat<-effort.dat[is.na(effort.dat$EffortID)==FALSE,]
      
    #Create an index column
        effort.dat$Effort.Index<-0
        for(i in 1:nrow(effort.dat)){effort.dat$Effort.Index[i]<-i}
        head(effort.dat) 
      
    #Format Date and Time data
          head(effort.dat)
        #Survey Date
            effort.dat$Survey_Date.Formatted<-as.Date(effort.dat$Survey_Date, format="%m/%d/%Y")
        # #Survey Start Time
        #     effort.dat$Survey_Start_Time.Formatted<-format(strptime(paste(effort.dat$Survey_Date.Formatted, effort.dat$Survey_Start_Time, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
        #     effort.dat$Decimal_Survey_Start_Time<-round(as.numeric(times(effort.dat$Survey_Start_Time.Formatted))*24, 3)
        # #Survey End Time    
        #     effort.dat$Survey_End_Time.Formatted<-format(strptime(paste(effort.dat$Survey_Date.Formatted, effort.dat$Survey_End_Time, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
        #     effort.dat$Decimal_Survey_End_Time<-round(as.numeric(times(effort.dat$Survey_End_Time.Formatted))*24, 3)
        #Survey Section Start Time  
            effort.dat$Effort_StartTime.Formatted<-format(strptime(paste(effort.dat$Survey_Date.Formatted, effort.dat$Effort_StartTime, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
            effort.dat$Decimal_Effort_Start_Time<-round(as.numeric(times(effort.dat$Effort_StartTime.Formatted))*24, 3)
        #Survey Section End Time    
            effort.dat$Effort_EndTime.Formatted<-format(strptime(paste(effort.dat$Survey_Date.Formatted, effort.dat$Effort_EndTime, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
            effort.dat$Decimal_Effort_End_Time<-round(as.numeric(times(effort.dat$Effort_EndTime.Formatted))*24, 3)
          head(effort.dat)
   
        
          
          #****************
          #modified by Jillian Howard 3/4/2019 to adjust for new tie-in survey data format
          #****************
    #Format count columns
        effort.dat$CountNum<-as.numeric(as.character(effort.dat$CountNum))
        effort.dat$Car_Count<-as.numeric(as.character(effort.dat$Car_Count))
        effort.dat$Trailer_Count<-as.numeric(as.character(effort.dat$Trailer_Count))
        #effort.dat$Number_Floating_Craft<-as.numeric(as.character(effort.dat$Number_Floating_Craft))
        #effort.dat$Number_Boats<-as.numeric(as.character(effort.dat$Number_Boats))
        #effort.dat$Number_Car_Toppers<-as.numeric(as.character(effort.dat$Number_Car_Toppers))
        #effort.dat$Floating_Angler_Count<-as.numeric(as.character(effort.dat$Floating_Angler_Count))
        #effort.dat$Shore_Angler_Count<-as.numeric(as.character(effort.dat$Shore_Angler_Count))
        #effort.dat$Total_Angler_Count<-as.numeric(as.character(effort.dat$Total_Angler_Count))
        effort.dat$Shore_No_Craft<-as.numeric(as.character(effort.dat$Shore_No_Craft))
        effort.dat$Shore_Boat_Motor<-as.numeric(as.character(effort.dat$Shore_Boat_Motor))
        effort.dat$Shore_Drift_Raft<-as.numeric(as.character(effort.dat$Shore_Drift_Raft))
        effort.dat$Shore_Pontoon<-as.numeric(as.character(effort.dat$Shore_Pontoon))
        effort.dat$Boat_Boat_Motor<-as.numeric(as.character(effort.dat$Boat_Boat_Motor))
        effort.dat$Boat_Drift_Raft<-as.numeric(as.character(effort.dat$Boat_Drift_Raft))
        
        # effort.dat$VehicleCt<-as.numeric(as.character(effort.dat$VehicleCt))
        # effort.dat$VehicleTrailerCt<-as.numeric(as.character(effort.dat$VehicleTrailerCt))
        # effort.dat$BoatAnglers<-as.numeric(as.character(effort.dat$BoatAnglers))
        # effort.dat$BankAnglers<-as.numeric(as.character(effort.dat$BankAnglers))
        
        #Calculate total shore and total bank anglers from the tie-in survey data 
        effort.dat$Floating_Angler_Count<-as.numeric(effort.dat$Shore_Boat_Motor + effort.dat$Shore_Drift_Raft + effort.dat$Boat_Boat_Motor + effort.dat$Boat_Drift_Raft)
        effort.dat$Shore_Angler_Count<-as.numeric(effort.dat$Shore_No_Craft + effort.dat$Shore_Pontoon)
        head(effort.dat)
        
        #***************
        #***************
        
        
      
    #Identify holidays
        all.holidays<-GetHolidays(unique(effort.dat$Survey_Date.Formatted))
      
    #Create new columns for day, month, week, YearGroup, and Season 
        effort.dat<-effort.dat %>% mutate(
                      Day = weekdays(Survey_Date.Formatted) 
                    , DayType = if_else(weekdays(Survey_Date.Formatted)=="Saturday" | weekdays(Survey_Date.Formatted)=="Sunday" | Survey_Date.Formatted%in%all.holidays, "Weekend", "Weekday")
                    , Month = format(Survey_Date.Formatted,"%b")
                    , Month.no = as.numeric(format(Survey_Date.Formatted, "%m"))
                    , Year = as.numeric(format(Survey_Date.Formatted, "%Y"))
                    , Weeknum = as.numeric(format(Survey_Date.Formatted, "%V"))
                    , j.date =  as.numeric(format(Survey_Date.Formatted, "%j"))
                    , YearGroup = if_else(as.numeric(format(Survey_Date.Formatted, "%j")) >= YearBegin, paste(as.numeric(format(Survey_Date.Formatted, "%Y")), as.numeric(format(Survey_Date.Formatted, "%Y"))+1, sep="-"), paste(as.numeric(format(Survey_Date.Formatted, "%Y"))-1, as.numeric(format(Survey_Date.Formatted, "%Y")), sep="-"))
                    , Season = if_else(as.numeric(format(Survey_Date.Formatted, "%j")) >= summerBegin & as.numeric(format(Survey_Date.Formatted, "%j"))<=summerEnd, "Summer", "Winter")
          )
        effort.dat[1:20, ]

    #Create new column and change formatting for "TieInSurvey" data
        # unique(effort.dat$TieInSurvey)
        # effort.dat$Survey.Type<-999
        # 
        # for(i in 1:nrow(effort.dat)){
        #   ifelse(effort.dat$TieInSurvey[i]== "0", effort.dat$Survey.Type[i]<-"Creel", effort.dat$Survey.Type[i]<-"TieIn")
        # }

        
        
            #**********************
            #**********************
    #Remove unnecessary columns and rename  
        effort.dat$Survey_Date<-NULL; 
        effort.dat$Survey_Start_Time<-NULL; effort.dat$Survey_End_Time<-NULL;
        effort.dat$Effort_StartTime<-NULL; effort.dat$Effort_EndTime<-NULL;
        #effort.dat$Weather<-NULL; effort.dat$Flow<-NULL;
        # effort.dat$CreatedDateTime<-NULL; effort.dat$CreatedBy<-NULL; effort.dat$EffortID<-NULL; effort.dat$HeaderID.1<-NULL;
        # effort.dat$CreatedDateTime.1<-NULL; effort.dat$CreatedBy.1<-NULL; effort.dat$TieInSurvey<-NULL;
        
        #*******************
        #modified by Jillian Howard 3/4/2019 to adjust for new tie-in survey data format
        #*******************
        
        effort.dat$Shore_No_Craft<-NULL;
        effort.dat$Shore_Boat_Motor<-NULL;
        effort.dat$Shore_Drift_Raft<-NULL;
        effort.dat$Shore_Pontoon<-NULL;
        effort.dat$Boat_Boat_Motor<-NULL;
        effort.dat$Boat_Drift_Raft<-NULL;
        
        #******************
        #******************
        
    #Rename column headers
        names(effort.dat)[names(effort.dat) == 'Survey_Date.Formatted'] <- 'Date'
        # names(effort.dat)[names(effort.dat) == 'Survey_Start_Time.Formatted'] <- 'Survey_StartTime'
        # names(effort.dat)[names(effort.dat) == 'Decimal_Survey_Start_Time'] <- 'Survey_StartTime_Dec'
        # names(effort.dat)[names(effort.dat) == 'Survey_End_Time.Formatted'] <- 'Survey_EndTime'
        # names(effort.dat)[names(effort.dat) == 'Decimal_Survey_End_Time'] <- 'Survey_EndTime_Dec'
        
        names(effort.dat)[names(effort.dat) == 'Effort_StartTime.Formatted'] <- 'Effort_StartTime'
        names(effort.dat)[names(effort.dat) == 'Decimal_Effort_Start_Time'] <- 'Effort_StartTime_Dec'
        names(effort.dat)[names(effort.dat) == 'Effort_EndTime.Formatted'] <- 'Effort_EndTime'
        names(effort.dat)[names(effort.dat) == 'Decimal_Effort_End_Time'] <- 'Effort_EndTime_Dec'
        
        names(effort.dat)[names(effort.dat) == 'Car_Count'] <- 'Vehicles'
        names(effort.dat)[names(effort.dat) == 'Trailer_Count'] <- 'Trailers'
        names(effort.dat)[names(effort.dat) == 'Floating_Angler_Count'] <- 'Boat'
        names(effort.dat)[names(effort.dat) == 'Shore_Angler_Count'] <- 'Bank'
        
        head(effort.dat)
        
    #Create a "HeaderID" column that denotes each unique survey date
        #effort.dat$HeaderID<-c(9999)
        effort.dat<-effort.dat[order(effort.dat$Date, effort.dat$Survey.Type, effort.dat$Effort_StartTime),]
        HeaderID<-c()
        for(date in 1:length(unique(effort.dat$Date))){
            sub.date<-effort.dat[effort.dat$Date == unique(effort.dat$Date)[date],]
            sub.HeaderID<-rep(date, nrow(sub.date))
            HeaderID<-c(HeaderID, sub.HeaderID)
        }
        effort.dat$HeaderID<-HeaderID
head(effort.dat)
effort.dat[1000:1030,]

    #Order columns  
        effort.dat<-effort.dat[, c( "Effort.Index", "HeaderID", "StreamName",  "Date", "Surveyor", "Year", "YearGroup", "Season", "Month", "Month.no"
                           , "Weeknum", "j.date", "Day", "DayType"
                           , "Survey.Type", "SectionName", "CountNum", "Effort_StartTime", "Effort_StartTime_Dec", "Effort_EndTime", "Effort_EndTime_Dec"
                           , "Vehicles", "Trailers", "Boat", "Bank")]
        
        # effort.dat<-effort.dat[, c( "Effort.Index", "StreamName",  "Date", "Surveyor", "Year", "YearGroup", "Season", "Month", "Month.no"
        #                            , "Weeknum", "j.date", "Day", "DayType",  "Survey_StartTime", "Survey_StartTime_Dec", "Survey_EndTime", "Survey_EndTime_Dec"
        #                            , "Survey.Type", "SectionName", "CountNum", "Effort_StartTime", "Effort_StartTime_Dec", "Effort_EndTime", "Effort_EndTime_Dec"
        #                            , "Vehicles", "Trailers", "Boat", "Bank", "NoSurveyCode", "Comments", "HeaderID")]
        head(effort.dat)
        
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # format "group.dat" data
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # # Remove data rows with no "GroupID" (i.e., GroupID is blank)
    # # These rows are a result of:
    # #  (1) TieIn Surveys (generates a blank "group" row even though no anglers were ever going to be interviewed)
    # #  (2) No angler interview days (creel survey occurred but no anglers were ever interviewed)
    #     nrow(group.dat[is.na(group.dat$GroupNum)==TRUE,])
    #     length(unique(group.dat$Survey_Date[is.na(group.dat$GroupNum)==TRUE]))
    #     group.dat<-group.dat[is.na(group.dat$GroupNum)==FALSE,]
      
    #Create an index column
        group.dat$Group.Index<-0
        for(i in 1:nrow(group.dat)){group.dat$Group.Index[i]<-i}
        head(group.dat)  
      
    #Format Date and Time data
            head(group.dat);names(group.dat)
        #Survey Date      
            group.dat$Survey_Date.Formatted<-as.Date(group.dat$Survey_Date, format="%m/%d/%Y")
        #Fishing Start Time  
            group.dat$Fishing_Start_Time.Formatted<-format(strptime(paste(group.dat$Survey_Date.Formatted, group.dat$Fishing_Start_Time, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
            group.dat$Decimal_Fishing_Start_Time<-round(as.numeric(times(group.dat$Fishing_Start_Time.Formatted))*24, 3)
        #Fishing Interview Time    
            group.dat$Fishing_Interview_Time.Formatted<-format(strptime(paste(group.dat$Survey_Date.Formatted, group.dat$Interview_Start_Time, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
            group.dat$Decimal_Fishing_Interview_Time<-round(as.numeric(times(group.dat$Fishing_Interview_Time.Formatted))*24, 3)
        #Fishing End Time    
            group.dat$Fishing_End_Time.Formatted<-format(strptime(paste(group.dat$Survey_Date.Formatted, group.dat$Fishing_End_Time, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
            group.dat$Decimal_Fishing_End_Time<-round(as.numeric(times(group.dat$Fishing_End_Time.Formatted))*24, 3)
      
    #Create temporary "GroupNum" column
        for(row in 1:nrow(group.dat)){group.dat$temp.GroupNum[row]<-paste(group.dat$Page..[row], group.dat$Row[row], sep = "-")}
    
    #Create x-walk table for "GroupNum" 
        group.num_xwalk<-setNames(as.data.frame(matrix(NA, nrow=0, ncol=3)), c("Date", "temp.GroupNum", "GroupNum"))    
        for(date in 1:length(unique(group.dat$Survey_Date.Formatted))){
            sub.date<-group.dat[group.dat$Survey_Date.Formatted == unique(group.dat$Survey_Date.Formatted)[date],]
            
            temp.GroupNum<-unique(sub.date$temp.GroupNum)
            new.GroupNum<-seq(1, length(temp.GroupNum),1)
            sub.date<-as.character(rep(unique(group.dat$Survey_Date.Formatted)[date], length(temp.GroupNum)))
            sub.group.num_xwalk<-setNames(as.data.frame(cbind(sub.date, temp.GroupNum, new.GroupNum)), c("Date", "temp.GroupNum", "GroupNum"))
            
            group.num_xwalk<-rbind(group.num_xwalk, sub.group.num_xwalk)
        }
    
    #Create "GroupNum" column
        group.dat$GroupNum<-NA
        for(row in 1:nrow(group.dat)){
          group.dat$GroupNum[row]<-as.character(group.num_xwalk$GroupNum[as.Date(group.num_xwalk$Date) == as.Date(group.dat$Survey_Date.Formatted)[row] & as.character(group.num_xwalk$temp.GroupNum) == as.character(group.dat$temp.GroupNum)[row]])
          
        }

        head(group.dat)
        
        
    #Format data types    
        #group.dat$GroupNum<-as.numeric(as.character(group.dat$GroupNum))
        #group.dat$ZipCode<-as.numeric(as.character(group.dat$ZipCode))
        group.dat$NumAnglers<-as.numeric(as.character(group.dat$NumAnglers))
        #group.dat$NumPoles<-as.numeric(as.character(group.dat$NumPoles))
      
    #Create new columns for day, month, week, YearGroup, and Season 
        group.dat<-group.dat %>% mutate(
                    Day = weekdays(Survey_Date.Formatted) 
                  , DayType = if_else(weekdays(Survey_Date.Formatted)=="Saturday" | weekdays(Survey_Date.Formatted)=="Sunday" | Survey_Date.Formatted%in%all.holidays, "Weekend", "Weekday")
                  , Month = format(Survey_Date.Formatted,"%b")
                  , Month.no = as.numeric(format(Survey_Date.Formatted, "%m"))
                  , Year = as.numeric(format(Survey_Date.Formatted, "%Y"))
                  , Weeknum = as.numeric(format(Survey_Date.Formatted, "%V"))
                  , j.date =  as.numeric(format(Survey_Date.Formatted, "%j"))
                  , YearGroup = if_else(as.numeric(format(Survey_Date.Formatted, "%j")) >= YearBegin, paste(as.numeric(format(Survey_Date.Formatted, "%Y")), as.numeric(format(Survey_Date.Formatted, "%Y"))+1, sep="-"), paste(as.numeric(format(Survey_Date.Formatted, "%Y"))-1, as.numeric(format(Survey_Date.Formatted, "%Y")), sep="-"))
                  , Season = if_else(as.numeric(format(Survey_Date.Formatted, "%j")) >= summerBegin & as.numeric(format(Survey_Date.Formatted, "%j"))<=summerEnd, "Summer", "Winter")
        )
      group.dat[1:20, ]  

    #Remove unnecessary columns and rename  
        group.dat$Surveyor<-NULL; group.dat$Survey_Date<-NULL; group.dat$Page..<-NULL; group.dat$Row<-NULL; 
        #group.dat$SectionName<-NULL;
        #group.dat$Survey_Start_Time<-NULL; group.dat$Survey_End_Time<-NULL; 
        #group.dat$UpperRM<-NULL; group.dat$LowerRM<-NULL;  
        #group.dat$Weather<-NULL; group.dat$Flow<-NULL; 
        group.dat$temp.GroupNum<-NULL
        group.dat$County1<-NULL; 
        group.dat$Comments<-NULL; 
        #group.dat$TieInSurvey<-NULL; 
        #group.dat$CreatedDateTime<-NULL; group.dat$CreatedBy<-NULL; group.dat$GroupID<-NULL; group.dat$HeaderID.1<-NULL;
        #group.dat$ZipCode<-NULL; group.dat$FishingLocation<-NULL; 
        group.dat$Fishing_Start_Time<-NULL; group.dat$Interview_Start_Time<-NULL; group.dat$Fishing_End_Time<-NULL; 
        #group.dat$CRCArea<-NULL; 
        group.dat$Guided.<-NULL; 
        group.dat$AnglerType<-NULL;
        group.dat$TargetSpecies<-NULL;
        group.dat$FishingMethod<-NULL;
        #group.dat$CreatedDateTime.1<-NULL; group.dat$CreatedBy.1<-NULL

    #Rename column headers
        names(group.dat)[names(group.dat) == 'Survey_Date.Formatted'] <- 'Date'
        names(group.dat)[names(group.dat) == 'CompleteTrip'] <- 'Trip.Status'
        names(group.dat)[names(group.dat) == 'Fishing_Start_Time.Formatted'] <- 'Start.Time'
        names(group.dat)[names(group.dat) == 'Decimal_Fishing_Start_Time'] <- 'Start.Time.Dec'
        names(group.dat)[names(group.dat) == 'Fishing_Interview_Time.Formatted'] <- 'Interview.Time'
        names(group.dat)[names(group.dat) == 'Decimal_Fishing_Interview_Time'] <- 'Interview.Time.Dec'
        names(group.dat)[names(group.dat) == 'Fishing_End_Time.Formatted'] <- 'End.Time'
        names(group.dat)[names(group.dat) == 'Decimal_Fishing_End_Time'] <- 'End.Time.Dec'
        #names(group.dat)[names(group.dat) == 'SpeciesCode'] <- 'Target.Spp'
        names(group.dat)[names(group.dat) == 'AnglerCategory'] <- 'Angler.Type'
        
        names(group.dat)[names(group.dat) == 'Num_Cars'] <- 'Vehicles'
        names(group.dat)[names(group.dat) == 'Num_Trailers'] <- 'Trailers'
        names(group.dat)[names(group.dat) == 'Qty'] <- 'Count'
    
    #Rename "Trip.Status" values
        group.dat$Trip.Status<-as.character(group.dat$Trip.Status)  
        group.dat$Trip.Status[group.dat$Trip.Status == "Yes"]<-"C"
        group.dat$Trip.Status[group.dat$Trip.Status == "No"]<-"I"
        group.dat$Trip.Status[group.dat$Trip.Status == "UNK"]<-"I"
      
    #Rename "Angler.Type" values
        group.dat$Angler.Type<-as.character(group.dat$Angler.Type) 
        group.dat$Angler.Type[group.dat$Angler.Type == "Shore"]<-"S"
        group.dat$Angler.Type[group.dat$Angler.Type == "Boat"]<-"B"
        
    #Replace "Count" values entered as "NA" to "0"
        group.dat[c("Count")][is.na(group.dat[c("Count")])] <- 0
        
    #Order columns  
        group.dat<-group.dat[, c("Group.Index", "StreamName", "SectionName", "Date", "Year", "YearGroup", "Season", "Month", "Month.no"
                                , "Weeknum", "j.date", "Day", "DayType", "GroupNum"
                                ,"Angler.Type"
                               #, "FishFromBoat", "NumPoles"
                                , "NumAnglers", "Vehicles", "Trailers"
                                , "Start.Time", "Start.Time.Dec","Interview.Time","Interview.Time.Dec", "End.Time", "End.Time.Dec"
                                , "Trip.Status"
                                , "Species", "Origin", "Fate", "Count"
                               #, "Target.Spp"
                               # , "SectionName", "HeaderID"
          )
                            ]
        head(group.dat)

  # #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # # format "gearfish.dat" data                                               
  # #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  #   #Remove data rows with no "GroupNum" (i.e., GroupNum is blank)
  #       nrow(gearfish.dat[is.na(gearfish.dat$GroupNum)==TRUE,])
  #       gearfish.dat<-gearfish.dat[is.na(gearfish.dat$GroupNum)==FALSE,]
  #   
  #   #Create an index column
  #       gearfish.dat$GearFish.Index<-0
  #       for(i in 1:nrow(gearfish.dat)){gearfish.dat$GearFish.Index[i]<-i}
  #       head(gearfish.dat) 
  #     
  #   #Format Date and Time data
  #       head(gearfish.dat);names(gearfish.dat)
  #       gearfish.dat$Survey_Date.Formatted<-as.Date(gearfish.dat$Survey_Date, format="%m/%d/%Y")
  #       #gearfish.dat$CreatedDateTime<-as.POSIXlt(gearfish.dat$CreatedDateTime, format="%Y-%m-%d %H:%M:%S")
  #     
  #   #Formata data types
  #       gearfish.dat$GroupNum<-as.numeric(as.character(gearfish.dat$GroupNum))
  #       gearfish.dat$HookCt<-as.numeric(as.character(gearfish.dat$HookCt))
  #       gearfish.dat$HookPoints<-as.numeric(as.character(gearfish.dat$HookPoints))
  #       gearfish.dat$Count<-as.numeric(as.character(gearfish.dat$Count))
  # 
  #   #Create new columns for day, month, week, YearGroup, and Season 
  #       gearfish.dat<-gearfish.dat %>% mutate(
  #                   Day = weekdays(Survey_Date.Formatted) 
  #                 , DayType = if_else(weekdays(Survey_Date.Formatted)=="Saturday" | weekdays(Survey_Date.Formatted)=="Sunday" | Survey_Date.Formatted%in%all.holidays, "Weekend", "Weekday")
  #                 , Month = format(Survey_Date.Formatted,"%b")
  #                 , Month.no = as.numeric(format(Survey_Date.Formatted, "%m"))
  #                 , Year = as.numeric(format(Survey_Date.Formatted, "%Y"))
  #                 , Weeknum = as.numeric(format(Survey_Date.Formatted, "%V"))
  #                 , j.date =  as.numeric(format(Survey_Date.Formatted, "%j"))
  #                 , YearGroup = if_else(as.numeric(format(Survey_Date.Formatted, "%j")) >= YearBegin, paste(as.numeric(format(Survey_Date.Formatted, "%Y")), as.numeric(format(Survey_Date.Formatted, "%Y"))+1, sep="-"), paste(as.numeric(format(Survey_Date.Formatted, "%Y"))-1, as.numeric(format(Survey_Date.Formatted, "%Y")), sep="-"))
  #                 , Season = if_else(as.numeric(format(Survey_Date.Formatted, "%j")) >= summerBegin & as.numeric(format(Survey_Date.Formatted, "%j"))<=summerEnd, "Summer", "Winter")
  #       )
  #       gearfish.dat[1:20, ]    
  # 
  #   #Remove unnecessary columns and rename  
  #       gearfish.dat$GearFishID<-NULL; gearfish.dat$GroupID<-NULL; gearfish.dat$Surveyor<-NULL;
  #       gearfish.dat$Survey_Date<-NULL; gearfish.dat$Survey_Start_Time<-NULL; gearfish.dat$Survey_End_Time<-NULL; 
  #       gearfish.dat$Weather<-NULL; gearfish.dat$Flow<-NULL; gearfish.dat$Comments<-NULL; gearfish.dat$TieInSurvey<-NULL; 
  #       gearfish.dat$ReleasedBleeding<-NULL; gearfish.dat$CreatedDateTime<-NULL; gearfish.dat$CreatedBy<-NULL;
  # 
  #   #Rename column headers
  #     names(gearfish.dat)[names(gearfish.dat) == 'MethodCode'] <- 'Method'
  #     names(gearfish.dat)[names(gearfish.dat) == 'GearCode'] <- 'Gear.Type'
  #     names(gearfish.dat)[names(gearfish.dat) == 'SpeciesCode'] <- 'Species'
  #     names(gearfish.dat)[names(gearfish.dat) == 'OriginCode'] <- 'Origin'
  #     names(gearfish.dat)[names(gearfish.dat) == 'LifeStageCode'] <- 'LifeStage'
  #     names(gearfish.dat)[names(gearfish.dat) == 'RunCode'] <- 'Run'
  #     names(gearfish.dat)[names(gearfish.dat) == 'SexCode'] <- 'Sex'
  #     names(gearfish.dat)[names(gearfish.dat) == 'Comments.1'] <- 'Comments'
  #     names(gearfish.dat)[names(gearfish.dat) == 'Survey_Date.Formatted'] <- 'Date'
  #     
  #   #Drop Gear and Supplementation Information (columns)?
  #     drop.gear.data<-"Y"  #Enter "Y" (Yes) or "N" (No)
  #     if(drop.gear.data=="Y"){
  #         gearfish.dat$Method<-NULL; gearfish.dat$Gear.Type<-NULL; gearfish.dat$Barbed<-NULL; gearfish.dat$HookCt<-NULL; gearfish.dat$HookPoints<-NULL; 
  #         gearfish.dat$HookSize<-NULL; gearfish.dat$CRCArea<-NULL; gearfish.dat$Gutted<-NULL; gearfish.dat$FLcm<-NULL; gearfish.dat$HookLocReported<-NULL; 
  #         gearfish.dat$HookLocActual<-NULL; gearfish.dat$CWT<-NULL; gearfish.dat$PIT<-NULL; gearfish.dat$DNA<-NULL; gearfish.dat$Sex<-NULL;
  #         gearfish.dat$FloyTag1<-NULL; gearfish.dat$FloyTag2<-NULL; gearfish.dat$TagColor<-NULL; gearfish.dat$PITcode<-NULL; gearfish.dat$MarkTypeCode<-NULL;
  #         gearfish.dat$SNID<-NULL; gearfish.dat$ScaleCardNumber<-NULL; gearfish.dat$ScaleCardPosition<-NULL; gearfish.dat$ScaleAge<-NULL;
  #         gearfish.dat$ScalePattern<-NULL; gearfish.dat$CWTTagCode<-NULL; gearfish.dat$ImageLink<-NULL;
  #     }
  #     head(gearfish.dat)
  # 
  #   #Order columns (This will need to be updated if "drop.gear.data" <- "N")
  #     gearfish.dat<-gearfish.dat[, c("GearFish.Index", "StreamName", "Date", "Year", "YearGroup", "Season", "Month", "Month.no"
  #                                    , "Weeknum", "j.date", "Day", "DayType"
  #                                    , "GroupNum", "Species", "Count", "Origin", "LifeStage", "Run", "Fate", "Comments", "HeaderID"
  #                                    )]
  #     head(gearfish.dat)
  #     nrow(gearfish.dat[is.na(gearfish.dat$Count)==TRUE,])
  #     nrow(gearfish.dat[is.na(gearfish.dat$Count)==FALSE,])
  #     
  #   #Combine "Species", "Origin", and "Fate" to create a unique "Catch.Group" index
  #     for(row in 1:nrow(gearfish.dat)){
  #       gearfish.dat$Catch.Group[row]<-paste(gearfish.dat$Species[row], gearfish.dat$Origin[row], gearfish.dat$Fate[row], sep = "_")
  #     }   
  #     
  #   unique(gearfish.dat$Catch.Group[order(gearfish.dat$Catch.Group)])    

  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # Create a "header.dat" data frame (DF)
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    #Extract pertenate columns from "effort.dat" to create "header.dat" DF
        effort.dat.for.header<-effort.dat[, c( "StreamName", "Date", "Year", "YearGroup", "Season", "Month", "Month.no"
                                   , "Weeknum", "j.date", "Day", "DayType"
                                   #, "Survey_StartTime", "Survey_StartTime_Dec", "Survey_EndTime", "Survey_EndTime_Dec" 
                                   , "Survey.Type", "Surveyor"
                                   #, "NoSurveyCode",  "Comments"
                                   , "HeaderID"
                                  )]

        header.dat<-effort.dat.for.header[!duplicated(effort.dat.for.header$HeaderID),]
        nrow(header.dat)
        head(header.dat)

    # #Remove unnecessary rows from "effort.dat.formatted" (included with "header.dat" and not needed)
    #   effort.dat$Survey_StartTime<-NULL; effort.dat$Survey_StartTime_Dec<-NULL; effort.dat$Survey_EndTime<-NULL; effort.dat$Survey_EndTime_Dec<-NULL;effort.dat$Comments<-NULL
      head(effort.dat)
        
#------------------------------------------------------------------------------------------------------------     
      
      