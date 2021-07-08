#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file imports raw Skagit River creel survey files and formats them so that they can then be formatted for time-series analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#---------------------- -   
# FORMAT EFFORT DATA                                                                                      
#---------------------- -
# Import creel survey effort .csv data file
  effort.dat<-read.csv(paste(wd_data, effort_file_name, sep="/"), header=TRUE, na.strings=c("NA","NaN", "", " "))

# Remove data rows with no stream name <NAs> 
  effort.dat<-effort.dat[is.na(effort.dat$StreamName)==FALSE,]

#Create an index column
    effort.dat$Effort.Index<-0
    for(i in 1:nrow(effort.dat)){effort.dat$Effort.Index[i]<-i}
    head(effort.dat) 
  
#Format Date and Time data
      head(effort.dat)
    #Survey Date
        effort.dat$Survey_Date.Formatted<-as.Date(effort.dat$Survey_Date, format="%m/%d/%Y")
    #Survey Section Start Time  
        effort.dat$Effort_StartTime.Formatted<-format(strptime(paste(effort.dat$Survey_Date.Formatted, effort.dat$Effort_StartTime, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
        effort.dat$Decimal_Effort_Start_Time<-round(as.numeric(times(effort.dat$Effort_StartTime.Formatted))*24, 3)
    #Survey Section End Time    
        effort.dat$Effort_EndTime.Formatted<-format(strptime(paste(effort.dat$Survey_Date.Formatted, effort.dat$Effort_EndTime, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
        effort.dat$Decimal_Effort_End_Time<-round(as.numeric(times(effort.dat$Effort_EndTime.Formatted))*24, 3)

#Format count columns
    effort.dat$CountNum<-as.numeric(as.character(effort.dat$CountNum))
    effort.dat$Car_Count<-as.numeric(as.character(effort.dat$Car_Count))
    effort.dat$Trailer_Count<-as.numeric(as.character(effort.dat$Trailer_Count))
    effort.dat$Shore_No_Craft<-as.numeric(as.character(effort.dat$Shore_No_Craft))
    effort.dat$Shore_Boat_Motor<-as.numeric(as.character(effort.dat$Shore_Boat_Motor))
    effort.dat$Shore_Drift_Raft<-as.numeric(as.character(effort.dat$Shore_Drift_Raft))
    effort.dat$Shore_Pontoon<-as.numeric(as.character(effort.dat$Shore_Pontoon))
    effort.dat$Boat_Boat_Motor<-as.numeric(as.character(effort.dat$Boat_Boat_Motor))
    effort.dat$Boat_Drift_Raft<-as.numeric(as.character(effort.dat$Boat_Drift_Raft))

#Calculate total shore and total bank anglers from the tie-in survey data 
    effort.dat$Floating_Angler_Count<-as.numeric(effort.dat$Shore_Boat_Motor + effort.dat$Shore_Drift_Raft + effort.dat$Boat_Boat_Motor + effort.dat$Boat_Drift_Raft, na.rm = TRUE)
    effort.dat$Shore_Angler_Count<-as.numeric(effort.dat$Shore_No_Craft + effort.dat$Shore_Pontoon, na.rm = TRUE)
    head(effort.dat)

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

#Remove unnecessary columns and rename  
    effort.dat$Survey_Date<-NULL; 
    effort.dat$Survey_Start_Time<-NULL; effort.dat$Survey_End_Time<-NULL;
    effort.dat$Effort_StartTime<-NULL; effort.dat$Effort_EndTime<-NULL;
    effort.dat$Shore_No_Craft<-NULL;
    effort.dat$Shore_Boat_Motor<-NULL;
    effort.dat$Shore_Drift_Raft<-NULL;
    effort.dat$Shore_Pontoon<-NULL;
    effort.dat$Boat_Boat_Motor<-NULL;
    effort.dat$Boat_Drift_Raft<-NULL;

#Rename column headers
    names(effort.dat)[names(effort.dat) == 'Survey_Date.Formatted'] <- 'Date'
    names(effort.dat)[names(effort.dat) == 'Effort_StartTime.Formatted'] <- 'Effort_StartTime'
    names(effort.dat)[names(effort.dat) == 'Decimal_Effort_Start_Time'] <- 'Effort_StartTime_Dec'
    names(effort.dat)[names(effort.dat) == 'Effort_EndTime.Formatted'] <- 'Effort_EndTime'
    names(effort.dat)[names(effort.dat) == 'Decimal_Effort_End_Time'] <- 'Effort_EndTime_Dec'
    names(effort.dat)[names(effort.dat) == 'Car_Count'] <- 'Vehicles'
    names(effort.dat)[names(effort.dat) == 'Trailer_Count'] <- 'Trailers'
    names(effort.dat)[names(effort.dat) == 'Floating_Angler_Count'] <- 'Boat'
    names(effort.dat)[names(effort.dat) == 'Shore_Angler_Count'] <- 'Bank'

#Create a "HeaderID" column that denotes each unique survey date
    effort.dat<-effort.dat[order(effort.dat$Date, effort.dat$Survey.Type, effort.dat$Effort_StartTime),]
    HeaderID<-c()
    for(date in 1:length(unique(effort.dat$Date))){
        sub.date<-effort.dat[effort.dat$Date == unique(effort.dat$Date)[date],]
        sub.HeaderID<-rep(date, nrow(sub.date))
        HeaderID<-c(HeaderID, sub.HeaderID)
    }
    effort.dat$HeaderID<-HeaderID

#Order columns  
    effort.dat<-effort.dat[, c( "Effort.Index", "HeaderID", "StreamName",  "Date", "Surveyor", "Year", "YearGroup", "Season", "Month", "Month.no"
                       , "Weeknum", "j.date", "Day", "DayType"
                       , "Survey.Type", "SectionName", "CountNum", "Effort_StartTime", "Effort_StartTime_Dec", "Effort_EndTime", "Effort_EndTime_Dec"
                       , "Vehicles", "Trailers", "Boat", "Bank")]

#---------------------- -  
# FORMAT INTERVIEW DATA                                                                                      
#---------------------- -  
# Import creel survey effort and interview .csv data files
  group.dat<-read.csv(paste(wd_data, interview_file_name, sep="/"), header=TRUE)
        
# Remove data rows with no stream name <NAs> 
  group.dat<-group.dat[is.na(group.dat$StreamName)==FALSE,]

#Create an index column
  group.dat$Group.Index<-0
  for(i in 1:nrow(group.dat)){group.dat$Group.Index[i]<-i}

#Format Date and Time data
    #Survey Date      
        group.dat$Survey_Date.Formatted<-as.Date(group.dat$Survey_Date, format="%m/%d/%Y")
    #Fishing Start Time  
        group.dat$Fishing_Start_Time.Formatted<-format(strptime(paste(group.dat$Survey_Date.Formatted, group.dat$Fishing_Start_Time, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
        group.dat$Decimal_Fishing_Start_Time<-round(as.numeric(times(group.dat$Fishing_Start_Time.Formatted))*24, 3)
    #Fishing Interview Time    
        group.dat$Fishing_Interview_Time.Formatted<-format(strptime(paste(group.dat$Survey_Date.Formatted, group.dat$Interview_Start_Time, sep=" "), "%Y-%m-%d %I:%M %p"), '%H:%M:%S')
        group.dat$Decimal_Fishing_Interview_Time<-round(as.numeric(times(group.dat$Fishing_Interview_Time.Formatted))*24, 3)

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

#Format data types    
    group.dat$NumAnglers<-as.numeric(as.character(group.dat$NumAnglers))

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

#Remove unnecessary columns and rename  
    group.dat$Surveyor<-NULL; group.dat$Survey_Date<-NULL; group.dat$Page..<-NULL; group.dat$Row<-NULL; 
    group.dat$temp.GroupNum<-NULL
    group.dat$County1<-NULL; 
    group.dat$Comments<-NULL; 
    group.dat$Fishing_Start_Time<-NULL; group.dat$Interview_Start_Time<-NULL; group.dat$Fishing_End_Time<-NULL; 
    group.dat$Fishing_time_corrected<-NULL; group.dat$Fishing_End_Time_Corrected<-NULL;
    group.dat$Guided.<-NULL; 
    group.dat$AnglerType<-NULL;
    group.dat$TargetSpecies<-NULL;
    group.dat$FishingMethod<-NULL;

#Rename column headers
    names(group.dat)[names(group.dat) == 'Survey_Date.Formatted'] <- 'Date'
    names(group.dat)[names(group.dat) == 'CompleteTrip'] <- 'Trip.Status'
    names(group.dat)[names(group.dat) == 'Fishing_Start_Time.Formatted'] <- 'Start.Time'
    names(group.dat)[names(group.dat) == 'Decimal_Fishing_Start_Time'] <- 'Start.Time.Dec'
    names(group.dat)[names(group.dat) == 'Fishing_Interview_Time.Formatted'] <- 'Interview.Time'
    names(group.dat)[names(group.dat) == 'Decimal_Fishing_Interview_Time'] <- 'Interview.Time.Dec'
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
                            , "Start.Time", "Start.Time.Dec","Interview.Time","Interview.Time.Dec" #, "End.Time", "End.Time.Dec"
                            , "Percent_time_fished"
                            , "Trip.Status"
                            , "Species", "Origin", "Fate", "Count"
                           #, "Target.Spp"
                           # , "SectionName", "HeaderID"
      )
  ]

#---------------------- -  
# Create a "header.dat"                                                                                    
#---------------------- -      
#Extract pertenate columns from "effort.dat" to create "header.dat" DF
    effort.dat.for.header<-effort.dat[, c( "StreamName", "Date", "Year", "YearGroup", "Season", "Month", "Month.no"
                               , "Weeknum", "j.date", "Day", "DayType"
                               #, "Survey_StartTime", "Survey_StartTime_Dec", "Survey_EndTime", "Survey_EndTime_Dec" 
                               , "Survey.Type", "Surveyor"
                               #, "NoSurveyCode",  "Comments"
                               , "HeaderID"
                              )]

    header.dat<-effort.dat.for.header[!duplicated(effort.dat.for.header$HeaderID),]
