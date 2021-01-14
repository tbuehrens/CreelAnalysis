#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file extract the creel survey data of interest and calculates additional fields of information to be used later
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#NOTE: Must run the following files before the code below will work:

      # #---------------------------------------------------------------------------------------------------------- -  
      # # (2) LOAD R PACKAGES, FUNCTIONS, AND X-WALK TABLES                                                       ----
      # #---------------------------------------------------------------------------------------------------------- -

      # #---------------------------------------------------------------------------------------------------------- -
      # # (3) IMPORT ALL CREEL SURVEY DATA AND FORMAT                                                            ####
      # #---------------------------------------------------------------------------------------------------------- -      

      #---------------------------------------------------------------------------------------------------------- -
      # (4) DENOTE DATA OF INTEREST (i.e., data to extract and evaluate)                                       ####
      #---------------------------------------------------------------------------------------------------------- - 


#---------------------------------------------------------------------------------------------------------- -
# (5A) EXTRACT DATA OF INTEREST                                                                            ----
#---------------------------------------------------------------------------------------------------------- - 
  # create new DFs for each data group that will represent subsetted data set 
    effort.dat.formatted<-effort.dat
    group.dat.formatted<-group.dat
    # gearfish.dat.formatted<-gearfish.dat
    header.dat.formatted<-header.dat
    
  #Extract by "YearGroup"?
    if(by.YearGroup == "Y"){
        sub.effort.dat<-effort.dat.formatted[effort.dat.formatted$YearGroup %in% YearGroup.of.Interest,]
        sub.group.dat<-group.dat.formatted[group.dat.formatted$YearGroup %in% YearGroup.of.Interest,]
        # sub.gearfish.dat<-gearfish.dat.formatted[gearfish.dat.formatted$YearGroup %in% YearGroup.of.Interest,]
        sub.header.dat<-header.dat.formatted[header.dat.formatted$YearGroup %in% YearGroup.of.Interest,]
    }else{
        sub.effort.dat<-effort.dat.formatted
        sub.group.dat<-group.dat.formatted
        # sub.gearfish.dat<-gearfish.dat.formatted
        sub.header.dat<-header.dat.formatted
    }

  #Extract by "Season"? (Summer vs. Winter)
    if(by.Season == "Y"){
      sub.effort.dat<-sub.effort.dat[sub.effort.dat$Season %in% Season.of.Interest,]
      sub.group.dat<-sub.group.dat[sub.group.dat$Season %in% Season.of.Interest,]
      # sub.gearfish.dat<-sub.gearfish.dat[sub.gearfish.dat$Season %in% Season.of.Interest,]
      sub.header.dat<-sub.header.dat[sub.header.dat$Season %in% Season.of.Interest,]
    }  
    
  #Extract by "Year"?
    if(by.Year == "Y"){
      sub.effort.dat<-sub.effort.dat[sub.effort.dat$Year %in% as.numeric(Year.of.Interest),]
      sub.group.dat<-sub.group.dat[sub.group.dat$Year %in% as.numeric(Year.of.Interest),]
      # sub.gearfish.dat<-sub.gearfish.dat[sub.gearfish.dat$Year %in% as.numeric(Year.of.Interest),]
      sub.header.dat<-sub.header.dat[sub.header.dat$Year %in% as.numeric(Year.of.Interest),]
    }
    
  #Extract by "StreamName"?
    if(by.StreamName == "Y"){
      sub.effort.dat<-sub.effort.dat[sub.effort.dat$StreamName %in% StreamName.of.Interest,]
      sub.group.dat<-sub.group.dat[sub.group.dat$StreamName %in% StreamName.of.Interest,]
      # sub.gearfish.dat<-sub.gearfish.dat[sub.gearfish.dat$StreamName %in% StreamName.of.Interest,]
      sub.header.dat<-sub.header.dat[sub.header.dat$StreamName %in% StreamName.of.Interest,]
    }
  
  #Extract by Date (Range)?
    sub.Date.Range<-seq(as.Date(Begin.Date), as.Date(End.Date), 1)
    if(by.Date == "Y"){
      sub.effort.dat<-sub.effort.dat[sub.effort.dat$Date %in% sub.Date.Range,]
      sub.group.dat<-sub.group.dat[sub.group.dat$Date %in% sub.Date.Range,]
      # sub.gearfish.dat<-sub.gearfish.dat[sub.gearfish.dat$Date %in% sub.Date.Range,]
      sub.header.dat<-sub.header.dat[sub.header.dat$Date %in% sub.Date.Range,]
    }  
    
  #Extract by Surveyor?
    if(by.Surveyor == "Y"){
      sub.effort.dat<-sub.effort.dat[sub.effort.dat$Surveyor %in% Surveyor.of.Interest,]
      sub.group.dat<-sub.group.dat[sub.group.dat$Surveyor %in% Surveyor.of.Interest,]
      # sub.gearfish.dat<-sub.gearfish.dat[sub.gearfish.dat$Surveyor %in% Surveyor.of.Interest,]
      sub.header.dat<-sub.header.dat[sub.header.dat$Surveyor %in% Surveyor.of.Interest,]
    }

#------------------------------------------------------------------------------------------------------------   
  
#---------------------------------------------------------------------------------------------------------- - 
# (5B) CALCULATE FIELDS                                                                                    ####
#---------------------------------------------------------------------------------------------------------- -    
    #Julian to Calendar Date Table
        month.one<-as.data.frame(matrix(as.character(seq.Date(as.Date(paste(as.numeric(format(min(sub.header.dat$Date), "%Y")), format(min(sub.header.dat$Date), "%m"), "01", sep="-"))
                                                              , as.Date(paste(as.numeric(format(max(sub.header.dat$Date), "%Y")), format(max(sub.header.dat$Date)+30, "%m"), "01", sep="-")),
                                                              by="month"))))
        names(month.one)<-c("Date")
        month.one$j.date<-(as.numeric(format(as.Date(month.one$Date), "%j")))
        month.one$month.day<-format(as.Date(month.one$Date), "%b-%d")
        month.one$month<-format(as.Date(month.one$Date), "%b")
        month.one  
    
    #Define "Section.Name" and "Section.Num" based on cross-walk table
      #Effort data  
        sub.effort.dat$final.Section.Name<-NA; sub.effort.dat$Section.Num<-NA; 
        for(row in 1:nrow(sub.effort.dat)){
          sub.effort.dat$final.Section.Name[row]<-as.character(effort_xwalk$Section.Name[as.character(effort_xwalk$Section.Field) == as.character(sub.effort.dat$SectionName[row]) & as.character(effort_xwalk$YearGroup) == as.character(sub.effort.dat$YearGroup[row])])
          sub.effort.dat$Section.Num[row]<-effort_xwalk$Section.Summ[as.character(effort_xwalk$Section.Field) == as.character(sub.effort.dat$SectionName[row]) & as.character(effort_xwalk$YearGroup) == as.character(sub.effort.dat$YearGroup[row])]
        }  

      #Interview data
        unique(sub.group.dat$SectionName); table(sub.group.dat$SectionName)
        #NOTE: 14 groups had their fishing section denoted as "Both".  Since this is not an option in the lookup table and we want to define as either Skagit or Sauk, I arbitrarily defined the section for these groups as being Skagit
        sub.group.dat[sub.group.dat$SectionName == "Both",]
        sub.group.dat$SectionName[sub.group.dat$SectionName == "Both"]<-"Dalles bridge to Marblemount bridge"
        sub.group.dat$final.Section.Name<-NA; sub.group.dat$Section.Num<-NA
        for(row in 1:nrow(sub.group.dat)){
          sub.group.dat$final.Section.Name[row]<-as.character(effort_xwalk$Section.Name[as.character(effort_xwalk$Section.Field) == as.character(sub.group.dat$SectionName[row]) & as.character(effort_xwalk$YearGroup) == as.character(sub.group.dat$YearGroup[row])])
          sub.group.dat$Section.Num[row]<-effort_xwalk$Section.Summ[as.character(effort_xwalk$Section.Field) == as.character(sub.group.dat$SectionName[row]) & as.character(effort_xwalk$YearGroup) == as.character(sub.group.dat$YearGroup[row])]
        }  

    #Create index effort x-walk table (that will help define "outage" dates by section in "master" creel analysis file)
          sub.river.effort.xwalk<-effort_xwalk[effort_xwalk$StreamName %in% unique(sub.effort.dat$StreamName), ]
          final.effort.section.xwalk<-setNames(as.data.frame(matrix(NA, nrow=length(unique(sub.river.effort.xwalk$Section.Name)), ncol=2)), c("Section.Name", "Section.Num"))
          for(section in 1:length(unique(sub.river.effort.xwalk$Section.Name))){
            final.effort.section.xwalk[section, "Section.Name"]<-as.character(unique(sub.river.effort.xwalk$Section.Name[sub.river.effort.xwalk$Section.Name == unique(sub.river.effort.xwalk$Section.Name)[section]]))
            final.effort.section.xwalk[section, "Section.Num"] <-unique(sub.river.effort.xwalk$Section.Summ[sub.river.effort.xwalk$Section.Name == unique(sub.river.effort.xwalk$Section.Name)[section]])
          }
          final.effort.section.xwalk
              
    # #Calculate time to complete effort count by section
    #     sub.effort.dat$Count.Time.Minutes<-NA
    #     for(row in 1:nrow(sub.effort.dat)){as.numeric(ifelse(is.na(sub.effort.dat$Effort_EndTime_Dec[row])==FALSE & is.na(sub.effort.dat$Effort_StartTime_Dec[row])==FALSE ,sub.effort.dat$Count.Time.Minutes[row]<-round((sub.effort.dat$Effort_EndTime_Dec[row] - sub.effort.dat$Effort_StartTime_Dec[row])*60, 0), NA))}  
      
    #Identify dates when both index and tie-in surveys were completed on same day 
        index.dates<-unique(sub.effort.dat$Date[sub.effort.dat$Survey.Type == "Creel" ])
        tie.dates<-unique(sub.effort.dat$Date[sub.effort.dat$Survey.Type == "TieIn" ])
        paired.dates<-index.dates[index.dates %in% tie.dates]
        
    #Create summary of creel counts to identify which one pairs best with tie-in count
        paired.surveys<-setNames(as.data.frame(matrix(NA, nrow=0, ncol=8)), c("Date", "Count", "Index.Start", "Index.End", "TI.Start", "TI.End", "Start.Diff", "End.Diff"))
        for(date in 1:length(paired.dates)){
          sub.date<-sub.effort.dat[sub.effort.dat$Date == unique(paired.dates)[date],]
          counts<-unique(sub.date$CountNum)
          
          sub.paired.surveys<-setNames(as.data.frame(matrix(NA, nrow=max(sub.effort.dat$CountNum), ncol=8)), c("Date", "Count", "Index.Start", "Index.End", "TI.Start", "TI.End", "Start.Diff", "End.Diff"))
          
          tie.in.data<-sub.date[sub.date$Survey.Type == "TieIn", ]
          
            for(count in 1:length(counts)){
                sub.count<-sub.date[sub.date$CountNum == unique(sub.date$CountNum)[count] & sub.date$Survey.Type == "Creel",]
                
                sub.paired.surveys[count, "Date"]<-as.character(unique(paired.dates)[date])    
                sub.paired.surveys[count, "Count"]<-as.character(unique(sub.date$CountNum)[count])
                sub.paired.surveys[count, "Index.Start"]<-min(sub.count$Effort_StartTime_Dec, na.rm=TRUE)
                sub.paired.surveys[count, "Index.End"]<-max(sub.count$Effort_EndTime_Dec, na.rm=TRUE)
                sub.paired.surveys[count, "TI.Start"]<-min(tie.in.data$Effort_StartTime_Dec, na.rm=TRUE)
                sub.paired.surveys[count, "TI.End"]<-max(tie.in.data$Effort_EndTime_Dec, na.rm=TRUE)
                sub.paired.surveys[count, "Start.Diff"]<-abs(sub.paired.surveys$Index.Start[count] - sub.paired.surveys$TI.Start[count])
                sub.paired.surveys[count, "End.Diff"]<-abs(sub.paired.surveys$Index.End[count] - sub.paired.surveys$TI.End[count] )
            }
          paired.surveys<-rbind(paired.surveys, sub.paired.surveys)
        }  
        paired.surveys

    #Update "CountNum" for TieIn surveys based on summary in "paired.surveys" DF    
        sub.effort.dat$CountNum_New<-NA
        for(row in 1:nrow(sub.effort.dat)){
            ifelse(sub.effort.dat$Survey.Type[row] != "Creel" & as.character(sub.effort.dat$Date[row]) %in% as.character(paired.surveys$Date)
              , sub.effort.dat$CountNum_New[row]<-as.numeric(paired.surveys$Count[paired.surveys$Date == sub.effort.dat$Date[row] & paired.surveys$Start.Diff ==  min(paired.surveys$Start.Diff[paired.surveys$Date == sub.effort.dat$Date[row]])])
              , sub.effort.dat$CountNum_New[row]<-sub.effort.dat$CountNum[row])
        }
        unique(sub.effort.dat$Date[sub.effort.dat$CountNum != sub.effort.dat$CountNum_New])
        
    #Calculate "corrected" End time (if using "Percent_time_fished" field) - unique to Skagit River fishery
        if(any(colnames(sub.group.dat)=="Percent_time_fished")==TRUE){
          sub.group.dat$End.Time.Dec<-NA
          for(row in 1:nrow(sub.group.dat)){
            if(sub.group.dat$Percent_time_fished[row] == 1.0){
              sub.group.dat$End.Time.Dec[row]<-sub.group.dat$Interview.Time.Dec[row]
              
            }else{
              sub.group.dat$End.Time.Dec[row]<-sub.group.dat$Start.Time.Dec[row] + (sub.group.dat$Interview.Time.Dec[row] - sub.group.dat$Start.Time.Dec[row])*sub.group.dat$Percent_time_fished[row] 
            }
          }
        }

    #Calculate "Hours" fished by Group
        #unique(sub.group.dat$Trip.Status)
        if(any(colnames(sub.group.dat)=="Percent_time_fished")==TRUE){
          for(row in 1:nrow(sub.group.dat)){
          sub.group.dat$Hours[row]<-sub.group.dat$End.Time.Dec[row] - sub.group.dat$Start.Time.Dec[row]
          }
        }else{  
        
        for(row in 1:nrow(sub.group.dat)){
          ifelse(sub.group.dat$Trip.Status[row] == "C", sub.group.dat$Hours[row]<-(sub.group.dat$End.Time.Dec[row] - sub.group.dat$Start.Time.Dec[row]) , sub.group.dat$Hours[row]<-(sub.group.dat$Interview.Time.Dec[row] - sub.group.dat$Start.Time.Dec[row]) )
        }
        }
        
          
    #Calculate "Total_Hours" ("Hours" * "NumAnglers") by Group
        #unique(sub.group.dat$Trip.Status)
        for(row in 1:nrow(sub.group.dat)){
          sub.group.dat$Total_Hours[row]<-(sub.group.dat$Hours[row] * sub.group.dat$NumAnglers[row]) 
        }    

    #Combine "Species", "Origin", and "Fate" to create a unique "Catch.Group" index
      for(row in 1:nrow(sub.group.dat)){
        sub.group.dat$Catch.Group[row]<-paste(sub.group.dat$Species[row], sub.group.dat$Origin[row], sub.group.dat$Fate[row], sep = "_")
      }
      # for(row in 1:nrow(sub.gearfish.dat)){
      #   sub.gearfish.dat$Catch.Group[row]<-paste(sub.gearfish.dat$Species[row], sub.gearfish.dat$Origin[row], sub.gearfish.dat$Fate[row], sep = "_")
      # }
