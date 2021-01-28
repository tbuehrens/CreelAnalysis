#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file summarizes creel effort and catch data that will then be fed directly into time-series model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Denote date range of dataset
  Date_Begin<-min(sub.effort.dat$Date) #c("2018-04-01") #Define date as the first day for which you want to make an estimate for the fishery (not necessarily first survey date)
  Date_End<-  max(sub.effort.dat$Date) #c("2018-04-30")    

#---------------------------------------------------------------------------------------------------------- - 
# (6A) Create "all.Dates" x-walk table and create vector for open/closed fishery dates                   ####
#---------------------------------------------------------------------------------------------------------- - 
  #Create cross-walk table "all.Dates" that defines the relative "day" and "week" number based on "Date_Begin" and "Date_End"
      all.Dates<-setNames(as.data.frame(cbind(seq(1, as.numeric(as.Date(Date_End)-as.Date(Date_Begin)+1), 1) , as.character(seq(as.Date(Date_Begin), as.Date(Date_End), 1)))), c("Day", "Date"))
      all.Dates$Year<-as.numeric(format(as.Date(all.Dates$Date), "%Y"))
      all.Dates$Month<-format(as.Date(all.Dates$Date), "%b")
      all.Dates$Month.no<-as.numeric(format(as.Date(all.Dates$Date), "%m"))
      all.Dates$DayofWeek<-weekdays(as.Date(all.Dates$Date)); 
      all.Dates$DayType<-ifelse(all.Dates$DayofWeek=="Saturday"|all.Dates$DayofWeek=="Sunday",1,0)
      all.Dates$DOY<-(as.numeric(format(as.Date(all.Dates$Date), "%j")))
      all.Dates$WeekNum_abs<-as.numeric(format(as.Date(all.Dates$Date), "%V"))
      all.Dates$Yr_Week<-paste(as.numeric(all.Dates$Year), as.numeric(all.Dates$WeekNum_abs), sep="_")
      head(all.Dates)
      
  ##Day Length
      #NOTE: I have specified day length using dawn & dusk, but this can be changed to several other metrics (e.g., sunrise & sunset)
      day.light<-getSunlightTimes(keep=c("dawn", "dusk"), date=seq(min(as.Date(all.Dates$Date)), max(as.Date(all.Dates$Date)),1), lat = river_loc$Lat[river_loc$River == StreamName.of.Interest], lon=river_loc$Long[river_loc$River == StreamName.of.Interest], tz = "America/Los_Angeles")
      day.light$DayL<-as.numeric(day.light[[5]]-day.light[[4]])    

  #Calculate relative day (DayNum_Rel) and week (WeekNum_Rel) columns for "Dates.of.Interest" DF
      #weeknums.abs<-unique(as.numeric(format(as.Date(Dates.of.Interest$Date), "%V")))
      weeknums.abs<-unique(all.Dates$Yr_Week)
      weeknums.rel<-seq(1, length(weeknums.abs),1)
      weeknum_xwalk<-setNames(as.data.frame(cbind(weeknums.abs, weeknums.rel)), c("Yr_Week", "weeknum"))
      for(row in 1:nrow(all.Dates)){all.Dates$Weeknum_Rel[row]<-as.numeric(weeknum_xwalk$weeknum[as.character(weeknum_xwalk$Yr_Week) == as.character(all.Dates$Yr_Week[row])])}
  
      all.Dates$Daynum_Rel<-all.Dates$Day
      # daynums.abs<-unique(all.Dates$Date)
      # daynum.rel<-seq(1, length(daynums.abs),1)
      # daynum_xwalk<-setNames(as.data.frame(cbind(daynums.abs, daynum.rel)), c("DOY", "daynum"))
      # for(row in 1:nrow(all.Dates)){all.Dates$Daynum_Rel[row]<-as.character(daynum_xwalk$daynum[as.character(daynum_xwalk$DOY) == as.character(all.Dates$Day)[row]])}
      head(all.Dates)
        
  #Using user defined "closed.Dates", create vector of data denoting whether the fishery was open (1) or closed (1E-9) by section 
          if(total.closed.dates==0){
              fishery.open.closed<-setNames(cbind(all.Dates$Date, as.data.frame(matrix(1, nrow=nrow(all.Dates), ncol=nrow(final.effort.section.xwalk)))), c("Date", as.character(final.effort.section.xwalk$Section.Name)))
          }else{
              fishery.open.closed<-setNames(cbind(all.Dates$Date, as.data.frame(matrix(1, nrow=nrow(all.Dates), ncol=nrow(final.effort.section.xwalk)))), c("Date", as.character(final.effort.section.xwalk$Section.Name)))
              closed.Dates.DF<-setNames(as.data.frame(matrix(closed.Dates.Sections, nrow=total.closed.dates, ncol=nrow(final.effort.section.xwalk)+1, byrow=TRUE)), c("Date", final.effort.section.xwalk$Section.Name))
              
              for(date in 1:nrow(all.Dates)){
                  if(all.Dates$Date[date] %in% closed.Dates.DF$Date){
                    for(section in 1:nrow(final.effort.section.xwalk)){
                        
                      fishery.open.closed[date, final.effort.section.xwalk$Section.Name[section]]<-as.numeric(as.character(closed.Dates.DF[as.Date(closed.Dates.DF$Date) == as.Date(all.Dates$Date)[date], final.effort.section.xwalk$Section.Name[section]]))
                    }
                  }
              }
          }
      fishery.open.closed
      fishery.open.closed$Date<-NULL

#---------------------------------------------------------------------------------------------------------- - 
# (6B) SUMMARIZE EFFORT DATA                                                                     ####
#---------------------------------------------------------------------------------------------------------- -         
  # Sub-set effort data based on defined "Date_Begin" and "Date_End"
    final_effort.dat<-sub.effort.dat[as.Date(sub.effort.dat$Date) %in% as.Date(all.Dates$Date),]
    length(unique(final_effort.dat$Date))

  # Sub-set Day Length data frame based on defined Date_Begin" and "Date_End"
    head(day.light)
    nrow(day.light) 
    DayL<-day.light[as.Date(day.light$date) %in% as.Date(all.Dates$Date),] #>= as.Date(Date_Begin) & as.Date(day.light$date) <= as.Date(Date_End), c("date", "DayL")]
    nrow(DayL)

  #Define aggregates of effort data
    creel.dates<-unique(final_effort.dat$Date)                    # Unique creel survey dates
    n_creel.dates<-length(creel.dates)                            # Number of unique creel survey dates
    
    survey.types<-unique(final_effort.dat$Survey.Type)            # Unique creel survey types (should be two - "Creel" & "TieIn")
    n_survey.types<-length(survey.types)                          # Number of unique creel survey types 
    
    counts<-as.vector(unique(final_effort.dat$CountNum[order(as.numeric(final_effort.dat$CountNum))])) # Unique counts (should be two - "1" & "2")
    n_counts<-length(counts)                                      # Number of unique (index) counts 
    
    gear.types<-c("Vehicles", "Trailers", "Bank", "Boat")         # Unique gear types (of interest)
    gear.short<-c("V", "T", "S", "B")                                       # Abbreviations for gear groups
    n_gear.types<-length(gear.types)                              # Number of unique gear types
    
    sections<-as.vector(unique(final_effort.dat$Section.Num[order(as.numeric(final_effort.dat$Section.Num))])) # Unique survey sections
    section.xwalk<-setNames(as.data.frame(matrix(c(unique(final_effort.dat$Section.Num), unique(final_effort.dat$final.Section.Name)),nrow=length(sections), ncol=2, byrow=FALSE)), c("Section_Num", "Section_Name"))
    n_sections<-length(sections)

  #Create summary effort data frame ("effort_samp_sum") to fill  
    effort_samp_sum<-setNames(as.data.frame(matrix(NA, nrow=0, ncol=8)), c("Date", "Survey", "Gear_Alpha", "Section", "CountNum", "CountTime", "CountTime_Dec", "Count"))
    
  #Loop through effort data (of interest) and fill  
    for(date in 1:n_creel.dates){
        sub_date<-final_effort.dat[final_effort.dat$Date == creel.dates[date], ]
        
        for(survey_type in 1:n_survey.types){
          sub_date.survey<-sub_date[sub_date$Survey.Type == survey.types[survey_type], ]
          
            for(section in 1:n_sections){
              sub_date.survey.section<-sub_date.survey[sub_date.survey$Section.Num == sections[section],]
              
              for(count in 1:n_counts){
                sub_date.survey.section.count<-sub_date.survey.section[sub_date.survey.section$CountNum_New == counts[count], ]
            
                sub.effort_samp_sum<-as.data.frame(matrix(NA, nrow=n_gear.types, ncol=8))
                names(sub.effort_samp_sum)<-c("Date", "Survey", "Gear_Alpha", "Section", "CountNum", "CountTime", "CountTime_Dec", "Count")
                
                for(gear in 1:n_gear.types){
                    sub.effort_samp_sum[gear, "Date"]<-as.character(creel.dates[date])
                    sub.effort_samp_sum[gear, "Survey"]<-as.character(survey.types[survey_type])
                    sub.effort_samp_sum[gear, "Gear_Alpha"]<-as.character(gear.types[gear])
                    sub.effort_samp_sum[gear, "Section"]<- as.character(sections[section])
                    sub.effort_samp_sum[gear, "CountNum"]<-counts[count]
                    sub.effort_samp_sum[gear, "CountTime"]<-ifelse(nrow(sub_date.survey.section.count)==0, NA, sub_date.survey.section.count$Effort_StartTime)
                    sub.effort_samp_sum[gear, "CountTime_Dec"]<-ifelse(nrow(sub_date.survey.section.count)==0, NA, sub_date.survey.section.count$Effort_StartTime_Dec)
                    sub.effort_samp_sum[gear, "Count"]<-ifelse(nrow(sub_date.survey.section.count)==0, NA, sum(sub_date.survey.section.count[, gear.types[gear]] ) )
                    
                }
                effort_samp_sum<-rbind(effort_samp_sum, sub.effort_samp_sum)
              }
            }
        }
    }
    effort_samp_sum[1:20,]

  #Add columns to "effort_samp_sum" that make variable values numeric 
    ##Day   
      effort_samp_sum$Day<-NA; for(row in 1:nrow(effort_samp_sum)){ effort_samp_sum$Day[row]<-as.numeric(as.character(all.Dates$Daynum_Rel[all.Dates$Date == effort_samp_sum$Date[row]]))}
        
    ##DayType    
      effort_samp_sum$DayType<-NA; for(row in 1:nrow(effort_samp_sum)){ effort_samp_sum$DayType[row]<-as.numeric(as.character(all.Dates$DayType[all.Dates$Date == effort_samp_sum$Date[row]]))}
        
    ##Weeknum
      effort_samp_sum$Week<-NA; for(row in 1:nrow(effort_samp_sum)){ effort_samp_sum$Week[row]<-as.numeric(as.character(all.Dates$Weeknum_Rel[all.Dates$Date == effort_samp_sum$Date[row]]))}
      n_week<-length(unique(effort_samp_sum$Week))  
        
    ##Gear
      gear.types; gear.short
      all.Gear<-setNames(as.data.frame(cbind(seq(1, length(gear.types),1), gear.types, gear.short)), c("Gear", "Gear_Alpha", "Gear_Short"))
      effort_samp_sum$Gear<-NA; for(row in 1:nrow(effort_samp_sum)){ effort_samp_sum$Gear[row]<-as.numeric(as.character(all.Gear$Gear[all.Gear$Gear_Alpha == effort_samp_sum$Gear_Alpha[row]]))}

    ##UniqueID (based on "Week"_"Day"_"CountNum")  
      effort_samp_sum$UniqueID<-NA; for(row in 1:nrow(effort_samp_sum)){effort_samp_sum$UniqueID[row]<-paste(effort_samp_sum$Week[row], effort_samp_sum$Day[row], effort_samp_sum$CountNum[row], sep = "_")}
      head(effort_samp_sum)
      
    ##CountNum by Week
        CountNum_byWeek_Xwalk<-setNames(as.data.frame(matrix(NA,nrow=0, ncol=2)), c("UniqueID", "CountNum_Week"))
        for(week in 1:length(unique(effort_samp_sum$Week))){
          sub.week<-effort_samp_sum[effort_samp_sum$Week == unique(effort_samp_sum$Week)[week],]
          
          sub.CountNum_byWeek_Xwalk<-setNames(as.data.frame(matrix(NA,nrow=length(unique(sub.week$UniqueID)), ncol=2)), c("UniqueID", "CountNum_Week"))
          
          for(ID in 1:length(unique(sub.week$UniqueID))){
            sub.CountNum_byWeek_Xwalk[ID, "UniqueID"]<-as.character(unique(sub.week$UniqueID)[ID])
            sub.CountNum_byWeek_Xwalk[ID, "CountNum_Week"]<-ID
                      }
          CountNum_byWeek_Xwalk<-rbind(CountNum_byWeek_Xwalk, sub.CountNum_byWeek_Xwalk)
        }
        head(CountNum_byWeek_Xwalk)
        
    effort_samp_sum$CountNum_Week<-NA; for(row in 1:nrow(effort_samp_sum)){effort_samp_sum$CountNum_Week[row]<- CountNum_byWeek_Xwalk$CountNum_Week[CountNum_byWeek_Xwalk$UniqueID == effort_samp_sum$UniqueID[row]]}
    
    # #Summarize data to make sure "CountNum_Week" summary worked
    #   effort_samp_sum %>%group_by(Week) %>%summarise(n_distinct(Date)) %>% print(n=50)
    #   effort_samp_sum %>%group_by(Week) %>%summarise(n_distinct(CountNum_Week))
    #   effort_samp_sum %>%group_by(Week) %>% distinct(Date)%>% print(n=60)

  #Index samples
    I_samp<-effort_samp_sum[effort_samp_sum$Survey == "Creel", c("Day", "DayType", "Week", "Gear", "Section", "CountNum", "CountNum_Week", "Count")]
    I_samp<-I_samp[!is.na(I_samp$Count),] #Drop rows where Count == "NA"
    I_samp$Section<-as.numeric(I_samp$Section)
    V_samp<-I_samp[I_samp$Gear==1,] # & !is.na(I_samp$Count)
    T_samp<-I_samp[I_samp$Gear==2,]
    A_samp<-I_samp[I_samp$Gear==3 | I_samp$Gear==4,]

  #Tie-In samples
    E_s_samp<-effort_samp_sum[effort_samp_sum$Survey == "TieIn", c("Day", "DayType", "Week", "Gear", "Section", "CountNum", "CountNum_Week", "Count")]
    E_s_samp<-E_s_samp[is.na(E_s_samp$Count)==FALSE,] #Drop rows where Count == "NA"
    E_s_samp$Section<-as.numeric(E_s_samp$Section)
    
  #Re-index gear-types for tie-in counts (bank(3) == 1; boat(4) ==2)  
    E_s_samp$Gear[E_s_samp$Gear == 3] <- 1; 
    E_s_samp$Gear[E_s_samp$Gear == 4] <- 2;
    
#---------------------------------------------------------------------------------------------------------- - 
# (6C) SUMMARIZE cATCH DATA                                                                     ####
#---------------------------------------------------------------------------------------------------------- - 
    head(sub.group.dat)
    #head(sub.gearfish.dat)
  
  # Sub-set group (interview) data based on defined "Date_Begin" and "Date_End"
    final_group.dat<-sub.group.dat[as.Date(sub.group.dat$Date) %in% as.Date(all.Dates$Date),]
    length(unique(final_group.dat$Date))    
  
  #Order data by Date and GroupNum
    final_group.dat<-final_group.dat[order(as.Date(final_group.dat$Date), final_group.dat$GroupNum),]
    #sub.gearfish.dat<-sub.gearfish.dat[order(as.Date(sub.gearfish.dat$Date), sub.gearfish.dat$GroupNum),]
    
  #Drop "final_group.dat" data if "Start.Time" is <NA>, "Interview.Time" is <NA> for groups with Trip.Status == I, or "End.Time" is <NA> for groups with Trip.Status == C
    drop.group.indices<-c(final_group.dat$GroupNum[is.na(final_group.dat$Start.Time)==TRUE] #Missing Start Time
      , final_group.dat$GroupNum[is.na(final_group.dat$Interview.Time)==TRUE & final_group.dat$Trip.Status=="I" ] #Missing Interview Time for Incomplete Trips
      , final_group.dat$GroupNum[is.na(final_group.dat$Start.Time)==FALSE & is.na(final_group.dat$End.Time.Dec)==TRUE & final_group.dat$Trip.Status=="C" ] #Missing Interview Time for Completed Trips
      )
    length(drop.group.indices)
    ifelse(length(drop.group.indices)==0, final_group.dat<-final_group.dat, final_group.dat<-final_group.dat[as.character(final_group.dat$GroupNum) %!in% as.character(drop.group.indices),])

  #Drop rows if hours fished by a Group is Negative 
    nrow(final_group.dat[final_group.dat$Hours<=0 | is.na(final_group.dat$Hours)==TRUE,])
    final_group.dat<-final_group.dat[final_group.dat$Hours>0 & is.na(final_group.dat$Hours)==FALSE,]
    
  #Assign values for "Count" - prior to eDate capture, "Count" was not a entered field and therefore all rows = NA
    final_group.dat$Count_New<-final_group.dat$Count
    
  #Summarize group level data by "Date" and "GroupNum" and "final.Section.Name" (NOTE: needed to add section for Skagit data because anglers could report fishing in multiple sections)
    for(row in 1:nrow(final_group.dat)){final_group.dat$temp.Group[row]<-paste(final_group.dat$Date[row], final_group.dat$GroupNum[row], final_group.dat$final.Section.Name[row], sep="_")}
    #uniq.Groups<-unique(final_group.dat$temp.Group)
    sub.unique.group.dat<-final_group.dat[!duplicated(final_group.dat$temp.Group),]
    
  #Identify catch groupings of interest
    unique(final_group.dat$Catch.Group)
    n_catch.groups<-length(catch.group.of.interest)
    
  #Summarize Catch of Interest by Angler Group
    catch.dat_byGroup<-setNames(as.data.frame(matrix(NA, nrow=length(unique(final_group.dat$temp.Group))*length(catch.group.of.interest), ncol=3)), c("temp.Group", "Catch.Group", "Count"))
    for(group in 1:length(unique(final_group.dat$temp.Group))){
        sub.Group.Index<-final_group.dat[final_group.dat$temp.Group == unique(final_group.dat$temp.Group)[group], ]
      
        for(spp.group in 1:length(catch.group.of.interest)){
          
          catch.dat_byGroup[(group-1) * n_catch.groups  + spp.group, "temp.Group"]<-unique(final_group.dat$temp.Group)[group]
          catch.dat_byGroup[(group-1) * n_catch.groups  + spp.group, "Catch.Group"]<-catch.group.of.interest[spp.group]
          catch.dat_byGroup[(group-1) * n_catch.groups  + spp.group, "Count"]<-sum(sub.Group.Index$Count_New[sub.Group.Index$Catch.Group==catch.group.of.interest[spp.group]])
        }
    }
    head(catch.dat_byGroup)
    tail(catch.dat_byGroup)
    aggregate(catch.dat_byGroup$Count ~ catch.dat_byGroup$Catch.Group, FUN=sum)
    
  #Summarize "catch.dat_byUniqueGroup"
   catch.dat_byUniqueGroup<- catch.dat_byGroup %>% spread(Catch.Group, Count) 
   head(catch.dat_byUniqueGroup)
   length(unique(catch.dat_byUniqueGroup$temp.Group)); nrow(catch.dat_byUniqueGroup)
   
  #Create new datasets by joining "sub.unique.group.dat" and "catch.dat_byUniqueGroup" using "temp.Group"
    group.fish.dat<-left_join(sub.unique.group.dat, catch.dat_byUniqueGroup, by="temp.Group")
    length(unique(group.fish.dat$Group.Index)); nrow(group.fish.dat)
    head(group.fish.dat) 
    
  # #Assign 0s (catch) for rows with values == NA (NOTE: this happens if a group was interviewed but no gear/fish entry was created either in database and/or on back of datasheet) 
  #   group.fish.dat[, catch.group.of.interest][is.na(group.fish.dat[, catch.group.of.interest])]<-0

  #Sub-set data of interest
    Group_Catch<-group.fish.dat[, c("Date", "Weeknum", "DayType", "Angler.Type", "temp.Group",  "Section.Num", "Hours", "Trip.Status", "NumAnglers", "Vehicles", "Trailers", "Total_Hours", catch.group.of.interest)]
    head(Group_Catch)

  #Drop group interviews if "Hours" less than threshold AND "Trip.Status" == I
    drop.short.trips<-"Y" #Enter "Y" (yes) or "N" (no)
    min.fish.threshold<-0.5 #If "Y", enter as minimum trip length threshold for groups that are still fishing (i.e., Trip.Status == "I")
    if(drop.short.trips == "Y"){
        Group_Catch<-Group_Catch[Group_Catch$Trip.Status == "C" |  (Group_Catch$Trip.Status == "I" & Group_Catch$Hours >= min.fish.threshold),]
    }
    
  #Drop data if "Angler.Type" is equal to "NA"
    Group_Catch<-Group_Catch[is.na(Group_Catch$Angler.Type)==FALSE,]
    
  #Sub-set catch data based on defined Date_Begin" and "Date_End"
    final_Group_Catch.dat<-Group_Catch[as.Date(Group_Catch$Date) >= as.Date(Date_Begin) & as.Date(Group_Catch$Date) <= as.Date(Date_End), ]
    length(unique(final_Group_Catch.dat$Date)) 
    length(unique(final_effort.dat$Date))
    
  #Add columns to "final_Group_Catch.dat" that make variable values numeric 
    ##Day
        final_Group_Catch.dat$Day<-NA; for(row in 1:nrow(final_Group_Catch.dat)){ final_Group_Catch.dat$Day[row]<-as.numeric(as.character(all.Dates$Daynum_Rel[as.Date(all.Dates$Date) == as.Date(final_Group_Catch.dat$Date[row])]))}
    ##DayType    
        final_Group_Catch.dat$DayType<-NA; for(row in 1:nrow(final_Group_Catch.dat)){ final_Group_Catch.dat$DayType[row]<-as.numeric(as.character(all.Dates$DayType[as.Date(all.Dates$Date) == as.Date(final_Group_Catch.dat$Date[row])]))}
    ##Gear
        final_Group_Catch.dat$Gear<-NA; for(row in 1:nrow(final_Group_Catch.dat)){ final_Group_Catch.dat$Gear[row]<-as.numeric(as.character(all.Gear$Gear[all.Gear$Gear_Short == final_Group_Catch.dat$Angler.Type[row]]))}
    ##Group
      all.Groups<-setNames(as.data.frame(cbind(seq(1, nrow(final_Group_Catch.dat), 1) , final_Group_Catch.dat$temp.Group)), c("Group", "temp.Group"))
      final_Group_Catch.dat$Angler<-NA; for(row in 1:nrow(final_Group_Catch.dat)){ final_Group_Catch.dat$Angler[row]<-as.numeric(as.character(all.Groups$Group[as.character(all.Groups$temp.Group) == as.character(final_Group_Catch.dat$temp.Group[row])]))}
          #NOTE: if you get a warning here there may be two groups with the same "ID" which would result from a data entry error
      
    ##Weeknum
      final_Group_Catch.dat$Week<-NA; for(row in 1:nrow(final_Group_Catch.dat)){ final_Group_Catch.dat$Week[row]<-as.numeric(as.character(all.Dates$Weeknum_Rel[as.Date(all.Dates$Date) == as.Date(final_Group_Catch.dat$Date[row])]))}

  #Catch samples (to estimate CPUE)
    c_samp<-final_Group_Catch.dat[is.na(final_Group_Catch.dat$Hours)==FALSE, c("Day", "DayType", "Week", "Gear", "Angler", "Section.Num", "Total_Hours", catch.group.of.interest)]
    head(c_samp)
    nrow(c_samp)
    
  #Angler samples (to estimate boats & trailers per angler)
    a_samp<-final_Group_Catch.dat[is.na(final_Group_Catch.dat$NumAnglers)==FALSE & is.na(final_Group_Catch.dat$Vehicles)==FALSE & is.na(final_Group_Catch.dat$Trailers)==FALSE, c("Day", "DayType", "Week", "Gear", "Angler", "Section.Num", "NumAnglers", "Vehicles", "Trailers")]
    head(a_samp)
    nrow(a_samp)  
    
  #Re-index gear-types for interview anglers (bank(3) == 1; boat(4) ==2)  
    c_samp$Gear[c_samp$Gear == 3] <- 1; c_samp$Gear[c_samp$Gear == 4] <- 2;
    a_samp$Gear[a_samp$Gear == 3] <- 1; a_samp$Gear[a_samp$Gear == 4] <- 2;
    gear.xwalk<-setNames(as.data.frame(matrix(c(c(1,2), as.character(all.Gear$Gear_Alpha[c(3:4)])),nrow=2, ncol=2, byrow=FALSE)), c("Gear_Num", "Gear_Name"))

  #total hours creel and total catch sample a per day/gear/section to compare with effort
    C_sample<-c_samp%>%
      group_by(Day,Gear,Section.Num)%>%
      summarise(Total_Hours=sum(Total_Hours),Total_Catch=sum((!!as.name(catch.group.of.interest))))%>%
      filter(Total_Hours>0)
#=====================================================================================================================
# (6E) make stan data
#=====================================================================================================================
standat<-list(
  D=nrow(all.Dates),                                               # total number of days in the fishery we are estimating catch for
  G=length(unique(I_samp$Gear)),                                   # total number of unique gear types (#previously G=n_gear.types)
  S=n_sections,                                                    # total number of census/tie in Sections
  H=max(as.numeric(V_samp$CountNum), as.numeric(T_samp$CountNum)), # max. number of index effort counts completed on a single survey day

  #day attributes
  w=all.Dates$DayType,                                             # an indexing column with length equal to total number of days in the fishery we are estimating catch for with a value listed as either 1 or 2 depending on whether the daytype was a weekday or weekend, respectively
  L=DayL$DayL,                                                     # decimal daylength in hrs for each day in 1 : D
  O=as.matrix(fishery.open.closed),                                # matrix denoting whether the fishery was open (1) or closed (1E-9) for each individual "D" (row) and section 1:S (column)

  #Vehicles
  V_n=nrow(V_samp),            # the total number of index effort counts  (n = index survey dates X sections X counts X angler types)
  V_I=V_samp$Count,            # sum of the index effort vehicle count within a section during an effort count
  day_V=V_samp$Day,            # an indexing column with length equal to count of Index effort counts with the day of the fishery listed starting at 1 and going to the last day n
  gear_V=V_samp$Gear,          # an indexing column with length equal to count of Index effort counts with 	a 1 or 2 depending on whether the interview was a boat or bank fisherman
  section_V=V_samp$Section,    # an indexing column with length equal to count of Index effort counts with the section number starting at 1
  countnum_V=as.numeric(V_samp$CountNum),

  #Trailers
  T_n=nrow(T_samp),            # the total number of index effort counts  (n = index survey dates X sections X # of index count for each day)
  T_I=T_samp$Count,            # sum of the index effort counts (of vehicles & trailers) within a section during an effort count
  day_T=T_samp$Day,            # an indexing column with length equal to count of Index effort counts with the day of the fishery listed starting at 1 and going to the last day n
  gear_T=T_samp$Gear,          # an indexing column with length equal to count of Index effort counts with 	a 1 or 2 depending on whether the interview was a boat or bank fisherman
  section_T=T_samp$Section,    # an indexing column with length equal to count of Index effort counts with the section number starting at 1
  countnum_T=as.numeric(T_samp$CountNum),
  
  #Angler counts
  A_n=nrow(A_samp),
  A_I=A_samp$Count,
  day_A=A_samp$Day,
  gear_A=A_samp$Gear,
  section_A=A_samp$Section,
  countnum_A=as.numeric(A_samp$CountNum),
  
  #tie ins
  E_n=nrow(E_s_samp),          # the total number of census effort counts 
  E_s=E_s_samp$Count,          # the effort count in a tie-in section
  day_E=E_s_samp$Day,          # an indexing column with length equal to count of Census effort counts with the day of the fishery listed starting at 1 and going to the last day n.
  gear_E=E_s_samp$Gear,        # an indexing column with length equal to count of Census effort counts with a 1 or 2 depending on whether the interview was a boat or bank 	fisherman
  section_E=E_s_samp$Section,  # an indexing column with length equal to count of Census effort counts 	with the section number starting at 1 
  countnum_E=as.numeric(E_s_samp$CountNum),

  #interview data - CPUE
  IntC=nrow(c_samp),              # total number of interviews conducted across all surveys dates where CPUE data (c & h) were collected 
  gear_IntC=c_samp$Gear,          # an indexing column with length equal to count of angler interviews with a 	1 or 2 depending on whether the interview was a boat or bank fisherman
  day_IntC=c_samp$Day,            # an indexing column with length equal to count angler interviews with the day of the fishery listed starting at 1 and going to the last day n.
  section_IntC = c_samp$Section.Num, #Section where angler was fishing (1 = Skagit, 2 = Sauk)
  c=c_samp[, catch.group.of.interest[1]], # total catch of [spp.of.interest] for an individual group #c_samp$SH_NOR_R          
  h=abs(c_samp$Total_Hours),        # number of hours an individual angler/group spent fishing
  
  #interview data total creeled effort and catch
  IntCreel = nrow(C_sample),
  day_Creel = C_sample$Day,
  gear_Creel = C_sample$Gear,
  section_Creel = C_sample$Section.Num,
  C_Creel = C_sample$Total_Catch,
  H_Creel = C_sample$Total_Hours,
  
  #interview data - angler expansions
  IntA=nrow(c_samp),              # total number of interviews across all surveys dates where angler expansion data (V_A, T_A, A_A) were collected 
  gear_IntA=c_samp$Gear,          # an indexing column with length equal to count of angler interviews with a 	1 or 2 depending on whether the interview was a boat or bank fisherman
  day_IntA=c_samp$Day,            # an indexing column with length equal to count angler interviews with the day of the fishery listed starting at 1 and going to the last day n.
  section_IntA = c_samp$Section.Num, #Section where angler was fishing (1 = Skagit, 2 = Sauk)
  V_A= a_samp$Vehicles,        # Total number of vehicles an individual angler group brought to the river
  T_A= a_samp$Trailers,        # Total number of Trailers an individual angler group brought to the river
  A_A= a_samp$NumAnglers       #Total number of anglers in each group interviewed
)
#standat$p_TE<-matrix(1,nrow=standat$G,ncol=standat$S)   # proportion of section covered by tie in counts: Option 1:usually 1.0, Option 2: miles of tie-in divided by total miles in section, Option 3:Best guess or something else.
   