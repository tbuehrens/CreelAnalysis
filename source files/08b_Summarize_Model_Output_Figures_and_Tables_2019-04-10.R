#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file summarizes creel model output by creating summary plots and tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Reset working directory
    if(model_source=="new"){filepath_outputs<-paste(wd_output_files, catch.group.of.interest, model.name, new_model_run_date, sep="/")
    }else{
    if(model_source=="saved"){filepath_outputs<-paste(wd_output_files, catch.group.of.interest, model.name, saved_model_date, sep="/")}}

#--------------------------------------------------------------------------------------------------------------------- -
# SUMMARIZE SEASON TOTAL EFFORT AND CATCH                                                                ----
#--------------------------------------------------------------------------------------------------------------------- - 
  #calculate season total effort
    #Total_season_catch<-c(setNames(mean(res$C_sum), "Mean"), quantile(res$C_sum,c(0.025,0.25,0.5,0.75,0.975)), setNames(sd(res$C_sum)/mean(res$C_sum), "CV"))
    Total_season_catch<-c(setNames(round(mean(c(apply(res$lambda_Ctot_S, c(1), sum))),0), "Mean"), round(quantile(apply(res$lambda_Ctot_S, c(1), sum),c(0.025,0.25,0.5,0.75,0.975)),0), setNames(round(sd(c(apply(res$lambda_Ctot_S, c(1), sum)))/mean(c(apply(res$lambda_Ctot_S, c(1), sum))),3), "CV"))
  
  #calculate season total catch
    Total_season_effort<-c(setNames(round(mean(res$E_sum),0), "Mean"), round(quantile(res$E_sum,c(0.025,0.25,0.5,0.75,0.975)),0), setNames(round(sd(res$E_sum)/mean(res$E_sum),3), "CV"))
    
  #Export .csv of total catch and effort
    catch.effort.totals<-rbind(Total_season_catch, Total_season_effort)
    write.csv(catch.effort.totals, paste(filepath_outputs, paste("Summary_Total_Catch_and_Effort", catch.group.of.interest, model.name, ".csv", sep="_"), sep="/"), row.names = T)

#--------------------------------------------------------------------------------------------------------------------- -
# SUMMARIZE TOTAL EFFORT DATA - by date, section, and gear-type (all Skagit models estimate effort by section and gear)                                                                                 ----
#--------------------------------------------------------------------------------------------------------------------- -   
  #Extract effort data and name dimensions (so that they are meaningful)
      effort.results<-res$E; dim(effort.results)
      dimnames(effort.results)<-list(seq(1:nrow(effort.results)), section.xwalk$Section_Name, paste("day",seq(1:standat$D), sep="_"), gear.xwalk$Gear_Name )
      # effort.results[1:2, , 1:2,]
      # E.mean<-setNames(as.data.frame(matrix(matrix(apply(effort.results,c(2:4),mean),nrow=dim(effort.results)[2]*dim(effort.results)[3],byrow=T),nrow=dim(effort.results)[3]))
      #                 , CJ(dimnames(effort.results)[[2]], dimnames(effort.results)[[4]], sorted = FALSE)[, paste(V1, V2, sep =" - ")])
  
  #create data frame to fill with summarized effort data
      Effort.summary<-setNames(as.data.frame(matrix(NA, nrow=0, ncol=7)), c("Day", "Section", "Gear", "Mean", "Median", "l95", "u95"))
    
  #loop through array and summarize by day, section, and gear type 
      for(period in 1:dim(effort.results)[3]){
          sub.period<-effort.results[1:nrow(effort.results), 1:dim(effort.results)[2], period, 1:dim(effort.results)[4]]
          sub.period[1:5,,]
          
          for(gear in 1:dim(sub.period)[2]){
            gear.xwalk
            sub.gear<-sub.period[1:nrow(effort.results), 1:dim(sub.period)[2], gear]
            sub.gear[1:5, ]
            
            sub.Effort.summary<-setNames(as.data.frame(matrix(NA, nrow=dim(sub.period)[2], ncol=7)), c("Day", "Section", "Gear", "Mean", "Median", "l95", "u95"))
            
            for(section in 1:dim(sub.period)[2]){
              sub.section<-sub.gear[, section]
              section.xwalk
              
              sub.Effort.summary[section, "Day"]<-as.numeric(period)
              sub.Effort.summary[section, "Section"]<-as.character(section.xwalk$Section_Name[section.xwalk$Section_Num == section])
              sub.Effort.summary[section, "Gear"]<-as.character(gear.xwalk$Gear_Name[gear.xwalk$Gear_Num == gear])
              sub.Effort.summary[section, "Mean"]<-mean(sub.section)
              sub.Effort.summary[section, "Median"]<-quantile(sub.section, 0.5)
              sub.Effort.summary[section, "l95"]<-quantile(sub.section, 0.025)
              sub.Effort.summary[section, "u95"]<-quantile(sub.section, 0.975)
            }
           Effort.summary<-rbind(Effort.summary, sub.Effort.summary) 
          }
      }
      all.Dates$Day<-as.numeric(as.character(all.Dates$Day)) #make sure "all.Dates$Day" is a numeric variable 
      
  #Join "Effort.summary" with "all.Dates" date and daytype information   
     Effort.summary<-left_join(Effort.summary, all.Dates[, c("Day", "Date", "DayType")], by="Day") 

  # write "Effort.summary" to a .csv file   
      write.csv(Effort.summary, paste(filepath_outputs, paste("Summary_Effort (total hours per Period) by Section and Gear", catch.group.of.interest, model.name, ".csv", sep="_"), sep="/"), row.names = F)

#--------------------------------------------------------------------------------------------------------------------- -
# SUMMARIZE CATCH AND CPUE DATA                                                                                 ----
#--------------------------------------------------------------------------------------------------------------------- -   
  #=============================================================================================================== =
  # CATCH SUMMARY
  #=============================================================================================================== =
    #Extract catch data and name dimensions (so that they are meaningful)
        catch.results<-res$C; dim(catch.results)
        dimnames(catch.results)<-list(seq(1:nrow(catch.results)), section.xwalk$Section_Name, paste("day",seq(1:standat$D), sep="_"), gear.xwalk$Gear_Name )
        # C.mean<-setNames(as.data.frame(matrix(matrix(apply(catch.results,c(2:4),mean),nrow=dim(catch.results)[2]*dim(catch.results)[3],byrow=T),nrow=dim(catch.results)[3]))
        #             , CJ(dimnames(catch.results)[[2]], dimnames(catch.results)[[4]], sorted = FALSE)[, paste(V1, V2, sep =" - ")])
    
    #create data frame to fill with summarized catch data
        Catch.summary<-setNames(as.data.frame(matrix(NA, nrow=0, ncol=7)), c("Day", "Section", "Gear", "Mean", "Median", "l95", "u95"))
      
    #loop through array and summarize by day, gear type, and section
        for(period in 1:dim(catch.results)[3]){
            sub.period<-catch.results[1:nrow(catch.results), 1:dim(catch.results)[2], period, 1:dim(catch.results)[4]]
            sub.period[1:5,,]
            
            for(gear in 1:dim(sub.period)[2]){
              
              gear.xwalk
              sub.gear<-sub.period[1:nrow(catch.results), 1:dim(sub.period)[2], gear]
              sub.gear[1:5, ]
              
              sub.Catch.summary<-setNames(as.data.frame(matrix(NA, nrow=dim(sub.period)[2], ncol=7)), c("Day", "Section", "Gear", "Mean", "Median", "l95", "u95"))
              
              for(section in 1:dim(sub.period)[2]){
                sub.section<-sub.gear[, section]
                section.xwalk
                
                sub.Catch.summary[section, "Day"]<-as.numeric(period)
                sub.Catch.summary[section, "Section"]<-as.character(section.xwalk$Section_Name[section.xwalk$Section_Num == section])
                sub.Catch.summary[section, "Gear"]<-as.character(gear.xwalk$Gear_Name[gear.xwalk$Gear_Num == gear])
                sub.Catch.summary[section, "Mean"]<-mean(sub.section)
                sub.Catch.summary[section, "Median"]<-quantile(sub.section, 0.5)
                sub.Catch.summary[section, "l95"]<-quantile(sub.section, 0.025)
                sub.Catch.summary[section, "u95"]<-quantile(sub.section, 0.975)
              }
             Catch.summary<-rbind(Catch.summary, sub.Catch.summary) 
            }
        }
        
    # Join "Catch.summary" with "all.Dates" date and daytype information   
       Catch.summary<-left_join(Catch.summary, all.Dates[, c("Day", "Date", "DayType")], by="Day") 
       
    # write "Catch.summary" to a .csv file   
       write.csv(Catch.summary, paste(filepath_outputs, paste("Summary_Catch (total fish per Period) by Gear and Section", catch.group.of.interest, model.name, ".csv", sep="_"), sep="/"), row.names = F)
       
  #=============================================================================================================== =
  # CPUE SUMMARY
  #=============================================================================================================== =
    #Extract catch data and name dimensions (so that they are meaningful)
        cpue.results<-res$lambda_C_S; dim(cpue.results)
        dimnames(cpue.results)<-list(seq(1:nrow(cpue.results)), section.xwalk$Section_Name, paste("day",seq(1:standat$D), sep="_"), gear.xwalk$Gear_Name )
        # C.mean<-setNames(as.data.frame(matrix(matrix(apply(cpue.results,c(2:4),mean),nrow=dim(cpue.results)[2]*dim(cpue.results)[3],byrow=T),nrow=dim(cpue.results)[3]))
        #             , CJ(dimnames(cpue.results)[[2]], dimnames(cpue.results)[[4]], sorted = FALSE)[, paste(V1, V2, sep =" - ")])
    
    #create data frame to fill with summarized catch data
        CPUE.summary<-setNames(as.data.frame(matrix(NA, nrow=0, ncol=7)), c("Day", "Section", "Gear", "Mean", "Median", "l95", "u95"))
      
    #loop through array and summarize by day, gear type, and section
        for(period in 1:dim(cpue.results)[3]){
            sub.period<-cpue.results[1:nrow(cpue.results), 1:dim(cpue.results)[2], period, 1:dim(cpue.results)[4]]
            sub.period[1:5,,]
            
            for(gear in 1:dim(sub.period)[2]){
              
              gear.xwalk
              sub.gear<-sub.period[1:nrow(cpue.results), 1:dim(sub.period)[2], gear]
              sub.gear[1:5, ]
              
              sub.CPUE.summary<-setNames(as.data.frame(matrix(NA, nrow=dim(sub.period)[2], ncol=7)), c("Day", "Section", "Gear", "Mean", "Median", "l95", "u95"))
              
              for(section in 1:dim(sub.period)[2]){
                sub.section<-sub.gear[, section]
                section.xwalk
                
                sub.CPUE.summary[section, "Day"]<-as.numeric(period)
                sub.CPUE.summary[section, "Section"]<-as.character(section.xwalk$Section_Name[section.xwalk$Section_Num == section])
                sub.CPUE.summary[section, "Gear"]<-as.character(gear.xwalk$Gear_Name[gear.xwalk$Gear_Num == gear])
                sub.CPUE.summary[section, "Mean"]<-round(mean(sub.section),3)
                sub.CPUE.summary[section, "Median"]<-round(quantile(sub.section, 0.5),3)
                sub.CPUE.summary[section, "l95"]<-round(quantile(sub.section, 0.025),3)
                sub.CPUE.summary[section, "u95"]<-round(quantile(sub.section, 0.975),3)
              }
             CPUE.summary<-rbind(CPUE.summary, sub.CPUE.summary) 
            }
        }
        
    # Join "CPUE.summary" with "all.Dates" date and daytype information   
       CPUE.summary<-left_join(CPUE.summary, all.Dates[, c("Day", "Date", "DayType")], by="Day") 
       
    # write "CPUE.summary" to a .csv file   
       write.csv(CPUE.summary, paste(filepath_outputs, paste("Summary_CPUE (fish per hr per Period) by Gear and Section", catch.group.of.interest, model.name, ".csv", sep="_"), sep="/"), row.names = F)   

#--------------------------------------------------------------------------------------------------------------------- -
# GENERATE PLOTS OF EFFORT AND CATCH SUMMARIES                                                                      ----
#--------------------------------------------------------------------------------------------------------------------- -        
      
  #plot timeseries and total effort and catch
    plot.width<-8.5
    plot.height<-11
    
  #Set up plotting arguments
    eff.pch=21
    eff.cex=2
    gear.cols<-c(colorRampPalette(brewer.pal(max(3,as.numeric(length(unique(Effort.summary$Gear)))),"Blues"))(length(unique(Effort.summary$Gear))))
    section.cols<-c(colorRampPalette(brewer.pal(max(3,as.numeric(length(unique(Effort.summary$Section)))),"Greens"))(length(unique(Effort.summary$Section))))

  #Self-made jitter sequence for x-axis
    jitter.right<-seq(0,1,0.25)
    temp.jitter<-c()
    for(value in 1:length(jitter.right)){
      temp.jitter<-c(temp.jitter, c(jitter.right[value], jitter.right[value]*-1))
    }
    jitter.seq<-as.numeric(unique(temp.jitter))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
#START OF PDF FUNCTION!!!!! 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
  pdf(paste(filepath_outputs, paste("BSS creel model summary plots", catch.group.of.interest, model.name,".pdf", sep="_"), sep="/"), width=plot.width, height=plot.height)
  mpg_axis<-0
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
  # Plot #1 - PDF of total seasonal catch and total seasonal effort
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
      # set par arguments  
        #windows(width=8.5, height=11)
        par(mfcol=c(2,1), family='sans',  xaxs="i", yaxs="i", cex.axis=1, cex=1, mgp=c(2, 0.75, mpg_axis), mar=c(3,3,1,1), oma=c(4,3,1.5,1)) 
    
      #total seasonal catch    
        plot(density(apply(res$lambda_Ctot_S, c(1), sum)), col="blue", xlab="Total Catch (fish)", ylab="Probability Density", main="", bty="n")
        #abline(v=mean(c(apply(res$lambda_Ctot_S, c(1), sum))),lwd=2, lty=2, col="red") #add abline for mean
        abline(v=median(c(apply(res$lambda_Ctot_S, c(1), sum))),lwd=2, lty=2,  col="red") #add abline for median
        abline(v=quantile(apply(res$lambda_Ctot_S, c(1), sum), 0.025),lwd=2, lty=2,  col="black"); abline(v=quantile(apply(res$lambda_Ctot_S, c(1), sum), 0.975),lwd=2, lty=2,  col="black")  #add ablines for 95%
        legend("topright", c( "median", "95%CI"), lty=2, col=c("red", "black"), bty="n", lwd=2)
      
      #total seasonal effort  
        plot(density(res$E_sum),col="blue",xlab="Total Effort (hrs)", ylab="Probability Density", main="", bty="n")
        #abline(v=mean(res$E_sum),lwd=2, lty=2, col="red") #add abline for mean
        abline(v=quantile(res$E_sum, 0.5),lwd=2, lty=2,  col="red") #add abline for median
        abline(v=quantile(res$E_sum, 0.025),lwd=2, lty=2,  col="black"); abline(v=quantile(res$E_sum, 0.975),lwd=2, lty=2,  col="black")  #add ablines for 95%
        
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
  # Plot #2 - Plot Total Daily Effort (hrs) by Section and Gear Type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
      # windows(height=11, width=8.5)
        
      # set par arguments    
          par(mfcol=c(length(unique(Effort.summary$Section)),1), family='sans', xaxs="i", yaxs="i",cex.axis=1, cex=1, mgp=c(2,0.75, mpg_axis),mar=c(2,2,1,1), oma=c(3,3,1.5,1)) 
      
      # Loop through "Effort.summary" and plot
          for(section in 1:length(unique(Effort.summary$Section))){
              sub.section<-Effort.summary[Effort.summary$Section == unique(Effort.summary$Section)[section],]
              
              plot(NA, bty="n", ylim=c(-10, round_any(max(Effort.summary$u95, na.rm=T),200,f=ceiling)), xlim=c(0, max(Effort.summary$Day, na.rm=T)+1)) #, main=paste("Section - ", unique(Effort.summary$Section)[section], sep=""))
              
              for(day in 1:length(unique(sub.section$Day))){
                  sub.day<-sub.section[sub.section$Day == unique(sub.section$Day)[day],]
                  if(sub.day$DayType[1]==2){
                      abline(v=unique(sub.section$Day)[day], col="gray", lty=2, lwd=2)
              }}
                  for(gear in 1:length(unique(sub.section$Gear))){
                    sub.gear<-sub.section[sub.section$Gear == unique(sub.section$Gear)[gear],]
                    x.axis<-as.numeric(as.character(sub.gear$Day))+jitter.seq[gear]
                    
                    arrows(x0=(as.numeric(as.character(sub.gear$Day))+jitter.seq[gear]), y0=as.numeric(sub.gear$l95)
                         , x1=(as.numeric(as.character(sub.gear$Day))+jitter.seq[gear]), y1=as.numeric(sub.gear$u95)
                         , length=0.05, angle=90, code=3, lwd=2)
                    points(sub.gear$Median ~ x.axis, bg=gear.cols[gear], pch=eff.pch, cex=eff.cex, col="black")
        
                  }
              for(day in 1:length(unique(sub.section$Day))){
                  sub.day<-sub.section[sub.section$Day == unique(sub.section$Day)[day],]
                  if(sub.day$Mean[1] == 0){
                    x.line<-sub.day$Day+jitter.seq[1:length(unique(sub.section$Gear))]
                    lines(x=c(min(x.line), max(x.line)), y=c(0,0), col='gray', lend=2, lwd=30)
              }
              legend("topleft", legend = unique(Effort.summary$Section)[section], bty="n", bg="white", box.col = "white")  
              }
          }
          legend("topright", legend = unique(Effort.summary$Gear), bty="n", cex=1, pch=eff.pch, col="black", pt.bg = gear.cols, pt.cex=eff.cex)
          title(xlab = "Day", ylab = "Total Daily Effort (hrs)", outer=T, line=1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
  # Plot #3 - Plot Total Daily Catch (fish) by Section and Gear Type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
      #windows(height=11, width=8.5)
      par(mfcol=c(length(unique(Catch.summary$Section)),1), family='sans', xaxs="i", yaxs="i",cex.axis=1, cex=1, mgp=c(2,0.75, mpg_axis),mar=c(2,2,1,1), oma=c(3,3,1.5,1)) 
  

      for(section in 1:length(unique(Catch.summary$Section))){
          sub.section<-Catch.summary[Catch.summary$Section == unique(Catch.summary$Section)[section],]
          
          plot(NA, bty="n", ylim=c(-1, round_any(max(Catch.summary$u95, na.rm=T),10,f=ceiling)), xlim=c(0, max(Catch.summary$Day, na.rm=T)+1)) #, main=paste("Section - ", unique(Catch.summary$Section)[section], sep=""))
          
          for(day in 1:length(unique(sub.section$Day))){
              sub.day<-sub.section[sub.section$Day == unique(sub.section$Day)[day],]
              if(sub.day$DayType[1]==2){
                  abline(v=unique(sub.section$Day)[day], col="gray", lty=2, lwd=2)
          }}
              for(gear in 1:length(unique(sub.section$Gear))){
                sub.gear<-sub.section[sub.section$Gear == unique(sub.section$Gear)[gear],]
                x.axis<-as.numeric(as.character(sub.gear$Day))+jitter.seq[gear]
                
                arrows(x0=(as.numeric(as.character(sub.gear$Day))+jitter.seq[gear]), y0=as.numeric(sub.gear$l95)
                     , x1=(as.numeric(as.character(sub.gear$Day))+jitter.seq[gear]), y1=as.numeric(sub.gear$u95)
                     , length=0.05, angle=90, code=3, lwd=2)
                points(sub.gear$Median ~ x.axis, bg=gear.cols[gear], pch=eff.pch, cex=eff.cex, col="black")
    
              }
          for(day in 1:length(unique(sub.section$Day))){
              sub.day<-sub.section[sub.section$Day == unique(sub.section$Day)[day],]
              if(sub.day$Mean[1] == 0){
                x.line<-sub.day$Day+jitter.seq[1:length(unique(sub.section$Gear))]
                lines(x=c(min(x.line), max(x.line)), y=c(0,0), col='gray', lend=2, lwd=30)
          }
          legend("topleft", legend = unique(Catch.summary$Section)[section], bty="n", bg="white", box.col = "white")  
          }
      }
      legend("topright", legend = unique(Catch.summary$Gear), bty="n", cex=1, pch=eff.pch, col="black", pt.bg = gear.cols, pt.cex=eff.cex)
      title(xlab = "Day", ylab = "Total Daily Catch (fish)", outer=T, line=1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
  # Plot #4 - Plot  Daily CPUE (fish/hr)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
      #windows(height=11, width=8.5)
      par(mfcol=c(length(unique(CPUE.summary$Section)),1), family='sans', xaxs="i", yaxs="i",cex.axis=1, cex=1, mgp=c(2,0.75, mpg_axis),mar=c(2,2,1,1), oma=c(3,3,1.5,1)) 
  

      for(section in 1:length(unique(CPUE.summary$Section))){
          sub.section<-CPUE.summary[CPUE.summary$Section == unique(CPUE.summary$Section)[section],]
          
          plot(NA, bty="n", ylim=c(0, round_any(max(CPUE.summary$u95, na.rm=T),0.05,f=ceiling)), xlim=c(0, max(CPUE.summary$Day, na.rm=T)+1)) #, main=paste("Section - ", unique(CPUE.summary$Section)[section], sep=""))
          
          for(day in 1:length(unique(sub.section$Day))){
              sub.day<-sub.section[sub.section$Day == unique(sub.section$Day)[day],]
              if(sub.day$DayType[1]==2){
                  abline(v=unique(sub.section$Day)[day], col="gray", lty=2, lwd=2)
          }}
              for(gear in 1:length(unique(sub.section$Gear))){
                sub.gear<-sub.section[sub.section$Gear == unique(sub.section$Gear)[gear],]
                x.axis<-as.numeric(as.character(sub.gear$Day))+jitter.seq[gear]
                
                arrows(x0=(as.numeric(as.character(sub.gear$Day))+jitter.seq[gear]), y0=as.numeric(sub.gear$l95)
                     , x1=(as.numeric(as.character(sub.gear$Day))+jitter.seq[gear]), y1=as.numeric(sub.gear$u95)
                     , length=0.05, angle=90, code=3, lwd=2)
                points(sub.gear$Median ~ x.axis, bg=gear.cols[gear], pch=eff.pch, cex=eff.cex, col="black")
    
              }
          for(day in 1:length(unique(sub.section$Day))){
              sub.day<-sub.section[sub.section$Day == unique(sub.section$Day)[day],]
              if(sub.day$Mean[1] == 0){
                x.line<-sub.day$Day+jitter.seq[1:length(unique(sub.section$Gear))]
                lines(x=c(min(x.line), max(x.line)), y=c(0,0), col='gray', lend=2, lwd=30)
          }
          legend("topleft", legend = unique(CPUE.summary$Section)[section], bty="n", bg="white", box.col = "white")  
          }
      }
      legend("topright", legend = unique(CPUE.summary$Gear), bty="n", cex=1, pch=eff.pch, col="black", pt.bg = gear.cols, pt.cex=eff.cex)
      title(xlab = "Day", ylab = "Daily CPUE (fish/hr)", outer=T, line=1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
  # Plot #5 - Total catch and effort by day
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~


dev.off()
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
#END OF PDF FUNCTION!!!!! 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
