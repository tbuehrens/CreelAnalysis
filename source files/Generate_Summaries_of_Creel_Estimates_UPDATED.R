#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file summarizes creel model estimates by creating summary plots and tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#--------------------------------------------------------------------------------------------------------------------- -
# SUMMARIZE SEASON TOTAL EFFORT AND CATCH                                                                ----
#--------------------------------------------------------------------------------------------------------------------- - 
   #str(res$E)
    catch.effort.totals<-
      res_Stan %>% 
      gather_draws(C_sum, E_sum) %>% 
      group_by(.variable) %>% 
      summarise(mean = mean(.value), SD = sd(.value), l95 = quantile(.value, c(0.025)), Median = quantile(.value, c(0.5)), u95 = quantile(.value, c(0.975)), .groups = "drop", "CV" = SD/mean)
    write.csv(catch.effort.totals, paste(filepath_Run, paste("summary - season total catch and effort", catch.group.of.interest, paste0("Run_", Model_Run), ".csv", sep="_"), sep="/"), row.names = F)

#--------------------------------------------------------------------------------------------------------------------- -
# SUMMARIZE TOTAL EFFORT DATA - by date, section, and gear-type (all creel models estimate effort by section and gear)                                                                                 ----
#--------------------------------------------------------------------------------------------------------------------- - 
  #str(res$E)
  Effort.summary<-  
    res_Stan %>% 
      spread_draws(E[Section, Day, Gear]) %>% 
      group_by(Section, Day, Gear) %>% 
      summarise(mean = mean(E), SD = sd(E), l95 = quantile(E, c(0.025)), Median = quantile(E, c(0.5)), u95 = quantile(E, c(0.975)), .groups = "drop") %>% 
      left_join(all.Dates[, c("Day", "Date")] %>% mutate(Day = as.integer(Day)), by="Day") %>% 
      mutate(DOW = lubridate::wday(as_date(Date), label=TRUE)) %>% 
      mutate_if(is.integer, as.character) %>% 
      left_join(gear.xwalk, by=c("Gear" = "Gear_Num")) %>% 
      left_join(section.xwalk, by=c("Section" = "Section_Num")) %>% 
      dplyr::select(-Section, -Day, -Gear) %>% 
      dplyr::select(Date, DOW, Gear = Gear_Name, Section = Section_Name, everything() )

  # write "Effort.summary" to a .csv file   
    write.csv(Effort.summary, paste(filepath_Run, paste("summary - angler effort (hours per Period) by Section and Gear", catch.group.of.interest, paste0("Run_", Model_Run), ".csv", sep="_"), sep="/"), row.names = F)

#--------------------------------------------------------------------------------------------------------------------- -
# SUMMARIZE CATCH AND                                                                               ----
#--------------------------------------------------------------------------------------------------------------------- -  
  #str(res$C)
  Catch.summary<-  
    res_Stan %>% 
      spread_draws(C[Section, Day, Gear]) %>% 
      group_by(Section, Day, Gear) %>% 
      summarise(mean = mean(C), SD = sd(C), l95 = quantile(C, c(0.025)), Median = quantile(C, c(0.5)), u95 = quantile(C, c(0.975)), .groups = "drop") %>% 
      left_join(all.Dates[, c("Day", "Date")] %>% mutate(Day = as.integer(Day)), by="Day") %>% 
      mutate(DOW = lubridate::wday(as_date(Date), label=TRUE)) %>% 
      mutate_if(is.integer, as.character) %>% 
      left_join(gear.xwalk, by=c("Gear" = "Gear_Num")) %>% 
      left_join(section.xwalk, by=c("Section" = "Section_Num")) %>% 
      dplyr::select(-Section, -Day, -Gear) %>% 
      dplyr::select(Date, DOW, Gear = Gear_Name, Section = Section_Name, everything() )

  # write "Effort.summary" to a .csv file   
    write.csv(Catch.summary, paste(filepath_Run, paste("summary - catch (hours per Period) by Section and Gear", catch.group.of.interest, paste0("Run_", Model_Run), ".csv", sep="_"), sep="/"), row.names = F)
 
#--------------------------------------------------------------------------------------------------------------------- -
# SUMMARIZE CPUE                                                                             ----
#--------------------------------------------------------------------------------------------------------------------- -  
  #str(res$lambda_C_S)
  CPUE.summary<-  
    res_Stan %>% 
      spread_draws(lambda_C_S[Section, Day, Gear]) %>% 
      group_by(Section, Day, Gear) %>% 
      summarise(mean = mean(lambda_C_S), SD = sd(lambda_C_S), l95 = quantile(lambda_C_S, c(0.025)), Median = quantile(lambda_C_S, c(0.5)), u95 = quantile(lambda_C_S, c(0.975)), .groups = "drop") %>% 
      left_join(all.Dates[, c("Day", "Date")] %>% mutate(Day = as.integer(Day)), by="Day") %>% 
      mutate(DOW = lubridate::wday(as_date(Date), label=TRUE)) %>% 
      mutate_if(is.integer, as.character) %>% 
      left_join(gear.xwalk, by=c("Gear" = "Gear_Num")) %>% 
      left_join(section.xwalk, by=c("Section" = "Section_Num")) %>% 
      dplyr::select(-Section, -Day, -Gear) %>% 
      dplyr::select(Date, DOW, Gear = Gear_Name, Section = Section_Name, everything() )

  # write "Effort.summary" to a .csv file   
    write.csv(Catch.summary, paste(filepath_Run, paste("summary - CPUE (fish per hr per Period) by Section and Gear", catch.group.of.interest, paste0("Run_", Model_Run), ".csv", sep="_"), sep="/"), row.names = F)

#--------------------------------------------------------------------------------------------------------------------- -
# GENERATE PLOTS OF EFFORT AND CATCH SUMMARIES                                                                      ----
#--------------------------------------------------------------------------------------------------------------------- -        
  #plot timeseries and total effort and catch
    plot.width<-8.5
    plot.height<-if_else(nrow(section.xwalk)>1, 11,plot.width)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
#START OF PDF FUNCTION!!!!! 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
  pdf(paste(filepath_Run, paste("summary - plots", catch.group.of.interest, paste0("Run_", Model_Run),".pdf", sep="_"), sep="/"), width=plot.width, height=plot.height)
  mpg_axis<-0
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
  # Plot #1 - PDF of total seasonal catch and total seasonal effort
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
      lims<-
        res_Stan %>% 
        gather_draws(C_sum, E_sum) %>% 
        group_by(.variable) %>% 
        summarise(value = quantile(.value, c(0, 0.99)), q = c(0,0.99), .groups = "drop") %>%
        pivot_wider(names_from = q, values_from=value)

      season_results_trunc<-
        res_Stan %>% 
        gather_draws(C_sum, E_sum) %>% 
        left_join(lims, by = ".variable") %>% 
        filter(.value>as.numeric(`0`) & .value < as.numeric(`0.99`))

      season_total_plot<-  
          ggplot(season_results_trunc, aes(x=.value, fill=.variable))+
            facet_wrap(~.variable, ncol=1,  scales = 'free')+
            theme_bw()+
            geom_density()+
            geom_vline(res_Stan %>% gather_draws(C_sum, E_sum) %>% group_by(.variable)%>% summarise(value = quantile(.value, c(0.025, 0.5, 0.975)), q = c(0.025, 0.5, 0.975), .groups = "drop"), mapping=aes(xintercept=value, group=.variable), linetype="dashed")+
            ylab("Density of Posterior Draws")+
            theme(legend.position = "none", legend.title = element_blank())
      print(season_total_plot)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
  # Plot #2 - Plot Total Daily Effort (hrs) by Section and Gear Type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
      plot_effort<-
        Effort.summary %>% 
        ggplot(aes(x=as.Date(Date), y=Median, colour=as.factor(Gear))) +
        geom_line()+
        facet_wrap(~Section, ncol = 1)+
        ylab('Angler Effort (hours)') + xlab('Date')+
        geom_ribbon(aes(ymin=l95, ymax=u95, fill=as.factor(Gear)),alpha=0.2,col=NA)+ 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank() )
      print(plot_effort)
          
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
  # Plot #3 - Plot Total Daily Catch (fish) by Section and Gear Type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
      plot_catch<-
        Catch.summary %>% 
        ggplot(aes(x=as.Date(Date), y=Median, colour=as.factor(Gear))) +
        geom_line()+
        facet_wrap(~Section, ncol = 1)+
        ylab(paste0('Catch - ', catch.group.of.interest)) + xlab('Date')+
        geom_ribbon(aes(ymin=l95, ymax=u95, fill=as.factor(Gear)),alpha=0.2,col=NA)+ 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank() )
      print(plot_catch)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
  # Plot #4 - Plot  Daily CPUE (fish/hr)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~
      plot_cpue<-
        CPUE.summary %>% 
        ggplot(aes(x=as.Date(Date), y=Median, colour=as.factor(Gear))) +
        geom_line()+
        facet_wrap(~Section, ncol = 1)+
        ylab(paste0('CPUE (catch per hour) - ', catch.group.of.interest)) + xlab('Date')+
        geom_ribbon(aes(ymin=l95, ymax=u95, fill=as.factor(Gear)),alpha=0.2,col=NA)+ 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank() )
      print(plot_cpue)

dev.off()
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
#END OF PDF FUNCTION!!!!! 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
