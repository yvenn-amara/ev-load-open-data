##### 1. Global Libraries #####
library(tidyverse)
library(lubridate)
library(stringr)
Sys.setlocale("LC_ALL","English")

##### 2. Data Prep #####

## Boulder

# Previous Format
# boulder_prep = function(df){
#   Sys.setlocale("LC_ALL","English")
#   boulder = na.omit(df) %>%
#     rename("Energy" = "Energy..kWh.",
#            "Station" = "ï..Station.Name") %>%
#     mutate(Charge.Duration = as.numeric(sub(",","",as.character(Charging.Time..minutes.),fixed=T)),
#            Start = parse_date_time(paste(Transaction.Date, Transaction.Start.Time), '%m/%d/%Y %I:%M %p', tz='America/Denver'),
#            End = Start + minutes(Charge.Duration),
#            Arrival=round(hour(Start)+minute(Start)/60,2),
#            Day = wday(Start,label=T,abbr=F),
#            Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0))%>%
#     select(Start,End,Arrival,Charge.Duration,Energy,Day,Weekend,Station) %>%
#     filter(Charge.Duration > 0,
#            Charge.Duration < 1440, 
#            Energy < 100, 
#            Energy > 0 ) %>%
#     arrange(Start)
#   
#   return(boulder)
# }


boulder2_prep = function(df){
  boulder = raw_boulder %>%
    select(Start_Date___Time,End_Date___Time,Total_Duration__hh_mm_ss_,Charging_Time__hh_mm_ss_,Energy__kWh_) %>%
    separate(Charging_Time__hh_mm_ss_,c("Hour_Charge_Duration","Minute_Charge_Duration","Second_Charge_Duration"),":") %>%
    separate(Total_Duration__hh_mm_ss_,c("Hour_Total_Duration","Minute_Total_Duration","Second_Total_Duration"),":") %>%
    mutate(Start_Date___Time = gsub("\\+00","",Start_Date___Time),
           End_Date___Time = gsub("\\+00","",End_Date___Time)) %>%
    rename('Energy'='Energy__kWh_') %>%     
    mutate(Park.Duration = as.numeric(Hour_Total_Duration)*60 + as.numeric(Minute_Total_Duration) + as.numeric(Second_Total_Duration)/60,
           Charge.Duration = as.numeric(Hour_Charge_Duration)*60 + as.numeric(Minute_Charge_Duration) + as.numeric(Second_Charge_Duration)/60,
           Start = parse_date_time(Start_Date___Time, '%Y/%m/%d %H:%M:%S', tz='America/Denver'),
           End = parse_date_time(End_Date___Time, '%Y/%m/%d %H:%M:%S', tz='America/Denver'),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0),
           Arrival=round(hour(Start)+minute(Start)/60,2)) %>%
    filter(Charge.Duration > 0,
           Charge.Duration < 1440,
           Park.Duration > 0,
           Park.Duration < 1440,
           Energy < 100,
           Energy > 0 ) %>%
    arrange(Start) %>% 
    select(Start,End,Day,Weekend,Arrival,Charge.Duration,Park.Duration,Energy) %>%
    na.omit(.)
  return(boulder)
}


## Palo Alto

# Previous Data Format
# palo_alto_prep = function(df){
#   Sys.setlocale("LC_ALL","English")
#   df = df %>%
#     select(Start.Date,End.Date,Park.Duration,Charge.Duration,Energy..kWh.,Ended.By,Latitude,Longitude,Postal.Code,Driver.Postal.Code) %>%
#     rename('Energy'='Energy..kWh.') %>%
#     mutate(Park.Duration = as.numeric(sub(",","",as.character(Park.Duration),fixed = T)),
#            Charge.Duration = as.numeric(sub(",","",as.character(Charge.Duration),fixed=T)),
#            Start = parse_date_time(Start.Date, '%d/%m/%Y %H:%M', tz='US/Pacific'),
#            End = Start + minutes(round(Park.Duration)),
#            Day = wday(Start,label=T,abbr=F),
#            Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0),
#            Arrival=round(hour(Start)+minute(Start)/60,2)) %>%
#     filter(Charge.Duration > 0,
#            Charge.Duration < 1440, 
#            Park.Duration > 0,
#            Park.Duration < 1440,
#            Energy < 100, 
#            Energy > 0 ) %>%
#     arrange(Start)
#   return(df)
# }

palo_alto_prep = function(df){
  Sys.setlocale("LC_ALL","English")
  df = df %>%
    select(Start.Date,End.Date,Total.Duration..hh.mm.ss.,Charging.Time..hh.mm.ss.,Energy..kWh.) %>%
    separate(Charging.Time..hh.mm.ss.,c("Hour_Charge_Duration","Minute_Charge_Duration","Second_Charge_Duration"),":") %>%
    separate(Total.Duration..hh.mm.ss.,c("Hour_Total_Duration","Minute_Total_Duration","Second_Total_Duration"),":") %>%
    rename('Energy'='Energy..kWh.') %>%
    mutate(Park.Duration = as.numeric(Hour_Total_Duration)*60 + as.numeric(Minute_Total_Duration) + as.numeric(Second_Total_Duration)/60,
           Charge.Duration = as.numeric(Hour_Charge_Duration)*60 + as.numeric(Minute_Charge_Duration) + as.numeric(Second_Charge_Duration)/60,
           Start = parse_date_time(Start.Date, '%m/%d/%Y %H:%M', tz='US/Pacific'),
           End = parse_date_time(End.Date, '%m/%d/%Y %H:%M', tz='US/Pacific'),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0),
           Arrival=round(hour(Start)+minute(Start)/60,2)) %>%
    filter(Charge.Duration > 0,
           Charge.Duration < 1440,
           Park.Duration > 0,
           Park.Duration < 1440,
           Energy < 100,
           Energy > 0 ) %>%
    arrange(Start) %>%
    select(Start,End,Day,Weekend,Arrival,Charge.Duration,Park.Duration,Energy) %>%
    na.omit(.)
  return(df)
}

## Dundee
dundee_prep = function(df_list){
  Sys.setlocale("LC_ALL","English")
  df_list[[4]] = df_list[[4]] %>%
    select(Start.Date,Start.Time,End.Date,End.Time,Total.kWh) %>%
    na.omit() %>%
    rename('Energy'='Total.kWh') %>%
    mutate(Start.Date = as.Date(Start.Date,format='%d/%m/%Y'),
           End.Date = as.Date(End.Date,format='%d/%m/%Y'),
           Start = parse_date_time(paste(Start.Date, Start.Time), '%Y-%m-%d %H:%M', tz='Europe/London'),
           End = parse_date_time(paste(End.Date, End.Time), '%Y-%m-%d %H:%M', tz='Europe/London'),
           Park.Duration = as.numeric(difftime(End,Start,'Europe/London',"mins")),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0)
    )
  
  df = bind_rows(df_list[[1]],df_list[[2]],df_list[[3]]) %>%
    select(Start.Date,Start.Time,End.Date,End.Time,Total.kWh) %>%
    na.omit() %>%
    rename('Energy'='Total.kWh') %>%
    mutate(Start.Date = as.Date(Start.Date),
           End.Date = as.Date(End.Date),
           Start = parse_date_time(paste(Start.Date, Start.Time), '%Y-%m-%d %H:%M', tz='Europe/London'),
           End = parse_date_time(paste(End.Date, End.Time), '%Y-%m-%d %H:%M', tz='Europe/London')
    ) %>%
    filter(End > Start) %>% # because row 7848 has 1970-01-01T00:00:00 as an end date which is certainly an issue in the data and they seem to be many more
    mutate(Park.Duration = as.numeric(difftime(End,Start,'Europe/London',"mins")),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0)
    )
  return(bind_rows(df,df_list[[4]]) %>% 
           mutate(Arrival=round(hour(Start)+minute(Start)/60,2)) %>%
           filter(Park.Duration > 0,
                  Park.Duration < 1440, # Remove durations of more than 1 day; Indeed, there is one transaction that lasts for a total of 250 days (so about all data) => broken plug or signal
                  Energy < 100, # Remove Energy above 200 kWh; There is a transactions of 1430.17 kWh (the only one removed by this filtering after previous filters have been applied)
                  Energy > 0 # There are transactions with negative Energy (V2G?)
           ) %>%
           arrange(Start) %>% # order by Start Column
           select(Start,End,Day,Weekend,Arrival,Park.Duration,Energy) %>%
           na.omit(.)
  )
}

## Perth and Kinross
prep_perth = function(df){
  df = df %>%
    select(Start.Date,Start.Time,End.Date,End.Time, Total.kWh, Site) %>%
    separate(Start.Date, c("Start.Date","Start.Date_remainder"),"T") %>%
    separate(End.Date, c("End.Date","End.Date_remainder"),"T") %>%
    na.omit() %>%
    rename('Energy'='Total.kWh') %>%
    mutate(Start = parse_date_time(paste(Start.Date, Start.Time), '%Y-%m-%d %H:%M', tz='Europe/London'),
           End = parse_date_time(paste(End.Date, End.Time), '%Y-%m-%d %H:%M', tz='Europe/London'),
           Park.Duration = as.numeric(difftime(End,Start,'Europe/London',"mins")),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0),
           Arrival=round(hour(Start)+minute(Start)/60,2)
    )
  
  return(df %>% filter(Park.Duration > 0,
                       Park.Duration < 1440, 
                       Energy < 100, 
                       Energy > 0 ) %>% # Some negative values # There are transactions with negative Energy (V2G?)
           arrange(Start) %>% # order by Start Column 
           select(Start,End,Day,Weekend,Arrival,Park.Duration,Energy) %>%
           na.omit(.)) 
}

## Paris

prep_paris = function(df){
  Sys.setlocale("LC_ALL","English")
  df = df %>%
    select(Date.de.dÃ.but, Date.de.fin, L.Ã.nergie..Wh.) %>%
    rename('Energy'="L.Ã.nergie..Wh.") %>%
    mutate(Start = parse_date_time(sub(' GMT+0200 (CEST)','',Date.de.dÃ.but,fixed=T), '%a %b %d %Y %H:%M:%S', tz='Europe/Paris'),
           End = parse_date_time(sub(' GMT+0200 (CEST)','',Date.de.fin,fixed=T), '%a %b %d %Y %H:%M:%S', tz='Europe/Paris'),
           Park.Duration = as.numeric(difftime(End,Start,'Europe/Paris',"mins")),
           Energy = Energy/1000,
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0),
           Arrival=round(hour(Start)+minute(Start)/60,2)
    )
  
  return(df %>% filter(Park.Duration > 0,
                       Park.Duration < 1440, 
                       Energy < 100, 
                       Energy > 0 ) %>%
           arrange(Start) %>% # order by Start Column
           select(Start,End,Day,Weekend,Arrival,Park.Duration,Energy) %>%
           na.omit(.)) 
}

## Domestics UK

prep_domestics = function(df){
  
  df = df %>%
    mutate(Start = parse_date_time(paste(StartDate, StartTime), '%Y-%m-%d %H:%M:%S', tz='Europe/London'),
           End = parse_date_time(paste(EndDate, EndTime), '%Y-%m-%d %H:%M:%S', tz='Europe/London'),
           PluginDuration = PluginDuration*60,
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0),
           Arrival=round(hour(Start)+minute(Start)/60,2)) %>%
    rename('Park.Duration'='PluginDuration')
  
  
  return(df %>% filter(Park.Duration > 0,
                       Park.Duration < 1440, 
                       Energy < 100, 
                       Energy > 0 ) %>%
           arrange(Start) %>%
           select(Start,End,Day,Weekend,Arrival,Park.Duration,Energy) %>%
           na.omit(.))
}

## ACN

to_char_names = function(x){
  y = as.character(x)
  names(y) = names(x)
  return(y)
}

prep_acn = function(df){
  df = df %>%
    separate(connectionTime, c("start_dow","Start_Time"),", ") %>%
    separate(disconnectTime, c("end_dow","End_Time"),", ") %>%
    separate(doneChargingTime, c("end_charge_dow","End_Charge"),", ") %>%
    na.omit(.) %>%
    mutate(Start_Time = gsub(" GMT", "", Start_Time),
           End_Time = gsub(" GMT", "", End_Time),
           End_Charge = gsub(" GMT", "", End_Charge)) %>%
    mutate(Start = parse_date_time(Start_Time, '%d %b %Y %H:%M:%S', tz='GMT'),
           End = parse_date_time(End_Time, '%d %b %Y %H:%M:%S', tz='GMT'),
           End_Charge = parse_date_time(End_Charge, '%d %b %Y %H:%M:%S', tz='GMT'),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0)) %>%
    na.omit(.) %>%
    mutate(Energy = as.numeric(as.character(kWhDelivered)),
           Park.Duration = as.numeric(difftime(End, Start, units='mins')),
           Charge.Duration = as.numeric(difftime(End_Charge, Start, units='mins')),
           Idle.Duration = Park.Duration - Charge.Duration) %>%
    mutate(Start = with_tz(Start, "America/Los_Angeles"),
           End = with_tz(End, "America/Los_Angeles"),
           Arrival=round(hour(Start)+minute(Start)/60,2)) %>%
    select(Start,End,Day,Weekend,Arrival,Charge.Duration,Idle.Duration,Park.Duration,Energy)


  return(df %>% filter(Charge.Duration > 0,
                       Charge.Duration < 1440,
                       Park.Duration > 0,
                       Park.Duration < 1440,
                       Energy < 100,
                       Energy > 0 ) %>%
           arrange(Start))
  
  return(df)
}

## SAP
prep_sap = function(df){
  df = df %>%
    separate(Date.demarrage, c("Start_Date","Start_Time"),"T") %>%
    separate(Start_Time, c("Start_Time","Start_UTC+"),"\\+") %>%
    separate(Date.fin, c("End_Date","End_Time"),"T") %>%
    separate(End_Time, c("End_Time","End_UTC+"),"\\+") %>%
    mutate(Start = parse_date_time(paste(Start_Date,Start_Time), '%Y-%m-%d %H:%M:%S', tz='Europe/Paris'),
           End = parse_date_time(paste(End_Date,End_Time), '%Y-%m-%d %H:%M:%S', tz='Europe/Paris'),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0)) %>%
    rename('Park.Duration'='Duree.totale..s.',
           'Idle.Duration' = 'Duree.inactivite.totale..s.',
           "Energy" = "Consommation.totale..Wh.") %>%
    mutate(Energy = Energy/1000,
           Park.Duration = Park.Duration/60,
           Idle.Duration = Idle.Duration/60,
           Charge.Duration = Park.Duration-Idle.Duration,
           Arrival=round(hour(Start)+minute(Start)/60,2)) %>%
    select(Start,End,Day,Weekend,Arrival,Charge.Duration,Idle.Duration,Park.Duration,Energy)
  
  
  return(df %>% filter(Charge.Duration > 0,
                       Charge.Duration < 1440,
                       Park.Duration > 0,
                       Park.Duration < 1440,
                       Energy < 100, 
                       Energy > 0 ) %>%
           arrange(Start))
}




##### 3. General Statistics #####

quick_stats = function(raw,cleaned){
  
  print(paste('Transactions Retained: ',nrow(cleaned)," (",round(100*nrow(cleaned)/nrow(raw),2), "%)", sep=""   ))
  print(paste('Transactions Discarded: ',nrow(raw) - nrow(cleaned)," (",round(100*(nrow(raw) - nrow(cleaned))/nrow(raw),2), "%)", sep="" ))
  
  print(paste('First Transaction:',min(date(cleaned$Start))))
  print(paste('Last Transaction:',max(date(cleaned$Start))))

}

##### 4. Fitting Statistical Distributions ########

distr = function(series){
  require(fitdistrplus)
  results = tibble(distrib = c("gamma",
                               "exp",
                               "lnorm",
                               "weibull",
                               "norm"),AIC = rep(0,5))
  for (i in 1:5){
    fit = fitdist(series + 0.0001#fixing boundary issues
                  , distr = results$distrib[i], method = "mle", lower = c(0, 0))
    results$AIC[i] = fit$aic
  }
  return(results %>% arrange(AIC))
}


