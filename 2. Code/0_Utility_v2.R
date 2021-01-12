##### 1. Global Libraries #####
library(tidyverse)
library(lubridate)
Sys.setlocale("LC_ALL","English")

##### 2. Data Prep #####

## Boulder

boulder_prep = function(df){
  Sys.setlocale("LC_ALL","English")
  boulder = na.omit(df) %>%
    rename("Energy" = "Energy..kWh.") %>%
    mutate(Charge.Duration = as.numeric(sub(",","",as.character(Charging.Time..minutes.),fixed=T)),
           Start = parse_date_time(paste(Transaction.Date, Transaction.Start.Time), '%m/%d/%Y %I:%M %p', tz='America/Denver'),
           End = Start + minutes(Charge.Duration),
           #Hour_plug_in = hour(Start),
           #Hour_plug_out = hour(End),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0))%>%
    select(Start,End,Charge.Duration,Energy,Day,Weekend) %>%
    filter(Charge.Duration > 0,
           Charge.Duration < 1440, 
           Energy < 100, 
           Energy > 0 ) %>%
    arrange(Start)
  
  return(boulder)
}

## Palo Alto

palo_alto_prep = function(df){
  Sys.setlocale("LC_ALL","English")
  df = df %>%
    select(Start.Date,End.Date,Park.Duration,Charge.Duration,Energy..kWh.,Ended.By,Latitude,Longitude,Postal.Code,Driver.Postal.Code) %>%
    rename('Energy'='Energy..kWh.') %>%
    mutate(Park.Duration = as.numeric(sub(",","",as.character(Park.Duration),fixed = T)),
           Charge.Duration = as.numeric(sub(",","",as.character(Charge.Duration),fixed=T)),
           Start = parse_date_time(Start.Date, '%d/%m/%Y %H:%M', tz='US/Pacific'),
           End = Start + minutes(round(Park.Duration)),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0)) %>%
    filter(Charge.Duration > 0,
           Charge.Duration < 1440, 
           Park.Duration > 0,
           Park.Duration < 1440,
           Energy < 100, 
           Energy > 0 ) %>%
    arrange(Start)
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
           filter(Park.Duration > 0,
                  Park.Duration < 1440, # Remove durations of more than 1 day; Indeed, there is one transaction that lasts for a total of 250 days (so about all data) => broken plug or signal
                  Energy < 100, # Remove Energy above 200 kWh; There is a transactions of 1430.17 kWh (the only one removed by this filtering after previous filters have been applied)
                  Energy > 0 # There are transactions with negative Energy (V2G?)
           ) %>%
           arrange(Start) # order by Start Column
  )
}

## Perth and Kinross

prep_perth = function(df){
  Sys.setlocale("LC_ALL","English")
  df = df %>%
    select(Start.Date,Start.Time,End.Date,End.Time, Total.kWh) %>%
    na.omit() %>%
    rename('Energy'='Total.kWh') %>%
    mutate(Start.Date = as.Date(Start.Date),
           End.Date = as.Date(End.Date),
           Start = parse_date_time(paste(Start.Date, Start.Time), '%Y-%m-%d %H:%M', tz='Europe/London'),
           End = parse_date_time(paste(End.Date, End.Time), '%Y-%m-%d %H:%M', tz='Europe/London'),
           Park.Duration = as.numeric(difftime(End,Start,'Europe/London',"mins")),
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0)
    )
  
  return(df %>% filter(Park.Duration > 0,
                       Park.Duration < 1440, 
                       Energy < 100, 
                       Energy > 0 ) %>% # Some negative values # There are transactions with negative Energy (V2G?)
           arrange(Start)) # order by Start Column 
}

## Paris

prep_paris = function(df){
  Sys.setlocale("LC_ALL","English")
  df = df %>%
    select(Date.de.dÃ.but, Date.de.fin, L.Ã.nergie..Wh.) %>%
    rename('Energy'="L.Ã.nergie..Wh.") %>%
    mutate(Start = parse_date_time(sub(' GMT+0200 (CEST)','',Date.de.dÃ.but,fixed=T), '%a %b %d %Y %H:%M:%S', tz='CET'),
           End = parse_date_time(sub(' GMT+0200 (CEST)','',Date.de.fin,fixed=T), '%a %b %d %Y %H:%M:%S', tz='CET'),
           Park.Duration = as.numeric(difftime(End,Start,'Europe/London',"mins")),
           Energy = Energy/1000,
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0)
    )
  
  return(df %>% filter(Park.Duration > 0,
                       Park.Duration < 1440, 
                       Energy < 100, 
                       Energy > 0 ) %>%
           arrange(Start)) # order by Start Column
}

## Domestics UK

prep_domestics = function(df){
  
  df = df %>%
    mutate(Start = parse_date_time(paste(StartDate, StartTime), '%Y-%m-%d %H:%M:%S', tz='Europe/London'),
           End = parse_date_time(paste(EndDate, EndTime), '%Y-%m-%d %H:%M:%S', tz='Europe/London'),
           PluginDuration = PluginDuration*60,
           Day = wday(Start,label=T,abbr=F),
           Weekend = ifelse(Day %in% c("Saturday","Sunday"),1,0)) %>%
    rename('Park.Duration'='PluginDuration') %>%
    select(Start,End,Energy,Park.Duration,Day,Weekend)
  
  
  return(df %>% filter(Park.Duration > 0,
                       Park.Duration < 1440, 
                       Energy < 100, 
                       Energy > 0 ) %>%
           arrange(Start))
}

##### 3. General Statistics #####

overall_stats = function(df,df_weekdays,df_weekends){
  
  print('Dates')
  print(paste('First Transaction:',min(date(df$Start))))
  print(paste('Last Transaction:',max(date(df$Start))))
  print('=====================')
  
  print('Total Transactions')
  print(paste('All:',nrow(df)))
  print(paste('Weekdays:',nrow(df_weekdays)))
  print(paste('Weekends:',nrow(df_weekends)))
  
  print('=====================')
  print('Average Transactions')
  print(paste('Weekdays:', round(nrow(df_weekdays)/length(unique(date(df_weekdays$Start))),2) ))
  print(paste('Weekends:', round(nrow(df_weekends)/length(unique(date(df_weekends$Start))),2) ))
  
  print('=====================')
  print('Park Duration')
  print(paste('Mean:', round(mean(df$Park.Duration)/60,2), 'hour(s)'))
  print(paste('Standard Deviation:', round(sd(df$Park.Duration)/60,2), 'hour(s)'))
  
  print('=====================')
  print('Charge Duration')
  print(paste('Mean:', round(mean(df$Charge.Duration)/60,2), 'hour(s)'))
  print(paste('Standard Deviation:', round(sd(df$Charge.Duration)/60,2), 'hour(s)'))
  
  print('=====================')
  print('Energy Demand')
  print(paste('Mean:', round(mean(df$Energy),2), 'kWh'))
  print(paste('Standard Deviation:', round(sd(df$Energy),2), 'kWh'))
  

}

##### 4. Graphs #####

all_histograms_Charge_Duration = function(data_list, cities_list, colScale){
  
  for (i in 1:length(data_list)){
    data_list[[i]] =  data_list[[i]] %>%
      select(Charge.Duration) %>%
      mutate(Charge.Duration = Charge.Duration/60) %>%
      mutate(city = cities_list[[i]])
  } 
  
  ggplot(bind_rows(data_list), aes(Charge.Duration, colour = city, fill= city)) + 
    geom_density(alpha = 0.2) +
    xlab('Charge Duration (hours)')  +
    theme_minimal() +
    theme(legend.title=element_text(size=30),
          legend.text=element_text(size=24),
          axis.title = element_text(size=24),
          axis.text = element_text(size=24)) +
    colScale
    
}

all_histograms_Park_Duration = function(data_list, cities_list, colScale){
  
  for (i in 1:length(data_list)){
    data_list[[i]] =  data_list[[i]] %>%
      select(Park.Duration) %>%
      mutate(Park.Duration = Park.Duration/60) %>%
      mutate(city = cities_list[[i]])
  } 
  
  ggplot(bind_rows(data_list), aes(Park.Duration, colour = city, fill= city)) + 
    geom_density(alpha = 0.2) +
    xlab('Park Duration (hours)') +
    theme_minimal() +
    theme(legend.title=element_text(size=30),
          legend.text=element_text(size=24),
          axis.title = element_text(size=24),
          axis.text = element_text(size=24)) +
    colScale
}

all_histograms_Energy = function(data_list, cities_list, colScale){
  
  for (i in 1:length(data_list)){
    data_list[[i]] =  data_list[[i]] %>%
      select(Energy) %>%
      mutate(city = cities_list[[i]])
  } 

  ggplot(bind_rows(data_list), aes(Energy, colour = city, fill= city)) + geom_density(alpha = 0.2) +
    xlab('Energy (kWh)') +
    theme_minimal() +
    theme(legend.title=element_text(size=30),
          legend.text=element_text(size=24),
          axis.title = element_text(size=24),
          axis.text = element_text(size=24)) +
    colScale
}

plug_profile = function(df,column='start'){
  
  if(column=='start'){
    df = df %>% 
      group_by(hour(Start)) %>%
      summarise(total_plug=n()) %>%
      mutate(avg_plug = total_plug/length(unique(date(df$End)))) %>%
      rename('Hour'='hour(Start)')
  }
  
  else if(column=='end'){
    df = df %>% 
      group_by(hour(End)) %>%
      summarise(total_plug=n()) %>%
      mutate(avg_plug = total_plug/length(unique(date(df$End)))) %>%
      rename('Hour'='hour(End)')
  }
  
  return(df)
}

all_plug_profile = function(data_list,cities_list,colScale,column='start'){
  
  for (i in 1:length(data_list)){
    data_list[[i]] = plug_profile(data_list[[i]],column=column) %>%
      mutate(city = cities_list[[i]],
             Hour = as.factor(Hour))
  }
  
  ggplot(bind_rows(data_list),aes(x=Hour, y=avg_plug, group =city, colour=city)) +
    geom_line(size=2) + 
    geom_point(size=3.5) +
    xlab('Hour') +
    labs(col="City") +
    ylab(paste('Average number of charging session ', column,'s', sep='')) +
    theme_minimal() +
    theme(legend.title=element_text(size=30),
          legend.text=element_text(size=24),
          axis.title = element_text(size=24),
          axis.text = element_text(size=24))+
    colScale
}

plug_trend = function(df){
  trend = df %>%
    group_by(date(Start)) %>%
    summarise(plug_in=n()) %>%
    rename('date'='date(Start)')
  
  ggplot(trend, aes(x=date,y=plug_in)) + 
    geom_point() + 
    geom_line(aes(y=gam(plug_in~s(seq(1,nrow(trend))),data=trend)$fitted.values), col='red', size = 2.5)   +
    ylab('Number of session starts') +
    xlab('Date') +
    theme_minimal() +
    theme(legend.title=element_text(size=30),
          legend.text=element_text(size=24),
          axis.title = element_text(size=24),
          axis.text = element_text(size=24))
}

##### Appendix: Fitting Statistical Distributions

distr = function(series){
  require(fitdistrplus)
  results = tibble(distrib = c("gamma",
                               "exp",
                               "lnorm",
                               "weibull",
                               "norm"),AIC = rep(0,5))
  for (i in 1:5){
    fit = fitdist(series, distr = results$distrib[i], method = "mle")
    results$AIC[i] = fit$aic
  }
  return(results %>% arrange(AIC))
}

##### Generic Histogram

histo = function(column){
  ggplot() +
    geom_histogram(aes(column, ..density..), colour = "black", fill = "white") +
    geom_density(aes(column)) +
    ylab("Density") +
    theme_minimal() +
    theme(legend.title=element_text(size=30),
          legend.text=element_text(size=24),
          axis.title = element_text(size=24),
          axis.text = element_text(size=24))
}

histo_beta = function(column){
  require(fitdistrplus)
  
  s_column = column/(max(column)+1)
  
  a = fitdist(s_column,
              "beta", 
              "mle")
  
  ax = seq(0,1,length=100)
  
  print(ggplot() +
    #geom_histogram(aes(s_column, ..density..), colour = "black", fill = "#F2D80C") +
    geom_density(aes(s_column), color ='#E69F00', fill= '#E69F00', alpha=0.5) +
    geom_line(aes(x=ax,y=dbeta(ax,a$estimate[[1]],a$estimate[[2]])), color ='#1B18E6') +
    ylab("Density") +
    theme_minimal() +
    theme(legend.title=element_text(size=30),
          legend.text=element_text(size=24),
          axis.title = element_text(size=24),
          axis.text = element_text(size=24)))
  
  print(a$aic)
  print(paste("Shape 1:", a$estimate[[1]]))
  print(paste("Shape 2:", a$estimate[[2]]))
}

histo_others = function(column){
  
  a = fitdist(column,
              "gamma", 
              "mle")
  b = fitdist(column,
              "exp", 
              "mle")
  c = fitdist(column,
              "lnorm", 
              "mle")
  d = fitdist(column,
              "weibull", 
              "mle")
  e = fitdist(column,
              "norm", 
              "mle")
  
  ax = seq(0,max(column),length=100)
  
  df = tibble(quantiles = ax,
              #series = column,
              gamma = dgamma(ax,shape=a$estimate[[1]],rate=a$estimate[[2]]),
              exp=dexp(ax,rate=b$estimate[[1]]),
              lnorm=dlnorm(ax,meanlog=c$estimate[[1]],sdlog=c$estimate[[2]]),
              norm=dnorm(ax,mean=e$estimate[[1]],sd=e$estimate[[2]]),
              weibull=dweibull(ax,d$estimate[[1]],d$estimate[[2]]))
  

  print(ggplot(df) +
          
          geom_density(data=tibble(column=column),aes(x=column), colour ='#E69F00', fill= '#E69F00', alpha=0.5) +
          
          geom_line(aes(x=ax,y=gamma, colour="Gamma")) +
          
          geom_line(aes(x=ax,y=exp,colour="Exp")) +
          
          geom_line(aes(x=ax,y=lnorm,colour="Lnorm")) +
          
          geom_line(aes(x=ax,y=weibull,colour="Weibull")) +
          
          geom_line(aes(x=ax,y=norm,colour="Norm")) +
          
          scale_color_manual(values= c("darkgreen","darkred","steelblue","violet","darkgrey")) +
          
          ylab("Density") +
          theme_minimal() +
          theme(legend.title=element_text(size=30),
                legend.text=element_text(size=24),
                axis.title = element_text(size=24),
                axis.text = element_text(size=24)))
  
  criteria = tibble(gamma = a$aic,
         exp=b$aic,
         lnorm=c$aic,
         norm=e$aic,
         weibull=d$aic)
  
  return(criteria[which.min(criteria)])
   
}


histo_all = function(column){
  
  column = column/(max(column)+1)
  
  a = fitdist(column,
              "gamma", 
              "mle")
  b = fitdist(column,
              "exp", 
              "mle")
  c = fitdist(column,
              "lnorm", 
              "mle")
  d = fitdist(column,
              "weibull", 
              "mle")
  e = fitdist(column,
              "norm", 
              "mle")
  f=fitdist(column,
            "beta", 
            "mle")
  
  ax = seq(0,max(column),length=100)
  
  df = tibble(quantiles = ax,
              #series = column,
              gamma = dgamma(ax,shape=a$estimate[[1]],rate=a$estimate[[2]]),
              exp=dexp(ax,rate=b$estimate[[1]]),
              lnorm=dlnorm(ax,meanlog=c$estimate[[1]],sdlog=c$estimate[[2]]),
              norm=dnorm(ax,mean=e$estimate[[1]],sd=e$estimate[[2]]),
              weibull=dweibull(ax,d$estimate[[1]],d$estimate[[2]]),
              beta=dbeta(ax,a$estimate[[1]],a$estimate[[2]]))
  
  
  print(ggplot(df) +
          
          geom_density(data=tibble(column=column),aes(x=column), colour ='#E69F00', fill= '#E69F00', alpha=0.5) +
          
          geom_line(aes(x=ax,y=gamma, colour="Gamma"), size=2) +
          
          geom_line(aes(x=ax,y=exp,colour="Exp"), size=2) +
          
          geom_line(aes(x=ax,y=lnorm,colour="Lnorm"), size=2) +
          
          geom_line(aes(x=ax,y=weibull,colour="Weibull"), size=2) +
          
          geom_line(aes(x=ax,y=norm,colour="Norm"), size=2) +
          
          geom_line(aes(x=ax,y=beta,colour="Beta"), size=2) +
          
          scale_color_manual(values= c("darkgreen","darkred","steelblue","violet","darkgrey","darkorange")) +
          
          ylab("Density") +
          theme_minimal() +
          theme(legend.title=element_text(size=30),
                legend.text=element_text(size=24),
                axis.title = element_text(size=24),
                axis.text = element_text(size=24)))
  
  criteria = tibble(gamma = a$aic,
                    exp=b$aic,
                    lnorm=c$aic,
                    norm=e$aic,
                    weibull=d$aic,
                    beta=f$aic)
  
  return(criteria[which.min(criteria)])
}

histo_all_2 = function(column, flexmixmodel, flexmixmodel2, filename){
  
  #column = column/(max(column)+1)
  
  a = fitdist(column,
              "gamma", 
              "mle")
  b = fitdist(column,
              "exp", 
              "mle")
  c = fitdist(column,
              "lnorm", 
              "mle")
  d = fitdist(column,
              "weibull", 
              "mle")
  e = fitdist(column,
              "norm", 
              "mle")
  f=fitdist(column,
            "beta", 
            "mle")
  
  
  ax = seq(0,max(column),length=100)
  
  df = tibble(quantiles = ax,
              #series = column,
              gamma = dgamma(ax,shape=a$estimate[[1]],rate=a$estimate[[2]]),
              exp=dexp(ax,rate=b$estimate[[1]]),
              lnorm=dlnorm(ax,meanlog=c$estimate[[1]],sdlog=c$estimate[[2]]),
              norm=dnorm(ax,mean=e$estimate[[1]],sd=e$estimate[[2]]),
              weibull=dweibull(ax,d$estimate[[1]],d$estimate[[2]]),
              beta=dbeta(ax,a$estimate[[1]],a$estimate[[2]]),
              mixture = simu_univ(flexmixmodel,ax),
              mixturegamma = simu_gamma(flexmixmodel2,ax))
  
  # 1. Open jpeg file
  jpeg(paste("Plots/",filename,".jpg",sep=""), width = 800, height = 600)
  
  print(ggplot(df) +
          
          geom_density(data=tibble(column=column),aes(x=column), colour ='#E69F00', fill= '#E69F00', alpha=0.5) +
          
          geom_line(aes(x=ax,y=gamma, colour="Gamma"), size=2) +
          
          geom_line(aes(x=ax,y=exp,colour="Exp"), size=2) +
          
          geom_line(aes(x=ax,y=lnorm,colour="Lnorm"), size=2) +
          
          geom_line(aes(x=ax,y=weibull,colour="Weibull"), size=2) +
          
          geom_line(aes(x=ax,y=norm,colour="Norm"), size=2) +
          
          geom_line(aes(x=ax,y=beta,colour="Beta"), size=2) +
          
          geom_line(aes(x=ax,y=mixture,colour=paste("Gaussian Mixture k =", ncol(parameters(flexmixmodel)))), size=2) +
          
          geom_line(aes(x=ax,y=mixturegamma,colour=paste("Gamma Mixture k =", ncol(parameters(flexmixmodel2)))), size=2) +
          
          scale_color_manual(name = "Distribution",values= c("darkgreen","darkred","steelblue","violet","darkgrey","darkorange","black","pink")) +
          
          ylab("Density") +
          
          theme_minimal() +
          theme(legend.title=element_text(size=30),
                legend.text=element_text(size=24),
                axis.title = element_text(size=24),
                axis.text = element_text(size=24)))
  
  # 3. Close the file
  dev.off()
  
  column_gaussian = paste("gaussianmixture k =",ncol(parameters(flexmixmodel)))
  column_gamma = paste("gammamixture k =",ncol(parameters(flexmixmodel2)))
  
  criteria = tibble(gamma = a$aic,
                    exp=b$aic,
                    lnorm=c$aic,
                    norm=e$aic,
                    weibull=d$aic,
                    beta=f$aic,
                    gaussianmixture=AIC(flexmixmodel),
                    gammamixture=AIC(flexmixmodel2))
  
  colnames(criteria)[7:8]=c(column_gaussian,column_gamma)
  
  print(paste("Best AIC:",colnames(criteria)[which.min(criteria)]))
  return(criteria-min(criteria))
}

simu_univ = function(flexmixmodel,quantiles){
  
  params = parameters(flexmixmodel)
  probs = prior(flexmixmodel)
  
  dprob=0
  for (i in 1:ncol(params)){
    dprob = dprob + probs[i]*dnorm(quantiles,params[1,i],params[2,i])
  }
  
  
  #dprob = probs[1]*dnorm(quantiles,params[1,1],params[2,1]) + probs[2]*dnorm(quantiles,params[1,2],params[2,2])
  
  return(dprob)
}

simu_gamma = function(flexmixmodel,quantiles){
  
  params = parameters(flexmixmodel)
  probs = prior(flexmixmodel)
  
  dprob=0
  for (i in 1:ncol(params)){
    dprob = dprob + probs[i]*dgamma(x=quantiles,shape = params[2,i],rate = params[2,i]*params[1,i])
  }
  #dprob = probs[1]*dgamma(x=quantiles,shape = params[2,1],rate = params[2,1]*params[1,1]) + probs[2]*dgamma(x=quantiles,shape = params[2,2], rate = params[2,2]*params[1,2])
  
  return(dprob)
}
