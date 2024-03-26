#
library(signal)
library(readxl)
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)
library(akima)

epi_hyp_lake_wash_nointerp <- function(data=data,parm='Temperature',lake='Washington',station='0852',yrstart=1993,yrend=2018){
  
  # volume weighting fractions for each meter depth for deepest lake station    
  vol_frac <- read_excel('//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/!CSOWQA/Lake Washington/Data/water/R/Copy of LW-depth-volume2.xls','vol_fract')  
  
  data <- data %>% filter(ParmDisplayName == parm)
  data <- data %>% filter(Locator == station)
  data <- data %>% filter(!is.na(Value))
  data$Year <- year(data$CollectDate)
  data$Month <- month(data$CollectDate)
  data <- data %>% filter(Year>=yrstart & Year<=yrend)
  
  df_filter <- data %>% group_by(Year,Month) %>% summarize(Value = mean(Value,na.rm=TRUE)) %>% select(Year,Month)
  
  df <- data %>% group_by(CollectDate,Depth) %>% summarize(Value = mean(Value))
  df <- ungroup(df)
  
  # remove days with only one surface sample
  rmv <- df %>% group_by(CollectDate) %>% 
    summarize(nums = length(Value))
  
  rmv <- subset(rmv,rmv$nums >1)
  df <- subset(df,df$CollectDate %in% rmv$CollectDate)
  
  interp.dates <- unique(df$CollectDate)
  
  ################################################
  #
  ################################################
  # Depth <- seq(0,30)
  Depth <- seq(0,55)
  xi <- seq(0,55)
  # xi <- seq(0,30)
  t.profile <- data.frame(Date = as.Date(character()),Date_dec = double(),Depth = double(),Value = double())
  ti <- NULL
  
  for(i in 1:length(interp.dates)){
    inp <- subset(df,df$CollectDate==interp.dates[i])
    inp$Depth[1]<- 0
    if(nrow(inp)>1){
      Value = interp1(inp$Depth,inp$Value,xi,method='linear',inp$Value[nrow(inp)])
      t.dummy <- cbind(Date = rep(inp$CollectDate[1],length(xi)),Date_dec=rep(interp.dates[i],length(xi)),Depth,Value)
      t.profile <- rbind(t.profile,t.dummy)
    }
  }
  # still need to figure out how to keep Date as.Date
  t.profile$Date <- as.Date(t.profile$Date,origin="1970-01-01")
  
  #interp.dates <- unique(data_df$DateTime_day)
  ##CLD Ti <- seq(as.Date(interp.dates[1]), as.Date(interp.dates[length(interp.dates)]), by="days")
  Ti <- unique(df$CollectDate)
  ##CLD Tii <- with(t.profile,(akima::interp(Date,Depth,Value,as.numeric(Ti),xi)))
  
  
  ###########################################################
  # ready data for rLakeAnalyzer
  # Tii2 <- data.frame("Date"=Tii$x,"Depth" = rep(xi,length(unique(Tii$x))),"Value"=Tii$z)
  ##CLD Tii2 <- data.frame("Date"=Tii$x,"Value"=Tii$z)
  Tii2 <- t.profile %>% select(-Date_dec) %>% spread(key=Depth,value=Value)
  
  ### back to work....actually now to create whole lake time-volume-weighted annual average values
  ### this one for surface 0-10 m
  tmp <- as.matrix(Tii2[,2:12])
  #tmp2 <- apply(tmp,1,rev) 
  #tmp2 <- apply(tmp2,2,rev) 
  tmp2 <- t(tmp)
  
  dim(tmp2)
  
  vol_fct <- as.matrix(vol_frac$epi_fct)
  vol_fct <- vol_fct[1:11]
  
  ts <- tmp2*vol_fct
  
  ts2 <- colSums(ts)
  plot(Ti,ts2,type="l")
  
  ###########################################################
  ###########################################################
  ###########################################################
  
  if(station=='0852'){
  
      ### this one for bottom 15 - 55 m
      tmp <- as.matrix(Tii2[,18:57])
      #tmp2 <- apply(tmp,1,rev) 
      #tmp2 <- apply(tmp2,2,rev) 
      tmp2 <- t(tmp)
      
      dim(tmp2)
      
      vol_fct <- as.matrix(vol_frac$hyp_fct)
      vol_fct <- vol_fct[17:56]
      
      ts <- tmp2*vol_fct
      
      ts3 <- colSums(ts)
      lines(Ti,ts3,type="l",col='red')
      
      res <- data.frame(time=Ti,epi=ts2,hyp=ts3)
      res$Year <- year(res$time)
      res$Month <- month(res$time)
      res$doy <- as.numeric(strftime(res$time, format = "%j"))
      res$Locator <- '0852'
      res$Parameter <- parm
      
  } else if(station == '0826') {
    
    ### this one for bottom 15 - ? m
    tmp <- as.matrix(Tii2[,18:49])
    #tmp2 <- apply(tmp,1,rev) 
    #tmp2 <- apply(tmp2,2,rev) 
    tmp2 <- t(tmp)
    
    dim(tmp2)
    
    vol_fct <- as.matrix(vol_frac$hyp_0826_fct)
    vol_fct <- vol_fct[17:48]
    
    ts <- tmp2*vol_fct
    
    ts3 <- colSums(ts)
    lines(Ti,ts3,type="l",col='red')
    
    res <- data.frame(time=Ti,epi=ts2,hyp=ts3)
    res$Year <- year(res$time)
    res$Month <- month(res$time)
    res$doy <- as.numeric(strftime(res$time, format = "%j"))
    res$Locator <- '0826'
    res$Parameter <- parm
    
  } else if(station =='0890') {
    
    ### this one for bottom 15 - ? m
    tmp <- as.matrix(Tii2[,18:49])
    #tmp2 <- apply(tmp,1,rev) 
    #tmp2 <- apply(tmp2,2,rev) 
    tmp2 <- t(tmp)
    
    dim(tmp2)
    
    vol_fct <- as.matrix(vol_frac$hyp_0826_fct)
    vol_fct <- vol_fct[17:48]
    
    ts <- tmp2*vol_fct
    
    ts3 <- colSums(ts)
    lines(Ti,ts3,type="l",col='red')
    
    res <- data.frame(time=Ti,epi=ts2,hyp=ts3)
    res$Year <- year(res$time)
    res$Month <- month(res$time)
    res$doy <- as.numeric(strftime(res$time, format = "%j"))
    res$Locator <- '0890'
    res$Parameter <- parm
    
  } else if(station == '0831') {
    
    ### this one for bottom 15 - ? m
    tmp <- as.matrix(Tii2[,18:27])
    #tmp2 <- apply(tmp,1,rev) 
    #tmp2 <- apply(tmp2,2,rev) 
    tmp2 <- t(tmp)
    
    dim(tmp2)
    
    vol_fct <- as.matrix(vol_frac$hyp_0831_fct)
    vol_fct <- vol_fct[17:26]
    
    ts <- tmp2*vol_fct
    
    ts3 <- colSums(ts)
    lines(Ti,ts3,type="l",col='red')
    
    res <- data.frame(time=Ti,epi=ts2,hyp=ts3)
    res$Year <- year(res$time)
    res$Month <- month(res$time)
    res$doy <- as.numeric(strftime(res$time, format = "%j"))
    res$Locator <- '0831'
    res$Parameter <- parm
    
  }
      
  else if(station == '0840') {
    
    ### this one for bottom 15 - ? m
    tmp <- as.matrix(Tii2[,18:27])
    #tmp2 <- apply(tmp,1,rev) 
    #tmp2 <- apply(tmp2,2,rev) 
    tmp2 <- t(tmp)
    
    dim(tmp2)
    
    vol_fct <- as.matrix(vol_frac$hyp_0840_fct)
    vol_fct <- vol_fct[17:26]
    
    ts <- tmp2*vol_fct
    
    ts3 <- colSums(ts)
    lines(Ti,ts3,type="l",col='red')
    
    res <- data.frame(time=Ti,epi=ts2,hyp=ts3)
    res$Year <- year(res$time)
    res$Month <- month(res$time)
    res$doy <- as.numeric(strftime(res$time, format = "%j"))
    res$Locator <- '0840'
    res$Parameter <- parm
    
  }      
  
      ##CLD res <- merge(res,df_filter,by=c("Year","Month"))
      return(res)
 
  
}


