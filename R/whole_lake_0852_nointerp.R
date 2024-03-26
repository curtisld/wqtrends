#
library(signal)
library(readxl)
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)
library(akima)

whole_lake_0852_nointerp <- function(data=data,parm='Temperature',lake='Washington',station='0852',yrstart=1993,yrend=2018,opath){
  
  if(lake=='Washington'){
      
      # volume weighting fractions for each meter depth for deepest lake station    
      vol_frac <- read_excel('//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/!CSOWQA/Lake Washington/Data/water/R/Copy of LW-depth-volume2.xls','vol_fract')  
      
      data <- data %>% dplyr::filter(ParmDisplayName == parm)
      
      data <- data %>% dplyr::filter(Locator == station)
      data <- data %>% dplyr::filter(!is.na(Value))
      
      data$Year <- year(data$CollectDate)
      data$Month <- month(data$CollectDate)
      data <- data %>% dplyr::filter(Year>=yrstart & Year<=yrend)
      
      df_filter <- data %>% group_by(Year,Month) %>% summarize(Value = mean(Value,na.rm=TRUE)) %>% select(Year,Month)
      
      df <- data %>% group_by(CollectDate,Depth) %>% summarize(Value = mean(Value))
      df <- ungroup(df)
      
      # remove days with only one surface sample
      rmv <- df %>% group_by(CollectDate) %>% 
        summarize(nums = length(Value))
      
      rmv <- subset(rmv,rmv$nums >1)
      df <- subset(df,df$CollectDate %in% rmv$CollectDate)
      
      
      interp.dates <- unique(df$CollectDate)
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
          # base::print(paste(inp$CollectDate))
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
      
      
      ###########################################################3
      # ready data for rLakeAnalyzer
      # Tii2 <- data.frame("Date"=Tii$x,"Depth" = rep(xi,length(unique(Tii$x))),"Value"=Tii$z)
      ##CLD Tii2 <- data.frame("Date"=Tii$x,"Value"=Tii$z)
      Tii2 <- t.profile %>% select(-Date_dec) %>% spread(key=Depth,value=Value)
      
      ### a diversion to calculate isopleths...or at least output average year to LakeAnalyzer file
      ##CLD Tii3 <- calc_iso_lines(Tii2,lake,Depth,parm,station,opath,yrstart,yrend)
      
      ### back to work....actually now to create whole lake time-volume-weighted annual average values
      tmp <- as.matrix(Tii2[,2:57])
      #tmp2 <- apply(tmp,1,rev) 
      #tmp2 <- apply(tmp2,2,rev) 
      tmp2 <- t(tmp)
      
      dim(tmp2)
      
      vol_fct <- as.matrix(vol_frac$vol_fct)
      vol_fct <- vol_fct[1:56]
      
      ts <- tmp2*vol_fct
      
      ts2 <- colSums(ts)
      plot(Ti,ts2,type="l")
      
      res <- data.frame(time=Ti,Value=ts2)
      res$Year <- year(res$time)
      res$Month <- month(res$time)
      res$doy <- as.numeric(strftime(res$time, format = "%j"))
      res$Locator <- '0852'
      res$Parameter <- parm
      
      ###################################################
      # now write daily interpolated profile data to LakeAnalyzer-friendly file
      ###################################################
      
      # Tii2 are very close to Washington.wtr input to rLakeAnalyzer
      names(Tii2)[1] <- "dateTime"
      names(Tii2)[2:ncol(Tii2)] <- paste("wtr", 0:(ncol(Tii2)-2), sep="_")
      # more date time fun (not)....3420 = 1979-05-14 but....
      Tii2$dateTime <- as.Date(Tii2$dateTime,origin="1970-01-01")
      
      # Lake Washington temperature profile data only good beginning in 1993
      # Tii2 <- subset(Tii2,Tii2$dateTime>="1993-01-01")
      
  
      Tii2$dateTime <- paste0(as.character(Tii2$dateTime)," 00:00")
      
      # write.table(Tii2, "Washington.wtr", sep="\t",quote = FALSE, row.names = FALSE)
      # write.table(Tii2, "Sammamish.wtr", sep="\t",quote = FALSE, row.names = FALSE)
      ##CLD
      write_LA_file(Tii2,Tii2,lake,station,parm,opath,yrstart,yrend)

      ##CLD res <- merge(res,df_filter,by=c("Year","Month"))
      return(res)
  }
  
}