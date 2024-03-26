#
require(signal)
require(tidyverse)
require(lubridate)

# surface_lake <- function(data=data,parm='Temperature',lake='Washington',station='0804',yrstart=1993,yrend=2018,depth=10){
surface_lake_nointerp <- function(data=data,parm='Temperature',lake='Washington',station='0804',yrstart=1993,yrend=2018,depthlim=2.0){
  
  ##############################################################
  ### interpolate average of values from surface to specified depth
  ##############################################################

  data <- data %>% filter(ParmDisplayName == parm)
  data <- data %>% filter(Locator == station)
  data <- data %>% filter(!is.na(Value))
  data$Year <- year(data$CollectDate)
  data$Month <- month(data$CollectDate)
  data <- data %>% filter(Year>=yrstart & Year<=yrend)
  data <- data %>% filter(Depth <= depthlim)

  df_filter <- data %>% group_by(Year,Month) %>% summarize(Value = mean(Value,na.rm=TRUE)) %>% select(Year,Month)
  
  df <- data %>% group_by(CollectDate) %>% summarize(Value = mean(Value,na.rm=TRUE))
  df <- ungroup(df)

  # interp.dates <- unique(df$CollectDate)
  # 
  # #interp.dates <- unique(data_df$DateTime_day)
  # Ti <- seq(as.Date(interp.dates[1]), as.Date(interp.dates[length(interp.dates)]), by="days")
  # Tii <- with(df,(interp1(as.numeric(CollectDate),Value,as.numeric(Ti))))

  plot(df$CollectDate,df$Value,type="p")
  # lines(Ti,Tii,type="l")
  title(main=station)

  ###########################################################
   # res <- data.frame("time"=Ti,"Value"=Tii)
   # res<-NULL
   # res$Year <- df$CollectDate
   # res$Month <- df$CollectDate
   # res$doy <- as.numeric(strftime(df$CollectDate, format = "%j"))
   # res$Locator <- station
   # res$Parameter <- parm
   # res$layer <- 'surface'
   # res$Value <- df$Value
   
   df <- df %>% mutate(time = CollectDate, Year = year(CollectDate), Month = month(CollectDate), 
                       doy = as.numeric(strftime(df$CollectDate, format = "%j")),
                  Locator = station, Parameter = parm, layer = 'surface') %>% select(-CollectDate)

      # res <- merge(res,df_filter,by=c("Year","Month"))   
      # return(res)
     return(df)
  
}


