library(ggmap)
library(tidyverse)
library(RSocrata)
library(lubridate)

library(wqtrends)

library(rLakeAnalyzer)

source('R/get_socrata_sites_func.R')
source('R/get_socrata_data_func.R')
source('R/ts.schmidt.stability.kc.R')

# lake interpolation schemes
source('R/whole_lake_0612_nointerp.R')
source('R/whole_lake_0852_nointerp.R')
source('R/whole_lake_A522_nointerp.R')
source('R/epi_hyp_lake_wash_nointerp.R')
source('R/epi_hyp_lake_union_nointerp.R')
source('R/epi_hyp_lake_samm_nointerp.R')
source('R/surface_lake_nointerp.R')
source('R/write_LA_file.R')

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")

options(scipen=8,digits=7)

# set most recent complete year of data for processing
yrendn = 2023

##########################
### get stations/locations
locns <- c('0611','0612','0804','0826','0852','0831','0540','A522','0512')
##########################
stas <- get_socrata_sites_func(locns = locns)
locs <- stas$Locator

######################################################################################################
### get_socrata_data_func includes method adjustments and conversion of nutrient data from mg/L to ug/L
######################################################################################################
data <- get_socrata_data_func(locns = locs,parms = c("Dissolved Oxygen", "Dissolved Oxygen, Field", "pH", "pH, Field", "Secchi Transparency", 
                                                     "Ammonia Nitrogen", "Orthophosphate Phosphorus", "Nitrite + Nitrate Nitrogen", "Silica", "Total Nitrogen",
                                                     "Total Phosphorus", "Chlorophyll a", "Chlorophyll, Field", "Temperature", "Total Suspended Solids", 
                                                     "Total Alkalinity", "Conductivity", "Conductivity, Field", "Fecal Coliform", "E. coli"))
######################################################################################################

######################################################################################################
### Deal with non-Project data?
######################################################################################################
prjnum <- read_excel('R/Sample_Curtis_Query_May9_930AM_ProjectID_421235.xlsx') %>% 
  transmute(CollectDate = as.Date(CollectDate), LabSampleNum = LabSampleNum, ProjectNum = ProfileId)
data <- left_join(data,prjnum, by = c("CollectDate","LabSampleNum"))
data <- filter(data,ProjectNum == '421235')

#########################################################################################################
### create interactive map
#########################################################################################################
# You will need your own Google Key stored in your .REnviron file Sys.setenv("GOOGLE_KEY" = "your key here")
register_google(key = Sys.getenv("GOOGLE_KEY"))

locs<-stas %>%
  transmute(SiteName=SiteName,
            Locator=Locator,
            Station = Locator,
            lon=Longitude,
            lat=Latitude,
            Area=Area) 
  save(locs, file = "data/locs.RData")
#######################################################

ps <- c(lon = -122.2, lat = 47.63)
map <- get_map(location = ps, zoom = 11, scale = 2, color = "bw")
testmap <- ggmap(map) + geom_point(data=locs,aes(lon,lat),color="darkgrey",size=4,alpha=0.8) +
  geom_text(data=locs,aes(label=Locator,vjust=-1,hjust=0))

save(map, file = "data/map.RData")

ggplot(data %>% filter(ParmDisplayName=="Dissolved Oxygen, Field"),aes(CollectDate,Value)) + geom_line() +
  facet_grid(Locator~ParmDisplayName)

ggplot(data %>% filter(ParmDisplayName=="Chlorophyll a"),aes(CollectDate,Value)) + geom_line() +
  facet_wrap(~Locator)

########## sort out missing depths and chlorophyll composite vs discrete
tmpx <- filter(data,ParmDisplayName=="Secchi Transparency")

########## sort out odd A522 surface DO values
tmpx <- filter(data,round(Depth,0)==1.0&ParmDisplayName=="Dissolved Oxygen, Field"&Locator=="A522")
tmpx <- filter(data,ParmDisplayName=="Dissolved Oxygen, Field"&Locator=="A522"&
                 CollectDate==as.Date("2007-09-10"))
tmpx <- filter(data,ParmDisplayName=="Dissolved Oxygen, Field"&Locator=="A522"&
                 CollectDate==as.Date("2000-10-03"))
# remove odd A522 DO value with no depth ...probably a bottom sample. The other values are all the same
data <- filter(data,!(ParmDisplayName=="Dissolved Oxygen, Field"&Locator=="A522"&
                 CollectDate==as.Date("2000-10-03")&Value==3.0))
#####################################################################
data$Depth <- with(data,ifelse(ParmDisplayName=="Secchi Transparency",1.0,Depth))

chla <- filter(data,ParmDisplayName=="Chlorophyll a")

data <- filter(data, !ParmDisplayName=="Chlorophyll a")
data$Depth <- with(data,ifelse(is.na(Depth),1.0,Depth))

chla$type <- with(chla,ifelse(is.na(Depth),'Composite',"Discrete"))
ggplot(chla,aes(CollectDate,Value)) + geom_line() +
  facet_grid(Locator~type)

chla_c <- filter(chla,type=="Composite"&!Locator %in% c('0540','0512','0804')) %>% mutate(Depth = 1.0)
chla_d <- filter(chla,between(CollectDate,as.Date("1993-01-01"),as.Date("1994-03-31"))&(round(Depth,0)==1|round(Depth,0)==0)&
                   type=='Discrete'&!Locator %in% c('0540','0512','0804')) %>%
# chla_d <- filter(chla,between(CollectDate,as.Date("1993-01-01"),as.Date("1994-03-31"))&round(Depth,0)==1) %>% 
  mutate(Value = ifelse(Value<0.012,0.25,Value))

chla_c0804 <- filter(chla,type=="Composite"&Locator %in% c('0804')) %>% mutate(Depth = 1.0)
chla_d0804 <- filter(chla,(round(Depth,0)==1|round(Depth,0)==0)&
                   type=='Discrete'&Locator %in% c('0804')) %>%
  # chla_d <- filter(chla,between(CollectDate,as.Date("1993-01-01"),as.Date("1994-03-31"))&round(Depth,0)==1) %>% 
  mutate(Value = ifelse(Value<0.012,0.25,Value))

# issue with initial chla values..<1

ggplot(chla_d0804,aes(CollectDate,Value)) + geom_line() +
  facet_wrap(~Locator,ncol=1) +
  scale_y_log10()
ggplot(chla_c0804,aes(CollectDate,Value)) + geom_line() +
  facet_wrap(~Locator,ncol=1) +
  scale_y_log10()

data <- bind_rows(data,chla_c,chla_d)
ggplot(data %>% filter(ParmDisplayName=="Chlorophyll a"),aes(CollectDate,Value)) + geom_line() +
  facet_wrap(~Locator,ncol=1) +
  scale_y_log10()
data <- bind_rows(data,chla_c0804,chla_d0804)
ggplot(data %>% filter(ParmDisplayName=="Chlorophyll a"),aes(CollectDate,Value)) + geom_line() +
  facet_wrap(~Locator,ncol=1) +
  scale_y_log10()

chla_c0512 <- filter(chla,type=="Composite"&Locator %in% c('0512')) %>% mutate(Depth = 1.0)
chla_d0512 <- filter(chla,(round(Depth,0)==1|round(Depth,0)==0)&
                       type=='Discrete'&Locator %in% c('0512')) %>%
  mutate(Value = ifelse(Value<0.012,0.25,Value))
chla_c0540 <- filter(chla,type=="Composite"&Locator %in% c('0540')) %>% mutate(Depth = 1.0)
chla_d0540 <- filter(chla,(round(Depth,0)==1|round(Depth,0)==0)&
                       type=='Discrete'&Locator %in% c('0540')) %>%
  mutate(Value = ifelse(Value<0.012,0.25,Value))
data <- bind_rows(data,chla_c0512,chla_d0512)
ggplot(data %>% filter(ParmDisplayName=="Chlorophyll a"),aes(CollectDate,Value)) + geom_line() +
  facet_wrap(~Locator,ncol=1) +
  scale_y_log10()
data <- bind_rows(data,chla_c0540,chla_d0540)
ggplot(data %>% filter(ParmDisplayName=="Chlorophyll a"),aes(CollectDate,Value)) + geom_line() +
  facet_wrap(~Locator,ncol=1) +
  scale_y_log10()

#############################################################################
#########################
### check for zero depths
#########################
ggplot(data,aes(CollectDate,round(Depth,0))) + geom_point() + scale_y_reverse()
#################################################################################
# yes, but...??
# data_bak <- data
# data <- data_bak

parms <- c("Total Phosphorus","Temperature","Total Nitrogen","Nitrite + Nitrate Nitrogen","Orthophosphate Phosphorus","Dissolved Oxygen, Field",
           "Total Alkalinity","Conductivity, Field","pH, Field")

# create epi_hyp and whole lake output for 0611, 0612, 0826, 0852, 0831, and A522
# yrendn = 2022
res_f <- NULL
for (parm_in in parms){
  res <- epi_hyp_lake_samm_nointerp(data=data,parm=parm_in,lake='Sammamish',station='0612',yrstart=1993,yrend=yrendn)
  res_f <- bind_rows(res_f,res)
  res <- epi_hyp_lake_samm_nointerp(data=data,parm=parm_in,lake='Sammamish',station='0611',yrstart=1993,yrend=yrendn)
  res_f <- bind_rows(res_f,res)
  res <- epi_hyp_lake_wash_nointerp(data=data,parm=parm_in,lake='Washington',station='0826',yrstart=1993,yrend=yrendn)
  res_f <- bind_rows(res_f,res)
  res <- epi_hyp_lake_wash_nointerp(data=data,parm=parm_in,lake='Washington',station='0852',yrstart=1993,yrend=yrendn)
  res_f <- bind_rows(res_f,res)
  res <- epi_hyp_lake_wash_nointerp(data=data,parm=parm_in,lake='Washington',station='0831',yrstart=1993,yrend=yrendn)
  res_f <- bind_rows(res_f,res)
  res <- epi_hyp_lake_union_nointerp(data=data,parm=parm_in,lake='Union',station='A522',yrstart=1993,yrend=yrendn)
  res_f <- bind_rows(res_f,res)

}

res_f <- gather(res_f,key=layer,value=Value,epi,hyp,)
res_f <- res_f %>% group_by(time,doy,Year,Month,Locator,Parameter,layer) %>% summarize(Value=mean(Value,na.rm = TRUE)) %>% ungroup() 

ggplot(res_f %>% filter(Parameter=="Total Phosphorus"),aes(time,Value)) + geom_point() +
  facet_grid(layer~Locator,scales='free_y')

opath <- 'data/LakeAnalyzer'
res_w <- NULL
for (parm_in in parms){
  res <- whole_lake_0852_nointerp(data=data,parm=parm_in,lake='Washington',station='0852',yrstart=1993,yrend=yrendn,opath)
  res_w <- bind_rows(res_w,res)
  res <- whole_lake_0612_nointerp(data=data,parm=parm_in,lake='Sammamish',station='0612',yrstart=1993,yrend=yrendn,opath)
  res_w <- bind_rows(res_w,res)
  res <- whole_lake_A522_nointerp(data=data,parm=parm_in,lake='Union',station='A522',yrstart=1993,yrend=yrendn,opath)
  res_w <- bind_rows(res_w,res)
  
}

res_w$layer <- 'whole'

res_w <- res_w %>% group_by(time,doy,Year,Month,Locator,Parameter,layer) %>% summarize(Value=mean(Value,na.rm = TRUE)) %>% ungroup() 

ggplot(res_w %>% filter(Parameter=="Total Phosphorus"),aes(time,Value)) + geom_point() +
  facet_grid(layer~Locator,scales='free_y')

##################################################################
res_s <- NULL
for (parm_in in parms){
  res <- surface_lake_nointerp(data=data,parm=parm_in,lake='Union',station='0512',yrstart=1993,yrend=yrendn,depthlim=2)
  res_s <- bind_rows(res_s,res)
  res <- surface_lake_nointerp(data=data,parm=parm_in,lake='Union',station='0540',yrstart=1993,yrend=yrendn,depthlim=2)
  res_s <- bind_rows(res_s,res)
  res <- surface_lake_nointerp(data=data,parm=parm_in,lake='Union',station='0804',yrstart=1993,yrend=yrendn,depthlim=2)
  res_s <- bind_rows(res_s,res)
  
}

res_t <- bind_rows(res_f,res_w,res_s)

res_t$Station <- with(res_t,paste(Locator,layer,sep="_"))

### total alkalinity only for 0612, 0852, and A522
res_t <- filter(res_t,!(Locator %in% c('0804','0826','0831','0512','0540','0611')&Parameter=="Total Alkalinity"))

####
# add back Secchi, Chl a, and fecal coliform 1993-2021
tmpcs <- filter(data,ParmDisplayName %in% c("Chlorophyll a","Secchi Transparency"))
ggplot(tmpcs,aes(CollectDate,Value)) + geom_point() +
  facet_grid(ParmDisplayName~Locator,scales='free_y')

tmpcs <- transmute(tmpcs,time = CollectDate,Year = year(time), Month = month(time), doy = yday(time), 
                   Locator = Locator, Parameter = ParmDisplayName, Value = Value, layer = NA, Station = Locator)

tmpfc <- filter(data,ParmDisplayName %in% c("Fecal Coliform")&Locator %in% c("0540","A522","0512"))
ggplot(tmpfc %>% filter(ParmDisplayName=="Fecal Coliform"),aes(CollectDate,Value)) + geom_point() +
  facet_grid(ParmDisplayName~Locator,scales='free_y')

tmpfc <- transmute(tmpfc,time = CollectDate,Year = year(time), Month = month(time), doy = yday(time), 
                   Locator = Locator, Parameter = ParmDisplayName, Value = Value, layer = NA, Station = Locator)
  
#############################################################################
# zero fecal coliform? and no fix yet for TnC
tmpfc$Value <- with(tmpfc,ifelse(Parameter=="Fecal Coliform"&Value==0,1,Value))

###############################
tmpf <- bind_rows(res_t,tmpcs,tmpfc) 

##############################################
### add tsi / dosat
tmptsichla <- tmpf %>% filter(Parameter=="Chlorophyll a") %>% mutate(Value = 10*(6-(2.04-0.68*log(Value))/log(2)))
ggplot(tmptsichla,aes(time,Value)) + geom_line() + facet_wrap(~Locator)  
tmptsichla$Parameter <- 'TSI_chla'

tmptsisecchi <- tmpf %>% filter(Parameter=="Secchi Transparency") %>% mutate(Value = 10*(6-log(Value)/log(2)))
ggplot(tmptsisecchi,aes(time,Value)) + geom_line() + facet_wrap(~Locator)  
tmptsisecchi$Parameter <- 'TSI_secchi'

# tmptsiTPs <- tmpf %>% filter(Parameter=="Total Phosphorus"&layer %in% c('epi','surface')) %>% mutate(Value = 10*(6-log(48/(Value*1000))/log(2)))
tmptsiTPs <- tmpf %>% filter(Parameter=="Total Phosphorus"&layer %in% c('epi','surface')) %>% mutate(Value = 10*(6-log(48/(Value))/log(2)))
ggplot(tmptsiTPs,aes(time,Value)) + geom_line() + facet_grid(layer~Locator)  
tmptsiTPs$Parameter <- 'TSI_TP'

# tmptsiTNs <- tmpf %>% filter(Parameter=="Total Nitrogen"&layer %in% c('epi','surface')) %>% mutate(Value = 10*(6-log(1.47/Value)/log(2)))
tmptsiTNs <- tmpf %>% filter(Parameter=="Total Nitrogen"&layer %in% c('epi','surface')) %>% mutate(Value = 10*(6-log(1.47/Value/1000)/log(2)))
ggplot(tmptsiTNs,aes(time,Value)) + geom_line() + facet_grid(layer~Locator)  
tmptsiTNs$Parameter <- 'TSI_TN'

tmpf <- bind_rows(tmpf,tmptsichla,tmptsisecchi,tmptsiTPs,tmptsiTNs) 
unique(tmpf$Parameter)

## do sat
library(wql)
library(rMR)
tmpdosat <- tmpf %>% filter(Parameter %in% c('Dissolved Oxygen, Field','Temperature')) %>% spread(key=Parameter,value=Value) %>% 
  mutate(Value = 100*`Dissolved Oxygen, Field`/oxySol(t=Temperature,S=0))
tmpdosat <- tmpf %>% filter(Parameter %in% c('Dissolved Oxygen, Field','Temperature')) %>% spread(key=Parameter,value=Value) %>% 
  mutate(Value = 100*DO.saturation(`Dissolved Oxygen, Field`,Temperature,elevation.m = 6)) %>% select(-c('Dissolved Oxygen, Field','Temperature')) %>% 
  filter(!is.na(Value))
tmpdosat$Parameter <- 'DOsat'

ggplot(tmpdosat,aes(time,Value)) + geom_line() + facet_grid(layer~Locator)

tmpf <- bind_rows(tmpf,tmpdosat) 
#################################################################################
# Argh!!! I will need to remove interpolated values for Jan-Mar 2020
# Also, have to deal with detection limit change in DO in Sammamish...
tmpf <- filter(tmpf,!between(time,as.Date("2020-01-01"),as.Date("2020-04-01")))
tmpx <- filter(tmpf,is.na(tmpf$Value))
#################################################################################

#################
### rLakeAnalyzer - Schmidt Stability
#################
opath <- 'data/LakeAnalyzer'
path2file <- paste0(opath,'/temperature/0852/1993_',yrendn)
filepath <- paste0(path2file,'/Washington.wtr')
Washington.wtr <- load.ts(filepath, tz = "GMT")
file.copy(from = paste0(opath, '/Washington.bth'), to = paste0(path2file,'/Washington.bth'))
filepath <- paste0(path2file,'/Washington.bth')
Washington.bth <- load.bathy(filepath)

thermo.ts <- ts.thermo.depth(Washington.wtr)
thermo.ts$Date <- as.Date(thermo.ts$datetime)
plot(thermo.ts$datetime,thermo.ts$thermo.depth,type="l",col=1)

thermo.ts <- thermo.ts %>% mutate(thermo.depth = round(thermo.depth,2)) %>% select(Date,thermo.depth) %>% rename(thermocline_depth = thermo.depth)

s.s <- ts.schmidt.stability(Washington.wtr, Washington.bth, na.rm = FALSE)
plot(s.s,type="l",col=1)
lw.s <- s.s %>% mutate(Locator = '0852')

opath <- 'data/LakeAnalyzer'
path2file <- paste0(opath,'/temperature/0612/1993_',yrendn)
filepath <- paste0(path2file,'/Sammamish.wtr')
Sammamish.wtr <- load.ts(filepath, tz = "GMT")
file.copy(from = paste0(opath, '/Sammamish.bth'), to = paste0(path2file,'/Sammamish.bth'))
filepath <- paste0(path2file,'/Sammamish.bth')
Sammamish.bth <- load.bathy(filepath)

thermo.ts <- ts.thermo.depth(Sammamish.wtr)
thermo.ts$Date <- as.Date(thermo.ts$datetime)
plot(thermo.ts$datetime,thermo.ts$thermo.depth,type="l",col=1)

thermo.ts <- thermo.ts %>% mutate(thermo.depth = round(thermo.depth,2)) %>% select(Date,thermo.depth) %>% rename(thermocline_depth = thermo.depth)

s.s <- ts.schmidt.stability(Sammamish.wtr, Sammamish.bth, na.rm = FALSE)
plot(s.s,type="l",col=1)
ls.s <- s.s %>% mutate(Locator = '0612')

#######################################################################
### account for salt in Lake Union?
#######################################################################

opath <- 'data/LakeAnalyzer'
path2file <- paste0(opath,'/temperature/A522/1993_',yrendn)
filepath <- paste0(path2file,'/Union.wtr')
Union.wtr <- load.ts(filepath, tz = "GMT")
file.copy(from = paste0(opath, '/Union.bth'), to = paste0(path2file,'/Union.bth'))
filepath <- paste0(path2file,'/Union.bth')
Union.bth <- load.bathy(filepath)

# Union salt
path2file <- paste0(opath,'/Conductivity, Field/A522/1993_',yrendn)
filepath <- paste0(path2file,'/Union.wtr')
Union.sal <- load.ts(filepath, tz = "GMT")
# fewer conductivity profiles than temperature profiles...
Union.wtr <- filter(Union.wtr,datetime %in% unique(Union.sal$datetime))

thermo.ts <- ts.thermo.depth(Union.wtr)
thermo.ts$Date <- as.Date(thermo.ts$datetime)
plot(thermo.ts$datetime,thermo.ts$thermo.depth,type="l",col=1)

thermo.ts <- thermo.ts %>% mutate(thermo.depth = round(thermo.depth,2)) %>% select(Date,thermo.depth) %>% rename(thermocline_depth = thermo.depth)

# s.s <- ts.schmidt.stability(Union.wtr, Union.bth, na.rm = FALSE)
s.s <- ts.schmidt.stability.kc(wtr=Union.wtr, bathy=Union.bth, sal=Union.sal, na.rm = FALSE)
wtr.mat = as.matrix(drop.datetime(Union.wtr))
sal.mat = as.matrix(drop.datetime(Union.sal))
sal.mat = ec2pss(sal.mat/1000,wtr.mat,p=0)
plot(s.s,type="l",col=1)
lu.s <- s.s %>% mutate(Locator = 'A522')

s.s <- bind_rows(lw.s,ls.s,lu.s)

lakes.s <- s.s %>% transmute(time = as.Date(datetime), doy = yday(time), Year = year(time), Month = month(time), Locator = Locator,
                          Parameter = "Schmidt Stability", layer = "", Value = schmidt.stability, Station = Locator)

tmpf <- bind_rows(tmpf,lakes.s)

########################################################################################################
tmpx <- filter(tmpf,Locator=='0612'&Parameter=="Total Alkalinity")
ggplot(tmpx,aes(time,Value,color=layer)) + geom_line()
# GAM for total alkalinity not working...remove for now

# tmpf <- filter(tmpf,!Parameter=="Total Alkalinity")
########################################################################################################

unique(tmpf$Parameter)
# format data
datprc <- tmpf %>% 
  # transmute(date = CollectDate, locator = Locator, station = as.numeric(substring(Locator,2,4)), param = ParmDisplayName, value = Value,
  transmute(date = time, station = Station, locator = Locator, param = Parameter, value = Value,
            doy = doy, layer = layer) %>% 
  mutate(
    # date = ymd(date),
    # doy = yday(date), 
    cont_year = decimal_date(date),
    yr = year(date),
    mo = month(date, label = T),
    #param = tolower(param)
  ) %>% 
  # filter(yr >= 1993 & yr <= 2019) %>%
  filter(yr >= 1993 & yr <= yrendn)

# %>% 
#   filter(!is.na(value)) %>% group_by(date,doy,cont_year,yr,mo,locator,station,param,depth) %>% 
#   summarize(value = mean(value)) %>% ungroup()


unique(datprc$param)
datprc$param <- with(datprc,ifelse(param=="Dissolved Oxygen, Field","do",
                             ifelse(param=="Secchi Transparency","secchi",
                             ifelse(param=="Temperature","temp",
                                    ifelse(param=="Chlorophyll a","chl",
                                           ifelse(param=="Conductivity, Field","cond",
                                                  ifelse(param=="Nitrite + Nitrate Nitrogen","no3",
                                                         ifelse(param=="Orthophosphate Phosphorus","srp",
                                                                ifelse(param=="pH, Field","ph",
                                                                       ifelse(param=="Total Alkalinity","alk",
                                                                              ifelse(param=="Total Phosphorus","tp",
                                                                                     ifelse(param=="Total Nitrogen","tn",
                                                                                            ifelse(param=="Fecal Coliform","fc",
                      ifelse(param=="DOsat","dosat",
                             ifelse(param=="Schmidt Stability","ss",
                                           param)))))))))))))))
unique(datprc$param)

save(datprc, file = 'data/datprc.RData', compress = 'xz')
# save(datprc, file = 'datprc.RData', compress = 'xz')
# load('data/datprc.RData')

# GAM for total alkalinity not working...remove for now
# datprc <- filter(datprc,!param=="alk")

ggplot(datprc,aes(date,value)) + geom_line() +
  facet_grid(param~locator,scales="free_y")

ggplot(datprc %>% filter(layer=='epi'),aes(date,value)) + geom_line() +
  facet_grid(param~locator,scales="free_y")
tmpx <- datprc %>% filter(layer=='epi')
# data to model, same as datprc, params in wide format, nested by station
# crossed with frms
library(tidyverse)
library(lubridate)
library(sf)
library(wqtrends)

tomod <- datprc %>% 
  filter(param %in% c('temp', 'do', 'secchi', 'chl','cond','tp','srp','tn','no3','ph','alk','fc','dosat','TSI_chla','TSI_secchi','TSI_TP','TSI_TN','ss')) %>% # add parameters here
  group_by(station, param) %>% 
  nest %>% 
  mutate(
    trans = case_when(
      param %in% c('chl','fc') ~ 'log10', 
      T ~ 'ident'
    )
  )

# create models for every station, gam model eval
modssta <- tomod %>%
  mutate(
    modi = purrr::pmap(list(station, param, trans, data), function(station, param, trans, data){
      
      cat(station, param, '\n')
      out <- anlz_gam(data, trans = trans)
      return(out)
      
    })
  )

# separate models into diff files by parameter and stations (single is too large for git)
tosv <- modssta %>% 
  select(station, param) %>% 
  unique

for(i in 1:nrow(tosv)){
  
  cat(i, 'of', nrow(tosv), '\n')
  
  sta <- tosv[[i, 'station']]
  param <- tosv[[i, 'param']]
  
  fl <- modssta %>% 
    filter(station %in% !!sta) %>% 
    filter(param %in% !!param)
  
  flnm <- paste0('mods_', param, sta)
  
  assign(flnm, fl)
  
  save(list = flnm, file = paste0('data/', flnm, '.RData'), compress = 'xz')
  
}

