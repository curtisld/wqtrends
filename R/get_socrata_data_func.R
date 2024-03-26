## function to extract water quality data from Socrata

library(tidyverse)
library(RSocrata)
library(lubridate)
library(miscTools)

# Clark's lab data method changes adjustment function [https://green2.kingcounty.gov/ScienceLibrary/Document.aspx?ArticleID=324]
source("//kc.kingcounty.lcl/dnrp/WLRD/STS/WQStats/R Code Library/freshwater_nutrient_adjustment/function_for_fixing_method_change_nutrients.R")
source("//kc.kingcounty.lcl/dnrp/WLRD/STS/WQStats/R Code Library/freshwater_nutrient_adjustment/lab_chlorophyll_correction.R")


# get_socrata_data_func <- function(locns = c('0852'),parms = c('Chlorophyll a','Secchi Transparency','Total Suspended Solids'), SiteType = 'Large Lakes'){
get_socrata_data_func <- function(locns = c('0852'),parms = c("Chlorophyll a", "Chlorophyll, Field", "Density", "Dissolved Organic Carbon", "Dissolved Oxygen", 
                                                              "Dissolved Oxygen, Field", "E. coli", "Enterococcus", "Fecal Coliform", "Light Intensity (PAR)", 
                                                              "Surface Light Intensity (PAR)", "Light Transmissivity", "Ammonia Nitrogen", "Nitrite + Nitrate Nitrogen", 
                                                              "Orthophosphate Phosphorus", "Pheophytin a", "pH, Field", "Salinity", "Salinity, Field", "Secchi Transparency", 
                                                              "Silica", "Temperature", "Total Kjeldahl Nitrogen", "Total Nitrogen", "Total Organic Carbon", "Total Phosphorus", 
                                                              "Total Suspended Solids", "Turbidity", "Turbidity, Field", "Aragonite Saturation State", 
                                                              "Calcite Saturation State", "CO₂", "CO₃²⁻", "Dissolved Inorganic Carbon", "fCO₂", "HCO₃⁻", "pCO₂", "pH, total scale", "Revelle Factor", "Total Alkalinity", "Biochemical Oxygen Demand", "Conductivity", "Conductivity, Field", "Dissolved Oxygen Saturation, Field", "Fecal Streptococcus", "Hardness, Calc", "Nitrate Nitrogen", "Nitrite Nitrogen", "Organic Nitrogen", "Sampling Method", "Settleable Solids, Gravimetric", "Storm Or Non-Storm", "Total Coliform", "Total Hydrolyzable Phosphorus", "Volatile Suspended Solids", "pH", "BGA PC, Field"
                                                              )){

loc_url_portal<-'https://data.kingcounty.gov/resource/wbhs-bbzf.csv'
  locs<-read.socrata(loc_url_portal) %>%
    transmute(SiteName=sitename,
              Locator=locator,
              lng=as.numeric(longitude),
              lat=as.numeric(latitude),
              SiteTypeName=site_type,
              Area=area) %>%
    filter(!is.na(lng)) 
  
  
  # Limit to central lake locations and two long-term locations (0540 at Montlake Cut and 0804 at north end of Lake Washington)
  # locs <- filter(locs,Locator %in% c("0612","0852","A522","0804","0540"))
  #  locs <- filter(locs,Locator %in% c("A522"))
  locs <- filter(locs,Locator %in% locns)
  #locs <- filter(locs,Locator %in% c("0512"))
  
  data_url_start<-'https://data.kingcounty.gov/resource/vwmt-pvjw.csv' #entire wq portal
  download_query<-paste0("?$where=",
                         "(",paste0("locator='",locs$Locator,"'",collapse=' OR '),')',
                         # " AND (",paste0("parameter='",c('Temperature','Conductivity, Field','Chlorophyll a','Secchi Transparency','Total Suspended Solids'),"'",collapse=' OR '),')',
                         " AND (",paste0("parameter='",parms,"'",collapse=' OR '),')',
                         " AND ","NOT qualityid = 4") # qualityid = 4 for rejected data

  # Convert nutrients from mg to ug
  ug.params<-c("Total Nitrogen",
               "Total Phosphorus",
               "Ammonia Nitrogen",
               "Nitrite + Nitrate Nitrogen",
               "Orthophosphate Phosphorus")
    
  data_out<-read.socrata(paste0(data_url_start,download_query)) %>% filter(qualityid != 9) %>% # also remove missing (not data values) data (qualityid = 9)
    # filter(!is.na(depth)) %>% 
    transmute(CollectDateTime=collect_datetime,
              CollectDate = as.Date(CollectDateTime),
              Year = year(CollectDate),
              Month = month(CollectDate),
              Day =day(CollectDate),
              doy=as.numeric(strftime(CollectDate, format = "%j")),
              pDate=as.Date(paste(2000,Month,Day,sep="-")), # for plotting years together as dates - 2000 is a leap year
              LabSampleNum=sample_number,
              Site=site,
              Locator=locator,
              Depth=depth,
              Parameter=parameter,
              ParmDisplayName=parameter,
              Value2 = value, # save original values. Assume these include override values (this column no longer in the Socrata table for some reason)
              Value3 = ifelse(is.na(Value2),mdl,NA), # for nondetect plotting
              Value = ifelse(is.na(Value2),mdl,Value2), # for nondetect handling in NADA and/or NADA2
              nd_Flag = ifelse(is.na(Value2),TRUE,FALSE), # for nondetect handling in NADA and/or NADA2
              Units=units,
              QfrCode=lab_qualifier,
              QualityId = qualityid,
              Mdl = mdl,
              MDL=mdl,
              RDL=rdl,
              Method = method,
              Replicates = replicates,
              ReplicateOf = replicateof,
              StewardNote = stewardnote,
              Text=textvalue) %>% 
    mutate(Value2 = ifelse(Parameter %in% ug.params,
                           Value2*1000,
                           Value2),
           
           Value3 = ifelse(Parameter %in% ug.params,
                           Value3*1000,
                           Value3),
           Value = ifelse(Parameter %in% ug.params,
                          Value*1000,
                          Value),
           Units = ifelse(Parameter %in% ug.params,
                          "ug/L",
                          Units),
           MDL = ifelse(Parameter %in% ug.params,
                        MDL*1000,
                        MDL),
           RDL = ifelse(Parameter %in% ug.params,
                        RDL*1000,
                        RDL)
    )
  
  # fix some bad Locators
  data_out$Locator <- with(data_out,(ifelse(Locator=='612','0612',
                                            ifelse(Locator=='852','0852',
                                                   ifelse(Locator=='512','0512',
                                                          
                                                          ifelse(Locator=='826','0826',
                                                                 ifelse(Locator=='831','0831',
                                                                        ifelse(Locator=='804','0804',
                                                                               Locator))))))))
  
  data_out <- left_join(data_out,locs,by="Locator")
  

  # any NAs left are nondetects with no MDLs
  tmp <- data_out[is.na(data_out$Value),]
  
  
  #### adjustments for lab method changes (nutrients and chlorophyll)
  ### note this only changes results reported prior to 2007 for nutrients and prior to July 1996 for chlorophyll a data
  data_out$Value <- lab_change_correction(Value = data_out$Value,Date=data_out$CollectDate,Parameter=data_out$Parameter)
  data_out$Value <- lab_chlorophyll_correction(Value = data_out$Value,Date=data_out$CollectDate,Parameter=data_out$Parameter)
  
  return(data_out)
  
  
}