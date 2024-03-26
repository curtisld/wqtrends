## function to extract water quality station data from Socrata

require(dplyr)
require(RSocrata)

get_socrata_sites_func <- function(locns = c('0852')){
  
  loc_url_portal<-'https://data.kingcounty.gov/resource/wbhs-bbzf.csv'
  locs<-read.socrata(loc_url_portal) %>%
    transmute(SiteName=sitename,
              Locator=locator,
              Longitude=as.numeric(longitude),
              Latitude=as.numeric(latitude),
              SiteType=site_type,
              Area=area) %>%
    filter(Locator %in% locns&!is.na(Longitude))
  return(locs)
}
  
 