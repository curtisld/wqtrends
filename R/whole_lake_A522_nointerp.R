#
library(signal)
library(readxl)
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)
library(akima)
# library(ContourFunctions)
# library(plotly)
library(metR)
library(viridis)
library(miscTools)

whole_lake_A522_nointerp <- function(data=data,parm='Temperature',lake='Union',station='A522',yrstart=1993,yrend=2018,opath){
  
  if(lake=='Union'){
      
      # volume weighting fractions for each meter depth for deepest lake station    
      vol_frac <- read_excel('//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/!CSOWQA/Lake Washington/Data/water/R/Lake_Union_depth_volume.xlsx','depth_vol')  
      
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
       Depth <- seq(0,14)
      xi <- seq(0,14)
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
      
      # https://stackoverflow.com/questions/62543112/how-to-make-discrete-gradient-color-bar-with-geom-contour-filled
      p<- ggplot(t.profile,aes(x=Date,y=Depth,z=Value)) + 
        # geom_contour_filled() +
        geom_contour_filled(aes(fill = ..level..)) +
        # metR::geom_contour_fill() + 
        # geom_contour(color = "black", size = 0.1) +
        # scale_fill_brewer(palette = "Greys") +
        scale_colour_viridis() +
        scale_y_reverse(expand = c(0, 0)) + scale_x_date(expand = c(0, 0)) +
        theme(axis.text=element_text(size=14,face="bold"), axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14),
              plot.title = element_text(size=20,hjust = 0.5),legend.text=element_text(size=12)) +
        ylab('Depth in meters') + xlab('') +
        guides(fill=guide_legend(title=parm)) +
        ggtitle(paste0('Lake ',lake))
      print(p)

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
      
      jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    contour(x=Ti,y=xi,z=as.matrix(Tii2[c(2:16)]),ylim=c(max(xi),0))
      # filled.contour(x=Ti,y=xi,z=as.matrix(Tii2[c(2:16)]),ylim=c(14,0),col = jet.colors(10))
      # filled.contour(x=Ti,y=xi,z=as.matrix(Tii2[c(2:16)]),ylim=c(14,0),color.palette = function(n) hcl.colors(n, "YlOrRd", rev = TRUE))
     filled.contour(x=Ti,y=xi,z=as.matrix(Tii2[c(2:16)]),ylim=c(max(xi),0),color.palette = jet.colors,ylab = "Depth in meters",
        plot.title = title(main = parm))
      # cf_grid(x=Ti,y=xi,z=as.matrix(Tii2[c(2:16)]),ylim=c(14,0),color.palette=jet.colors)
      # cf_grid(x=Ti,y=xi,z=as.matrix(Tii2[c(2:16)]),ylim=c(14,0),color.palette=jet.colors,bar=T)
      # gcf_grid(x=Ti,y=xi,z=as.matrix(Tii2[c(2:16)]),ylim=c(14,0),color.palette=jet.colors,bar=T)
     
     data <- Tii2 
     data$Date <- as.Date(data$Date,origin="1970-01-01")
     data$doy <- as.numeric(strftime(data$Date, format = "%j"))
     data$date <- as.Date(paste0('2000-',month(data$Date),'-',mday(data$Date)))
     
     colnms <- paste0("Value.",1:ncol(data)-1) 
     #  res <- data %>% group_by(date, colnms) %>% summarize() 
     
     # res <- NULL
     # for(i in 1:max(data$doy)){
     #   
     #   tmp <- subset(data,data$doy==i)
     #   l <- dim(tmp)
     #   l <- l[2]-2
     #   # tmp <- colMeans(tmp[,2:l])
     #   #### if one wanted medians
     #   tmp <- colMedians(as.matrix(tmp[,2:l]))
     #   res <- rbind(res,tmp)
     #   
     # }
     # 
     # res <- data.frame(res)
     # res$Date <- unique(data$date[order(data$date)])
     # 
     # res <- res %>% select(Date,everything())
     # 
     # res2 <- res
     # res2 <- gather(res2,-Date,key=Depth,value=Value) %>% mutate(Depth = as.numeric(substr(Depth,7,length(Depth)))-1)
     # 
     # pp <- ggplot(res2,aes(x=Date,y=Depth,z=Value)) + 
     #   geom_contour_filled() +
     #   # geom_contour_filled(aes(fill = ..level..)) +
     #   # metR::geom_contour_fill() + 
     #   geom_contour(color = "black", size = 0.1) +
     #   # scale_fill_brewer(palette = "Greys") +
     #   scale_colour_viridis() +
     #   scale_y_reverse(expand = c(0, 0)) + scale_x_date(expand = c(0, 0)) +
     #   theme(axis.text=element_text(size=14,face="bold"), axis.title=element_text(size=14,face="bold"),legend.title=element_text(size=14),
     #         plot.title = element_text(size=20,hjust = 0.5),legend.text=element_text(size=12)) +
     #   ylab('Depth in meters') + xlab('') +
     #   guides(fill=guide_legend(title=parm)) +
     #   ggtitle(paste0('Lake ',lake))
     # print(pp)
     # rm(res)
     
      ### back to work....actually now to create whole lake time-volume-weighted annual average values
      tmp <- as.matrix(Tii2[,2:16])
      #tmp2 <- apply(tmp,1,rev) 
      #tmp2 <- apply(tmp2,2,rev) 
      tmp2 <- t(tmp)
      
      dim(tmp2)
      
      vol_fct <- as.matrix(vol_frac$vol_fct)
      vol_fct <- vol_fct[1:15]
      
      ts <- tmp2*vol_fct
      
      ts2 <- colSums(ts)
      plot(Ti,ts2,type="l")
      
      res <- data.frame(time=Ti,Value=ts2)
      res$Year <- year(res$time)
      res$Month <- month(res$time)
      res$doy <- as.numeric(strftime(res$time, format = "%j"))
      res$Locator <- as.character(station)
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

