
write_LA_file <- function(data,data2,lake,station,parm,opath,yrstart,yrend){
  
  odir <- paste0(opath,'/',parm,'/',station,'/',yrstart,'_',yrend)
  dir.create(odir,recursive=TRUE)
  
  filename <- paste0(opath,'/',parm,'/',station,'/',yrstart,'_',yrend,'/',lake,'.wtr')
  write.table(data, filename, sep="\t",quote = FALSE, row.names = FALSE)
  
  filename <- paste0(opath,'/',parm,'/',station,'/',yrstart,'_',yrend,'/',lake,'.mean.wtr')
  write.table(data2, filename, sep="\t",quote = FALSE, row.names = FALSE)
  
}
