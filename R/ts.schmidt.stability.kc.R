ts.schmidt.stability.kc <- function (wtr, bathy, sal, na.rm = FALSE) {
  
  library(rLakeAnalyzer)
  library(wql)
  
  depths = get.offsets(wtr)
  n = nrow(wtr)
  s.s = rep(NA, n)
  wtr.mat = as.matrix(drop.datetime(wtr))
  sal.mat = as.matrix(drop.datetime(sal))
  sal.mat = ec2pss(sal.mat/1000,wtr.mat,p=0)
  dimnames(wtr.mat) <- NULL
  for (i in 1:n) {
    if (na.rm) {
      temps = wtr.mat[i, ]
      if (all(is.na(temps))) {
        next
      }
      notNA = !is.na(temps)
      s.s[i] = schmidt.stability(temps[notNA], depths[notNA], 
                                 bathy$areas, bathy$depths)
    }
    else {
      if (any(is.na(wtr.mat[i, ]))) {
        s.s[i] = NA
        next
      }
      s.s[i] = schmidt.stability(wtr.mat[i, ], depths, 
                                 bathy$areas, bathy$depths, sal = sal.mat[i, ])
    }
  }
  output = data.frame(datetime = get.datetime(wtr), schmidt.stability = s.s)
  return(output)
}

#' @title Find and drop the datetime column from the datatable
#'
#' @description Liberally looks for a datetime column and drops it, 
#'  returning a data.frame with only water temperature. Errors if datetime column is 
#'  ambiguous. Warns if there is no match.
#' @param data data arg
#' @param error defaults to FALSE
#'
#' @return A data.frame with only the data, after datetime has been dropped

drop.datetime = function(data, error=FALSE){
  datetime.pattern = "(datetime|timestamp|time|date)"
  
  header = names(data)
  dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)
  
  if(length(dt_indx) < 1){
    if(error){
      stop('Unable to find a datetime column. Datetime column was supplied.')
    }else{
      warning('Unable to find a datetime column. Assuming no datetime column was supplied.')
      return(data)
    }
    
  }else if(length(dt_indx) > 1){
    stop('datetime column ambiguity. You can only have one column of datetime.')
  }
  
  return(data[,-dt_indx, drop=FALSE])
}

#' @title Search for and return the datetime column from a ts data.frame
#'
#' @description Warns if unavailable then returns NULL.
#' 
#' @inheritParams drop.datetime
#'
get.datetime = function(data, error=FALSE){
  datetime.pattern = "(datetime|timestamp|time|date)"
  
  header = names(data)
  dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)
  
  if(length(dt_indx) < 1){
    if(error){
      stop('Unable to find a datetime column.')
    }else{
      warning('Unable to find a datetime column, attempting to ignore.')
      return(NULL)
    }
  }else if(length(dt_indx) > 1){
    stop('datetime column ambiguity. You can only have one column of datetime.')
  }
  
  return(data[,dt_indx])
}