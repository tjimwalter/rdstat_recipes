library(lubridate)
#' Transform string date into POSIXct
#' 
#' Converts a string/vector of date strings in MM/DD or YYYMMDD format to POSIXct. 
#' If orders is specified, the ellipsis parameters are passed to parse_date_time.
#' @param strDateTime a string containing date and time
#' @param ... parameters passed to lubridate::parse_date_time if orders is specified
#' @param tz the timezone to use, defaulting to Sys.timezone()
#' @return A POSIXct with tz=tz, defaulting to Sys.timezone()
###################################################################################################
DateExtractor <- function(strDateTime, ..., tz=Sys.timezone()){
  
  # if explicit format accept the results of parse_date_time
  #
  if (exists('orders', list(...))){
    return(parse_date_time(strDateTime, tz=tz, ...))
  }
  
  # next, try YYYY-MM-DD HH:MM:SS format
  #
  suppressWarnings(posixDate <- ymd_hms(strDateTime, tz=tz))
  if (sum(is.na(posixDate))!=length(posixDate)){  # date conversion worked
    return(posixDate)
  }
  
  # finally, try dstat default format
  #
  posixDate <- DstatDateExtractor(strDateTime, tz=tz)
  if (posixDate != -1){
    return(posixDate)
  } 
  #warning(paste("Date conversion failed on:", strDateTime))
  return(-1)
}
  

#' Translate a default dstat date string into a POSIXct
#' 
#' Converts a string formatted as DD-MM HH:MM:SS
#' @param strDateTime a string containing date and time
#' @param tz the timezone to use
#' @return A POSIXct with tz=tz, defaulting to Sys.timezone()
###################################################################################################
DstatDateExtractor <- function(strDateTime, tz=Sys.timezone()){
  len      <- nchar(strDateTime)
  startPos <- len - 7                                       
  time     <- substr(strDateTime, startPos, len)                    # last 8  HH:MM:SS
  
  day      <- substr(strDateTime, 1, 2)                             # day is first 2 DD-MM
  month    <- substr(strDateTime, 4, 5)                             # month is +1 and next 2
  year     <- as.character(year(Sys.time()))                        # absent year, use today
  date     <- paste(year, month, day, sep="-")
  
  strDateTime <- paste(date, time)
  posixDate   <- tryCatch( { ymd_hms(strDateTime, tz=tz) },
                           warning = function(war) { assign("last.warning", NULL, envir = baseenv()); return(-1) },
                           error   = function(err) { return(-1) } )
  return(posixDate)
}

#' Transform an experiment fullExperiment into an experiment name
#' 
#' Takes a fully qualified path and returns a shorter - but still unique - experiment name
#' @param fullExperiment The orginal path that was read, which might include directories.
#' @return A character vector of experiment
###################################################################################################
ExperimentExtractor <- function(fullExperiment) {
  experiment <- basename(fullExperiment)
  experiment <- gsub("_",    "-", experiment)
  experiment <-  sub(".csv", "",  experiment)
  
  return(experiment)
} 
