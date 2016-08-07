# Does a quick check to see if the file is dstat formatted
# return FALSE if "Dstat" isn't found in first line
IsDstatFile <- function(filename){
  fileLine <- readLines(filename, 1) 
  
  if (grepl("Dstat", fileLine, fixed=TRUE)){      # "Dstat" should be in the first line
    return (TRUE)
  } else {
    return(FALSE)
  }
}

#' Read dstat file and return a dataframe
#'
#' Reads a CSV-formatted dstat file and returns a dataframe
#' @param path CSV file representing one experimental result
#' @param skip Number of lines to skip at beginning of a file - defaults to 6
#' @return A dataframe where each experiment forms a panel
#' @export
###################################################################################################
ReadExperiment <-function(filename, skip=6) {
  
  if (!IsDstatFile(as.character(filename))){
    message(paste(filename, "is not a valid dstat file."))
    return(-1)
  }
  mdf            <- read.table(filename, header=TRUE, sep=",", skip=skip)
  mdf$tick       <- as.numeric(rownames(mdf))
  mdf$experiment <- filename
  
  # % CPU / time unit - transform into cpu seconds (assumes 1/sec)
  #
  mdf$usr  <- (mdf$usr / 100) 
  mdf$sys  <- (mdf$sys / 100)
  mdf$idl  <- (mdf$idl / 100)
  mdf$wai  <- (mdf$wai / 100)
  mdf$siq  <- (mdf$siq / 100)
  mdf$hiq  <- (mdf$hiq / 100)
  
  mdf$read <- mdf$read
  mdf$writ <- mdf$writ
  mdf$send <- mdf$send
  mdf$recv <- mdf$recv
  
  mdf$time       <- DateExtractor(as.character(mdf[, grepl("time", names(mdf))]))
  #mdf            <- cbind(mdf, ExtractFactors(mdf$experiment))
  mdf$experiment <- ExperimentExtractor(mdf$experiment)

  return(mdf)
}

mdf <- ReadExperiment("~/studies/compare_test_control/input/synload_ctrl_rep1.csv")