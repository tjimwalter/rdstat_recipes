#library(lubridate)
#library(testthat)
source("~/dev/rdstat_recipes/Extractors.R")
context("Date extraction")

#
# Helper function that prints intermediate feedback when fdebug==TRUE
#
dprint <- function(str_to_print){
  fdebug <- FALSE
  if (fdebug) { print(str_to_print) }
}

#
# Helper function that transforms a posix date into an illegal dstat date string
#
get_illegal_dstat_date <- function(posix_date){
  day <- "32" # no month has 32 days
  mon <- ifelse(nchar(as.character(month(posix_date)))==2, as.character(month(posix_date)),
                paste("0", as.character(month(posix_date)), sep=""))
  hour <- ifelse(nchar(as.character(hour(posix_date)))==2, as.character(hour(posix_date)),
                 paste("0", as.character(hour(posix_date)), sep=""))
  min <- ifelse(nchar(as.character(minute(posix_date)))==2, as.character(minute(posix_date)),
                paste("0", as.character(minute(posix_date)), sep=""))
  sec <- ifelse(nchar(as.character(second(posix_date)))==2, as.character(second(posix_date)),
                paste("0", as.character(second(posix_date)), sep=""))
  dstat_date <- paste(day, "-", mon, " ", hour, ":", min, ":", sec, sep="")
  return(dstat_date)
}

#
# Helper function that transforms a posix date into a dstat date string
#
get_legal_dstat_date <- function(posix_date){
  day <- ifelse(nchar(as.character(day(posix_date)))==2, as.character(day(posix_date)),
              paste("0", as.character(day(posix_date)), sep=""))
  mon <- ifelse(nchar(as.character(month(posix_date)))==2, as.character(month(posix_date)),
                paste("0", as.character(month(posix_date)), sep=""))
  hour <- ifelse(nchar(as.character(hour(posix_date)))==2, as.character(hour(posix_date)),
              paste("0", as.character(hour(posix_date)), sep=""))
  min <- ifelse(nchar(as.character(minute(posix_date)))==2, as.character(minute(posix_date)),
                paste("0", as.character(minute(posix_date)), sep=""))
  sec <- ifelse(nchar(as.character(second(posix_date)))==2, as.character(second(posix_date)),
              paste("0", as.character(second(posix_date)), sep=""))
  dstat_date <- paste(day, "-", mon, " ", hour, ":", min, ":", sec, sep="")
  return(dstat_date)
}

test_that("1. DateExtractor translates dstat default format into posixCT", {
  posix_date <- round_date(Sys.time(), unit="second")
  str_date   <- get_legal_dstat_date(posix_date)
  
  dprint(paste("posix date:",                posix_date, 
               "as dstat  in DD-MM format:", str_date, 
               "     returns as posix:",     DateExtractor(str_date)))
  
  expect_equal(posix_date, DateExtractor(str_date))
})

test_that("2. DateExtractor translates Sys.time format into posixCT", {
  posix_date <- round_date(Sys.time(), unit="second")
  str_date   <- (as.character(posix_date))
  
  dprint(paste("posix date:",                posix_date, 
               "as string in Sys   format:", str_date, 
               "returns as posix:",          DateExtractor(str_date)))
  
  expect_equal(posix_date, DateExtractor(str_date))
})

test_that("3. DateExtractor translates YMD_HMS format into posixCT", {
  posix_date <- round_date(Sys.time(), unit="second")
  str_date   <- strftime(posix_date, format="%Y-%m-%d %H:%M:%S")
  
  dprint(paste("posix date:",                posix_date, 
               "as string in YMD   format:", str_date, 
               "returns as posix:",          DateExtractor(str_date)))
  
  expect_equal(posix_date, DateExtractor(str_date))
})

test_that("4. DateExtractor translates DMY_HMS format into posixCT", {
  posix_date <- round_date(Sys.time(), unit="second")
  str_date   <- strftime(posix_date, format="%d-%m-%Y %H:%M:%S")
  
  dprint(paste("posix date:",                posix_date, 
               "as string in DMY   format:", str_date, 
               "returns as posix:",          DateExtractor(str_date, orders="dmyhms")))
  
  expect_equal(posix_date, DateExtractor(str_date, orders="dmyhms"))
})

test_that("5. DateExtractor translates MDY_HMS format into posixCT", {
  posix_date <- round_date(Sys.time(), unit="second")
  str_date   <- strftime(posix_date, format="%m/%d/%Y %H:%M:%S")
  
  dprint(paste("posix date:",                posix_date, 
               "as string in MDY   format:", str_date, 
               "returns as posix:",          DateExtractor(str_date, orders="mdyhms")))
  
  expect_equal(posix_date, DateExtractor(str_date, orders="mdyhms"))
})

test_that("6. DateExtractor fails with illegal day in dstat", {
  posix_date <- round_date(Sys.time(), unit="second")
  str_date   <- get_illegal_dstat_date(posix_date)
  
  dprint(paste("posix date:",                posix_date, 
               "as ILLEGAL dstat   format:", str_date, 
               "     returns as posix:",     DateExtractor(str_date)))
  
  expect_equal(-1, DateExtractor(str_date))
})

