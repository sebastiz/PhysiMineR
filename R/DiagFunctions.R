###################################### Diag Subset Prepare ###################################################

#Clean up the Diag stuff

#' Diag cleanup
#'
#' Cleans up the Diag data
#' @param x the Diag data frame
#' @keywords Diag data
#' @export
#' @examples
#' #dataDiagClean(x)

dataDiagClean<-function(x){
  #Clean Diag
  x$HospNum_Id<-toupper(x$HospNum_Id)
  x$FileCreationDate<-as.Date(gsub("(\\d{4}-\\d{2}-\\d{2}).*","\\1",x$FileCreationDate))
  #Convert VisitDate into a Date format and extract the year (the month and day are unreliable here as sometimes
  #American and sometimes British dates are used)

  #Convert the dates to date format
  x$VisitDate2<-as.Date(x$VisitDate,format="%d_%m_%Y")
  #For NA dates make sure it is not because the dates are the wrong way around
  x$VisitDate3<-as.Date(x$VisitDate,format="%m_%d_%Y")

  #Merge the dates if there are separate HRM and Diag results which are reporting the same thing
  x$VisitDate4<-as.Date(ifelse(is.na(x$VisitDate2),x$VisitDate3,x$VisitDate2),origin = "1970-01-01")
  #If still NA then use the file creation date as the visit date
  x$VisitDate4<-as.Date(ifelse(is.na(x$VisitDate4),x$FileCreationDate,x$VisitDate4),origin = "1970-01-01")

  #Extract the Distal LES as is likely to be in both the final and the HRM report so can be merged on this
  x$DistalLESnares<-gsub(".*Distal LES from nares.*?(\\d+).*","\\1",x$WholeReport)
  #Get rid of whole reports that are copied over for some reason
  x$DistalLESnares<-gsub(".*[A-Za-z].*","\\1",x$DistalLESnares)

  #Remove duplicate reports on the basis of being the same Hospital Number and date range here
  #Also need to get rid of the duplicates from the Diag table:

  x<-x %>%
    arrange(DistalLESnares,HospNum_Id) %>%
    group_by(DistalLESnares,HospNum_Id,lubridate::year(VisitDate4)) %>%
    summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))

  #Take only the first report date here:
  x$VisitDate4<-gsub(":.*","",x$VisitDate4)
  x$VisitDate4<-as.Date(x$VisitDate4)

  #Get rid of extra columns:
  x$VisitDate<-NULL
  x$VisitDate2<-NULL
  x$VisitDate3<-NULL
  x<-x%>% rename(VisitDate=VisitDate4)


  x<-data.frame(x, stringsAsFactors = FALSE)

  return (x)
}
