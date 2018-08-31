#######Data acquisition######

#' dataBT
#' This extracts the breath test data
#' @param channel the data base query
#' @keywords Fructose
#' @export
#' @examples #dataBT(channel)
dataBT<-function(channel){
  dataBT <- sqlQuery(channel , "SELECT BreathTests.* FROM BreathTests")
}


#### Breath Tests ####

#' dataFructose
#' This extracts the raw fructose data from the database
#' @param dataBT the data frame
#' @keywords Fructose
#' @export
#' @examples #dataFructose(dataBT)


dataFructose<-function(dataBT){
  #Creates a vector array of characters
  #For fructose assessment:
  #Just get the patients who had fructose breath tests:
  dataFruc<-dataBT[grepl(".*\\d.*",dataBT$frucH2,perl=T),]

  #Exclude columns that create confusion eg with H2 change
  dataFruc<-dataBT[!grepl(".*Hydrogen and Methane level.*|.*ppm.*",dataBT$frucH2),]

  #Select the columns you are interested in:
  dataFruc<-dataFruc%>%select(frucTimePoint,frucH2)
  dataFruc<-dataFruc[!is.na(dataFruc$frucTimePoint),]
  #
  VecArrFrucVal<-unlist(strsplit(dataFruc$frucH2, split="_"))
  VecArrFrucTimePoint<-unlist(strsplit(dataFruc$frucTimePoint, split="_"))
  Fruc<-as.data.frame(cbind(VecArrFrucVal,VecArrFrucTimePoint),stringsAsFactors=F)
  Fruc<-paste(VecArrFrucVal,VecArrFrucTimePoint,sep=":")


  #Split then combine

  Fruc<-paste(Fruc, collapse = "")
  Fruc<-as.data.frame(strsplit(Fruc, "H2"))
  names(Fruc)<-c("main")
  Fruc$main<-sub(":Time Point \\(min\\)","",Fruc$main)
  Fruc$main<-gsub("(\\d+)\\a(\\d+)","\\1,\\2",Fruc$main)
  Fruc$main<-gsub("\\a","",Fruc$main)
  Fruc$main<-strsplit(Fruc$main, ",",perl=T)
  #From here you can do individual graphs
}
