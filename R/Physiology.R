#Some more generic functions

#' Merge HRM and Impedance
#' This merges the overall impedance dataset with the HRM dataset
#' @param dataImpWhole the whole impedance dataset
#' @param dataHRM the whole HRM dataset
#' @keywords HRMAndSwallows acquisition
#' @export
#' @examples
#' #dataHRM<-dataHRM(channel)
#' #dataImp2<-dataImp2(channel)
#' #dataImp_Symp<-dataImp_Symp(channel)
#'
#' #dataImpWhole<-dataImpWhole(dataImp2,dataImp_Symp)
#' #MyImpedanceDataWithHRM(dataImpWhole,dataHRM)

MyImpedanceDataWithHRM<-function(dataImpWhole,dataHRM){
  MyImpedanceDataWithHRM<-merge(dataImpWhole,dataHRM,by=c("HospNum_Id"),all=TRUE)
  return(MyImpedanceDataWithHRM)
}
