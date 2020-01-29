#Demographic functions

#eg mean age at diagnosis with range and st dev. / sex ratio


#' testMerge
#' This extracts the symptoms
#' @param test1 dataframe
#' @param test2 dataframe
#' @param test1VisitDate VisitDate from the first test
#' @param test2VisitDate VisitDate from the second test
#' @keywords HRM CleanUp
#' @export
#' @examples #testMerge(test1,test2,test1VisitDate,test2VisitDate)



testMerge<-function(test1,test2,test1VisitDate,test2VisitDate){
  df3 <- test1 %>%
    inner_join(test2, by ="HospNum_Id") %>%
    mutate(Date_ABS_Diff = abs(test1VisitDate - test2VisitDate)) %>%
    arrange(HospNum_Id, Date_ABS_Diff) %>%
    group_by(HospNum_Id) %>%
    slice(1)
  return(df3)
}

