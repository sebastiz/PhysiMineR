#Data for Jafar....


#Achalasia and >1 HRM

#Need to get the achalasia diagnoses by grepping from the whole report
HRMAnd_diag <- HRMImportMainTwo %>% inner_join(Diag, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id)

#Then get the achalasias just by grepping- will need manual sorting from here
Achalasias<-HRMAnd_diag[grepl("achalasia",HRMAnd_diag$WholeReport,ignore.case = TRUE),]

#Select the achalasias where there arent any exact duplicates
Achalasias<-Achalasias %>% distinct(HospNum_Id, HRM_Id, .keep_all = TRUE)

#Get only those rows with duplicated hospital numbers so that you now have the truly repeated HRMs for a patient (rather than duplicate records)
#Achalasias<-Achalasias[duplicated(Achalasias$HospNum_Id),]

Achalasias<-data.frame(Achalasias,stringsAsFactors = FALSE)

Achalasiasdup<-Achalasias %>%
  group_by(HospNum_Id) %>%
  mutate(num_rows = sum(n())) %>%
  filter(num_rows > 1)

writexl::write_xlsx(Achalasiasdup,"Achalasias2.xlsx")

#Further cleaning to remove exact duplicated rows based on HRM_Id
#Achalasias<-Achalasias[!duplicated(Achalasias[c("HospNum_Id","HRM_Id")]),]

######################## Barium cross referencing study ######################################

Bariums <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Bariums.XLSX"))
library(janitor)
Bariums<-janitor::clean_names(Bariums)
#Need to rename columns for the merge

Bariums<-Bariums %>% rename(VisitDate = event_date,HospNum_Id=hosp_no)
Bariums$VisitDate<-as.Date(Bariums$VisitDate)

#Barium impedance test merges:
Barium_Impedance <- Bariums %>% inner_join(ImpAll, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)

writexl::write_xlsx(Barium_Impedance,"Barium_Impedance.xlsx")



#Barium with HRM with Diag test merges:
Barium_HRM_Diag <- Bariums %>% inner_join(HRMAnd_diag, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)

writexl::write_xlsx(Barium_HRM_Diag,"Barium_HRM_Diag.xlsx")
