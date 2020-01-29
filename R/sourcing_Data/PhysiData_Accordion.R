#Data Accordionisation
#Run after PhysiData_Acq
#Run after PhysiData_Clean

#Extract all the symptoms
ImpAll<-dataImpSymptoms(Impedance2_Clean)
rm(Impedance2_Clean)

#Extract who has reflux based on Demeester or on the Number of refluxes from both Impedance and BRAVO
dataImpSypmAndImpedanceMain<-GORD_AcidImp(ImpAll)

#Need to change this so that is part of the PhysiMineR codebase (when I can get AWS to stop crashing of course)
AllBravo<-GORD_AcidBRAVO(AllBravo)
