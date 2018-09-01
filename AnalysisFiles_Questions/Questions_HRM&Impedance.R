#Questions_HRM&Impedance


###################### Question: Which structural abnormalities are most likely to give you reflux ######################


######### Question: Is there a geriatric oesopahgus ie does the DCI decrease with age? ###########################


#dateDiff to get the age of the patients:

data<-data[data$Age<100&data$Age>0,]
data<-data[data$dx=="Normal",]
data<-data[data$DCI<5000&data$DCI>0,]

ggplot(data, aes(x=Age))+geom_density()

#Now calculate how the DCI varies with age
qplot(data$Age,data$DCI)+geom_smooth()

# Question: Association of acid reflux with hypomotility disorders of the oesophagus (frequent failed,aperistaltic etc.)....
# Question: Is there an association betweent the severity of GORD and the severity of IOM
# Question: Is the chance of developing IOM related to the frequency of reflux events?
# Question: IOM-Phenotype IOM categories hrm pushes towards gerd Lyon consensus rengarajan paper.
# Question: Frequent failed peristalsis vs structural also percentage FF vs structural anomalies or vs deMeester score? FF on water vs on solids - what proportion have both and what does that mean? Does FF peristalsis metric correlate with any particular aspect of
# Question: FFP-What is he difference between acid with FF vs acid without FF. What predicts whether someone gets FF or not?NEED TO DEFINE WHAT FF IS..
# Question: Aperistalsis- subcategories of GORD Aperistalsis....
# Question: Aperistalsis- What features of the aperistaltic oesophagus predict acid vs Scleroderma?
# Question: IOM- Does IOM increase the risk that a patient has reflux (IOM at pH impedance)?- does worse IOM mean worse reflux
