rm(list=ls())
############################################
# Defining the normalization function
############################################
norm <-function(x,minx,maxx) #Normalization
{
  z<-((x-minx)/(maxx-minx))
  return(z) 
}
############################################
# Reading the DIABETIC Dataset 
############################################
Dataset <- read.csv("/Users/avinashsingh/desktop/data 2.csv") 
#Loading csv file
View(Dataset)

Dataset2 <- read.csv("/Users/avinashsingh/desktop/data.csv")
View(Dataset2)
############################################
# CREATING NEW DATASET:
#############################################
# NUMERICAL VALUES FOR RACE
# CAUCASIAN= 1 ASIAN=2 AFRICANAMERICAN=3 HISPANIC=4 OTHER=0
race_num <- ifelse(Dataset$race=='Caucasian',1,
            ifelse(Dataset$race=='Asian',2,
            ifelse(Dataset$race=='AfricanAmerican',3,
            ifelse(Dataset$race=='Hispanic',4,0))))

# AGE
# 0-10=1,10-20=2,20-30=3,30-40-=4,40-50=5,
# 50-60=6,60-70=7,70-80=8,80-90=9,90-100=J
age_group <- ifelse(Dataset$age=='[0-10)','A', 
             ifelse(Dataset$age=='[10-20)','B',
             ifelse(Dataset$age=='[20-30)','C',
             ifelse(Dataset$age=='[30-40)','D',
             ifelse(Dataset$age=='[40-50)','E',
             ifelse(Dataset$age=='[60-70)','F',
             ifelse(Dataset$age=='[70-80)','G',
             ifelse(Dataset$age=='[70-80)','H',
             ifelse(Dataset$age=='[80-90)','I','J')))))))))

# CHANGE OF MEDICATIONS
# Change=1, NoChange=0
Change_of_Medic <- ifelse(Dataset$change=='Ch',1,0)

# METFORMIN INFORMATION
# Up=1, Down=2, Steady=3, No=0
Metformin_Info<-ifelse(Dataset$metformin=='up',1,
                ifelse(Dataset$metformin=='Down',2, 
                ifelse(Dataset$metformin=='Steady',3,0)))

# INSULIN INFORMATION
# Up=1, Down=2, Steady=3, No=0
Insulin_Info<-ifelse(Dataset$insulin=='up',1,
              ifelse(Dataset$insulin=='Down',2, 
              ifelse(Dataset$insulin=='Steady',3,0)))

New_Dataset <- cbind(Race=race_num
                     ,num_lab_procedures=norm(Dataset[,13],min(Dataset[,13]),max(Dataset[,13]))
                     ,num_procedures=norm(Dataset[,14],min(Dataset[,14]),max(Dataset[,14]))
                     ,num_medications=norm(Dataset[,15],min(Dataset[,15]),max(Dataset[,15]))
                     ,number_diagnoses=norm(Dataset[,22],min(Dataset[,22]),max(Dataset[,22]))
                     ,metformin=Metformin_Info
                     ,insulin=Insulin_Info
                     ,change=Change_of_Medic
                     ,Glucose_Serum=as.character(Dataset$max_glu_serum)
                     ,A1C_Result=as.character(Dataset$A1Cresult)
                     ,Diabetes_Meds=as.character(Dataset$diabetesMed)
                     ,Age_Group=as.character(age_group)
                     ,Gender=as.character(Dataset$gender))
View(New_Dataset)

############################################################################
# Using NaiveBayes for % classification of variables against A1C_Result  ###
############################################################################
install.packages("e1071")
library(e1071)
data <- Dataset
Naive_Bayes <- naiveBayes(A1Cresult ~ gender+race+age+metformin+insulin
                          +change+diabetesMed, data)
Naive_Bayes

################################################################################
# Storing 70% of the new datsets as TRAINING DATA and the rest as TEST DATA  ###
################################################################################
temp <- sample(nrow(New_Dataset),as.integer(0.70 * nrow(New_Dataset)))
Training <- New_Dataset[temp,]
View(Training)
Test <- New_Dataset[-temp,]
View(Test)

#####################
# Performing KNN  ###
#####################
library(class)
?knn()

# Classifying based on A1Cresult
Predict_a1c <- knn(Training[,1:8], Test[,1:8], Training[,10], k=23)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Error value for kNN when classifying with A1C result  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
e_result <- cbind(Test, as.character(Predict_a1c))
false <- e_result[,10]!=e_result[,14]
err_rate <- sum(false)/length(false)
err_rate
View(e_result)

#>>>>>>>>>>>>>>>>>>>>>>
# Frequency tables  >>>
#>>>>>>>>>>>>>>>>>>>>>>
table(Predict_a1c, Test[,2]) # Plotting A1Cresult vs race
table(Predict_a1c, Test[,11]) # Frequency Table A1Cresult vs Diabetes Meds
table(Predict_a1c, Test[,12]) # Frequency Table A1Cresult vs age
table(Predict_a1c, Test[,9]) # Frequency Table A1Cresult vs glucose serum
table(Predict_a1c, Test[,13]) # Frequency Table A1Cresult vs gender
plot(Predict_a1c, Test[,6],main="A1CResult vs metformin",las=1,xlab="A1CResult",ylab="metformin") # Plotting A1Cresult vs metformin
plot(Predict_a1c, Test[,7],main="A1CResult vs insulin",las=1,xlab="A1CResult",ylab="insulin") # Plotting A1Cresult vs insulin
table(Predict_a1c, Test[,8]) # Plotting A1Cresult vs Change of meds

#Applying K MEANS algorithm and classifyng data in clusters.
?kmeans()
race2 <- ifelse(Dataset2$race=='Caucasian',1,
                     ifelse(Dataset2$race=='Asian',2,
                            ifelse(Dataset2$race=='AfricanAmerican',3,
                                   ifelse(Dataset2$race=='Hispanic',4,0))))

Metformin_Info2<-ifelse(Dataset2$metformin=='up',1,
                       ifelse(Dataset2$metformin=='Down',2, 
                              ifelse(Dataset2$metformin=='Steady',3,0)))

Insulin_Info2<-ifelse(Dataset2$insulin=='up',1,
                     ifelse(Dataset2$insulin=='Down',2, 
                            ifelse(Dataset2$insulin=='Steady',3,0)))

A1C_result2<-ifelse(Dataset2$A1Cresult=='Norm',1,
                      ifelse(Dataset2$A1Cresult=='>7',2, 
                             ifelse(Dataset2$A1Cresult=='>8',3,0)))

New_Dataset2 <- cbind(Race=race2
                     ,num_medications=norm(Dataset2[,15],min(Dataset2[,15]),max(Dataset2[,15]))
                     ,number_diagnoses=norm(Dataset2[,22],min(Dataset2[,22]),max(Dataset2[,22]))
                     ,metformin=Metformin_Info2
                     ,insulin=Insulin_Info2
                     ,A1C_Result=A1C_result2)
View(New_Dataset2)

temp2 <- sample(nrow(New_Dataset2),as.integer(0.70 * nrow(New_Dataset2)))
Training2 <- New_Dataset2[temp2,]
View(Training2)
Test2 <- New_Dataset2[-temp2,]
View(Test2)

summary(New_Dataset2)

race2 <- Training2[,1]
A1C_Result2 <- Training2[,6]
num_med2 <- Training2[,2]
num_diag2 <- Training2[,3]
metformin2 <- Training2[,4]
insulin2 <- Training2[,5]

Predict_clus <- kmeans(Training2[,1:6], 2) # Kmeans
Predict_clus

?plot()
plot(A1C_Result2, num_med2, type="p", col = Predict_clus$cluster)
points(Predict_clus$centers,col = 1:3, pch = 17, cex = 2)

plot(A1C_Result2, num_diag2, type="p", col = Predict_clus$cluster)
points(Predict_clus$centers,col = 1:3, pch = 17, cex = 2)

library(ggplot2)
?qplot()
qplot(num_lab_procedures, race, data=Dataset,colour=race)
qplot(race, num_medications, data=Dataset,colour=race)

