library("readr")
library("psych")
library("car")
library("rcompanion")
library("Amelia")

#read the CSVs 
PHQ_Data = read_csv("PHQ9_file_for_Elina_with_PNC_FH.csv")
PNC_Core_Data_demographics = read_csv("PNC_Core_Data_demographics.csv")
PNC_diagnoses = read_csv("PNC_diagnoses.csv")

PNC_Core_Data_cognitive = read_csv("PNC_Core_Data_cognitive.csv")
PNC_Core_Data_cognitive_ALTERNATIVE = read_csv("PNC_Core_Data_cognitive_ALTERNATIVE.csv")



###Y_bucket
Y_bucket = PHQ_Data[,c("bblid", "Depression_mod_above_at_phq", "PHQ9_Sum")]
summary(Y_bucket)
#remove empty rows
Y_bucket = Y_bucket[!is.na(Y_bucket$Depression_mod_above_at_phq) & !is.na(Y_bucket$PHQ9_Sum),]

#the PHQ9_Sum is skewed
plotNormalHistogram(Y_bucket$PHQ9_Sum)

#check log as the transformation
plotNormalHistogram(log(Y_bucket$PHQ9_Sum+1), main= "log")
qqnorm(log(Y_bucket$PHQ9_Sum+1),main = "log")
qqline(log(Y_bucket$PHQ9_Sum+1),col="red")

#check sqrt as the transformation
plotNormalHistogram(sqrt(Y_bucket$PHQ9_Sum), main = "sqrt")
qqnorm(sqrt(Y_bucket$PHQ9_Sum),main = "sqrt")
qqline(sqrt(Y_bucket$PHQ9_Sum),col="red")

#check Yeo-Johnson (Box-Cox) as the transformation
lambda = powerTransform(Y_bucket$PHQ9_Sum ~1, family = "yjPower")$lambda
plotNormalHistogram(yjPower(Y_bucket$PHQ9_Sum,lambda), main = "powerTransform + yjPower" )
qqnorm(yjPower(Y_bucket$PHQ9_Sum, lambda),main="powerTransform + yjPower")
qqline(yjPower(Y_bucket$PHQ9_Sum, lambda),col="red")

#sqrt had the best results
Y_bucket$PHQ9_Sum_sqrt = sqrt(Y_bucket$PHQ9_Sum)


###Data
PNC_data = merge(PHQ_Data[,c("bblid", "goassessPhqDurMonths")], PNC_Core_Data_demographics[,c(1,2,4,6:7,11)])
PNC_data = merge(Y_bucket[,c("bblid")], PNC_data)
PNC_data = merge(PNC_data, PNC_diagnoses[,c("bblid", "smry_dep")])
summary(PNC_data)

#missing cnb age will be the goassess1 age
PNC_data$ageAtCnb1[is.na(PNC_data$ageAtCnb1)] = PNC_data$ageAtClinicalAssess1[is.na(PNC_data$ageAtCnb1)]
#get mean of age at goassess1 and cnb (we don't need both)
PNC_data$age = rowMeans(PNC_data[,c("ageAtClinicalAssess1","ageAtCnb1")])
#remove the other age features
PNC_data = subset(PNC_data, select=-c(ageAtClinicalAssess1,ageAtCnb1))

#create 2 variables for race2 
PNC_data$race2_White = ifelse(PNC_data$race2 == 1 , 1, 0)
PNC_data$race2_Black = ifelse(PNC_data$race2 == 2 , 1, 0)
#remove race2
PNC_data = subset(PNC_data, select=-c(race2))

#change sex range from [1,2] to [0,1]
PNC_data$sex =  PNC_data$sex -1

summary(PNC_data)

#imputation 
amelia_fit <- amelia(PNC_data,m=1, idvars=c("bblid"))
summary(amelia_fit)
PNC_data_amelia = amelia_fit$imputations[[1]]
summary(PNC_data_amelia)


#cognitive
PNC_cognitive = merge(Y_bucket[,c("bblid")], PNC_Core_Data_cognitive)
summary(PNC_cognitive)

#remove empty rows
PNC_cognitive = PNC_cognitive[ !(rowSums(is.na(PNC_cognitive)) >= 26),]

#check in raw data if there is more bblid that need to be removed 
PNC_cognitive=merge(PNC_cognitive, PNC_Core_Data_cognitive_ALTERNATIVE[,c("bblid","battery_valid_collapsed")])
table(PNC_cognitive$battery_valid_collapsed) #N=9
PNC_cognitive = PNC_cognitive[PNC_cognitive$battery_valid_collapsed != "N",]
PNC_cognitive = PNC_cognitive[,! names(PNC_cognitive) %in% c("battery_valid_collapsed")]

#remove the bblids also from the data and Y
Y_bucket = merge(Y_bucket, PNC_cognitive[,1, drop=FALSE])
PNC_data = merge(PNC_data, PNC_cognitive[,1, drop=FALSE])
PNC_data_amelia = merge(PNC_data_amelia, PNC_cognitive[,1, drop=FALSE])


# #calculate cor 
# apply(PNC_cognitive[,-1], 2, cor.test, Y_bucket$Depression_mod_above_at_phq)
# apply(PNC_cognitive[,-1], 2, cor.test, Y_bucket$PHQ9_Sum)
# 
# #t.test
# indexes_1= which(Y_bucket$Depression_mod_above_at_phq==1)
# cog_1 = merge(Y_bucket[indexes,1,drop=FALSE],PNC_cognitive)
# cog_0 = merge(Y_bucket[-indexes,1,drop=FALSE],PNC_cognitive)
# 
# for (i in 2:length(colnames(PNC_cognitive))) {
#   print(colnames(PNC_cognitive)[i])
#   print(t.test(cog_0[,i],cog_1[,i]))
# }
# 
# 
# #check cor between the 2 depression variables
# polychoric(data.frame(Y_bucket$Depression_mod_above_at_phq, PNC_data$smry_dep))$rho #0.141

