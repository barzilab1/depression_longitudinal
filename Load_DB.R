library(readr)
library(Amelia)

#read the CSVs 
PHQ_Data = read_csv("Data/PHQ9_file_for_Elina_with_PNC_FH.csv")
PNC_Core_Data_demographics = read_csv("Data/PNC_Core_Data_demographics.csv")
PNC_diagnoses = read_csv("Data/PNC_diagnoses.csv")
# PNC_SES_factor_score <- read_csv("Data/PNC_SES_factor_score.csv")


PNC_Core_Data_cognitive = read_csv("Data/PNC_Core_Data_cognitive.csv")
PNC_Core_Data_cognitive_ALTERNATIVE = read_csv("Data/PNC_Core_Data_cognitive_ALTERNATIVE.csv")


##########################################
# Y_bucket
##########################################

Y_bucket = PHQ_Data[,c("bblid", "Depression_mod_above_at_phq", "PHQ9_Sum")]
summary(Y_bucket)
#remove empty rows
Y_bucket = Y_bucket[!is.na(Y_bucket$Depression_mod_above_at_phq) & !is.na(Y_bucket$PHQ9_Sum),]

#the PHQ9_Sum is skewed
# plotNormalHistogram(Y_bucket$PHQ9_Sum)

# #check log as the transformation
# plotNormalHistogram(log(Y_bucket$PHQ9_Sum+1), main= "log")
# qqnorm(log(Y_bucket$PHQ9_Sum+1),main = "log")
# qqline(log(Y_bucket$PHQ9_Sum+1),col="red")
# 
# #check sqrt as the transformation
# plotNormalHistogram(sqrt(Y_bucket$PHQ9_Sum), main = "sqrt")
# qqnorm(sqrt(Y_bucket$PHQ9_Sum),main = "sqrt")
# qqline(sqrt(Y_bucket$PHQ9_Sum),col="red")
# 
# #check Yeo-Johnson (Box-Cox) as the transformation
# lambda = powerTransform(Y_bucket$PHQ9_Sum ~1, family = "yjPower")$lambda
# plotNormalHistogram(yjPower(Y_bucket$PHQ9_Sum,lambda), main = "powerTransform + yjPower" )
# qqnorm(yjPower(Y_bucket$PHQ9_Sum, lambda),main="powerTransform + yjPower")
# qqline(yjPower(Y_bucket$PHQ9_Sum, lambda),col="red")

#sqrt had the best results
Y_bucket$PHQ9_Sum_sqrt = sqrt(Y_bucket$PHQ9_Sum)
# plotNormalHistogram(Y_bucket$PHQ9_Sum_sqrt)

##########################################
# Covariates
##########################################

PNC_cov = merge(PHQ_Data[,c("bblid", "goassessPhqDurMonths")], PNC_Core_Data_demographics[,c(1,2,4,6:7,11)])
PNC_cov = merge(Y_bucket[,c("bblid")], PNC_cov)
PNC_cov = merge(PNC_cov, PNC_diagnoses[,c("bblid", "smry_dep")])
# PNC_cov = merge(PNC_cov, PNC_SES_factor_score)
summary(PNC_cov)

#missing cnb age will be the goassess1 age
PNC_cov$ageAtCnb1[is.na(PNC_cov$ageAtCnb1)] = PNC_cov$ageAtClinicalAssess1[is.na(PNC_cov$ageAtCnb1)]
#get mean of age at goassess1 and cnb (we don't need both)
PNC_cov$age = rowMeans(PNC_cov[,c("ageAtClinicalAssess1","ageAtCnb1")])
#remove the other age features
PNC_cov = subset(PNC_cov, select=-c(ageAtClinicalAssess1,ageAtCnb1))

#create 2 variables for race2 
PNC_cov$race2_White = ifelse(PNC_cov$race2 == 1 , 1, 0)
PNC_cov$race2_Black = ifelse(PNC_cov$race2 == 2 , 1, 0)
#remove race2
PNC_cov = subset(PNC_cov, select=-c(race2))

#change sex range from [1,2] to [0,1]
PNC_cov$sex =  PNC_cov$sex -1

summary(PNC_cov)

#imputation 
# set.seed(124)
amelia_fit <- amelia(PNC_cov,m=1, idvars=c("bblid"))
summary(amelia_fit)
PNC_cov_amelia = amelia_fit$imputations[[1]]
summary(PNC_cov_amelia)


##########################################
# cognitive
##########################################
#either this section or cognitive raw 

# PNC_cognitive = merge(Y_bucket[,c("bblid")], PNC_Core_Data_cognitive)
# summary(PNC_cognitive)
# 
# #remove empty rows
# PNC_cognitive = PNC_cognitive[ !(rowSums(is.na(PNC_cognitive)) >= 26),]
# 
# #check in raw data if there is more bblid that need to be removed 
# PNC_cognitive=merge(PNC_cognitive, PNC_Core_Data_cognitive_ALTERNATIVE[,c("bblid","battery_valid_collapsed")])
# table(PNC_cognitive$battery_valid_collapsed) #N=9
# PNC_cognitive = PNC_cognitive[PNC_cognitive$battery_valid_collapsed != "N",]
# PNC_cognitive = PNC_cognitive[,! names(PNC_cognitive) %in% c("battery_valid_collapsed")]
# 
# #remove the bblids also from the data and Y
# Y_bucket = merge(Y_bucket, PNC_cognitive[,1, drop=FALSE])
# PNC_cov = merge(PNC_cov, PNC_cognitive[,1, drop=FALSE])
# PNC_cov_amelia = merge(PNC_cov_amelia, PNC_cognitive[,1, drop=FALSE])


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


##########################################
#cognitive raw
##########################################

PNC_cognitive_raw = merge(Y_bucket[,c("bblid")], PNC_Core_Data_cognitive_ALTERNATIVE)

#remove bblid not valid 
table(PNC_cognitive_raw$battery_valid_collapsed) #N=9
PNC_cognitive_raw = PNC_cognitive_raw[PNC_cognitive_raw$battery_valid_collapsed != "N",]

#go over validation codes and remove not valid data
#adt (AGE DIFFERENTIATION)
indexes = which(PNC_cognitive_raw$adt_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,6:23] = NA

#cpf (FACE MEMORY)
indexes = which(PNC_cognitive_raw$cpf_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,26:40] = NA

#er40 (EMOTION RECOGNITION)
indexes = which(PNC_cognitive_raw$er40_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,43:59] = NA

#cpw (WORD/VERBAL MEMORY)
indexes = which(PNC_cognitive_raw$cpw_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,62:76] = NA

#pvrt (LANGUAGE REASONING)
indexes = which(PNC_cognitive_raw$pvrt_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,79:83] = NA

#medf (EMOTION DISCRIMINATION)
indexes = which(PNC_cognitive_raw$medf_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,86:113] = NA

#mpraxis (Motor Praxis)
indexes = which(PNC_cognitive_raw$mpraxis_validcode_collapsed == "N") #0

#pmat (NONVERBAL REASONING)
indexes = which(PNC_cognitive_raw$pmat_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,120:124] = NA

#tap (MOTOR SPEED)
indexes = which(PNC_cognitive_raw$tap_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,127:135] = NA

#volt (SPATIAL MEMORY)
indexes = which(PNC_cognitive_raw$volt_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,138:153] = NA

#lnb (WORKING MEMORY)
indexes = which(PNC_cognitive_raw$lnb_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,156:169] = NA

#pcet (ABSTRACTION & MENTAL FLEXIBILITY)
indexes = which(PNC_cognitive_raw$pcet_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,172:181] = NA

#pcpt (ATTENTION)
indexes = which(PNC_cognitive_raw$pcpt_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,184:206] = NA

#plot (SPATIAL ABILITY)
indexes = which(PNC_cognitive_raw$plot_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,209:214] = NA

#wrat (IQ estimate)
indexes = which(PNC_cognitive_raw$wrat_validcode_collapsed == "N")
PNC_cognitive_raw[indexes,217:220] = NA

#remove last tests
PNC_cognitive_raw = PNC_cognitive_raw[,-c(221:274)]

PNC_cognitive_raw = PNC_cognitive_raw[,! names(PNC_cognitive_raw) %in% c("battery_valid_collapsed", "datasetid", 
                                                                         "adt_genus", "adt_validcode_collapsed",
                                                                         "cpf_genus", "cpf_validcode_collapsed",
                                                                         "er40_genus", "er40_validcode_collapsed",
                                                                         "cpw_genus", "cpw_validcode_collapsed",
                                                                         "pvrt_genus", "pvrt_validcode_collapsed",
                                                                         "medf_genus", "medf_validcode_collapsed",
                                                                         "mpraxis_genus", "mpraxis_validcode_collapsed",
                                                                         "pmat_genus", "pmat_validcode_collapsed",
                                                                         "tap_genus", "tap_validcode_collapsed",
                                                                         "volt_genus", "volt_validcode_collapsed",
                                                                         "lnb_genus", "lnb_validcode_collapsed",
                                                                         "pcet_genus", "pcet_validcode_collapsed",
                                                                         "pcpt_genus", "pcpt_validcode_collapsed",
                                                                         "plot_genus", "plot_validcode_collapsed",
                                                                         "wrat_genus", "wrat_validcode_collapsed")]


# remove plot_pc (all NA)
#remove "wrat_cr_letter" - all 15 except for 2 NA and one value of 14
PNC_cognitive_raw = PNC_cognitive_raw[,! names(PNC_cognitive_raw) %in% c("plot_pc", "wrat_cr_letter")]


# write.csv(describe(PNC_cognitive_raw), "summary_PNC_cognitive_raw.csv")
# write.csv(cor_auto(PNC_cognitive_raw) ,file = "cor_cognitive_raw1.csv")

#remove column with more than 10% NA
which(colSums(is.na(PNC_cognitive_raw))>= 92) #7
PNC_cognitive_raw = PNC_cognitive_raw[,-c(which(colSums(is.na(PNC_cognitive_raw))>= 92))]

#remove rows with more than 80% NA
which(rowSums(is.na(PNC_cognitive_raw))>=144) #3
PNC_cognitive_raw = PNC_cognitive_raw[-c(which(rowSums(is.na(PNC_cognitive_raw))>=144)),]

#NA% < 3%
sum(is.na(PNC_cognitive_raw))/(ncol(PNC_cognitive_raw)*nrow(PNC_cognitive_raw)) 


names_cog_raw = names(PNC_cognitive_raw)
domains_names = list()

domains_names$memory = names_cog_raw[grepl(pattern = "^(cpf|cpw|volt)",names_cog_raw, ignore.case = T)]
domains_names$social_cognition = names_cog_raw[grep(pattern = "^(adt|er40|medf)",names_cog_raw, ignore.case = T)]
domains_names$executive = names_cog_raw[grep(pattern = "^(lnb|pcpt)",names_cog_raw, ignore.case = T)]
domains_names$complex_cognition = names_cog_raw[grep(pattern = "^(pvrt|pmat|pcet|plot)",names_cog_raw, ignore.case = T)]
domains_names$motor = names_cog_raw[grep(pattern = "^(mpraxis|tap)",names_cog_raw, ignore.case = T)]
domains_names$iq = names_cog_raw[grep(pattern = "^(wrat)",names_cog_raw, ignore.case = T)]
