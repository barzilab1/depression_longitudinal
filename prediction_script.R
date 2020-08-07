######## Prediction functions ##########################

library(spls)
# library(mediation)
# library(foreach)
library(doParallel)
# library(doMC)
library(caret)
# library(pracma)
# library(reshape2)
# library(dplyr)
# library(oro.nifti)
# library(RColorBrewer)
# library(scales)
# ###library(ppcor)
# library(lavaan)
# library(RVAideMemoire)
# library(psychometric)
# library(boot)
library(rlist)
# library(rjson)
# library(abind)
library(readr)
library(psych)
# library("car")
# library("rcompanion")
library(Amelia)
library(qgraph)
library(missForest)


source(paste(getwd(),"aux_pls.R", sep = "/"))


NPROCS = detectCores() ;
maxcomp = 178 # max number of k in spls. should take all predictors as we don't have too many 
NPERM = 100; 
NITER = 100; #bagging
N_FOLDS = 10

do_algo <- function(folds, x, y, covars, domain_name = NULL,i) {
  
  # create a parallel socket clusters
  cl <- makeCluster(NPROCS, type="FORK")
  registerDoParallel(cl)
  
  cv = foreach(fold_index = seq(length(folds))) %do%        
    do_crossvalidate_spls_covars_perm_par(fold_index, list(X = x, y = y, covars = covars, folds = folds
                                                           # ,subject = mydata.exp$Subject[valid.rows]
    ), 
    maxcomp = maxcomp, cluster = cl,
    NITER = NITER, NPERM = NPERM)
  
  stopCluster(cl)
  
  results = list()
  
  # extract and reorder so that it's in sync
  results$fold = unlist(lapply(cv, function(x){x$fold}))
  results$y.pred.perm = list.rbind(lapply(cv, function(x){x$y.pred.perm}))[order(results$fold), ]
  results$y.test.orig = unlist(lapply(cv, function(x){x$y.test.orig}))[order(results$fold)]
  results$y.pred = unlist(lapply(cv, function(x){x$y.pred}))[order(results$fold)]
  results$y.test = unlist(lapply(cv, function(x){x$y.test}))[order(results$fold)]
  
  y.res=(cbind(y.pred=(results[["y.pred"]]),y.test= results[["y.test"]],y.test.orig=results[["y.test.orig"]],PHQ9_Sum=x$PHQ9_Sum))
  write.csv(y.res, paste("res/y_res",i,"_",domain_name,".csv",sep = "" ) , na = "", row.names = F)
  
  results$cor.test$perm = apply(results$y.pred.perm, 2, function(x) cor.test(x, results$y.test)$estimate)
  results$cor.test$estimate = results$cor.test$perm[1] # this is the unpermutated prediction
  results$cor.test$p.value = sum(results$cor.test$perm[1] <= results$cor.test$perm)/length(results$cor.test$perm)
  
  print(results$cor.test)
  
  
  coefs.temp = list.cbind(lapply(cv, function(x){x$coefs}))
  results$coefs = data.frame(coefs.temp, mean = rowMeans(coefs.temp) ) 
  coefs.temp[coefs.temp == 0] = NA
  results$coefs = data.frame(results$coefs, meanNA = rowMeans(coefs.temp,na.rm = T))
  write.csv(results$coefs, paste("res/coefs",i,domain_name,".csv",sep = "_" ) , na = "")
  
}


for (i in 1:5){
  cat("\n####################################\n",i,"\n####################################\n")  

  source(paste(getwd(),"Load_DB.R", sep = "/"))
  
  #make sure the data and covars are align
  x= merge(PNC_cov_amelia, PNC_cognitive_raw)
  x= merge(Y_bucket,x)
  f_index = which(x$sex == 1)
  x_f = x[f_index, ! names(x) %in% c("sex")]
  x_m = x[-f_index, ! names(x) %in% c("sex")]
  
  #female
  folds_f = createFolds(x_f$PHQ9_Sum_sqrt, k = N_FOLDS)
  do_algo(folds_f, x_f[,11:188], x_f$PHQ9_Sum_sqrt, x_f[,5:10], "female", i)
  
  #male
  folds_m = createFolds(x_m$PHQ9_Sum_sqrt, k = N_FOLDS)
  do_algo(folds_m, x_m[,11:188], x_m$PHQ9_Sum_sqrt, x_m[,5:10], "male", i)
  
}

  