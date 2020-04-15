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


source(paste(getwd(),"aux_pls.R", sep = "/"))
source(paste(getwd(),"Load_DB.R", sep = "/"))

NPROCS = detectCores() -1 ;
maxcomp = 30 # max number of k in spls. should take all predictors as we don't have too many 
NPERM = 100; 
NITER = 1; #bagging try 1/5
N_FOLDS = 10
  
#create folders of y
folds = createFolds(Y_bucket$PHQ9_Sum_sqrt, k = N_FOLDS)

# create a parallel socket clusters
cl <- makeCluster(NPROCS, type="FORK")
registerDoParallel(cl)

#make sure the data and covars are align
x= merge(PNC_cognitive, PNC_data_amelia)
x= merge(x,Y_bucket)


cv = foreach(fold_index = seq(length(folds))) %do%        
  do_crossvalidate_spls_covars_perm_par(fold_index, list(X = x[,2:27], y = x$PHQ9_Sum_sqrt, covars = x[,28:34], 
                                                           folds = folds
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


results$cor.test$perm = apply(results$y.pred.perm, 2, function(x) cor.test(x, results$y.test)$estimate)
results$cor.test$estimate = results$cor.test$perm[1] # this is the unpermutated prediction
results$cor.test$p.value = sum(results$cor.test$perm[1] <= results$cor.test$perm)/length(results$cor.test$perm)










  