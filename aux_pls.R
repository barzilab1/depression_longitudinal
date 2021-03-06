
###############################################
# generate permutations taking into account group structure
# for permutation testing. group can be brothers 
###############################################
generate_permutation = function(subject, group){
  
  if (is.null(group)){
    mysample = sample(seq(length(subject)), replace = F)
  } else {
    mysample = rep(0, length(subject))
    
    group_size = table(subject)
    unique_subjects = as.numeric(names(group_size))
    
    for (g in unique(group_size)){
      # shuffle around groups of size g
      mysubjects = unique_subjects[group_size == g]
      myshuffle = sample(mysubjects, replace = F)
      
      if (g == 1) {
        indices = sapply(mysubjects, function(x) which(x == subject))
        new.indices = sapply(myshuffle, function(x) which(x == subject))
        mysample[indices] = new.indices
      } else {
        for (j in seq(length(mysubjects))){
          sub = mysubjects[j]
          newsub = myshuffle[j]
          indices = which(sub == subject)
          new.indices = which(newsub == subject)
          mysample[indices] = sample(new.indices, replace = F)
        }
      }
      #  print(mysample)
    }
  }
  
  return(mysample)
}

doperm = function(perm, y.train, X.train, X.test, maxcomp, subject, group, filterthr, nonzero, NITER, eta.steps = seq(0.1, 0.9, 0.1)){
  
  K.iter = eta.iter = coefs.iter = NULL
  
  set.seed(perm) # to keep iterations consistent across folds
  
  # permutate training data
  if (perm == 1) mysample = seq(length(y.train)) else mysample = sample(seq(length(y.train)), replace = F)

  y.pred.iter = y.train.pred.iter = nfeatures.iter = NULL    
  
  filtered = apply(X.train[, nonzero], 2, function(x) cor.test(y.train[mysample], x)$p.value < filterthr)
  K.steps = seq(1, min(maxcomp, sum(filtered)))
  
  #  browser() - finds optimal k and eta 
  mycv <- cv.spls( X.train[, nonzero][, filtered], y.train[mysample], eta = eta.steps, K = K.steps, plot.it = F, scale.x = F )
  
  for (iter in seq(NITER)){
    
    print(paste("Permutation", perm, "Iteration", iter))
    
    #bootstrap
    mysample.b = sample(seq(length(y.train)), replace = TRUE)
    X.train.sample = X.train[mysample.b, nonzero]
    y.train.sample = y.train[mysample][mysample.b]
    
    nonzero.b = apply(X.train.sample, 2, sd, na.rm= T) > 0
    X.train.sample = X.train.sample[, nonzero.b]
    
    # feature selection
    filtered = apply(X.train.sample, 2, function(x) cor.test(y.train.sample, x)$p.value < filterthr)
    
    if(sum(filtered) < 3) {
      filtered = nonzero[nonzero]
    }
    
    K.opt = min(mycv$K.opt, sum(filtered))
    mypls <- spls( X.train.sample[, filtered], 
                   y.train.sample, 
                   eta = mycv$eta.opt, 
                   K = K.opt, scale.x = F  )
    
    y.pred.iter = cbind(y.pred.iter, predict(mypls, X.test[, nonzero][, nonzero.b][, filtered]))
    y.train.pred.iter = cbind(y.train.pred.iter, predict(mypls, X.train.sample[, filtered]))
    
    nfeatures.iter[iter] = sum(filtered)
    
    # only keep these for the unpermuted model
    K.iter = c(K.iter, K.opt)
    eta.iter = c(eta.iter, mycv$eta.opt)
    coefs.full = 0*nonzero #TODO check that! should be the number of predictors
    coefs.full[nonzero][nonzero.b][filtered] = coef.spls(mypls)
    coefs.iter = cbind(coefs.iter, coefs.full)
    
  } # iter
  
  #average results over iterations
  y.pred.perm = rowMeans(y.pred.iter)
  y.train.pred.perm = rowMeans(y.train.pred.iter)
  nfeatures.perm = mean(nfeatures.iter)
  
  return(list(
    y.pred.perm = y.pred.perm, 
    y.train.pred.perm = y.train.pred.perm,
    nfeatures.perm = nfeatures.perm, 
    mysample = mysample, 
    coefs.iter = coefs.iter,
    K.iter = K.iter,
    eta.iter = eta.iter)
  )
}

do_crossvalidate_spls_covars_perm_par = function(fold_index, input, maxcomp, cluster, 
                                                 subject = NULL, 
                                                 group = NULL, 
                                                 NITER = 20, 
                                                 NPERM = 100, 
                                                 filterthr = 1){ #filterthr used to reduce the number of predictore togther with nonzero 
  
  #browser()
  # selecting all features by default  
  X = input$X
  y = input$y
  
  covars = input$covars
  fold = input$folds[[fold_index]]
  
  # to generate permutations
  subject = input$subject[-fold]
  if (!is.null(group)) group = input$group[-fold]
  
  #when there are a lot of predictors, reduce the number
  nonzero = apply(X, 2, sd, na.rm= T) > 0
  
  print(paste("Fold index", fold_index))
  
  X.train = as.matrix(X[-fold, , drop=FALSE])
  X.test = as.matrix(X[fold, , drop=FALSE])
  
  y.train = y[-fold]  
  y.test = y.test.orig = y[fold]
  
  #imput missing data in X
  X.train = missForest(X.train, variablewise = TRUE)$ximp
  X.test = missForest(X.test, variablewise = TRUE)$ximp
  
  # demean and scale only according to the train data
  mu = colMeans(X.train) 
  sigma = apply(X.train, 2, sd)
  
  # TODO: What about binary features? - check when we will have
  # scale only according to the train data and not test
  for (j in seq(ncol(X.train))){
    X.train[, j] = (X.train[, j] - mu[j])/sigma[j]     
    X.test[, j] = (X.test[, j] - mu[j])/sigma[j]    
  }
  
  
  # deconfound
  myform = as.formula(paste("y.train ~ 1 + ", paste(colnames(covars), collapse=" + ")))  
  
  covars.train = as.data.frame(covars[-fold, , drop=FALSE])
  covars.test = as.data.frame(covars[fold, , drop=FALSE])
  
  #remove the info/variance of the cov features from Y
  mycovarmod = lm(myform, data = covars.train)
  y.train = y.train - predict(mycovarmod, covars.train)
  y.test = y.test.orig - predict(mycovarmod, covars.test)
  
  cat("\nNA in train", which(is.na(X.train)))
  cat("NA in test", which(is.na(X.test)))
  cat("NA in Y train", which(is.na(y.train)))
  cat("NA in Y test\n", which(is.na(y.test)))
  
  # run permutations
  results_list = foreach(perm = seq(NPERM), .packages=c('spls'), 
                         .export=c('doperm')) %dopar% 
    doperm(perm, y.train, X.train, X.test, maxcomp, subject, group, filterthr, nonzero, NITER)
  
  y.pred.perm = sapply(results_list, function(x) x$y.pred.perm)
  y.train.pred.perm = sapply(results_list, function(x) x$y.train.pred.perm)
  nfeatures.perm = sapply(results_list, function(x) x$nfeatures.perm)
  
  mysample.perm = sapply(results_list, function(x) x$mysample)
  K.iter = results_list[[1]]$K.iter
  eta.iter = results_list[[1]]$eta.iter
  y.pred = y.pred.perm[, 1]
  y.train.pred = y.train.pred.perm[, 1] 
  coefs.perm = sapply(results_list, function(x) rowMeans(x$coefs.iter))
  coefs = coefs.perm[, 1]
  preprocessing = list(mycovarmod = mycovarmod, 
                       mu = mu,
                       sigma = sigma)
  
  return(list(fold = fold,
              y.pred = y.pred,
              mysample.perm = mysample.perm,
              y.pred.perm = y.pred.perm,
              y.train = y.train, 
              y.train.pred = y.train.pred, 
              y.test = y.test, 
              y.test.orig = y.test.orig,
              nfeatures.perm = nfeatures.perm,
              eta.iter = eta.iter, 
              K.iter = K.iter,
              preprocessing = preprocessing,
              coefs.perm = coefs.perm,
              coefs = coefs
              )
  )
}


