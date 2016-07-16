# ---- init ----

# clear envirionment

rm(list = ls())

# load setup variables

source("config.R")

# set randomization

set.seed(SEED)

# load library management system

library(checkpoint)

if (CHECKPOINT.QUICK.LOAD) # approx. x10 faster checkpoint library loading
{
    # assume https
    options(checkpoint.mranUrl = CHECKPOINT.MRAN.URL)
    # disable url checking
    assignInNamespace("is.404", function(mran, warn = TRUE) { FALSE },
                      "checkpoint")
}

checkpoint(CHECKPOINT.QUICK.LOAD, verbose = TRUE, scanForPackages = TRUE)

# load logging system

library(futile.logger)

flog.threshold(LOGGER.LEVEL)

# load libraries

library(sets)
library(plyr)
library(dplyr)
library(ttutils)
library(lazyeval)
library(R.utils)
# caret - core
library(caret)
library(e1071)
# caret - classifiers
library(RWeka) # OneR
library(rJava)
library(kernlab) # SVM
library(C50) # C5.0
library(nnet) # neural network
# caret - feature selection
library(randomForest)
library(ipred)
library(rpart)
# imputation
library(mlr) # median & mode
library(missForest) # random forest
library(mice) # chained equations
# optimization
library(optimx)

# load helper functions

source("utils.R")

# perform additional custom init

if (file.exists(USER.INIT.FILE))
{
    source(USER.INIT.FILE)
}
