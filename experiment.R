# OVERVIEW:
#
# Classifiers:
#   K1 - logistic regression
#   K2 - decision tree
#   K3 - SVM
# 
# Imputation:
#   I1 - median
#   I2 - kNN
#   I3 - SVD
# 
# Agregation operators:
#   A1 - ...
# 
# Datasets:
#   D1 - 
#   D2 - 
#   D3 - 
#     
# Procedure for each Di:
#   1. Divide Di into Di^1 and Di^2
#   2. Learn classifiers Kj on Di^1 (10-CV)
#   3. Du^2 := randomly obscured Di^2 with data loss from 0% to 50%
#   4. Calculate accuracy, sensitivity, specificity and decisiveness for
#      classifiers Kj on Di^2
#   5. Choose the best impuation on Di^2
#   6. Choose the best aggregation operator on Di^2
#      6.1 Use optimx to calculate intervals of Kj
#   7. Compare Kj with the best imputation and agregation operator
#

# ---- init ----

rm(list=ls())

SEED = 1337

set.seed(SEED)

library(checkpoint)
checkpoint("2016-04-01", verbose=TRUE)

library(futile.logger)
library(dplyr)
library(lazyeval)
library(caret)
library(e1071) # caret dependency
library(klaR) # Naive Bayes

LOGGER_LEVEL = futile.logger::INFO
flog.threshold(LOGGER_LEVEL)

# ---- step-1-divide-data ----

flog.info("Step 1: divide data")

datasets.names = c("bank-marketing", "magic", "wine-quality")
datasets.class.size = 300

for (dataset.name in datasets.names)
{
    flog.info(paste("Dataset:", dataset.name))
    
    set.seed(SEED)
    dataset = readRDS(file.path("datasets", paste0(dataset.name, ".rds")))
    
    dataset.levels = levels(dataset[, ncol(dataset)])
    dataset.classname = tail(colnames(dataset), 1)
    
    dataset.1 = dataset %>% filter_(interp(quote(a==b), 
                                           a=as.name(dataset.classname), 
                                           b=dataset.levels[1]))
    dataset.2 = dataset %>% filter_(interp(quote(a==b), 
                                           a=as.name(dataset.classname), 
                                           b=dataset.levels[2]))
    
    dataset.1.sample = dataset.1 %>% sample_n(datasets.class.size)
    dataset.2.sample = dataset.2 %>% sample_n(datasets.class.size)
    
    dataset.sample = rbind(dataset.1.sample, dataset.2.sample)
    dataset.sample = dataset.sample[sample(nrow(dataset.sample)),]
    
    dataset.full.sample = dataset.sample[1:datasets.class.size, ]
    dataset.obscured.sample = 
        dataset.sample[(datasets.class.size+1):nrow(dataset.sample), ]
    
    saveRDS(dataset.full.sample, 
            file.path("datasets", paste0(dataset.name, "-full-sample.rds")))
    saveRDS(dataset.obscured.sample, 
            file.path("datasets", paste0(dataset.name, "-obscured-sample.rds")))
}


# ---- step-2-learn-classifiers ----

flog.info("Step 2: learn classifiers")

# https://topepo.github.io/caret/bytag.html
# https://topepo.github.io/caret/modelList.html

CV_FOLDS                  = 10
CV_PREPROCESSING_METHODS  = c("center", "scale")
CV_PERFORMANCE_SELECTOR   = "Accuracy"
CV_PERFORMANCE_MAXIMIZE   = TRUE

classifiers.list = list(
    
    glm = NULL,
    
    nb = expand.grid(.fL=c(0), 
                     .usekernel=c(FALSE, TRUE))
)

nestedCrossValidation = function(dataset, no.folds, model.name, model.grid)
{
    set.seed(SEED)
    
    flog.info(paste("Classifier:", model.name))
    
    colnames(dataset)[ncol(dataset)] = "Class"
    
    preproc.scheme = preProcess(dataset, 
                                method=CV_PREPROCESSING_METHODS)
    dataset = predict(preproc.scheme, dataset)
    
    idx.outer = createFolds(dataset$Class, 
                            k=no.folds)
    
    folds.performance = data.frame()
    
    for (i in 1:no.folds)
    {
        flog.debug(paste("Fold", i))
        
        folds.inner = idx.outer[setdiff(1:no.folds, i)]
        dataset.inner = dataset[as.numeric(unlist(folds.inner)), ]
        
        idx.inner = createFolds(dataset.inner$Class, 
                                k=no.folds)
        
        train.control = trainControl(method="cv",
                                     index=idx.inner)
        
        # TODO: suppress warnings
        model = train(Class ~ ., 
                      data=dataset.inner, 
                      trControl=train.control, 
                      method=model.name, 
                      tuneGrid=model.grid,
                      metric=CV_PERFORMANCE_SELECTOR,
                      maximize=CV_PERFORMANCE_MAXIMIZE)
        
        folds.holdout = idx.outer[[i]]
        dataset.holdout = dataset[folds.holdout, ]
        
        predictions = predict(model, dataset.holdout)
        cf.matrix = confusionMatrix(predictions, dataset.holdout$Class)
        
        folds.performance = rbind(folds.performance,
                                  data.frame(t(cf.matrix$overall)))
    }
    
    flog.info("Training final model")
    
    train.control = trainControl(method="cv",
                                 index=idx.outer)
    
    # TODO: suppress warnings
    model = train(Class ~ ., 
                  data=dataset, 
                  trControl=train.control, 
                  method=model.name, 
                  tuneGrid=model.grid,
                  metric=CV_PERFORMANCE_SELECTOR,
                  maximize=CV_PERFORMANCE_MAXIMIZE)
    
    flog.info(paste0("Estimated performance of ", class(model$finalModel)[1],
                     " - ", CV_PERFORMANCE_SELECTOR, ": ", 
                     round(mean(folds.performance$Accuracy), 3)))
    
    return(list(model=model, folds.performance=folds.performance))
}

for (dataset.name in datasets.names)
{
    flog.info(paste("Dataset:", dataset.name))
    
    dataset = readRDS(file.path("datasets", paste0(dataset.name, "-full-sample.rds")))
    
    for (model.name in names(classifiers.list))
    {
        model.grid = classifiers.list[[model.name]]
        
        result.list = nestedCrossValidation(dataset, CV_FOLDS,
                                            model.name, model.grid)
        
        #model.file.name  = gsub("\\*", model.name, CV_CLASSIFIER_MODEL_FILE)
        
        #saveRDS(result.list$model, 
        #        paste0(classifiers.dir, model.file.name))
    }
    
    flog.info("*****")
}


# ---- step-3-obsucre-dataset ----

flog.info("Step 3: obscure dataset")



# ---- step-4-calculate-classifiers-performance ----

flog.info("Step 4: calculate classifiers performance")



# ---- step-5-choose-best-imputation ----

flog.info("Step 5: choose best imputation")



# ---- step-6-choose-best-aggregation ----

flog.info("Step 6: choose best aggregation")



# ---- step-7-compare-results ----

flog.info("Step 7: compare results")
 
