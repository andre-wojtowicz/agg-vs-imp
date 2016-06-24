# OVERVIEW:
#
# Classifiers:
#   K1 - SVM
#   K2 - C5.0
#   K3 - kNN
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

rm(list = ls())

SEED = 1337
set.seed(SEED)

library(checkpoint)

CHECKPOINT_QUICK_LOAD = TRUE

if (CHECKPOINT_QUICK_LOAD) # approx. x10 faster checkpoint library loading
{
    options(checkpoint.mranUrl = "https://mran.microsoft.com/") # assume https
    assignInNamespace("is.404", function(mran, warn = TRUE) { FALSE },
                      "checkpoint") # disable url checking
}

checkpoint("2016-04-01", verbose = TRUE, scanForPackages = TRUE)

library(futile.logger)
library(sets)
library(plyr)
library(dplyr)
library(ttutils)
library(lazyeval)
# caret - core
library(caret)
library(e1071)
# caret - classifiers
library(kernlab) # SVM
library(C50) # C5.0
# caret - feature selection
library(randomForest)
library(ipred)
library(rpart)


LOGGER_LEVEL = futile.logger::INFO
flog.threshold(LOGGER_LEVEL)

# ---- step-1-divide-data ----

flog.info("Step 1: divide data")

datasets.names = c("bank-marketing", "magic", "wine-quality")
datasets.size.feature.selection = 150
datasets.size.classification    = 300
datasets.size.obscuration       = 300

datasets.size.per.class = (datasets.size.feature.selection +
                           datasets.size.classification +
                           datasets.size.obscuration) / 2

for (dataset.name in datasets.names)
{
    flog.info(paste("Dataset:", dataset.name))

    set.seed(SEED)
    dataset = readRDS(file.path("datasets", paste0(dataset.name, ".rds")))

    dataset.levels = levels(dataset[, ncol(dataset)])
    dataset.classname = tail(colnames(dataset), 1)

    dataset.class.1 = dataset %>%
                      filter_(interp(quote(a == b),
                                     a = as.name(dataset.classname),
                                     b = dataset.levels[1])) %>%
                      sample_n(datasets.size.per.class)

    dataset.class.2 = dataset %>%
                      filter_(interp(quote(a == b),
                              a = as.name(dataset.classname),
                              b = dataset.levels[2])) %>%
                      sample_n(datasets.size.per.class)


    dataset.class.1.feature.selection = dataset.class.1 %>%
        filter(row_number() <= datasets.size.feature.selection / 2)

    dataset.class.1.classification = dataset.class.1 %>%
        filter(between(row_number(),
                       datasets.size.feature.selection / 2 + 1,
                       (datasets.size.feature.selection +
                            datasets.size.classification) / 2))

    dataset.class.1.obscuration = dataset.class.1 %>%
        filter(row_number() >= (datasets.size.feature.selection +
                                datasets.size.classification) / 2 + 1)


    dataset.class.2.feature.selection = dataset.class.2 %>%
        filter(row_number() <= datasets.size.feature.selection / 2)

    dataset.class.2.classification = dataset.class.2 %>%
        filter(between(row_number(),
                       datasets.size.feature.selection / 2 + 1,
                       (datasets.size.feature.selection +
                            datasets.size.classification) / 2))

    dataset.class.2.obscuration = dataset.class.2 %>%
        filter(row_number() >= (datasets.size.feature.selection +
                                    datasets.size.classification) / 2 + 1)


    saveRDS(rbind(dataset.class.1.feature.selection, dataset.class.2.feature.selection),
            file.path("datasets", paste0(dataset.name, "-feature-selection.rds")))
    saveRDS(rbind(dataset.class.1.classification, dataset.class.2.classification),
            file.path("datasets", paste0(dataset.name, "-classification.rds")))
    saveRDS(rbind(dataset.class.1.obscuration, dataset.class.2.obscuration),
            file.path("datasets", paste0(dataset.name, "-obscuration.rds")))

}

flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-2-learn-classifiers ----

flog.info("Step 2: learn classifiers")

# https://topepo.github.io/caret/bytag.html
# https://topepo.github.io/caret/modelList.html

ncv.folds                  = 10
ncv.preprocessing.methods  = c("center", "scale")
ncv.performance.selector   = "Accuracy"
ncv.performance.maximize   = TRUE

nestedCrossValidation = function(dataset, no.folds, model.name,
                                 model.grid, model.attrs)
{
    set.seed(SEED)

    colnames(dataset)[ncol(dataset)] = "Class"

    preproc.scheme = preProcess(dataset,
                                method = ncv.preprocessing.methods)
    dataset = predict(preproc.scheme, dataset)

    idx.outer = createFolds(dataset$Class,
                            k = no.folds)

    folds.performance = data.frame()

    for (i in 1:no.folds)
    {
        flog.info(paste("Fold", i))

        folds.inner = idx.outer[setdiff(1:no.folds, i)]
        dataset.inner = dataset[as.numeric(unlist(folds.inner)), ]

        idx.inner = createFolds(dataset.inner$Class,
                                k = no.folds)

        train.control = trainControl(method = "cv",
                                     index = idx.inner,
                                     allowParallel = TRUE)

        training.arguments = merge(
            list(form      = Class ~ .,
                 data      = dataset.inner,
                 trControl = train.control,
                 method    = model.name,
                 tuneGrid  = model.grid,
                 metric    = ncv.performance.selector,
                 maximize  = ncv.performance.maximize),
            model.attrs)

        model = suppressWarnings(do.call(train, training.arguments))

        folds.holdout = idx.outer[[i]]
        dataset.holdout = dataset[folds.holdout, ]

        predictions = predict(model, dataset.holdout)
        cf.matrix = confusionMatrix(predictions, dataset.holdout$Class)

        folds.performance = rbind(folds.performance,
                                  data.frame(t(cf.matrix$overall)))
    }

    flog.info("Training final model")

    train.control = trainControl(method = "cv", index = idx.outer)

    training.arguments = merge(
        list(form      = Class ~ .,
             data      = dataset,
             trControl = train.control,
             method    = model.name,
             tuneGrid  = model.grid,
             metric    = ncv.performance.selector,
             maximize  = ncv.performance.maximize),
        model.attrs)

    model = suppressWarnings(do.call(train, training.arguments))

    attr(model, "folds.performance") = folds.performance

    flog.info(paste0("Estimated performance of ", class(model$finalModel)[1],
                     " - ", ncv.performance.selector, ": ",
                     round(mean(folds.performance[[ncv.performance.selector]]), 3)))

    used.predictors = set()

    predictors.names = tryCatch(
        predictors(model),
        error = function(e) { model$coefnames })

    for (column.name in colnames(dataset)[1:(ncol(dataset) - 1)])
    {
        if (any(grepl(paste0("^", column.name), predictors.names)))
            used.predictors = used.predictors | set(column.name)
    }

    attr(model, "used.predictors") = used.predictors

    flog.info(paste("Used predictors:", length(used.predictors)))

    return(model)
}

classifiers.list = c("svmLinear",
                     "C5.0",
                     "knn")

classifiers.feature.selection.method = list(

    svmLinear = "rfFuncs",

    C5.0 = NULL, # internal

    knn = "treebagFuncs"
)

classifiers.tuning.params = list(

    svmLinear = expand.grid(C = 10 ^ seq(-5,2)),

    C5.0 = expand.grid(trials = c(1, 5, 10, 15, 20),
                       model  = c("tree", "rules"),
                       winnow = c(TRUE)),

    knn = expand.grid(k = c(1:10))
)

classifiers.basic.attributes = list(

    svmLinear = list(scaled = FALSE),

    C5.0 = NULL,

    knn = NULL
)

if (!dir.exists("models"))
{
    dir.create("models")
}

for (dataset.name in datasets.names)
{
    flog.info(paste("Dataset:", dataset.name))

    for (model.name in classifiers.list)
    {
        flog.info(paste("Classifier:", model.name))

        if (file.exists(file.path("models",
                                  paste0(dataset.name, "-", model.name, ".rds"))))
        {
            flog.info("Model exists, skipping")
            next
        }

        dataset.feature.selection =
            readRDS(file.path("datasets", paste0(dataset.name, "-feature-selection.rds")))

        dataset.classification =
            readRDS(file.path("datasets", paste0(dataset.name, "-classification.rds")))

        fs.method  = classifiers.feature.selection.method[[model.name]]

        if (!is.null(fs.method))
        {
            set.seed(SEED)

            flog.info(paste("Feature selection:", fs.method))

            fs.results =
                rfe(dataset.feature.selection[, 1:(ncol(dataset.feature.selection) - 1)],
                    dataset.feature.selection[, ncol(dataset.feature.selection)],
                    sizes = 1:ncol(dataset.feature.selection),
                    rfeControl = rfeControl(functions = eval(as.name(fs.method)),
                                            method = "cv",
                                            number = 10))

            flog.info(paste("Selected", length(predictors(fs.results)),
                            "from", ncol(dataset.feature.selection) - 1, "featrues"))

            dataset.classification =
                dataset.classification[, c(predictors(fs.results),
                                           tail(colnames(dataset.classification), 1))]

        } else {
            flog.info("Internal feature selection")
            flog.info("Enlarging classification dataset")

            dataset.classification = rbind(dataset.feature.selection,
                                           dataset.classification)
        }


        model.grid  = classifiers.tuning.params[[model.name]]
        model.attrs = classifiers.basic.attributes[[model.name]]

        model = nestedCrossValidation(dataset.classification, ncv.folds,
                                      model.name, model.grid, model.attrs)

        saveRDS(model, file.path("models",
                                 paste0(dataset.name, "-", model.name, ".rds")))

        flog.info(paste(rep("*", 10), collapse = ""))
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}

flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-3-obsucre-dataset ----

flog.info("Step 3: obscure dataset")

for (dataset.name in datasets.names)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscuration =
        readRDS(file.path("datasets", paste0(dataset.name, "-obscuration.rds")))

    dataset.used.predictors = set()

    for (model.name in classifiers.list)
    {
        model = readRDS(file.path("models",
                                  paste0(dataset.name, "-", model.name, ".rds")))

        dataset.used.predictors = dataset.used.predictors | attr(model, "used.predictors")
    }

    flog.info(paste("Used predictors:", length(dataset.used.predictors),
                    "of", ncol(dataset.obscuration) - 1))
}

flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-4-calculate-classifiers-performance ----

flog.info("Step 4: calculate classifiers performance")



flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-5-choose-best-imputation ----

flog.info("Step 5: choose best imputation")



flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-6-choose-best-aggregation ----

flog.info("Step 6: choose best aggregation")



flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-7-compare-results ----

flog.info("Step 7: compare results")



flog.info(paste(rep("*", 50), collapse = ""))
