# OVERVIEW:
#
# Datasets:
#   D1 - bank-marketing
#   D2 - magic
#   D3 - wine-quality
#
# Classifiers:
#   K1 - SVM
#   K2 - C5.0
#   K3 - kNN
#
# Imputation:
#   I1 - median & mode
#   I2 - random forest
#   I3 - chained equations
#
# Agregation operators:
#   A1 - ...
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
library(RWeka) # OneR
library(rJava)
library(kernlab) # SVM
library(C50) # C5.0
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

LOGGER_LEVEL = futile.logger::INFO
flog.threshold(LOGGER_LEVEL)

# ---- step-1-divide-data ----

flog.info("Step 1: divide data")

datasets.names = c("bank-marketing",
                   "magic",
                   "wine-quality")
datasets.size.feature.selection = 150         # TODO: 250
datasets.size.classification    = 300         #       750
datasets.size.obscuration       = 300         #       750

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
ncv.preprocessing.methods  = c("range")
ncv.performance.selector   = "Accuracy"
ncv.performance.maximize   = TRUE

nestedCrossValidation = function(dataset, no.folds, model.name,
                                 model.grid, model.attrs)
{
    set.seed(SEED)

    colnames(dataset)[ncol(dataset)] = "Class"

    preproc.scheme = caret::preProcess(dataset,
                                       method = ncv.preprocessing.methods)
    dataset = stats::predict(preproc.scheme, dataset)


    idx.outer = caret::createFolds(dataset$Class,
                                   k = no.folds)

    folds.performance = data.frame()

    for (i in 1:no.folds)
    {
        flog.info(paste("Fold", i))

        folds.inner = idx.outer[setdiff(1:no.folds, i)]
        dataset.inner = dataset[as.numeric(unlist(folds.inner)), ]

        idx.inner = caret::createFolds(dataset.inner$Class,
                                       k = no.folds)

        train.control = caret::trainControl(method = "cv",
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

        model = suppressWarnings(do.call(caret::train, training.arguments))

        folds.holdout = idx.outer[[i]]
        dataset.holdout = dataset[folds.holdout, ]

        predictions = stats::predict(model, dataset.holdout)
        cf.matrix = caret::confusionMatrix(predictions, dataset.holdout$Class)

        folds.performance = rbind(folds.performance,
                                  data.frame(t(c(cf.matrix$overall,
                                                 cf.matrix$byClass))))
    }

    flog.info("Training final model")

    train.control = caret::trainControl(method = "cv",
                                        index = idx.outer,
                                        classProbs = TRUE)

    training.arguments = merge(
        list(form      = Class ~ .,
             data      = dataset,
             trControl = train.control,
             method    = model.name,
             tuneGrid  = model.grid,
             metric    = ncv.performance.selector,
             maximize  = ncv.performance.maximize),
        model.attrs)

    suppressWarnings(
        capture.output(model <- do.call(caret::train, training.arguments)))

    attr(model, "folds.performance") = folds.performance
    attr(model, "preproc.scheme")    = preproc.scheme

    flog.info(paste0("Estimated ", ncv.performance.selector, ": ",
                     round(mean(folds.performance[[ncv.performance.selector]]), 3)))

    used.predictors = set()

    if (model.name == "OneR")
    {
        used.predictors = set(strsplit(model$finalModel$classifier$toString(),
                                       ":")[[1]][1])
    } else {
        predictors.names = tryCatch(
            caret::predictors(model),
            error = function(e) { model$coefnames })

        for (column.name in colnames(dataset)[1:(ncol(dataset) - 1)])
        {
            if (any(grepl(paste0("^", column.name), predictors.names)))
                used.predictors = used.predictors | set(column.name)
        }
    }

    attr(model, "used.predictors") = used.predictors

    flog.info(paste("Used predictors:", length(used.predictors)))

    return(model)
}

classifiers.baseline = "OneR"

classifiers.list = c("svmLinear",
                     "C5.0",
                     "knn")

classifiers.feature.selection.method = list(

    OneR = NULL, # internal

    svmLinear = "rfFuncs",

    C5.0 = NULL, # internal

    knn = "treebagFuncs"
)

classifiers.tuning.params = list(

    OneR = NULL,

    svmLinear = expand.grid(C = 10 ^ seq(-5,2)),

    C5.0 = expand.grid(trials = c(1, 5, 10, 15, 20),
                       model  = c("tree", "rules"),
                       winnow = c(TRUE)),

    knn = expand.grid(k = c(1:10))
)

classifiers.basic.attributes = list(

    OneR = NULL,

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

    for (model.name in c(classifiers.baseline, classifiers.list))
    {
        flog.info(paste("Classifier:", model.name))

        if (file.exists(file.path("models",
                                  paste0(dataset.name, "-", model.name, ".rds"))))
        {
            flog.info("Model exists, skipping leraning")

            model = readRDS(file.path("models",
                                      paste0(dataset.name, "-", model.name, ".rds")))

            folds.performance = attr(model, "folds.performance")

            flog.info(paste0("Estimated ", ncv.performance.selector, ": ",
                             round(mean(folds.performance[[ncv.performance.selector]]), 3)))

            flog.info(paste(rep("*", 10), collapse = ""))

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

        if (model.name == "OneR")
        {
            .jcache(model$finalModel$classifier)
        }

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

    set.seed(SEED)

    dataset.obscuration =
        readRDS(file.path("datasets", paste0(dataset.name, "-obscuration.rds")))

    dataset.used.predictors = set()

    for (model.name in c(classifiers.baseline, classifiers.list))
    {
        model = readRDS(file.path("models",
                                  paste0(dataset.name, "-", model.name, ".rds")))

        dataset.used.predictors = dataset.used.predictors | attr(model, "used.predictors")
    }

    flog.info(paste("Used predictors:", length(dataset.used.predictors),
                    "of", ncol(dataset.obscuration) - 1))

    dataset.class.levels = levels(dataset.obscuration[, ncol(dataset.obscuration)])
    dataset.class.name   = tail(colnames(dataset.obscuration), 1)

    dataset.1 = dataset.obscuration %>%
                filter_(interp(quote(a == b),
                        a = as.name(dataset.class.name),
                        b = dataset.class.levels[1]))

    dataset.2 = dataset.obscuration %>%
                filter_(interp(quote(a == b),
                        a = as.name(dataset.class.name),
                        b = dataset.class.levels[2]))

    dataset.obscured = data.frame()

    for (dataset in list(dataset.1, dataset.2))
    {
        dataset.obscured = rbind(dataset.obscured, dataset[1:(nrow(dataset)/3), ])

        dataset.nas.idx = caret::createFolds(dataset[(nrow(dataset)/3 + 1):nrow(dataset),
                                                     ncol(dataset)],
                                             k = length(dataset.used.predictors) - 1)

        for (i in 1:(length(dataset.used.predictors) - 1))
        {
            chunk = dataset[dataset.nas.idx[[i]] + (nrow(dataset)/3), ]
            chunk.not.used = chunk[, !(colnames(chunk) %in%
                                           c(as.character(dataset.used.predictors),
                                             dataset.class.name))]
            chunk.used     = chunk[,   colnames(chunk) %in%
                                           c(as.character(dataset.used.predictors),
                                             dataset.class.name)]

            nas.matrix = matrix(FALSE, nrow = nrow(chunk.used), ncol = ncol(chunk.used) - 1)
            nas.matrix = t(apply(nas.matrix, 1, function(row) {
                row[sample(1:length(row), i)] = TRUE;
                row}))

            for (pos in as.data.frame(t(which(nas.matrix == TRUE, arr.ind = TRUE))))
            {
                chunk.used[pos[1], pos[2]] = NA
            }

            chunk = cbind(chunk.used, chunk.not.used)
            chunk = chunk[, colnames(dataset)]

            dataset.obscured = rbind(dataset.obscured, chunk)
        }
    }

    saveRDS(dataset.obscured,
            file.path("datasets", paste0(dataset.name, "-obscured.rds")))

}

flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-4-classifiers-performance-on-obscured-dataset ----

flog.info("Step 4: classifiers performance on obscured dataset")

for (dataset.name in datasets.names)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured =
        readRDS(file.path("datasets", paste0(dataset.name, "-obscured.rds")))

    for (model.name in c(classifiers.baseline, classifiers.list))
    {
        flog.info(paste("Model:", model.name))

        model = readRDS(file.path("models",
                                  paste0(dataset.name, "-", model.name, ".rds")))

        preproc.scheme = attr(model, "preproc.scheme")
        dataset.obscured.preprocessed = stats::predict(preproc.scheme, dataset.obscured)

        dataset.no.nas =
            dataset.obscured.preprocessed[which(rowSums(is.na(
                dataset.obscured.preprocessed[
                    as.character(attr(model, "used.predictors"))])) == 0), ]

        predictions = stats::predict(model, dataset.no.nas, na.action = NULL)
        cf.matrix = caret::confusionMatrix(predictions,
                                           dataset.no.nas[, ncol(dataset.no.nas)])

        flog.info(paste("Accuracy:    ", round(cf.matrix$overall["Accuracy"], 3)))
        flog.info(paste("Sensitivity: ", round(cf.matrix$byClass["Sensitivity"], 3)))
        flog.info(paste("Specificity: ", round(cf.matrix$byClass["Specificity"], 3)))
        flog.info(paste("Decisiveness:",
                        round(nrow(dataset.no.nas)/nrow(dataset.obscured), 3)))

        flog.info(paste(rep("*", 10), collapse = ""))
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}


flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-5-choose-best-imputation ----

flog.info("Step 5: choose best imputation")

imputation.median.mode = function(data)
{
    colnames.ord.factor =
        names(which(sapply(colnames(data),
                           function(x){ all(class(data[[x]])
                                            == c("ordered", "factor"))})
                    == TRUE))

    mlr::impute(data,
                target  = tail(colnames(data), 1),
                classes = list(numeric   = mlr::imputeMedian(),
                               integer   = mlr::imputeMedian(),
                               factor    = mlr::imputeMode()),
                cols    = sapply(colnames.ord.factor,
                                 function(x){ x = mlr::imputeMode() },
                                 simplify = F))$data
}

imputation.random.forest = function(data)
{
    suppressWarnings(
        capture.output(
            data.new <- missForest::missForest(data[, -ncol(data)])$ximp
    ))

    cbind(data.new, data[ncol(data)])
}

imputation.mice = function(data)
{
    data.imputed = mice::complete(mice::mice(data[, -ncol(data)],
                                             m = 1, maxit = 10,
                                             printFlag = FALSE),
                                  action = 1)

    for (colname in colnames(data.imputed))
    {
        attr(data.imputed[[colname]], "contrasts") = NULL
    }

    cbind(data.imputed, data[ncol(data)])
}

crossValidationForImputation = function(datasets, models, no.folds)
{
    set.seed(SEED)

    idx.cv = caret::createFolds(1:nrow(datasets[[1]][[1]]),
                                k = no.folds)

    which.function = ifelse(ncv.performance.maximize, which.max, which.min)

    params.grid = expand.grid(1:length(datasets[[1]]), 1:length(models))

    folds.performances = data.frame()

    for (i in 1:no.folds)
    {
        flog.info(paste("Fold", i))

        idx.train = idx.cv[setdiff(1:no.folds, i)]
        idx.test  = idx.cv[i]

        perf.measures = apply(params.grid, 1,
              function(ids) {
                    dataset.train = datasets[[ids[2]]][[ids[1]]][unname(unlist(idx.train)), ]
                    model         = models[[ids[2]]]

                    predictions = stats::predict(model, dataset.train)
                    cf.matrix =
                        caret::confusionMatrix(predictions,
                                               dataset.train[, ncol(dataset.train)])
                    cf.matrix$overall[[ncv.performance.selector]]
        })

        best.id = which.function(perf.measures)

        dataset.test = datasets[[params.grid[best.id, 2]]][[params.grid[best.id, 1]]][unname(unlist(idx.test)), ]
        model        = models[[params.grid[best.id, 2]]]

        predictions = stats::predict(model, dataset.test)
        cf.matrix =
            caret::confusionMatrix(predictions,
                                   dataset.test[, ncol(dataset.test)])

        folds.performances = rbind(folds.performances,
                                   data.frame(t(c(cf.matrix$overall,
                                                  cf.matrix$byClass))))

    }

    flog.info("Choosing final model")

    perf.measures = apply(params.grid, 1,
              function(ids) {
                  dataset = datasets[[ids[2]]][[ids[1]]]
                  model   = models[[ids[2]]]

                  predictions = stats::predict(model, dataset)
                  cf.matrix = caret::confusionMatrix(predictions,
                                                     dataset[, ncol(dataset)])
                  cf.matrix$overall[[ncv.performance.selector]]
              })

    best.id = which.function(perf.measures)

    dataset      = datasets[[params.grid[best.id, 2]]][[params.grid[best.id, 1]]]
    model        = models[[params.grid[best.id, 2]]]

    if (length(models) > 1)
    {
        flog.info(paste("Choosed classifier:", model$method))
    }

    choosed.imputation.name =
        names(datasets[[params.grid[best.id, 2]]])[params.grid[best.id, 1]]

    flog.info(paste("Choosed imputation:", choosed.imputation.name))

    flog.info(paste0("Estimated ", ncv.performance.selector, ":    ",
                     round(mean(folds.performances[[ncv.performance.selector]]), 3)))
    flog.info(paste0("Estimated Sensitivity: ",
                     round(mean(folds.performances[["Sensitivity"]]), 3)))
    flog.info(paste0("Estimated Specificity: ",
                     round(mean(folds.performances[["Specificity"]]), 3)))

    return(list("model"              = model,
                "imputation.name"    = choosed.imputation.name,
                "imputation.dataset" = dataset,
                "folds.performances" = folds.performances))
}

for (dataset.name in datasets.names)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured =
        readRDS(file.path("datasets", paste0(dataset.name, "-obscured.rds")))

    imputation.methods = list("median/mode"       = imputation.median.mode,
                              "random forest"     = imputation.random.forest,
                              "chained equations" = imputation.mice)

    flog.info(paste("Baseline model:", classifiers.baseline))

    baseline.model.path = file.path("models",
                                    paste0(dataset.name, "-imputation-baseline.rds"))

    if (!file.exists(baseline.model.path))
    {
        model = readRDS(file.path("models",
                                  paste0(dataset.name, "-",
                                         classifiers.baseline, ".rds")))

        preproc.scheme = attr(model, "preproc.scheme")
        dataset.obscured.preprocessed = stats::predict(preproc.scheme,
                                                       dataset.obscured)

        datasets.imputed = list(lapply(names(imputation.methods), function(name){
            flog.info(paste("Imputation:", name))
            set.seed(SEED)
            imputation.methods[[name]](dataset.obscured.preprocessed) }))

        names(datasets.imputed[[1]]) = names(imputation.methods)

        baseline.model.with.imputation =
            crossValidationForImputation(datasets.imputed, list(model), ncv.folds)

        saveRDS(baseline.model.with.imputation, baseline.model.path)

    } else {
        flog.info("Baseline model exists, skipping leraning")

        baseline.model.with.imputation = readRDS(baseline.model.path)

        folds.performances = baseline.model.with.imputation$folds.performances

        flog.info(paste0("Estimated ", ncv.performance.selector, ":    ",
                         round(mean(folds.performances[[ncv.performance.selector]]), 3)))
        flog.info(paste0("Estimated Sensitivity: ",
                         round(mean(folds.performances[["Sensitivity"]]), 3)))
        flog.info(paste0("Estimated Specificity: ",
                         round(mean(folds.performances[["Specificity"]]), 3)))
    }

    flog.info(paste(rep("*", 10), collapse = ""))

    flog.info("Grid search: classifiers and imputation methods")

    classifier.model.path = file.path("models",
                                    paste0(dataset.name, "-imputation-classifier.rds"))

    if (!file.exists(classifier.model.path))
    {
        models = list()
        datasets.imputed = list()

        for (model.name in classifiers.list)
        {
            flog.info(paste("Model:", model.name))

            model = readRDS(file.path("models",
                                      paste0(dataset.name, "-", model.name, ".rds")))

            models = merge(models, list(model))

            preproc.scheme = attr(model, "preproc.scheme")
            dataset.obscured.preprocessed = stats::predict(preproc.scheme,
                                                           dataset.obscured)

            model.datasets.imputed = lapply(names(imputation.methods), function(name){
                flog.info(paste("Imputation:", name))
                set.seed(SEED)
                imputation.methods[[name]](dataset.obscured.preprocessed) })

            names(model.datasets.imputed) = names(imputation.methods)

            datasets.imputed[[model.name]] = model.datasets.imputed
        }

        classifier.model.with.imputation =
            crossValidationForImputation(datasets.imputed, models, ncv.folds)

        saveRDS(classifier.model.with.imputation, classifier.model.path)

    } else {
        flog.info("Classifier model exists, skipping leraning")

        classifier.model.with.imputation = readRDS(classifier.model.path)

        folds.performances = classifier.model.with.imputation$folds.performances

        flog.info(paste0("Estimated ", ncv.performance.selector, ":    ",
                         round(mean(folds.performances[[ncv.performance.selector]]), 3)))
        flog.info(paste0("Estimated Sensitivity: ",
                         round(mean(folds.performances[["Sensitivity"]]), 3)))
        flog.info(paste0("Estimated Specificity: ",
                         round(mean(folds.performances[["Specificity"]]), 3)))
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}

flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-6-choose-best-aggregation ----

flog.info("Step 6: choose best aggregation")

for (dataset.name in datasets.names)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured =
        readRDS(file.path("datasets", paste0(dataset.name, "-obscured.rds")))

    for (model.name in classifiers.list)
    {
        flog.info(paste("Model:", model.name))

        model = readRDS(file.path("models",
                                  paste0(dataset.name, "-", model.name, ".rds")))

        preproc.scheme = attr(model, "preproc.scheme")
        dataset.obscured.preprocessed = stats::predict(preproc.scheme, dataset.obscured)

        #browser()
        #stats::predict(model, dataset.obscured.preprocessed, type = "prob")

        flog.info(paste(rep("*", 10), collapse = ""))
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}

flog.info(paste(rep("*", 50), collapse = ""))

# ---- step-7-compare-results ----

flog.info("Step 7: compare results")



flog.info(paste(rep("*", 50), collapse = ""))
