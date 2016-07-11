# functions for cross-validations

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

    if (!is.null(model.grid))
    {
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
    } else {
        flog.info("No tuning grid, skipping nested CV")
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

    if (is.null(model.grid))
    {
        folds.performance = model$resample[ncv.performance.selector]
    }

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

# additional functions

setupLogger = function(output.file)
{
    if (!LOGGER.APPEND & file.exists(output.file))
    {
        file.remove(output.file)
    }

    flog.appender(appender.tee(output.file))
}

expand.grid.df = function(...) # https://stackoverflow.com/a/21911221
{
    Reduce(function(...) merge(..., by = NULL), list(...))
}
