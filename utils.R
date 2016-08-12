# functions for cross-validations

nested.cross.validation = function(dataset, model.name,
                                   model.grid, model.attrs, no.folds,
                                   preprocessing.methods, performance.selector,
                                   performance.maximize,
                                   allow.parallel,
                                   seed)
{
    set.seed(seed)

    colnames(dataset)[ncol(dataset)] = "Class"

    preproc.scheme = suppressWarnings( # in case of 'range' elimination
                        caret::preProcess(dataset,
                                          method = preprocessing.methods))
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
                                                index  = idx.inner,
                                                allowParallel = allow.parallel)

            training.arguments = merge(
                list(form      = Class ~ .,
                     data      = dataset.inner,
                     trControl = train.control,
                     method    = model.name,
                     tuneGrid  = model.grid,
                     metric    = performance.selector,
                     maximize  = performance.maximize),
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
                                        index  = idx.outer,
                                        classProbs    = TRUE,
                                        allowParallel = allow.parallel)

    training.arguments = merge(
        list(form      = Class ~ .,
             data      = dataset,
             trControl = train.control,
             method    = model.name,
             tuneGrid  = model.grid,
             metric    = performance.selector,
             maximize  = performance.maximize),
        model.attrs)

    suppressWarnings(
        capture.output(model <- do.call(caret::train, training.arguments)))

    if (is.null(model.grid))
    {
        folds.performance = model$resample[performance.selector]
    }

    attr(model, "folds.performance") = folds.performance
    attr(model, "preproc.scheme")    = preproc.scheme

    used.predictors = set()

    if (model.name == "OneR")
    {
        oner.predictor.name = strsplit(model$finalModel$classifier$toString(),
                                       ":")[[1]][1]
        predictors.matching = sapply(head(colnames(dataset), ncol(dataset) - 1),
                                     function(w){grepl(paste0("^", w),
                                                       oner.predictor.name)})

        if (sum(predictors.matching) != 1)
        {
            stop("Unable to determine predictors used by OneR classifier")
        }

        used.predictors = set(names(which(predictors.matching == TRUE)))
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

    if (length(used.predictors -
               as.set(head(colnames(dataset), ncol(dataset) - 1))) > 0)
    {
        stop(paste("Predictors used by", model.name,
                   "are not present in the dataset"))
    }

    attr(model, "used.predictors") = used.predictors

    return(model)
}

cross.validation.for.imputation = function(datasets, models, no.folds,
                                           performance.selector,
                                           performance.maximize, seed)
{
    set.seed(seed)

    idx.cv = caret::createFolds(1:nrow(datasets[[1]][[1]]),
                                k = no.folds)

    which.function = ifelse(performance.maximize, which.max, which.min)

    params.grid = expand.grid(1:length(datasets[[1]]), 1:length(models))

    folds.performances = data.frame()

    for (i in 1:no.folds)
    {
        flog.info(paste("Fold", i))

        idx.train = idx.cv[setdiff(1:no.folds, i)]
        idx.test  = idx.cv[i]

        perf.measures =
            apply(params.grid, 1, function(ids)
            {
                dataset.train =
                    datasets[[ids[2]]][[ids[1]]][unname(unlist(idx.train)), ]
                model         = models[[ids[2]]]

                predictions =
                    suppressWarnings(stats::predict(model, dataset.train))
                cf.matrix =
                  caret::confusionMatrix(predictions,
                                         dataset.train[, ncol(dataset.train)])
                cf.matrix$overall[[performance.selector]]
            })

        best.id = which.function(perf.measures)

        dataset.test =
            datasets[[params.grid[best.id, 2]]][[params.grid[best.id, 1]]][unname(unlist(idx.test)), ]
        model        = models[[params.grid[best.id, 2]]]

        predictions = suppressWarnings(stats::predict(model, dataset.test))
        cf.matrix   = caret::confusionMatrix(predictions,
                                             dataset.test[, ncol(dataset.test)])

        folds.performances = rbind(folds.performances,
                                   data.frame(t(c(cf.matrix$overall,
                                                  cf.matrix$byClass))))

    }

    flog.info("Choosing final model")

    perf.measures =
        apply(params.grid, 1, function(ids)
        {
            dataset = datasets[[ids[2]]][[ids[1]]]
            model   = models[[ids[2]]]

            predictions = suppressWarnings(stats::predict(model, dataset))
            cf.matrix   = caret::confusionMatrix(predictions,
                                                 dataset[, ncol(dataset)])
            cf.matrix$overall[[performance.selector]]
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

    flog.info(paste0("Estimated ", performance.selector, ":    ",
                     round(mean(folds.performances[[performance.selector]]), 3)))
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

setup.logger = function(output.file, overwrite.existing.files)
{
    if (overwrite.existing.files & file.exists(output.file))
    {
        file.remove(output.file)
    }

    invisible(flog.appender(appender.tee(output.file)))
}

expand.grid.df = function(...) # https://stackoverflow.com/a/21911221
{
    return(Reduce(function(...) merge(..., by = NULL), list(...)))
}

replace.strings = function(from, to, base.string)
{
    if (length(from) != length(to))
    {
        stop("Unable to replace list of strings of different lengths")
    }

    for (i in 1:length(from))
    {
        base.string = gsub(from[i], to[i], base.string)
    }

    return(base.string)
}

used.predictors.as.table = function(used.predictors.list, row.names)
{

    col.names = names(used.predictors.list)
    row.names = sort(row.names)

    df = revalue(sapply(col.names, function(x)
                 {
                    as.character(row.names %in% used.predictors.list[[x]])
                 }),
                 c("TRUE" = "*", "FALSE" = " "))

    rownames(df) = row.names

    return(as.table(df))
}

show.factor.levels.counts = function(dataset)
{
    for (name in head(colnames(dataset), ncol(dataset) - 1))
    {
        if (is.factor(dataset[[name]]) | is.ordered(dataset[[name]]))
        {
            tab = table(dataset[[name]])

            if (any(tab > 0 & tab < 10))
            {
                flog.debug(paste0("* ", name, ":"))
                flog.debug(paste(names(which(tab > 0 & tab < 10)), collapse = " "))
                flog.debug(paste(tab[which(tab > 0 & tab < 10)], collapse = " "))
            }
        }
    }
}
