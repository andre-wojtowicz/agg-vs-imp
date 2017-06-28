# functions for cross-validations

cross.validation.orig = function(dataset, models,
                                 no.folds, performance.selector,
                                 performance.maximize,
                                 .random.seed = NULL)
{
    if (!is.null(.random.seed))
    {
        assign(".Random.seed", .random.seed, envir = .GlobalEnv)
    }

    colnames(dataset)[ncol(dataset)] = "Class"

    idx.outer = caret::createFolds(dataset$Class,
                                   k = no.folds)

    folds.performance = data.frame()

    models.list = list()

    for (model.name in models)
    {
        model.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            CLASSIFIERS.LEARNED)

        model = readRDS(model.file.path)

        models.list[[model.name]] = model
    }

    for (i in 1:no.folds)
    {
        flog.info(paste("Fold", i))

        ds.train  = dataset[as.numeric(unlist(idx.outer[setdiff(1:no.folds, i)])), ]
        ds.test   = dataset[as.numeric(unlist(idx.outer[i])), ]

        results.train = c()

        for (model.name in models)
        {
            model = models.list[[model.name]]
            preproc.scheme = attr(model, "preproc.scheme")
            dataset.obscured.preprocessed =
                stats::predict(preproc.scheme, ds.train)

            dataset.no.nas =
                dataset.obscured.preprocessed[which(rowSums(is.na(
                    dataset.obscured.preprocessed[
                        as.character(attr(model, "used.predictors"))])) == 0), ]

            predictions =
                suppressWarnings(stats::predict(model, dataset.no.nas,
                                                na.action = NULL))
            cf.matrix = caret::confusionMatrix(predictions,
                                               dataset.no.nas[, ncol(dataset.no.nas)])

            results.train = c(results.train, cf.matrix$overall[[performance.selector]])
        }

        model =
            models.list[[ifelse(performance.maximize, which.max, which.min)(results.train)]]

        preproc.scheme = attr(model, "preproc.scheme")
        dataset.obscured.preprocessed =
            stats::predict(preproc.scheme, ds.test)

        dataset.no.nas =
            dataset.obscured.preprocessed[which(rowSums(is.na(
                dataset.obscured.preprocessed[
                    as.character(attr(model, "used.predictors"))])) == 0), ]

        predictions =
            suppressWarnings(stats::predict(model, dataset.no.nas,
                                            na.action = NULL))
        cf.matrix = caret::confusionMatrix(predictions,
                                           dataset.no.nas[, ncol(dataset.no.nas)])

        folds.performance =
            rbind(folds.performance,
               data.frame(Fold.id = i,
                          Missing.attributes = NA,
                          Accuracy     = cf.matrix$overall["Accuracy"],
                          Sensitivity  = cf.matrix$byClass["Sensitivity"],
                          Specificity  = cf.matrix$byClass["Specificity"],
                          Decisiveness = nrow(dataset.no.nas)/nrow(ds.test)))

        for (num.missing.attr in 0:max(rowSums(is.na(dataset.obscured.preprocessed))))
        {
            flog.info(paste("Missing attributes:", num.missing.attr))

            dataset.num.miss.attr =
                dataset.obscured.preprocessed[which(rowSums(is.na(
                    dataset.obscured.preprocessed)) == num.missing.attr), ]

            dataset.no.nas =
                dataset.num.miss.attr[which(rowSums(is.na(
                    dataset.num.miss.attr[
                        as.character(attr(model, "used.predictors"))])) == 0), ]

            if (nrow(dataset.no.nas) > 0)
            {
                predictions =
                    suppressWarnings(stats::predict(model, dataset.no.nas,
                                                    na.action = NULL))
                cf.matrix = caret::confusionMatrix(predictions,
                                                   dataset.no.nas[, ncol(dataset.no.nas)])

                flog.info(paste("  Accuracy:    ", round(cf.matrix$overall["Accuracy"], 3)))
                flog.info(paste("  Sensitivity: ", round(cf.matrix$byClass["Sensitivity"], 3)))
                flog.info(paste("  Specificity: ", round(cf.matrix$byClass["Specificity"], 3)))
                flog.info(paste("  Decisiveness:",
                                round(nrow(dataset.no.nas)/nrow(dataset.num.miss.attr), 3)))



                folds.performance = rbind(folds.performance,
                                       data.frame(Fold.id = i,
                                                  Missing.attributes = num.missing.attr,
                                                  Accuracy     = cf.matrix$overall["Accuracy"],
                                                  Sensitivity  = cf.matrix$byClass["Sensitivity"],
                                                  Specificity  = cf.matrix$byClass["Specificity"],
                                                  Decisiveness = nrow(dataset.no.nas)/nrow(dataset.num.miss.attr)))

            } else {
                flog.info("  Accuracy:     -")
                flog.info("  Sensitivity:  -")
                flog.info("  Specificity:  -")
                flog.info("  Decisiveness: 0")

                folds.performance = rbind(folds.performance,
                                       data.frame(Fold.id = i,
                                                  Missing.attributes = num.missing.attr,
                                                  Accuracy     = NA,
                                                  Sensitivity  = NA,
                                                  Specificity  = NA,
                                                  Decisiveness = 0.0))
            }
        }
    }

    folds.performance
}

# ----------------

cross.validation.unc  = function(dataset, models,
                                 no.folds, performance.selector,
                                 performance.maximize,
                                 .random.seed = NULL)
{
    if (!is.null(.random.seed))
    {
        assign(".Random.seed", .random.seed, envir = .GlobalEnv)
    }

    colnames(dataset)[ncol(dataset)] = "Class"

    idx.outer = caret::createFolds(dataset$Class,
                                   k = no.folds)

    folds.performance = data.frame()

    models.list = list()
    predictions.list = list()
    references.list  = list()
    missings.list    = list()

    for (model.name in models)
    {
        print(model.name)

        model.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            CLASSIFIERS.LEARNED)

        model = readRDS(model.file.path)

        models.list[[model.name]] = model

        preproc.scheme = attr(model, "preproc.scheme")
        dataset.obscured.preprocessed =
            stats::predict(preproc.scheme, dataset)

        dataset.interval =
            foreach::foreach(
                i = 1:nrow(dataset.obscured.preprocessed),
                .combine       = rbind,
                .multicombine  = TRUE,
                .maxcombine    = nrow(dataset.obscured.preprocessed),
                .export        = c("calculate.optim.interval", "optim.factor.numeric.classic", "optim.numeric.classic", "optim.factor.grid.search", "OPTIMIZATION.NUMERIC.REPS"),
                .packages      = c("nloptr", "data.table", "foreach")) %dopar%
                {
                    case.predictors.all = dataset.obscured.preprocessed[
                        i, 1:(ncol(dataset.obscured.preprocessed) - 1), drop = FALSE]
                    case.class = dataset.obscured.preprocessed[
                        i, ncol(dataset.obscured.preprocessed), drop = FALSE]

                    case.predictors.used = case.predictors.all[
                        , as.character(attr(model, "used.predictors")), drop = FALSE]

                    interval = calculate.optim.interval(case.predictors.all, case.class,
                                                        case.predictors.used, i, model)

                    interval.lower = interval[1]
                    interval.upper = interval[2]

                    flog.info(paste0("Predicted interval: [",
                                     round(interval.lower, 4), ", ",
                                     round(interval.upper, 4), "]"))

                    data.frame(lower = interval.lower, upper = interval.upper)
                }

        dataset.binary = apply(dataset.interval, 1, function(x){
            if (x[1] > 0.5)
            {
                return(1)
            } else if (x[2] <= 0.5) {
                return(0)
            }
            return(NA)
        })

        dataset.classes = as.integer(dataset.obscured.preprocessed[
            , ncol(dataset.obscured.preprocessed)]) - 1

        dataset.no.missing.attrs = rowSums(is.na(dataset.obscured.preprocessed))

        dataset.binded = data.frame(Predictions = dataset.binary,
                                    Reference   = dataset.classes,
                                    Missing.attribues = dataset.no.missing.attrs)

        #dataset.binded.complete = dataset.binded %>%
        #    filter(!is.na(Predictions))

        predictions.list[[model.name]] = dataset.binded$Predictions
        references.list[[model.name]]  = dataset.binded$Reference
        missings.list[[model.name]]    = dataset.binded$Missing.attribues
    }

    for (i in 1:no.folds)
    {
        flog.info(paste("Fold", i))

        results.train = c()

        for (model.name in models)
        {
            preds = predictions.list[[model.name]][as.numeric(unlist(idx.outer[setdiff(1:no.folds, i)]))]
            refs  = references.list[[model.name]][as.numeric(unlist(idx.outer[setdiff(1:no.folds, i)]))]

            refs  = refs[!is.na(preds)]
            preds = preds[!is.na(preds)]

            cf.matrix = suppressWarnings(
                caret::confusionMatrix(preds, refs))

            results.train = c(results.train, cf.matrix$overall[[performance.selector]])
        }

        model.name =
            models[ifelse(performance.maximize, which.max, which.min)(results.train)]

        preds = predictions.list[[model.name]][as.numeric(unlist(idx.outer[i]))]
        refs  = references.list[[model.name]][as.numeric(unlist(idx.outer[i]))]
        msgs  = missings.list[[model.name]][as.numeric(unlist(idx.outer[i]))]

        refs2  = refs[!is.na(preds)]
        preds2 = preds[!is.na(preds)]

        cf.matrix = suppressWarnings(
            caret::confusionMatrix(preds2,
                                   refs2))

        folds.performance =
            rbind(folds.performance,
                  data.frame(Fold.id = i,
                             Missing.attributes = NA,
                             Accuracy     = cf.matrix$overall["Accuracy"],
                             Sensitivity  = cf.matrix$byClass["Sensitivity"],
                             Specificity  = cf.matrix$byClass["Specificity"],
                             Decisiveness = length(preds2)/length(preds)))

        for (num.missing.attr in 0:max(msgs))
        {
            flog.info(paste("Missing attributes:", num.missing.attr))

            msgs.ids = which(msgs == num.missing.attr)


            if (length(msgs.ids) > 0)
            {
                preds.m = preds[msgs.ids]
                refs.m  = refs[msgs.ids]

                refs2  = refs.m[!is.na(preds.m)]
                preds2 = preds.m[!is.na(preds.m)]

                if (length(preds2) > 0)
                {

                    cf.matrix = suppressWarnings(
                        caret::confusionMatrix(factor(preds2, levels = c(0, 1)),
                                               factor(refs2, levels = c(0, 1))))

                    flog.info(paste("  Accuracy:    ", round(cf.matrix$overall["Accuracy"], 3)))
                    flog.info(paste("  Sensitivity: ", round(cf.matrix$byClass["Sensitivity"], 3)))
                    flog.info(paste("  Specificity: ", round(cf.matrix$byClass["Specificity"], 3)))
                    flog.info(paste("  Decisiveness:",
                                    round(length(preds2)/length(preds.m), 3)))

                    folds.performance = rbind(folds.performance,
                                              data.frame(Fold.id = i,
                                                         Missing.attributes = num.missing.attr,
                                                         Accuracy     = cf.matrix$overall["Accuracy"],
                                                         Sensitivity  = cf.matrix$byClass["Sensitivity"],
                                                         Specificity  = cf.matrix$byClass["Specificity"],
                                                         Decisiveness = length(preds2)/length(preds.m)))
                } else {
                    flog.info("  Accuracy:     -")
                    flog.info("  Sensitivity:  -")
                    flog.info("  Specificity:  -")
                    flog.info("  Decisiveness: 0")

                    folds.performance = rbind(folds.performance,
                                              data.frame(Fold.id = i,
                                                         Missing.attributes = num.missing.attr,
                                                         Accuracy     = NA,
                                                         Sensitivity  = NA,
                                                         Specificity  = NA,
                                                         Decisiveness = 0.0))
                }
            } else {
                flog.info("  Accuracy:     -")
                flog.info("  Sensitivity:  -")
                flog.info("  Specificity:  -")
                flog.info("  Decisiveness: 0")

                folds.performance = rbind(folds.performance,
                                          data.frame(Fold.id = i,
                                                     Missing.attributes = num.missing.attr,
                                                     Accuracy     = NA,
                                                     Sensitivity  = NA,
                                                     Specificity  = NA,
                                                     Decisiveness = 0.0))
            }
        }
    }

    folds.performance
}

# ----------------

nested.cross.validation = function(dataset, model.name,
                                   model.grid, model.attrs, no.folds,
                                   preprocessing.methods, performance.selector,
                                   performance.maximize,
                                   .random.seed = NULL)
{
    if (!is.null(.random.seed))
    {
        assign(".Random.seed", .random.seed, envir = .GlobalEnv)
    }

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
        train.seeds.ncv = vector(mode = "list", length = no.folds)
        for (i in 1:no.folds)
        {
            inner.seeds = vector(mode = "list", length = no.folds + 1)
            for (i in 1:no.folds)
            {
                inner.seeds[[i]] = sample.int(1000, nrow(model.grid))
            }
            inner.seeds[[no.folds + 1]] = sample.int(1000, 1)

            train.seeds.ncv[[i]] = inner.seeds
        }

        for (i in 1:no.folds)
        {
            flog.info(paste("Fold", i))

            folds.inner       = idx.outer[setdiff(1:no.folds, i)]
            dataset.inner     = dataset[as.numeric(unlist(folds.inner)), ]
            train.seeds.inner = train.seeds.ncv[[i]]

            idx.inner = caret::createFolds(dataset.inner$Class,
                                           k = no.folds)

            train.control = caret::trainControl(method = "cv",
                                                index  = idx.inner,
                                                seeds  = train.seeds.inner,
                                                allowParallel = TRUE)

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

    train.seeds = vector(mode = "list", length = no.folds + 1)
    for (i in 1:no.folds)
    {
        train.seeds[[i]] =
            sample.int(1000, ifelse(!is.null(model.grid), nrow(model.grid), 1))
    }
    train.seeds[[no.folds + 1]] = sample.int(1000, 1)

    train.control = caret::trainControl(method          = "cv",
                                        index           = idx.outer,
                                        classProbs      = TRUE,
                                        summaryFunction = caret::multiClassSummary,
                                        seeds           = train.seeds,
                                        allowParallel   = TRUE)

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
        folds.performance = model$resample[, c("Accuracy", "Sensitivity", "Specificity")]
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
            stop.script("Unable to determine predictors used by OneR classifier")
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
        stop.script(paste("Predictors used by", model.name,
                          "are not present in the dataset"))
    }

    attr(model, "used.predictors") = used.predictors

    return(model)
}

nested.cross.validation.for.imputation = function(dataset.obscured,
                                                  num.missing.attributes,
                                                  models,
                                                  imputation.methods,
                                                  no.folds,
                                                  performance.selector,
                                                  performance.maximize,
                                                  random.seed)
{
    assign(".Random.seed", random.seed, envir = .GlobalEnv)

    start.seed = sample.int(1000, 1)

    seeds.1.outer = get.seeds(start.seed, no.folds)
    seeds.1.inner = get.seeds(start.seed, c(no.folds, no.folds))
    seeds.2 = get.seeds(start.seed, no.folds)

    which.function = ifelse(performance.maximize, which.max, which.min)
    dataset.class.factor.levels = levels(dataset.obscured[, ncol(dataset.obscured)])

    params.grid    = expand.grid(1:length(models), 1:length(imputation.methods))
    colnames(params.grid) = c("model", "imputation.method")


    flog.info("Phase 1")

    folds.performances = data.frame()

    idx.outer = caret::createFolds(dataset.obscured[, ncol(dataset.obscured)],
                                   k = no.folds)

    for (i in 1:no.folds)
    {
        flog.info(paste("Outer fold", i))

        assign(".Random.seed", extract.seed(seeds.1.outer, i), envir = .GlobalEnv)

        inner.folds.performance = data.frame()

        folds.inner            = idx.outer[setdiff(1:no.folds, i)]
        dataset.obscured.inner = dataset.obscured[as.numeric(unlist(folds.inner)), ]

        idx.inner =
            caret::createFolds(dataset.obscured.inner[, ncol(dataset.obscured.inner)],
                               k = no.folds)

        for (j in 1:no.folds)
        {
            flog.info(paste("Inner fold", j))

            assign(".Random.seed", extract.seed(seeds.1.inner, c(i, j)), envir = .GlobalEnv)

            training.folds = idx.inner[setdiff(1:no.folds, j)]
            testing.fold   = idx.inner[j]
            dataset.obscured.training =
                dataset.obscured.inner[as.numeric(unlist(training.folds)), ]
            dataset.obscured.testing  =
                dataset.obscured.inner[as.numeric(unlist(testing.fold)), ]

            perfs = apply(params.grid, 1, function(cfg)
            {
                model             = models[[cfg["model"]]]
                imputation.method = imputation.methods[[cfg["imputation.method"]]]

                preproc.scheme = attr(model, "preproc.scheme")
                dataset.obscured.training.preprocessed =
                    stats::predict(preproc.scheme, dataset.obscured.training)
                dataset.obscured.testing.preprocessed =
                    stats::predict(preproc.scheme, dataset.obscured.testing)

                imputation.scheme =
                    imputation.method(dataset.obscured.training.preprocessed)

                dataset.imputed.testing =
                    imputation.scheme(dataset.obscured.testing.preprocessed,
                                      attr(imputation.scheme, "learned.obj"))

                predictions =
                    suppressWarnings(stats::predict(model, dataset.imputed.testing))


                fold.testing.performance =
                    caret::confusionMatrix(
                        factor(predictions, levels = dataset.class.factor.levels),
                        factor(dataset.imputed.testing[, ncol(dataset.imputed.testing)],
                               levels = dataset.class.factor.levels)
                    )

                unname(fold.testing.performance$overall[performance.selector])


            })

            inner.folds.performance = rbind(inner.folds.performance,
                data.frame(inner.fold      = j,
                           params.id       = 1:length(perfs),
                           performance     = perfs)
            )
        }

        assign(".Random.seed", extract.seed(seeds.1.outer, i), envir = .GlobalEnv)

        summarized.performance = aggregate(performance ~ params.id,
                                           data = inner.folds.performance, mean)

        selected.params = which.function(summarized.performance$performance)

        model             = models[[params.grid[selected.params, "model"]]]
        imputation.method = imputation.methods[[params.grid[selected.params,
                                                            "imputation.method"]]]

        training.folds = idx.inner[setdiff(1:no.folds, i)]
        testing.fold   = idx.inner[i]
        dataset.obscured.training =
            dataset.obscured.inner[as.numeric(unlist(training.folds)), ]
        dataset.obscured.testing  =
            dataset.obscured.inner[as.numeric(unlist(testing.fold)), ]

        preproc.scheme = attr(model, "preproc.scheme")
        dataset.obscured.training.preprocessed =
            stats::predict(preproc.scheme, dataset.obscured.training)
        dataset.obscured.testing.preprocessed =
            stats::predict(preproc.scheme, dataset.obscured.testing)

        imputation.scheme =
            imputation.method(dataset.obscured.training.preprocessed)

        dataset.imputed.testing =
            imputation.scheme(dataset.obscured.testing.preprocessed,
                              attr(imputation.scheme, "learned.obj"))

        predictions =
            suppressWarnings(stats::predict(model, dataset.imputed.testing))

        cf.matrix = suppressWarnings(
            caret::confusionMatrix(
                factor(predictions, levels = dataset.class.factor.levels),
                factor(dataset.imputed.testing[, ncol(dataset.imputed.testing)],
                       levels = dataset.class.factor.levels))
        )

        folds.performances = rbind(folds.performances,
            data.frame(Fold = i,
                       Missing.attributes = NA,
                       Accuracy     = cf.matrix$overall["Accuracy"],
                       Sensitivity  = cf.matrix$byClass["Sensitivity"],
                       Specificity  = cf.matrix$byClass["Specificity"]))

        for (j in 0:max(num.missing.attributes))
        {
            nma.selection =
                unname(unlist(testing.fold)) %in% which(unname(num.missing.attributes) == j)

            if (length(predictions[nma.selection]) == 0)
            {
                folds.performances = rbind(folds.performances,
                                           data.frame(Fold = i,
                                                      Missing.attributes = j,
                                                      Accuracy     = NA,
                                                      Sensitivity  = NA,
                                                      Specificity  = NA))
                next
            }

            cf.matrix = suppressWarnings(
                caret::confusionMatrix(
                    factor(predictions[nma.selection], levels = dataset.class.factor.levels),
                    factor(dataset.imputed.testing[nma.selection,
                                                   ncol(dataset.imputed.testing)],
                           levels = dataset.class.factor.levels))
            )

            folds.performances = rbind(folds.performances,
                data.frame(Fold = i,
                           Missing.attributes = j,
                           Accuracy     = cf.matrix$overall["Accuracy"],
                           Sensitivity  = cf.matrix$byClass["Sensitivity"],
                           Specificity  = cf.matrix$byClass["Specificity"]))
        }
    }

    flog.info("Phase 2")

    outer.folds.performance = data.frame()

    for (i in 1:no.folds)
    {
        flog.info(paste("Fold", i))

        assign(".Random.seed", extract.seed(seeds.2, i), envir = .GlobalEnv)

        training.folds = idx.outer[setdiff(1:no.folds, i)]
        testing.fold   = idx.outer[i]
        dataset.obscured.training =
            dataset.obscured[as.numeric(unlist(training.folds)), ]
        dataset.obscured.testing  =
            dataset.obscured[as.numeric(unlist(testing.fold)), ]

        perfs = apply(params.grid, 1, function(cfg)
        {
            model             = models[[cfg["model"]]]
            imputation.method = imputation.methods[[cfg["imputation.method"]]]

            preproc.scheme = attr(model, "preproc.scheme")
            dataset.obscured.training.preprocessed =
                stats::predict(preproc.scheme, dataset.obscured.training)
            dataset.obscured.testing.preprocessed =
                stats::predict(preproc.scheme, dataset.obscured.testing)

            imputation.scheme =
                imputation.method(dataset.obscured.training.preprocessed)

            dataset.imputed.testing =
                imputation.scheme(dataset.obscured.testing.preprocessed,
                                  attr(imputation.scheme, "learned.obj"))

            predictions =
                suppressWarnings(stats::predict(model, dataset.imputed.testing))


            fold.testing.performance =
                caret::confusionMatrix(
                    factor(predictions, levels = dataset.class.factor.levels),
                    factor(dataset.imputed.testing[, ncol(dataset.imputed.testing)],
                           levels = dataset.class.factor.levels)
                )

            unname(fold.testing.performance$overall[performance.selector])
        })

        outer.folds.performance = rbind(outer.folds.performance,
                                        data.frame(outer.fold      = i,
                                                   params.id       = 1:length(perfs),
                                                   performance     = perfs)
        )
    }

    flog.info("Choosing final model")

    assign(".Random.seed", random.seed, envir = .GlobalEnv)

    summarized.performance = aggregate(performance ~ params.id,
                                       data = outer.folds.performance, mean)

    selected.params = which.function(summarized.performance$performance)

    model             = models[[params.grid[selected.params, "model"]]]
    imputation.method = imputation.methods[[params.grid[selected.params,
                                                        "imputation.method"]]]

    preproc.scheme = attr(model, "preproc.scheme")
    dataset.obscured.preprocessed =
        stats::predict(preproc.scheme, dataset.obscured)

    imputation.scheme =
        imputation.method(dataset.obscured.preprocessed)

    return(list("model"              = model,
                "imputation.scheme"  = imputation.scheme,
                "imputation.name"    = attr(imputation.scheme, "imputation.name"),
                "folds.performances" = folds.performances))
}

nested.cross.validation.for.aggregation = function(aggregation.strategies,
                                                   dataset.folds,
                                                   no.folds,
                                                   performance.selector,
                                                   performance.maximize)
{
    # dataset.fold:
    #
    # | agg.outer.id | agg.inner.id | agg.type | agg.no.missing.att | agg.class | -
    # - | c1.lower | c1.upper | c2.lower | c2.upper | ... | cn.lower | cn.upper |
    #
    # agg.outer.id       : int { 1, ..., NCV.FOLDS }
    # agg.inner.id       : int { 1, ..., NCV.FOLDS, NA (if data is for Phase 2 of NCV) }
    # agg.type           : chr { training, testing }
    # agg.no.missing.att : int { 0, ... }
    # agg.class          : int { 0, 1 }
    # ci.lower, ci.upper : num [0.0, 1.0]

    # aggregation.strategies:
    #
    # group { 1, .., 8 }
    # subgroup { 1, ... }
    # configuration { 1, ... }
    # attributes:
    # - 1. function
    # - 2. codename
    # - 3. group name part 1
    # - 4. group name part 2

    interval.cols = grep("\\.(upper)|(lower)$", colnames(dataset.folds), value = TRUE)

    flog.info("Phase 1")

    folds.performances = data.frame()

    for (i in 1:no.folds)
    {
        flog.info(paste("Outer fold", i))

        inner.folds.performance = data.frame()

        for (j in 1:no.folds)
        {
            flog.info(paste("Inner fold", j))

            fold.training = dataset.folds[agg.outer.id == i &
                                          agg.inner.id == j &
                                          agg.type == "training",
                                          interval.cols, with = FALSE]

            fold.testing  = dataset.folds[agg.outer.id == i &
                                          agg.inner.id == j &
                                          agg.type == "testing",
                                          interval.cols, with = FALSE]

            fold.training.class = dataset.folds[agg.outer.id == i &
                                                agg.inner.id == j &
                                                agg.type == "training",
                                                agg.class]

            fold.testing.class  = dataset.folds[agg.outer.id == i &
                                                agg.inner.id == j &
                                                agg.type == "testing",
                                                agg.class]

            for (agg.group.id in 1:length(aggregation.strategies))
            {
                for (agg.subgroup.id in 1:length(aggregation.strategies[[agg.group.id]]))
                {
                    agg.funcs =
                        sapply(aggregation.strategies[[agg.group.id]][[agg.subgroup.id]],
                               "[[", 1)

                    agg.perf = foreach::foreach(
                        no.job   = 1:length(agg.funcs),
                        .export  = c("check.predictions"),
                        .combine = c) %dopar%
                    {
                        agg.func = agg.funcs[[no.job]]

                        fold.training.predictions =
                            apply(fold.training, 1, function(row.intervals){
                                agg.func(matrix(row.intervals, nrow = 2))
                            })

                        fold.training.predictions =
                            check.predictions(fold.training.predictions,
                                              fold.training.class)

                        fold.training.performance =
                            caret::confusionMatrix(
                                factor(fold.training.predictions, levels = c(0, 1)),
                                factor(fold.training.class,       levels = c(0, 1))
                            )

                        unname(fold.training.performance$overall[performance.selector])
                    }


                    agg.func.selected = agg.funcs[[
                        ifelse(performance.maximize, which.max, which.min)(agg.perf)
                    ]]

                    fold.testing.predictions =
                        apply(fold.testing, 1, function(row.intervals){
                            agg.func.selected(matrix(row.intervals, nrow = 2))
                        })

                    fold.testing.predictions =
                        check.predictions(fold.testing.predictions,
                                          fold.testing.class)

                    fold.testing.performance =
                        caret::confusionMatrix(
                            factor(fold.testing.predictions, levels = c(0, 1)),
                            factor(fold.testing.class,       levels = c(0, 1))
                        )

                    inner.folds.performance = rbind(inner.folds.performance,
                        data.frame(inner.fold      = j,
                                   agg.group.id    = agg.group.id,
                                   agg.subgroup.id = agg.subgroup.id,
                                   performance = unname(fold.testing.performance$overall[
                                       performance.selector])
                                   )
                    )


                }
            }
        }

        summarized.performance = aggregate(performance ~ agg.group.id + agg.subgroup.id,
                                           data = inner.folds.performance, mean)

        selected.agg.row.no = ifelse(performance.maximize, which.max, which.min)(
            summarized.performance$performance
        )

        selected.agg.group = summarized.performance$agg.group.id[selected.agg.row.no]
        selected.agg.subgroup = summarized.performance$agg.subgroup.id[selected.agg.row.no]

        fold.training = dataset.folds[agg.outer.id == i &
                                      is.na(agg.inner.id) &
                                      agg.type == "training",
                                      interval.cols, with = FALSE]

        fold.testing  = dataset.folds[agg.outer.id == i &
                                      is.na(agg.inner.id) &
                                      agg.type == "testing",
                                      interval.cols, with = FALSE]

        fold.training.class = dataset.folds[agg.outer.id == i &
                                            is.na(agg.inner.id) &
                                            agg.type == "training",
                                            agg.class]

        fold.testing.class  = dataset.folds[agg.outer.id == i &
                                            is.na(agg.inner.id) &
                                            agg.type == "testing",
                                            agg.class]

        agg.funcs =
            sapply(aggregation.strategies[[selected.agg.group]][[selected.agg.subgroup]],
                   "[[", 1)

        agg.perf = foreach::foreach(
            no.job   = 1:length(agg.funcs),
            .export  = c("check.predictions"),
            .combine = c) %dopar%
        {
            agg.func = agg.funcs[[no.job]]

            fold.training.predictions =
                apply(fold.training, 1, function(row.intervals){
                    agg.func(matrix(row.intervals, nrow = 2))
                })

            fold.training.predictions =
                check.predictions(fold.training.predictions,
                                  fold.training.class)

            fold.training.performance =
                caret::confusionMatrix(
                    factor(fold.training.predictions, levels = c(0, 1)),
                    factor(fold.training.class,       levels = c(0, 1))
                )

            unname(fold.training.performance$overall[performance.selector])
        }

        agg.func.selected = agg.funcs[[
            ifelse(performance.maximize, which.max, which.min)(agg.perf)
            ]]

        fold.testing.predictions =
            apply(fold.testing, 1, function(row.intervals){
                agg.func.selected(matrix(row.intervals, nrow = 2))
            })

        fold.testing.predictions =
            check.predictions(fold.testing.predictions,
                              fold.testing.class)

        cf.matrix =
            caret::confusionMatrix(
                factor(fold.testing.predictions, levels = c(0, 1)),
                factor(fold.testing.class,       levels = c(0, 1))
            )

        folds.performances = rbind(folds.performances,
            data.frame(Fold = i,
                       Missing.attributes = NA,
                       Accuracy     = cf.matrix$overall["Accuracy"],
                       Sensitivity  = cf.matrix$byClass["Sensitivity"],
                       Specificity  = cf.matrix$byClass["Specificity"]))

        num.missing.attributes =
            dataset.folds[agg.outer.id == i &
                              is.na(agg.inner.id) &
                              agg.type == "testing",
                          agg.no.missing.att]

        for (j in 0:max(num.missing.attributes))
        {
            nma.selection = which(num.missing.attributes == j)

            cf.matrix = suppressWarnings(
                caret::confusionMatrix(
                    factor(fold.testing.predictions[nma.selection], levels = c(0, 1)),
                    factor(fold.testing.class[nma.selection],       levels = c(0, 1))
                )
            )

            folds.performances = rbind(folds.performances,
                data.frame(Fold = i,
                           Missing.attributes = j,
                           Accuracy     = cf.matrix$overall["Accuracy"],
                           Sensitivity  = cf.matrix$byClass["Sensitivity"],
                           Specificity  = cf.matrix$byClass["Specificity"]))
        }
    }

    flog.info("Phase 2")

    outer.folds.performance = data.frame()

    for (i in 1:no.folds)
    {
        flog.info(paste("Fold", i))

        fold.training = dataset.folds[agg.outer.id == i &
                                      is.na(agg.inner.id) &
                                      agg.type == "training",
                                      interval.cols,
                                      with = FALSE]
        fold.testing  = dataset.folds[agg.outer.id == i &
                                      is.na(agg.inner.id) &
                                      agg.type == "testing",
                                      interval.cols,
                                      with = FALSE]

        fold.training.class = dataset.folds[agg.outer.id == i &
                                            is.na(agg.inner.id) &
                                            agg.type == "training",
                                            agg.class]

        fold.testing.class  = dataset.folds[agg.outer.id == i &
                                            is.na(agg.inner.id) &
                                            agg.type == "testing",
                                            agg.class]

        for (agg.group.id in 1:length(aggregation.strategies))
        {
            for (agg.subgroup.id in 1:length(aggregation.strategies[[agg.group.id]]))
            {
                agg.funcs =
                    sapply(aggregation.strategies[[agg.group.id]][[agg.subgroup.id]],
                           "[[", 1)

                agg.perf = foreach::foreach(
                    no.job   = 1:length(agg.funcs),
                    .export  = c("check.predictions"),
                    .combine = c) %dopar%
                {
                    agg.func = agg.funcs[[no.job]]

                    fold.training.predictions =
                        apply(fold.training, 1, function(row.intervals){
                            agg.func(matrix(row.intervals, nrow = 2))
                        })

                    fold.training.predictions =
                        check.predictions(fold.training.predictions,
                                          fold.training.class)

                    fold.training.performance =
                        caret::confusionMatrix(
                            factor(fold.training.predictions, levels = c(0, 1)),
                            factor(fold.training.class,       levels = c(0, 1))
                        )

                    unname(fold.training.performance$overall[performance.selector])
                }


                agg.func.selected = agg.funcs[[
                    ifelse(performance.maximize, which.max, which.min)(agg.perf)
                    ]]

                fold.testing.predictions =
                    apply(fold.testing, 1, function(row.intervals){
                        agg.func.selected(matrix(row.intervals, nrow = 2))
                    })

                fold.testing.predictions =
                    check.predictions(fold.testing.predictions,
                                      fold.testing.class)

                fold.testing.performance =
                    caret::confusionMatrix(
                        factor(fold.testing.predictions, levels = c(0, 1)),
                        factor(fold.testing.class,       levels = c(0, 1))
                    )

                outer.folds.performance = rbind(outer.folds.performance,
                    data.frame(outer.fold      = i,
                               agg.group.id    = agg.group.id,
                               agg.subgroup.id = agg.subgroup.id,
                               performance = unname(fold.testing.performance$overall[
                                   performance.selector])
                    )
                )
            }
        }
    }

    final.dataset = dataset.folds[is.na(agg.inner.id) &
                                  agg.type == "testing",
                                  interval.cols, with = FALSE]

    final.dataset.class = dataset.folds[is.na(agg.inner.id) &
                                        agg.type == "testing",
                                        agg.class]

    summarized.performance = aggregate(performance ~ agg.group.id + agg.subgroup.id,
                                       data = outer.folds.performance, mean)

    selected.agg.row.no = ifelse(performance.maximize, which.max, which.min)(
        summarized.performance$performance
    )

    selected.agg.group    = summarized.performance$agg.group.id[selected.agg.row.no]
    selected.agg.subgroup = summarized.performance$agg.subgroup.id[selected.agg.row.no]

    agg.funcs =
        sapply(aggregation.strategies[[selected.agg.group]][[selected.agg.subgroup]],
               "[[", 1)

    agg.perf = foreach::foreach(
        no.job   = 1:length(agg.funcs),
        .export  = c("check.predictions"),
        .combine = c) %dopar%
    {
        agg.func = agg.funcs[[no.job]]

        final.predictions =
            apply(final.dataset, 1, function(row.intervals){
                agg.func(matrix(row.intervals, nrow = 2))
            })

        final.predictions =
            check.predictions(final.predictions,
                              final.dataset.class)

        final.performance =
            caret::confusionMatrix(
                factor(final.predictions,   levels = c(0, 1)),
                factor(final.dataset.class, levels = c(0, 1))
            )

        unname(final.performance$overall[performance.selector])
    }

    selected.agg.configuration = ifelse(performance.maximize, which.max, which.min)(agg.perf)

    selected.agg = aggregation.strategies[[selected.agg.group]][[selected.agg.subgroup]][[
        selected.agg.configuration]]

    return(list("model"                = selected.agg[[1]],
                "aggregation.code"     = selected.agg[[2]],
                "aggregation.group"    = selected.agg[[3]],
                "aggregation.subgroup" = selected.agg[[4]],
                "folds.performances"   = folds.performances))
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
        stop.script("Unable to replace list of strings of different lengths")
    }

    for (i in 1:length(from))
    {
        base.string = gsub(from[i], to[i], base.string)
    }

    return(base.string)
}

stop.script = function(error)
{
    if (is.character(error))
    {
        flog.error(error)
    } else if ("message" %in% attributes(x)$names) {
        flog.error(error$message)
    } else {
        try(flog.error(getMessage(error)), silent = TRUE)
    }

    throw(error)
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

# https://stackoverflow.com/a/21568878
get.seeds <- function(init.seed, depths)
{
    RNGkind("L'Ecuyer-CMRG")
    set.seed(init.seed)

    seeds = vector("list", prod(depths))
    seeds[[1]] = .Random.seed
    for (i in seq_len(prod(depths) - 1))
    {
        seeds[[i + 1]] = nextRNGSubStream(seeds[[i]])
    }

    attr(seeds, "depths") = depths

    return(seeds)
}

extract.seed = function(seeds, positions)
{
    depths = attr(seeds, "depths")

    if (any(positions > depths))
    {
        stop.script("Bad index")
    }

    positions = positions - 1

    idx = 1 + positions[length(depths)]

    if (length(depths) > 1)
    {
        for (i in (length(depths) - 1):1)
        {
            idx = idx + positions[i] * prod( depths[length(depths):(i + 1)] )
        }
    }

    seeds[[idx]]
}

get.mode = function(v, na.rm = TRUE)
{
    uniqv = unique(v)
    if (na.rm)
    {
        uniqv = uniqv[!is.na(uniqv)]
    }
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

check.predictions = function(predictions, reference)
{
    if (length(predictions) != length(reference))
    {
        stop.script("Predictions length differ from reference")
    }

    if (any(is.na(predictions)))
    {
        ids = which(is.na(predictions))
        predictions[ids] = 1 - reference[ids]
    }

    return(predictions)
}
