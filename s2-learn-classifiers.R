# ---- step-2-learn-classifiers ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S2.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 2: learn classifiers")

if (PARALLEL.COMPUTING)
{
    source("init-parallel.R")
}

if (!dir.exists(CLASSIFIERS.DIR))
{
    dir.create(CLASSIFIERS.DIR)
}

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    used.predictors = list()

    for (model.name in c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST))
    {
        flog.info(paste("Classifier:", model.name))

        dataset.feature.selection.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            DATASETS.FEATURE.SELECTION)
        dataset.classification.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            DATASETS.CLASSIFICATION)

        dataset.feature.selection = readRDS(dataset.feature.selection.file.path)
        dataset.classification    = readRDS(dataset.classification.file.path)

        model.file.path = replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                                          c(dataset.name, model.name),
                                          CLASSIFIERS.LEARNED)

        if (!file.exists(model.file.path) | OVERWRITE.OUTPUT.FILES)
        {
            fs.method = CLASSIFIERS.FEATURE.SELECTION.METHOD[[model.name]]

            if (!is.null(fs.method))
            {
                set.seed(SEED)

                flog.info(paste("Feature selection:", fs.method))

                rfe.seeds = vector(mode = "list", length = FEATURE.SELECTION.FOLDS + 1)
                for (i in 1:FEATURE.SELECTION.FOLDS)
                {
                    rfe.seeds[[i]] = sample.int(1000, ncol(dataset.feature.selection) - 1)
                }
                rfe.seeds[[FEATURE.SELECTION.FOLDS + 1]] = sample.int(1000, 1)

                fs.results =
                    rfe(dataset.feature.selection[, 1:(ncol(dataset.feature.selection) - 1)],
                        dataset.feature.selection[, ncol(dataset.feature.selection)],
                        sizes = 1:ncol(dataset.feature.selection),
                        rfeControl = rfeControl(functions = eval(as.name(fs.method)),
                                                method = "cv",
                                                number = FEATURE.SELECTION.FOLDS,
                                                seeds  = rfe.seeds,
                                                allowParallel = CARET.ALLOW.PARALLEL))

                flog.info(paste("Selected", length(predictors(fs.results)),
                                "from", ncol(dataset.feature.selection) - 1, "features"))

                dataset.classification =
                    dataset.classification[, c(predictors(fs.results),
                                               tail(colnames(dataset.classification), 1))]

            } else {
                flog.info("Internal feature selection")
                flog.info("Enlarging classification dataset")

                dataset.classification = rbind(dataset.feature.selection,
                                               dataset.classification)
            }

            model.grid  = CLASSIFIERS.TUNING.PARAMS[[model.name]]
            model.attrs = CLASSIFIERS.BASIC.ATTRIBUTES[[model.name]]

            model = nested.cross.validation(dataset.classification, model.name,
                                            model.grid, model.attrs, NCV.FOLDS,
                                            NCV.PREPROCESSING.METHODS,
                                            NCV.PERFORMANCE.SELECTOR,
                                            NCV.PERFORMANCE.MAXIMIZE,
                                            CARET.ALLOW.PARALLEL, SEED)

            if (model.name == "OneR")
            {
                .jcache(model$finalModel$classifier)
            }

            saveRDS(model, model.file.path)
        } else {
            flog.warn("Model exists, skipping learning")

            model = readRDS(model.file.path)
        }

        folds.performance = attr(model, "folds.performance")

        flog.info(paste0("Estimated ", NCV.PERFORMANCE.SELECTOR, ": ",
                         round(mean(folds.performance[[NCV.PERFORMANCE.SELECTOR]]), 3)))

        flog.info(paste("Using", length(attr(model, "used.predictors")),
                        "of", ncol(dataset.feature.selection) - 1, "features"))

        used.predictors[[model.name]] = attr(model, "used.predictors")

        flog.info(paste(rep("*", 10), collapse = ""))
    }

    used.predictors.row.names =
        colnames(dataset.feature.selection)[-ncol(dataset.feature.selection)]

    flog.info("Features used by classifiers:")
    flog.info(capture.output(used.predictors.as.table(used.predictors,
                                                      used.predictors.row.names)))

    flog.info(paste(rep("*", 25), collapse = ""))
}

if (PARALLEL.COMPUTING)
{
    stop.cluster(cl)
}
