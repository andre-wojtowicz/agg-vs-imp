# ---- step-2-learn-classifiers ----

source("init.R")

setup.logger(LOGGER.OUTPUT.S2.FILE)

flog.info("Step 2: learn classifiers")

if (!dir.exists(CLASSIFIERS.DIR))
{
    dir.create(CLASSIFIERS.DIR)
}

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    for (model.name in c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST))
    {
        flog.info(paste("Classifier:", model.name))

        model.file.path = replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                                          c(dataset.name, model.name),
                                          CLASSIFIERS.LEARNED)

        if (file.exists(model.file.path))
        {
            flog.info("Model exists, skipping learning")

            model = readRDS(model.file.path)

            folds.performance = attr(model, "folds.performance")

            flog.info(paste0("Estimated ", NCV.PERFORMANCE.SELECTOR, ": ",
                             round(mean(folds.performance[[NCV.PERFORMANCE.SELECTOR]]), 3)))

            flog.info(paste(rep("*", 10), collapse = ""))

            next
        }

        dataset.feature.selection.file.path =
            replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.FEATURE.SELECTION)
        dataset.classification.file.path =
            replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.CLASSIFICATION)

        dataset.feature.selection = readRDS(dataset.feature.selection.file.path)
        dataset.classification    = readRDS(dataset.classification.file.path)


        fs.method = CLASSIFIERS.FEATURE.SELECTION.METHOD[[model.name]]

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

        model.grid  = CLASSIFIERS.TUNING.PARAMS[[model.name]]
        model.attrs = CLASSIFIERS.BASIC.ATTRIBUTES[[model.name]]

        model = nested.cross.validation(dataset.classification, NCV.FOLDS,
                                        model.name, model.grid, model.attrs)

        if (model.name == "OneR")
        {
            .jcache(model$finalModel$classifier)
        }

        saveRDS(model, model.file.path)

        flog.info(paste(rep("*", 10), collapse = ""))
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}
