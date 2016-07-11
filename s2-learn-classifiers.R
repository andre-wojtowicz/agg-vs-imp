# ---- step-2-learn-classifiers ----

source("init.R")

setupLogger(LOGGER.OUTPUT.S2.FILE)

flog.info("Step 2: learn classifiers")

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
            flog.info("Model exists, skipping learning")

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
