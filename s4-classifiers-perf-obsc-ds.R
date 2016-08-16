# ---- step-4-classifiers-performance-on-obscured-dataset ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S4.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 4: classifiers performance on obscured dataset")

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURED)

    dataset.obscured = readRDS(dataset.obscured.file.path)

    for (model.name in c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST))
    {
        flog.info(paste("Classifier:", model.name))

        model.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            CLASSIFIERS.LEARNED)

        model = readRDS(model.file.path)

        preproc.scheme = attr(model, "preproc.scheme")
        dataset.obscured.preprocessed =
            stats::predict(preproc.scheme, dataset.obscured)

        dataset.no.nas =
            dataset.obscured.preprocessed[which(rowSums(is.na(
                dataset.obscured.preprocessed[
                    as.character(attr(model, "used.predictors"))])) == 0), ]

        predictions =
            suppressWarnings(stats::predict(model, dataset.no.nas,
                                            na.action = NULL))
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
