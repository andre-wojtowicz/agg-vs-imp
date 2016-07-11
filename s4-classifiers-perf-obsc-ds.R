# ---- step-4-classifiers-performance-on-obscured-dataset ----

source("init.R")

setupLogger(LOGGER.OUTPUT.S4.FILE)

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
