# ---- step-4-1-classifiers-original-performance ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S4.1.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 4-1: original classifiers performance on obscured dataset")

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURED)

    dataset.obscured = readRDS(dataset.obscured.file.path)

    for (model.name in c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST))
    {
        flog.info(paste("Classifier:", model.name))

        classifier.performance.original.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            CLASSIFIERS.PERFORMANCE.ORIGINAL)

        if (!file.exists(classifier.performance.original.file.path) | OVERWRITE.OUTPUT.FILES)
        {
            performance.df = data.frame()

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

            flog.info("Overall:")
            flog.info(paste("  Accuracy:    ", round(cf.matrix$overall["Accuracy"], 3)))
            flog.info(paste("  Sensitivity: ", round(cf.matrix$byClass["Sensitivity"], 3)))
            flog.info(paste("  Specificity: ", round(cf.matrix$byClass["Specificity"], 3)))
            flog.info(paste("  Decisiveness:",
                            round(nrow(dataset.no.nas)/nrow(dataset.obscured), 3)))

            performance.df = rbind(performance.df,
                   data.frame(Missing.attributes = NA,
                              Accuracy     = cf.matrix$overall["Accuracy"],
                              Sensitivity  = cf.matrix$byClass["Sensitivity"],
                              Specificity  = cf.matrix$byClass["Specificity"],
                              Decisiveness = nrow(dataset.no.nas)/nrow(dataset.obscured)))

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

                    performance.df = rbind(performance.df,
                        data.frame(Missing.attributes = num.missing.attr,
                                   Accuracy     = cf.matrix$overall["Accuracy"],
                                   Sensitivity  = cf.matrix$byClass["Sensitivity"],
                                   Specificity  = cf.matrix$byClass["Specificity"],
                                   Decisiveness = nrow(dataset.no.nas)/nrow(dataset.num.miss.attr)))

                } else {
                    flog.info("  Accuracy:     -")
                    flog.info("  Sensitivity:  -")
                    flog.info("  Specificity:  -")
                    flog.info("  Decisiveness: 0")

                    performance.df = rbind(performance.df,
                       data.frame(Missing.attributes = num.missing.attr,
                                  Accuracy     = NA,
                                  Sensitivity  = NA,
                                  Specificity  = NA,
                                  Decisiveness = 0.0))
                }
            }

            rownames(performance.df) = NULL

            saveRDS(performance.df, classifier.performance.original.file.path)

        } else {
            flog.warn("Original classifier performance already calculated, skipping")

            performance.df = readRDS(classifier.performance.original.file.path)

            flog.info("Overall:")
            flog.info(paste("  Accuracy:    ", round(performance.df[1, "Accuracy"], 3)))
            flog.info(paste("  Sensitivity: ", round(performance.df[1, "Sensitivity"], 3)))
            flog.info(paste("  Specificity: ", round(performance.df[1, "Specificity"], 3)))
            flog.info(paste("  Decisiveness:", round(performance.df[1, "Decisiveness"], 3)))

            for (i in 2:nrow(performance.df))
            {
                flog.info(paste("Missing attributes:", performance.df[i, "Missing.attributes"]))
                flog.info(paste("  Accuracy:    ", round(performance.df[i, "Accuracy"], 3)))
                flog.info(paste("  Sensitivity: ", round(performance.df[i, "Sensitivity"], 3)))
                flog.info(paste("  Specificity: ", round(performance.df[i, "Specificity"], 3)))
                flog.info(paste("  Decisiveness:", round(performance.df[i, "Decisiveness"], 3)))
            }
        }

        flog.info(paste(rep("*", 10), collapse = ""))
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}
