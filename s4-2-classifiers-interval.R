# ---- step-4-2-classifiers-interval-performance ----

source("init.R")
source("methods-interval-predictions.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S4.2.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 4-2: interval classifiers performance on obscured dataset")

source("init-parallel.R")

seeds = get.seeds(SEED, c(length(DATASETS.NAMES), length(CLASSIFIERS.LIST) + 1))

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURED)

    dataset.obscured = readRDS(dataset.obscured.file.path)

    for (model.name in c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST))
    {
        flog.info(paste("Classifier:", model.name))

        classifier.performance.interval.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            CLASSIFIERS.PERFORMANCE.INTERVAL)

        if (!file.exists(classifier.performance.interval.file.path) | OVERWRITE.OUTPUT.FILES)
        {
            .Random.seed =
                extract.seed(
                    seeds,
                    c(which(dataset.name == DATASETS.NAMES),
                      which(model.name == c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST))))

            performance.df = data.frame()

            model.file.path =
                replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                                c(dataset.name, model.name),
                                CLASSIFIERS.LEARNED)

            model = readRDS(model.file.path)

            preproc.scheme = attr(model, "preproc.scheme")
            dataset.obscured.preprocessed =
                stats::predict(preproc.scheme, dataset.obscured)

            dataset.interval =
                foreach::foreach(
                    i = 1:nrow(dataset.obscured.preprocessed),
                    .combine       = rbind,
                    .multicombine  = TRUE,
                    .maxcombine    = nrow(dataset.obscured.preprocessed),
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

            dataset.binded.complete = dataset.binded %>%
                filter(!is.na(Predictions))

            cf.matrix = suppressWarnings(
                caret::confusionMatrix(dataset.binded.complete$Predictions,
                                       dataset.binded.complete$Reference))

            flog.info("Overall:")
            flog.info(paste("  Accuracy:    ", round(cf.matrix$overall["Accuracy"], 3)))
            flog.info(paste("  Sensitivity: ", round(cf.matrix$byClass["Sensitivity"], 3)))
            flog.info(paste("  Specificity: ", round(cf.matrix$byClass["Specificity"], 3)))
            flog.info(paste("  Decisiveness:",
                            round(nrow(dataset.binded.complete)/nrow(dataset.binded), 3)))

            performance.df = rbind(performance.df,
                   data.frame(Missing.attributes = NA,
                              Accuracy     = cf.matrix$overall["Accuracy"],
                              Sensitivity  = cf.matrix$byClass["Sensitivity"],
                              Specificity  = cf.matrix$byClass["Specificity"],
                              Decisiveness = nrow(dataset.binded.complete)/
                                  nrow(dataset.binded)))

            for (num.missing.attr in 0:max(dataset.binded$Missing.attribues))
            {
                flog.info(paste("Missing attributes:", num.missing.attr))

                dataset.filtered.nat = dataset.binded %>%
                    filter(Missing.attribues == num.missing.attr)

                dataset.filtered.nat.complete = dataset.filtered.nat %>%
                    filter(!is.na(Predictions))

                if (nrow(dataset.filtered.nat.complete) > 0)
                {
                    cf.matrix = suppressWarnings(
                        caret::confusionMatrix(
                            factor(dataset.filtered.nat.complete$Predictions, levels = c(0, 1)),
                            factor(dataset.filtered.nat.complete$Reference,   levels = c(0, 1))
                        )
                    )

                    flog.info(paste("  Accuracy:    ", round(cf.matrix$overall["Accuracy"], 3)))
                    flog.info(paste("  Sensitivity: ", round(cf.matrix$byClass["Sensitivity"], 3)))
                    flog.info(paste("  Specificity: ", round(cf.matrix$byClass["Specificity"], 3)))
                    flog.info(paste("  Decisiveness:",
                                    round(nrow(dataset.filtered.nat.complete)/
                                              nrow(dataset.filtered.nat), 3)))

                    performance.df = rbind(performance.df,
                        data.frame(Missing.attributes = num.missing.attr,
                                   Accuracy     = cf.matrix$overall["Accuracy"],
                                   Sensitivity  = cf.matrix$byClass["Sensitivity"],
                                   Specificity  = cf.matrix$byClass["Specificity"],
                                   Decisiveness = nrow(dataset.filtered.nat.complete)/
                                       nrow(dataset.filtered.nat)))

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

            saveRDS(performance.df, classifier.performance.interval.file.path)

        } else {
            flog.warn("Interval classifier performance already calculated, skipping")

            performance.df = readRDS(classifier.performance.interval.file.path)

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

    ## additional CV

    classifier.performance.interval.cv.file.path =
        replace.strings(c(DATASETS.NAME.PATTERN),
                        c(dataset.name),
                        CLASSIFIERS.PERFORMANCE.INTERVAL.CV)

    if (!file.exists(classifier.performance.interval.cv.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        flog.info("Additional CV")

        seed.cv = extract.seed(seeds,
                               c(which(dataset.name == DATASETS.NAMES), 2))

        unc.folds.performance =
            cross.validation.unc(dataset.obscured,
                                  CLASSIFIERS.LIST,
                                  NCV.FOLDS,
                                  NCV.PERFORMANCE.SELECTOR,
                                  NCV.PERFORMANCE.MAXIMIZE,
                                  seed.cv)

        saveRDS(unc.folds.performance, classifier.performance.interval.cv.file.path)
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}

stop.cluster()
