# ---- step-4-2-classifiers-interval-performance ----

source("init.R")
source("methods-interval-predictions.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S4.2.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 4-2: interval classifiers performance on obscured dataset")

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

            dataset.interval = data.frame()

            for (i in 1:nrow(dataset.obscured.preprocessed))
            {
                case.predictors.all = dataset.obscured.preprocessed[
                    i, 1:(ncol(dataset.obscured.preprocessed) - 1), drop = FALSE]
                case.class = dataset.obscured.preprocessed[
                    i, ncol(dataset.obscured.preprocessed), drop = FALSE]

                case.predictors.used = case.predictors.all[
                    , as.character(attr(model, "used.predictors")), drop = FALSE]

                interval.lower = NULL
                interval.upper = NULL

                if (all(!is.na(case.predictors.used)))
                {
                    # [1] All features are present
                    flog.info(paste("Case:", i, "- no optimization"))

                    predicted.value =
                        suppressWarnings(
                            stats::predict(model, case.predictors.all,
                                           type = "prob", na.action = NULL)[1, 2])

                    interval.lower = predicted.value
                    interval.upper = predicted.value

                } else {

                    features.factors =
                        names(which(sapply(colnames(case.predictors.used),
                                           function(x){
                                               is.factor(case.predictors.used[[x]])})
                                    == TRUE))

                    features.factors.nas =
                        features.factors[is.na(case.predictors.used[
                            , features.factors, drop = FALSE])]

                    features.numeric =
                        colnames(case.predictors.used)[!colnames(case.predictors.used)
                                                       %in% features.factors]

                    features.numeric.nas =
                        features.numeric[is.na(case.predictors.used[
                            , features.numeric, drop = FALSE])]

                    if (length(features.factors.nas) > 0)
                    {
                        factors.configs =
                            expand.grid(sapply(features.factors.nas,
                                               function(x){levels(case.predictors.used[[x]])},
                                               simplify = FALSE))

                        colnames(factors.configs) = features.factors.nas

                        if (length(features.numeric.nas) == 0)
                        {
                            # [2] Only factor features are not present
                            flog.info(paste("Case:", i, "- factor grid search"))

                            predicted.values =
                                optim.factor.grid.search(model,
                                                         case.predictors.all,
                                                         factors.configs)

                            interval.lower = min(predicted.values)
                            interval.upper = max(predicted.values)

                        } else {
                            # [3] Factor and numeric features are not present
                            if (model.name %in% OPTIMIZATION.NUMERIC.BF.CLASSIFIERS)
                            {
                                # [3.1] Non-smooth, derivative-free optimization
                                flog.info(paste("Case:", i,
                                                "- factor-numeric ns/df optimization"))

                                predicted.values =
                                    optim.factor.numeric.nsdf(model,
                                                              case.predictors.all,
                                                              factors.configs,
                                                              features.numeric.nas,
                                                              OPTIMIZATION.NUMERIC.BF.REPS)

                                interval.lower = min(predicted.values)
                                interval.upper = max(predicted.values)

                            } else {
                                # [3.2] Classic optimization
                                flog.info(paste("Case:", i,
                                                "- classic factor-numeric optimization"))

                                predicted.values =
                                    optim.factor.numeric.classic(
                                        model,
                                        case.predictors.all,
                                        factors.configs,
                                        features.numeric.nas,
                                        OPTIMIZATION.NUMERIC.REPS,
                                        OPTIMIZATION.NUMERIC.METHOD
                                    )

                                interval.lower = min(predicted.values[, 1])
                                interval.upper = max(predicted.values[, 2])
                            }
                        }

                    } else {

                        # [4] Only numeric features are not present
                        if (model.name %in% OPTIMIZATION.NUMERIC.BF.CLASSIFIERS)
                        {
                            # [4.1] Non-smooth, derivative-free optimization
                            flog.info(paste("Case:", i, "- numeric ns/df optimization"))

                            predicted.values =
                                optim.numeric.nsdf(model,
                                                   case.predictors.all,
                                                   features.numeric.nas,
                                                   OPTIMIZATION.NUMERIC.BF.REPS)

                            interval.lower = min(predicted.values)
                            interval.upper = max(predicted.values)

                        } else {
                            # [4.2] Classic optimization
                            flog.info(paste("Case:", i, "- classic numeric optimization"))

                            predicted.values =
                                optim.numeric.classic(model,
                                                      case.predictors.all,
                                                      features.numeric.nas,
                                                      OPTIMIZATION.NUMERIC.REPS,
                                                      OPTIMIZATION.NUMERIC.METHOD)

                            interval.lower = min(predicted.values[1, ])
                            interval.upper = max(predicted.values[2, ])
                        }
                    }
                }


                if (any(is.na(  c(interval.lower, interval.upper))) ||
                    any(is.null(c(interval.lower, interval.upper))))
                {
                    stop.script("Lower and/or upper bound is NA/NULL")
                }

                if (interval.lower > interval.upper)
                {
                    stop.script("Lower bound greater than upper bound")
                }

                flog.info(paste0("Predicted interval: [",
                                 round(interval.lower, 4), ", ",
                                 round(interval.upper, 4), "]"))

                dataset.interval = rbind(dataset.interval,
                    data.frame(lower = interval.lower,
                               upper = interval.upper))
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
                        caret::confusionMatrix(dataset.filtered.nat.complete$Predictions,
                                               dataset.filtered.nat.complete$Reference))

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

    flog.info(paste(rep("*", 25), collapse = ""))
}
