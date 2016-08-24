# ---- step-6a-calculate-interval-predictions ----

source("init.R")
source("methods-interval-predictions.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S6A.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 6a: calculate interval predictions")

source("init-parallel.R")

seeds = get.seeds(SEED, c(length(DATASETS.NAMES), length(CLASSIFIERS.LIST) + 1))

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.interval.predictions.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.INTERVAL)

    if (!file.exists(dataset.interval.predictions.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        dataset.obscured.file.path =
            replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURED)

        dataset.obscured = readRDS(dataset.obscured.file.path)

        dataset.interval.predictions =
            data.frame(matrix(ncol = 2 * (length(CLASSIFIERS.LIST) + 1),
                              nrow = nrow(dataset.obscured),
                              data = 0))

        for (model.name in c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST))
        {
            .Random.seed =
                extract.seed(
                    seeds,
                    c(which(dataset.name == DATASETS.NAMES),
                      which(model.name == c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST))))

            flog.info(paste("Model:", model.name))

            model.file.path =
                replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                                c(dataset.name, model.name),
                                CLASSIFIERS.LEARNED)

            model = readRDS(model.file.path)

            colnames.id = 2 * which(c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST) == model.name)
            colnames(dataset.interval.predictions)[c(colnames.id - 1, colnames.id)] =
                paste0(model.name, c(".lower", ".upper"))

            used.predictors = attr(model, "used.predictors")
            preproc.scheme  = attr(model, "preproc.scheme")

            dataset.obscured.preprocessed = stats::predict(preproc.scheme, dataset.obscured)

            flog.info(paste("Cases:", nrow(dataset.obscured.preprocessed)))

            for (i in 1:nrow(dataset.obscured.preprocessed))
            {
                case.predictors.all =
                    dataset.obscured.preprocessed[i, -ncol(dataset.obscured.preprocessed)]
                case.class =
                    dataset.obscured.preprocessed[i,  ncol(dataset.obscured.preprocessed)]

                case.predictors.used = case.predictors.all[as.character(used.predictors)]

                interval.lower = NULL
                interval.upper = NULL

                if (all(!is.na(case.predictors.used)))
                {
                    # [1] All features are present
                    flog.info(paste("Case:", i, "- no optimization"))

                    predicted.value =
                        suppressWarnings(
                            stats::predict(model, case.predictors.all,
                                           type = "prob", na.action = NULL)[1, 1])

                    interval.lower = predicted.value
                    interval.upper = predicted.value

                } else {
                    features.factors =
                        names(which(sapply(colnames(case.predictors.used),
                                           function(x){
                                               is.factor(case.predictors.used[[x]])})
                                    == TRUE))

                    features.factors.nas =
                        features.factors[is.na(case.predictors.used[features.factors])]

                    features.numeric =
                        colnames(case.predictors.used)[!colnames(case.predictors.used)
                                                       %in% features.factors]

                    features.numeric.nas =
                        features.numeric[is.na(case.predictors.used[features.numeric])]

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

                            progress.bar =
                                utils::txtProgressBar(min   = 0,
                                                      max   = nrow(factors.configs),
                                                      style = 3)

                            predicted.values =
                                optim.factor.grid.search(model,
                                                         case.predictors.all,
                                                         factors.configs,
                                                         progress.bar)

                            close(progress.bar)

                            interval.lower = min(predicted.values)
                            interval.upper = max(predicted.values)

                        } else {
                            # [3] Factor and numeric features are not present
                            if (model.name %in% OPTIMIZATION.NUMERIC.BF.CLASSIFIERS)
                            {
                                # [3.1] Non-smooth, derivative-free optimization
                                flog.info(paste("Case:", i,
                                                "- factor-numeric ns/df optimization"))

                                eval.num.points =
                                    sapply(1:length(features.numeric.nas), function(x)
                                    {
                                        runif(length(features.numeric.nas) *
                                                  OPTIMIZATION.NUMERIC.BF.REPS, 0, 1)
                                    })

                                colnames(eval.num.points) = features.numeric.nas

                                eval.fac.num.points =
                                    expand.grid.df(factors.configs, eval.num.points)

                                progress.bar =
                                    utils::txtProgressBar(min   = 0,
                                                          max   = nrow(eval.fac.num.points),
                                                          style = 3)

                                predicted.values =
                                    optim.factor.numeric.nsdf(model,
                                                              case.predictors.all,
                                                              eval.fac.num.points,
                                                              progress.bar)

                                close(progress.bar)

                                interval.lower = min(predicted.values)
                                interval.upper = max(predicted.values)

                            } else {
                                # [3.2] Classic optimization
                                flog.info(paste("Case:", i,
                                                "- classic factor-numeric optimization"))

                                progress.bar =
                                    utils::txtProgressBar(min   = 0,
                                                          max   = nrow(factors.configs),
                                                          style = 3)

                                predicted.values =
                                    optim.factor.numeric.classic(
                                        model,
                                        case.predictors.all,
                                        factors.configs,
                                        features.numeric.nas,
                                        OPTIMIZATION.NUMERIC.REPS,
                                        OPTIMIZATION.NUMERIC.METHOD,
                                        progress.bar
                                    )

                                close(progress.bar)

                                interval.lower = min(predicted.values[1, ])
                                interval.upper = max(predicted.values[2, ])
                            }
                        }

                    } else {
                        # [4] Only numeric features are not present
                        if (model.name %in% OPTIMIZATION.NUMERIC.BF.CLASSIFIERS)
                        {
                            # [4.1] Non-smooth, derivative-free optimization
                            flog.info(paste("Case:", i, "- numeric ns/df optimization"))

                            eval.points =
                                sapply(1:length(features.numeric.nas), function(x)
                                {
                                    runif(length(features.numeric.nas) *
                                              OPTIMIZATION.NUMERIC.BF.REPS, 0, 1)
                                })

                            progress.bar =
                                utils::txtProgressBar(min   = 0,
                                                      max   = nrow(eval.points),
                                                      style = 3)

                            predicted.values =
                                optim.numeric.nsdf(model,
                                                   case.predictors.all,
                                                   features.numeric.nas,
                                                   eval.points,
                                                   progress.bar)

                            close(progress.bar)

                            interval.lower = min(predicted.values)
                            interval.upper = max(predicted.values)

                        } else {
                            # [4.2] Classic optimization
                            flog.info(paste("Case:", i, "- classic numeric optimization"))

                            start.values =
                                matrix(runif(OPTIMIZATION.NUMERIC.REPS *
                                                 length(features.numeric.nas), 0, 1),
                                       ncol = length(features.numeric.nas))

                            progress.bar =
                                utils::txtProgressBar(min   = 0,
                                                      max   = nrow(start.values),
                                                      style = 3)

                            predicted.values =
                                optim.numeric.classic(model,
                                                      case.predictors.all,
                                                      features.numeric.nas,
                                                      start.values,
                                                      OPTIMIZATION.NUMERIC.METHOD,
                                                      progress.bar)

                            close(progress.bar)

                            interval.lower = min(predicted.values[1, ])
                            interval.upper = max(predicted.values[2, ])
                        }
                    }
                }

                if (any(is.na(c(interval.lower, interval.upper))) ||
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

                dataset.interval.predictions[i, c(colnames.id - 1, colnames.id)] =
                    c(interval.lower, interval.upper)
            }

            flog.info(paste(rep("*", 10), collapse = ""))
        }

        dataset.interval.predictions =
            cbind(data.frame(obscure.level =
                                 sapply(1:nrow(dataset.obscured), function(x)
                                 {
                                    sum(is.na(dataset.obscured[x, ]))/(ncol(dataset.obscured) - 1)
                                 })),
                  dataset.interval.predictions)

        saveRDS(dataset.interval.predictions, dataset.interval.predictions.file.path)
    } else {
        flog.warn("Interval predictions exists, skipping calculations")
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}

stop.cluster()
