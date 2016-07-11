# ---- step-6a-calculate-interval-predictions ----

source("init.R")

setup.logger(LOGGER.OUTPUT.S6A.FILE)

flog.info("Step 6a: calculate interval predictions")

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.interval.path = file.path("datasets", paste0(dataset.name, "-interval.rds"))

    if (file.exists(dataset.interval.path))
    {
        flog.info("Interval predictions exists, skipping calculations")
        next
    }

    dataset.obscured =
        readRDS(file.path("datasets", paste0(dataset.name, "-obscured.rds")))

    dataset.interval.predictions =
        data.frame(matrix(ncol = 2*length(classifiers.list),
                          nrow = nrow(dataset.obscured),
                          data = 0))

    for (model.name in classifiers.list)
    {
        set.seed(SEED)

        flog.info(paste("Model:", model.name))

        model = readRDS(file.path("models",
                                  paste0(dataset.name, "-", model.name, ".rds")))

        colnames.id = 2 * which(classifiers.list == model.name)
        colnames(dataset.interval.predictions)[c(colnames.id - 1, colnames.id)] =
            paste0(model.name, c(".lower", ".upper"))

        used.predictors = attr(model, "used.predictors")
        preproc.scheme  = attr(model, "preproc.scheme")

        dataset.obscured.preprocessed = stats::predict(preproc.scheme, dataset.obscured)

        for (i in 1:nrow(dataset.obscured.preprocessed))
        {
            case.predictors.all =
                dataset.obscured.preprocessed[i, -ncol(dataset.obscured.preprocessed)]
            case.class =
                dataset.obscured.preprocessed[i,  ncol(dataset.obscured.preprocessed)]

            case.predictors.used = case.predictors.all[, as.character(used.predictors)]

            if (all(!is.na(case.predictors.used)))
            {
                flog.info(paste("Case:", i, "- no opt."))

                predicted.value = stats::predict(model, case.predictors.all,
                                                 type = "prob", na.action = NULL)[1, 1]

                dataset.interval.predictions[i, c(colnames.id - 1, colnames.id)] =
                    predicted.value

            } else {
                features.factors =
                    names(which(sapply(colnames(case.predictors.used),
                                       function(x){ any(class(case.predictors.used[[x]])
                                                        == c("ordered", "factor"))})
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
                        flog.info(paste("Case:", i, "- factor opt."))

                        prog.bar =
                            utils::txtProgressBar(min   = 0,
                                                  max   = nrow(factors.configs),
                                                  style = 3)

                        predicted.values =
                            sapply(1:nrow(factors.configs), function(j)
                            {
                                utils::setTxtProgressBar(prog.bar, j)

                                case.config = case.predictors.all # copy

                                for (k in 1:ncol(factors.configs))
                                {
                                    case.config[[colnames(factors.configs)[k]]] =
                                        factors.configs[j, k]
                                }

                                stats::predict(model, case.config,
                                               type = "prob", na.action = NULL)[1, 1]
                            })

                        close(prog.bar)

                        dataset.interval.predictions[i, c(colnames.id - 1, colnames.id)] =
                            c(min(predicted.values), max(predicted.values))

                    } else {
                        if (model.name %in% opt.bf.num.classifiers)
                        {
                            flog.info(paste("Case:", i, "- b.f. factor-numeric opt."))

                            eval.num.points =
                                sapply(1:length(features.numeric.nas), function(x)
                                {
                                    runif(length(features.numeric.nas) *
                                              numeric.bf.optimization.reps, 0, 1)
                                })

                            colnames(eval.num.points) = features.numeric.nas

                            eval.fac.num.points =
                                expand.grid.df(factors.configs, eval.num.points)

                            prog.bar =
                                utils::txtProgressBar(min   = 0,
                                                      max   = nrow(eval.fac.num.points),
                                                      style = 3)

                            est.vals =
                                sapply(1:nrow(eval.fac.num.points), function(x)
                                {
                                    utils::setTxtProgressBar(prog.bar, x)

                                    case.config = case.predictors.all # copy

                                    for (j in 1:ncol(eval.fac.num.points))
                                    {
                                        case.config[[colnames(eval.fac.num.points)[j]]] =
                                            eval.fac.num.points[x, j]
                                    }

                                    stats::predict(model, case.config,
                                                   type = "prob", na.action = NULL)[1, 1]
                                })

                            close(prog.bar)

                            dataset.interval.predictions[i,
                                                         c(colnames.id - 1, colnames.id)] =
                                c(min(est.vals), max(est.vals))

                        } else {
                            flog.info(paste("Case:", i, "- std. factor-numeric opt."))

                            prog.bar =
                                utils::txtProgressBar(min   = 0,
                                                      max   = nrow(factors.configs),
                                                      style = 3)

                            predicted.values =
                                sapply(1:nrow(factors.configs), function(j)
                                {
                                    utils::setTxtProgressBar(prog.bar, j)

                                    case.config = case.predictors.all # copy

                                    for (k in 1:ncol(factors.configs))
                                    {
                                        case.config[[colnames(factors.configs)[k]]] =
                                            factors.configs[j, k]
                                    }

                                    targetOptFunc2 = function(x)
                                    {
                                        case.config2 = case.config # copy

                                        for (j in 1:length(features.numeric.nas))
                                        {
                                            case.config2[[features.numeric.nas[j]]] = x[j]
                                        }

                                        stats::predict(model, case.config2,
                                                       type = "prob",
                                                       na.action = NULL)[1, 1]
                                    }

                                    start.values =
                                        matrix(runif(numeric.optimization.reps *
                                                         length(features.numeric.nas),
                                                     0, 1),
                                               ncol = length(features.numeric.nas))
                                    lower.values = rep(0  , length(features.numeric.nas))
                                    upper.values = rep(1  , length(features.numeric.nas))

                                    minmax.vals =
                                        apply(start.values, 1, function(y)
                                        {
                                            opt.objs = t(sapply(c(FALSE, TRUE), function(x)
                                            {
                                                capture.output(
                                                    opt.obj <-
                                                        optimx(par     = y,
                                                               fn      = targetOptFunc2,
                                                               method  = opt.numeric.method,
                                                               lower   = lower.values,
                                                               upper   = upper.values,
                                                               control = list(
                                                                   kkt = FALSE,
                                                                   maximize = x,
                                                                   save.failures = TRUE,
                                                                   maxit = 2500,
                                                                   dowarn = FALSE)))
                                                opt.obj
                                            }))

                                            opt.objs = data.frame(opt.objs)

                                            if (any(unlist(opt.objs$convcode) != 0))
                                            {
                                                flog.warn(
                                                    paste("Numeric optimization: convcodes",
                                                          "not equal to 0"))
                                            }

                                            opt.values = unlist(opt.objs$value)

                                            if (any(is.na(opt.values)))
                                            {
                                                flog.error(
                                                    paste("Numeric optimization:",
                                                          "some values equal to NA"))
                                            }

                                            opt.values
                                        })

                                    c(min(minmax.vals[1, ]), max(minmax.vals[2, ]))
                                })

                            close(prog.bar)

                            dataset.interval.predictions[i, c(colnames.id - 1, colnames.id)] =
                                c(min(predicted.values[1, ]), max(predicted.values[2, ]))

                        }
                    }

                } else {
                    if (model.name %in% opt.bf.num.classifiers)
                    {
                        flog.info(paste("Case:", i, "- bf. numeric opt."))

                        eval.points =
                            sapply(1:length(features.numeric.nas), function(x)
                            {
                                runif(length(features.numeric.nas) *
                                          numeric.bf.optimization.reps, 0, 1)
                            })

                        est.vals =
                            apply(eval.points, 1, function(x)
                            {
                                case.config = case.predictors.all # copy

                                for (j in 1:length(features.numeric.nas))
                                {
                                    case.config[[features.numeric.nas[j]]] = x[j]
                                }

                                stats::predict(model, case.config,
                                               type = "prob", na.action = NULL)[1, 1]
                            })

                        dataset.interval.predictions[i, c(colnames.id - 1, colnames.id)] =
                            c(min(est.vals), max(est.vals))

                    } else {
                        flog.info(paste("Case:", i, "- std. numeric opt."))

                        targetOptFunc = function(x)
                        {
                            case.config = case.predictors.all # copy

                            for (j in 1:length(features.numeric.nas))
                            {
                                case.config[[features.numeric.nas[j]]] = x[j]
                            }

                            stats::predict(model, case.config,
                                           type = "prob", na.action = NULL)[1, 1]
                        }

                        start.values =
                            matrix(runif(numeric.optimization.reps *
                                             length(features.numeric.nas), 0, 1),
                                   ncol = length(features.numeric.nas))
                        lower.values = rep(0  , length(features.numeric.nas))
                        upper.values = rep(1  , length(features.numeric.nas))

                        prog.bar =
                            utils::txtProgressBar(min   = 0,
                                                  max   = nrow(start.values),
                                                  style = 3)

                        minmax.vals =
                            sapply(1:nrow(start.values), function(z)
                            {
                                utils::setTxtProgressBar(prog.bar, z)

                                y = start.values[z, ]

                                opt.objs = t(sapply(c(FALSE, TRUE), function(x)
                                {
                                    capture.output(
                                        opt.obj <-
                                            optimx(par     = y,
                                                   fn      = targetOptFunc,
                                                   method  = opt.numeric.method,
                                                   lower   = lower.values,
                                                   upper   = upper.values,
                                                   control = list(kkt           = FALSE,
                                                                  maximize      = x,
                                                                  save.failures = TRUE,
                                                                  maxit         = 2500,
                                                                  dowarn        = FALSE)))
                                    opt.obj
                                }))

                                opt.objs = data.frame(opt.objs)

                                if (any(unlist(opt.objs$convcode) != 0))
                                {
                                    flog.warn(paste("Numeric optimization: convcodes",
                                                    "not equal to 0"))
                                }

                                opt.values = unlist(opt.objs$value)

                                if (any(is.na(opt.values)))
                                {
                                    flog.error(paste("Numeric optimization: some values",
                                                     "equal to NA"))
                                }

                                opt.values
                            })

                        close(prog.bar)

                        dataset.interval.predictions[i, c(colnames.id - 1, colnames.id)] =
                            c(min(minmax.vals[1, ]), max(minmax.vals[2, ]))
                    }
                }

                vals = dataset.interval.predictions[i, c(colnames.id - 1, colnames.id)]


                if (vals[[1]] > vals[[2]])
                {
                    flog.error("Lower bound greater than upper bound")
                }
            }
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

    saveRDS(dataset.interval.path, dataset.interval.predictions)

    flog.info(paste(rep("*", 25), collapse = ""))
}
