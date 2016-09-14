optim.factor.grid.search = function(model, case.predictors.all, factors.configs)
{
    cases = foreach::foreach(idx = 1:nrow(factors.configs),
                             .combine = rbind) %do%
    {
        case.config = case.predictors.all # copy

        for (k in 1:ncol(factors.configs))
        {
            case.config[[colnames(factors.configs)[k]]] =
                factors.configs[idx, k]
        }

        case.config
    }

    suppressWarnings(stats::predict(model, cases,
                                    type = "prob", na.action = NULL)[, 2])
}

optim.numeric.classic = function(model, case.predictors.all, features.numeric.nas,
                                 start.values.reps, optimization.numeric.method)
{
    start.values =
        matrix(runif(start.values.reps * length(features.numeric.nas), 0, 1),
               ncol = length(features.numeric.nas))

    target.function = function(x)
    {
        case.config = case.predictors.all # copy

        for (j in 1:length(features.numeric.nas))
        {
            case.config[[features.numeric.nas[j]]] = x[j]
        }

        suppressWarnings(stats::predict(model, case.config,
                                        type = "prob", na.action = NULL)[1, 2])
    }

    lower.values = rep(0, length(features.numeric.nas))
    upper.values = rep(1, length(features.numeric.nas))

    foreach::foreach(idx = 1:nrow(start.values),
                     .packages = "optimx",
                     .combine  = cbind) %do%
    {
        opt.objs = t(sapply(c(FALSE, TRUE), function(maximize.opt)
        {
            capture.output(
                opt.obj <-
                    optimx(par     = start.values[idx, ],
                           fn      = target.function,
                           method  = optimization.numeric.method,
                           lower   = lower.values,
                           upper   = upper.values,
                           control = list(kkt           = FALSE,
                                          maximize      = maximize.opt,
                                          save.failures = TRUE,
                                          maxit         = 500,
                                          dowarn        = FALSE)))
            opt.obj
        }))

        opt.objs = data.frame(opt.objs)

        unlist(opt.objs$value)
    }
}

optim.numeric.nsdf = function(model, case.predictors.all, features.numeric.nas,
                              eval.points.reps)
{
    eval.points =
        sapply(1:length(features.numeric.nas), function(x)
        {
            runif(length(features.numeric.nas) * eval.points.reps, 0, 1)
        })

    cases = foreach::foreach(idx = 1:nrow(eval.points),
                             .combine = rbind) %do%
    {
        case.config = case.predictors.all # copy

        for (j in 1:length(features.numeric.nas))
        {
            case.config[[features.numeric.nas[j]]] = eval.points[idx, j]
        }

        case.config
    }

    suppressWarnings(stats::predict(model, cases,
                                    type = "prob", na.action = NULL)[, 2])
}

optim.factor.numeric.classic = function(model, case.predictors.all, factors.configs,
                                        features.numeric.nas, optimization.numeric.reps,
                                        optimization.numeric.method)
{
    pred.vals = data.frame()

    for (idx in 1:nrow(factors.configs))
    {
        case.config = case.predictors.all # copy

        for (k in 1:ncol(factors.configs))
        {
            case.config[[colnames(factors.configs)[k]]] =
                factors.configs[idx, k]
        }

        target.function = function(x)
        {
            case.config.internal = case.config # copy

            for (j in 1:length(features.numeric.nas))
            {
                case.config.internal[[features.numeric.nas[j]]] = x[j]
            }

            suppressWarnings(stats::predict(model, case.config.internal,
                                            type = "prob", na.action = NULL)[1, 2])
        }

        start.values =
            matrix(runif(optimization.numeric.reps * length(features.numeric.nas),
                         0, 1),
                   ncol = length(features.numeric.nas))
        lower.values = rep(0, length(features.numeric.nas))
        upper.values = rep(1, length(features.numeric.nas))

        minmax.vals =
            foreach::foreach(y = 1:nrow(start.values),
                             .packages = "optimx",
                             .combine  = cbind) %do%
            {
                opt.objs = t(sapply(c(FALSE, TRUE), function(maximize.opt)
                {
                    capture.output(
                        opt.obj <-
                            optimx(par     = start.values[y, ],
                                   fn      = target.function,
                                   method  = optimization.numeric.method,
                                   lower   = lower.values,
                                   upper   = upper.values,
                                   control = list(
                                       kkt           = FALSE,
                                       maximize      = maximize.opt,
                                       save.failures = TRUE,
                                       maxit         = 500,
                                       dowarn        = FALSE)))
                    opt.obj
                }))

                opt.objs = data.frame(opt.objs)

                unlist(opt.objs$value)
            }

        pred.vals = rbind(pred.vals,
                          c(min(minmax.vals[1, ]), max(minmax.vals[2, ])))
    }

    pred.vals
}

optim.factor.numeric.nsdf = function(model, case.predictors.all, factors.configs,
                                     features.numeric.nas, optimization.numeric.reps)
{
    eval.num.points =
        sapply(1:length(features.numeric.nas), function(x)
        {
            runif(length(features.numeric.nas) * optimization.numeric.reps, 0, 1)
        })

    colnames(eval.num.points) = features.numeric.nas

    eval.fac.num.points = expand.grid.df(factors.configs, eval.num.points)

    cases = foreach::foreach(idx = 1:nrow(eval.fac.num.points),
                             .combine = rbind) %do%
    {
        case.config = case.predictors.all # copy

        for (j in 1:ncol(eval.fac.num.points))
        {
            case.config[[colnames(eval.fac.num.points)[j]]] =
                eval.fac.num.points[idx, j]
        }

        case.config
    }

    suppressWarnings(stats::predict(model, cases,
                                    type = "prob", na.action = NULL)[, 2])
}

calculate.optim.interval = function(case.predictors.all, case.class,
                                    case.predictors.used)
{
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

    return(c(interval.lower, interval.upper))
}
