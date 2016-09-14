optim.factor.grid.search = function(model, case.predictors.all, factors.configs)
{
    cases = foreach::foreach(idx = 1:nrow(factors.configs),
                             .combine = rbind) %do%
    {
        case.config = case.predictors.all # to copy from DT (assign "=" do the job)

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
                                 optimization.numeric.reps)
{
    start.values =
        matrix(runif(optimization.numeric.reps * length(features.numeric.nas), 0, 1),
               ncol = length(features.numeric.nas))

    foreach::foreach(idx = 1:nrow(start.values),
                     .packages = "nloptr",
                     .combine  = cbind) %do%
    {
        opt.objs = t(sapply(c(FALSE, TRUE), function(maximize.opt)
        {
            target.function = function(x)
            {
                case.config = case.predictors.all # to copy from DT (assign "=" do the job)

                for (j in 1:length(features.numeric.nas))
                {
                    case.config[[features.numeric.nas[j]]] = x[j]
                }

                val = suppressWarnings(stats::predict(model, case.config,
                                                      type = "prob", na.action = NULL)[1, 2])

                ifelse(maximize.opt, -val, val)
            }

            opt.obj = nloptr(
                x0          = start.values[idx, ],
                eval_f      = target.function,
                lb          = rep(0, length(features.numeric.nas)),
                ub          = rep(1, length(features.numeric.nas)),
                eval_grad_f = NULL,
                opts = list("algorithm"  = "NLOPT_LN_NELDERMEAD",
                            "xtol_rel"   = -1, # disable
                            maxeval      = 100,
                            print_level  = 0)
            )

            ifelse(maximize.opt, -opt.obj$objective, opt.obj$objective)
        }))

        t(opt.objs)
    }
}


optim.factor.numeric.classic = function(model, case.predictors.all, factors.configs,
                                        features.numeric.nas, optimization.numeric.reps)
{
    pred.vals = data.frame()

    for (idx in 1:nrow(factors.configs))
    {
        case.config = case.predictors.all # to copy from DT (assign "=" do the job)

        for (k in 1:ncol(factors.configs))
        {
            case.config[[colnames(factors.configs)[k]]] =
                factors.configs[idx, k]
        }

        start.values =
            matrix(runif(optimization.numeric.reps * length(features.numeric.nas),
                         0, 1),
                   ncol = length(features.numeric.nas))

        minmax.vals =
            foreach::foreach(y = 1:nrow(start.values),
                             .packages = "nloptr",
                             .combine  = cbind) %do%
            {
                opt.objs = t(sapply(c(FALSE, TRUE), function(maximize.opt)
                {
                    target.function = function(x)
                    {
                        case.config.internal = case.config # to copy from DT
                                                           # (assign "=" do the job)

                        for (j in 1:length(features.numeric.nas))
                        {
                            case.config.internal[[features.numeric.nas[j]]] = x[j]
                        }

                        val = suppressWarnings(stats::predict(model, case.config.internal,
                                                              type = "prob",
                                                              na.action = NULL)[1, 2])

                        ifelse(maximize.opt, -val, val)
                    }

                    opt.obj = nloptr(
                        x0          = start.values[y, ],
                        eval_f      = target.function,
                        lb          = rep(0, length(features.numeric.nas)),
                        ub          = rep(1, length(features.numeric.nas)),
                        eval_grad_f = NULL,
                        opts = list("algorithm"  = "NLOPT_LN_NELDERMEAD",
                                    "xtol_rel"   = -1, # disable
                                    maxeval      = 100,
                                    print_level  = 0)
                    )

                    ifelse(maximize.opt, -opt.obj$objective, opt.obj$objective)
                }))

                t(opt.objs)
            }

        pred.vals = rbind(pred.vals,
                          c(min(minmax.vals[1, ]), max(minmax.vals[2, ])))
    }

    pred.vals
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
                # [3] Classic optimization
                flog.info(paste("Case:", i,
                                "- classic factor-numeric optimization"))

                predicted.values =
                    optim.factor.numeric.classic(
                        model,
                        case.predictors.all,
                        factors.configs,
                        features.numeric.nas,
                        OPTIMIZATION.NUMERIC.REPS
                    )

                interval.lower = min(predicted.values[, 1])
                interval.upper = max(predicted.values[, 2])
            }

        } else {
            # Only numeric features are not present
                # [4.2] Classic optimization
                flog.info(paste("Case:", i, "- classic numeric optimization"))

                predicted.values =
                    optim.numeric.classic(model,
                                          case.predictors.all,
                                          features.numeric.nas,
                                          OPTIMIZATION.NUMERIC.REPS)

                interval.lower = min(predicted.values[1, ])
                interval.upper = max(predicted.values[2, ])
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
