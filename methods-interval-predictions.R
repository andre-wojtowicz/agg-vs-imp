optim.factor.grid.search = function(model, case.predictors.all, factors.configs,
                                    progress.bar = NULL)
{
    sapply(1:nrow(factors.configs), function(j)
    {
        if (!is.null(progress.bar))
        {
            utils::setTxtProgressBar(progress.bar, j)
        }

        case.config = case.predictors.all # copy

        for (k in 1:ncol(factors.configs))
        {
            case.config[[colnames(factors.configs)[k]]] =
                factors.configs[j, k]
        }

        suppressWarnings(stats::predict(model, case.config,
                                        type = "prob", na.action = NULL)[1, 1])
    })
}

optim.numeric.classic = function(model, case.predictors.all, features.numeric.nas,
                                 start.values, optimization.numeric.method,
                                 progress.bar = NULL)
{
    target.function = function(x)
    {
        case.config = case.predictors.all # copy

        for (j in 1:length(features.numeric.nas))
        {
            case.config[[features.numeric.nas[j]]] = x[j]
        }

        suppressWarnings(stats::predict(model, case.config,
                                        type = "prob", na.action = NULL)[1, 1])
    }

    lower.values = rep(0, length(features.numeric.nas))
    upper.values = rep(1, length(features.numeric.nas))

    sapply(1:nrow(start.values), function(idx)
    {
        if (!is.null(progress.bar))
        {
            utils::setTxtProgressBar(progress.bar, idx)
        }

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
                                          maxit         = 2500,
                                          dowarn        = FALSE)))
            opt.obj
        }))

        opt.objs = data.frame(opt.objs)

        if (any(unlist(opt.objs$convcode) != 0))
        {
            flog.warn("Numeric optimization: convcodes not equal to 0")
        }

        opt.values = unlist(opt.objs$value)

        if (any(is.na(opt.values)))
        {
            stop.script("Numeric optimization: some values equal to NA")
        }

        opt.values
    })
}

optim.numeric.nsdf = function(model, case.predictors.all, features.numeric.nas,
                              eval.points, progress.bar = NULL)
{
    sapply(1:nrow(eval.points), function(idx)
    {
        if (!is.null(progress.bar))
        {
            utils::setTxtProgressBar(progress.bar, idx)
        }

        case.config = case.predictors.all # copy

        for (j in 1:length(features.numeric.nas))
        {
            case.config[[features.numeric.nas[j]]] = eval.points[idx, j]
        }

        suppressWarnings(stats::predict(model, case.config,
                                        type = "prob", na.action = NULL)[1, 1])
    })
}

optim.factor.numeric.classic = function(model, case.predictors.all, factors.configs,
                                        features.numeric.nas, optimization.numeric.reps,
                                        optimization.numeric.method,
                                        progress.bar = NULL)
{
    sapply(1:nrow(factors.configs), function(idx)
    {
        if (!is.null(progress.bar))
        {
            utils::setTxtProgressBar(progress.bar, idx)
        }

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
                                            type = "prob", na.action = NULL)[1, 1])
        }

        start.values =
            matrix(runif(OPTIMIZATION.NUMERIC.REPS * length(features.numeric.nas),
                         0, 1),
                   ncol = length(features.numeric.nas))
        lower.values = rep(0, length(features.numeric.nas))
        upper.values = rep(1, length(features.numeric.nas))

        minmax.vals =
            apply(start.values, 1, function(y)
            {
                opt.objs = t(sapply(c(FALSE, TRUE), function(maximize.opt)
                {
                    capture.output(
                        opt.obj <-
                            optimx(par     = y,
                                   fn      = target.function,
                                   method  = OPTIMIZATION.NUMERIC.METHOD,
                                   lower   = lower.values,
                                   upper   = upper.values,
                                   control = list(
                                       kkt           = FALSE,
                                       maximize      = maximize.opt,
                                       save.failures = TRUE,
                                       maxit         = 2500,
                                       dowarn        = FALSE)))
                    opt.obj
                }))

                opt.objs = data.frame(opt.objs)

                if (any(unlist(opt.objs$convcode) != 0))
                {
                    flog.warn("Numeric optimization: convcodes not equal to 0")
                }

                opt.values = unlist(opt.objs$value)

                if (any(is.na(opt.values)))
                {
                    stop.script("Numeric optimization: some values equal to NA")
                }

                opt.values
            })

        c(min(minmax.vals[1, ]), max(minmax.vals[2, ]))
    })
}

optim.factor.numeric.nsdf = function(model, case.predictors.all, eval.fac.num.points,
                                     progress.bar = NULL)
{
    sapply(1:nrow(eval.fac.num.points), function(idx)
    {
        if (!is.null(progress.bar))
        {
            utils::setTxtProgressBar(progress.bar, idx)
        }

        case.config = case.predictors.all # copy

        for (j in 1:ncol(eval.fac.num.points))
        {
            case.config[[colnames(eval.fac.num.points)[j]]] =
                eval.fac.num.points[idx, j]
        }

        suppressWarnings(stats::predict(model, case.config,
                                        type = "prob", na.action = NULL)[1, 1])
    })
}


