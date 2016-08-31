optim.factor.grid.search = function(model, case.predictors.all, factors.configs)
{
    foreach::foreach(idx = 1:nrow(factors.configs),
                     .combine = c) %do%
    {
        case.config = case.predictors.all # copy

        for (k in 1:ncol(factors.configs))
        {
            case.config[[colnames(factors.configs)[k]]] =
                factors.configs[idx, k]
        }

        suppressWarnings(stats::predict(model, case.config,
                                        type = "prob", na.action = NULL)[1, 2])
    }
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
                                          maxit         = 2500,
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

    foreach::foreach(idx = 1:nrow(eval.points),
                     .combine = c) %do%
    {
        case.config = case.predictors.all # copy

        for (j in 1:length(features.numeric.nas))
        {
            case.config[[features.numeric.nas[j]]] = eval.points[idx, j]
        }

        suppressWarnings(stats::predict(model, case.config,
                                        type = "prob", na.action = NULL)[1, 2])
    }
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
                                       maxit         = 2500,
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

    foreach::foreach(idx = 1:nrow(eval.fac.num.points),
                     .combine  = c) %do%
    {
        case.config = case.predictors.all # copy

        for (j in 1:ncol(eval.fac.num.points))
        {
            case.config[[colnames(eval.fac.num.points)[j]]] =
                eval.fac.num.points[idx, j]
        }

        suppressWarnings(stats::predict(model, case.config,
                                        type = "prob", na.action = NULL)[1, 2])
    }
}


