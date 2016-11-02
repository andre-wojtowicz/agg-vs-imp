imputation.median.mode = function(data)
{
    median.mode.list = sapply(data[, 1:(ncol(data) - 1)], function(x)
    {
        if (is.numeric(x) || is.integer(x)) {
            return(median(x, na.rm = TRUE))
        } else if (is.ordered(x) || is.factor(x)) {
            return(get.mode(x))
        } else {
            return(NA)
        }
    }, simplify = FALSE)

    imp.scheme = function(data, learned.obj)
    {
        data.copy = data

        for (colname in head(colnames(data), ncol(data) - 1)) {
            data.copy[is.na(data.copy[[colname]]), colname] =
                learned.obj[[colname]]
        }

        data.copy
    }

    attr(imp.scheme, "imputation.name") = "median/mode"
    attr(imp.scheme, "learned.obj")     = median.mode.list

    return(imp.scheme)
}

imputation.random.forest = function(data)
{
    imputation.trees = rfsrc(data       = data[, -ncol(data)],
                             ntree      = 1000,
                             seed       = sample.int(10000, 1))

    imp.scheme = function(data, learned.obj)
    {
        imp.obj = predict.rfsrc(learned.obj, data[, -ncol(data)], na.action = "na.impute")

        data[rowSums(is.na(data)) > 0, -ncol(data)] = imp.obj$imputed.data

        data
    }

    attr(imp.scheme, "imputation.name") = "random forest"
    attr(imp.scheme, "learned.obj")     = imputation.trees

    return(imp.scheme)
}

imputation.mice = function(data)
{
    mice.no.imp = 100
    mice.maxit  = 10
    mice.data   = droplevels(data)[, -ncol(data)]
    mice.default.methods = c("pmm",    # numeric
                             "logreg", # binary, factor with 2 lvls
                             "cart",   # unordered factor with > 2 lvls
                             "cart")   # ordered factor with > 2 lvls

    mice.max.attempts = 25
    par.seeds = matrix(sample.int(10000, mice.no.imp * mice.max.attempts),
                       nrow = mice.no.imp)

    data.mids =
        foreach::foreach(no.iter   = 1:mice.no.imp,
                         .combine  = ibind,
                         .packages = "mice") %dopar%
    {
        attempt = mice.max.attempts
        mice.mid = NULL
        while (attempt > 0)
        {
            set.seed(par.seeds[no.iter, attempt])
            mice.seed = par.seeds[no.iter, attempt]

            repeat.mice = FALSE
            tryCatch({mice.mid <- mice::mice(data          = mice.data,
                                            m             = 1,
                                            maxit         = mice.maxit,
                                            printFlag     = FALSE,
                                            defaultMethod = mice.default.methods,
                                            seed          = mice.seed)},
                     error = function(e){
                        flog.debug(paste(no.iter, "Mice algorithm failed"))
                        repeat.mice <<- TRUE
                     })

            if (repeat.mice)
            {
                attempt = attempt - 1
            } else {
                break
            }
        }
        mice.mid
    }

    imp.scheme = function(data, learned.obj)
    {
        mice.no.imp = learned.obj$m

        foreach::foreach(j         = 1:nrow(data),
                         .combine  = rbind,
                         .packages = "nnet") %dopar%
        {
            if (all(!is.na(data[j, ])))
            {
                return(data[j, ])
            }

            data.for.lm = complete(learned.obj, "long")
            data.for.lm = data.for.lm[, !(names(data.for.lm) %in% c(".imp", ".id"))]

            missing.attr.names  = colnames(data)[which(is.na(data[j, ]))]
            complete.attr.names = setdiff(colnames(data), c(missing.attr.names,
                                                            colnames(data)[ncol(data)]))

            # substitute factor level in data row by random sample if the level
            # was not included in imputation learned.obj; this is extremely rare
            # to happen
            for (complete.attr in complete.attr.names)
            {
                if (is.factor(data[[complete.attr]]) &&
                    !(data[j, complete.attr] %in% levels(data.for.lm[[complete.attr]])))
                {
                    data[j, complete.attr] = sample(levels(data.for.lm[[complete.attr]]), 1)
                }
            }

            for (missing.attr in missing.attr.names)
            {
                if (is.factor(data[[missing.attr]]))
                {
                    if (nlevels(data[[missing.attr]]) <= 2)
                    {
                        # binomial

                        mi.lm.fit = suppressWarnings(
                            with(data = learned.obj,
                                 exp = glm(
                                     as.formula(paste(missing.attr, "~",
                                                      paste(setdiff(complete.attr.names,
                                                                    missing.attr),
                                                            collapse = "+"))),
                                     family = binomial
                                 )))

                        for (i in 1:mice.no.imp)
                        {
                            mi.lm.fit$analyses[[i]]$coefficients =
                                mi.lm.fit$analyses[[i]]$coefficients[
                                    which(!is.na(mi.lm.fit$analyses[[i]]$coefficients))
                                    ]
                        }

                        mi.lm.pool = pool(mi.lm.fit)

                        imputation.model = suppressWarnings(
                            glm(formula =
                                    as.formula(paste(missing.attr, "~",
                                                     paste(setdiff(complete.attr.names,
                                                                   missing.attr),
                                                           collapse = "+"))),
                                data = rbind(data.for.lm, data[j, -ncol(data)]),
                                family = binomial)
                        )
                        imputation.model$coefficients = mi.lm.pool$qbar

                        pred.p = suppressWarnings(
                            predict(imputation.model, data[j, ], type = "response")
                        )

                        data[j, missing.attr] = levels(data[[missing.attr]])[
                            ifelse(pred.p > 0.5, 1, 2)
                        ]


                    } else {
                        # multinom

                        capture.output(
                            mi.mlm.fit <-
                                with(data = learned.obj,
                                     exp = multinom(as.formula(paste(missing.attr, "~",
                                                        paste(setdiff(complete.attr.names,
                                                         missing.attr),
                                                        collapse = "+"))))))

                        mi.mlm.pool = pool(mi.mlm.fit)

                        suppressWarnings(capture.output(
                            imputation.model <-
                               multinom(as.formula(paste(missing.attr, "~",
                                                         paste(setdiff(complete.attr.names,
                                                                       missing.attr),
                                                               collapse = "+"))),
                                        data = rbind(data.for.lm, data[j, -ncol(data)]))
                        ))
                        imputation.model$coefficients = mi.mlm.pool$qbar

                        data[j, missing.attr] =
                            predict(imputation.model, data[j, ], type = "class")
                    }
                } else {
                    # gaussian

                    mi.lm.fit = suppressWarnings(
                        with(data = learned.obj,
                             exp = glm(
                                 as.formula(paste(missing.attr, "~",
                                                  paste(setdiff(complete.attr.names, missing.attr),
                                                        collapse = "+"))),
                                 family = gaussian
                             )))

                    for (i in 1:mice.no.imp)
                    {
                        mi.lm.fit$analyses[[i]]$coefficients =
                            mi.lm.fit$analyses[[i]]$coefficients[
                                which(!is.na(mi.lm.fit$analyses[[i]]$coefficients))
                                ]
                    }

                    mi.lm.pool = pool(mi.lm.fit)

                    imputation.model = suppressWarnings(
                        glm(formula =
                                as.formula(paste(missing.attr, "~",
                                                 paste(setdiff(complete.attr.names, missing.attr),
                                                       collapse = "+"))),
                            data = rbind(data.for.lm, data[j, -ncol(data)]),
                            family = gaussian)
                    )
                    imputation.model$coefficients = mi.lm.pool$qbar

                    data[j, missing.attr] =
                        suppressWarnings(
                            predict(imputation.model, data[j, ])
                        )
                }
            }

            return(data[j, ])
        }
    }

    attr(imp.scheme, "imputation.name") = "chained equations"
    attr(imp.scheme, "learned.obj")     = data.mids

    return(imp.scheme)
}
