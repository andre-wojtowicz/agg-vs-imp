imputation.median.mode = function(data, seed, parallel.computing.enabled)
{
    colnames.ord.factor = names(which(sapply(data, is.ordered)))

    data.new =
        mlr::impute(data,
                    target  = tail(colnames(data), 1),
                    classes = list(numeric   = mlr::imputeMedian(),
                                   integer   = mlr::imputeMedian(),
                                   factor    = mlr::imputeMode()),
                    cols    = sapply(colnames.ord.factor,
                                     function(x){ x = mlr::imputeMode() },
                                     simplify = F))$data

    list(data.new)
}

imputation.random.forest = function(data, seed, parallel.computing.enabled)
{
    set.seed(seed)

    par.val = if (parallel.computing.enabled & foreach::getDoParWorkers() < 2)
    {
        flog.warn("Random forest imputation works only with at least 2 parallel workers")
        flog.warn("Disabled parallel computing for random forest imputation")
        "no"
    } else if (parallel.computing.enabled) {
        "forests"
    } else {
        "no"
    }

    suppressWarnings(
        capture.output(
            data.new <- missForest::missForest(droplevels(data)[, -ncol(data)],
                                               maxiter     = 10,
                                               ntree       = 200,
                                               parallelize = par.val)$ximp
        ))

    data.new = cbind(data.new, data[ncol(data)])
    list(data.new)
}

imputation.mice = function(data, seed, parallel.computing.enabled)
{
    mice.methods.for.predictors =
        sapply(head(colnames(data), ncol(data) - 1),
            function(x){
                y <- data[[x]]
                if (is.numeric(y))
                    return("pmm")
                else if (nlevels(y) == 2)
                    return("logreg")
                # mice cart/rf does not return reproducible results (even with
                # seed set) when a factor-variable levels have small count
                else if ((is.ordered(y) & nlevels(y) > 2 | nlevels(y) > 2))
                {
                    if (any(table(y) > 0 & table(y) < 10))
                    {
                        return("sample")
                    } else {
                        return("cart")
                    }
                }
                else if (is.logical(y))
                    return("logreg")
                else return("pmm")
            })

    set.seed(seed)

    mice.no.imp = 5
    mice.maxit  = 1
    mice.data   = droplevels(data)[, -ncol(data)]
    mice.default.methods = c("pmm", # numeric
                             "logreg", # binary, factor with 2 lvls
                             "cart", # unordered factor with > 2 lvls
                             "cart") # ordered factor with > 2 lvls

    data.mids = if (parallel.computing.enabled)
    {
        foreach::foreach(no = 1:mice.no.imp,
                         .combine = ibind,
                         #.export = c("mice.data", "mice.maxit",
                         #            "mice.methods.for.predictors",
                         #            "mice.default.methods"),
                         .packages = "mice") %dopar%
        {
            mice::mice(data          = mice.data,
                       m             = 1,
                       maxit         = mice.maxit,
                       printFlag     = FALSE,
                       method        = mice.methods.for.predictors,
                       defaultMethod = mice.default.methods)#,
                       #seed          = seed)
        }
    } else {
        mice::mice(data          = mice.data,
                   m             = mice.no.imp,
                   maxit         = mice.maxit,
                   printFlag     = FALSE,
                   method        = mice.methods.for.predictors,
                   defaultMethod = mice.default.methods,
                   seed          = seed)
    }

    data.new = lapply(1:mice.no.imp, function(i){mice::complete(data.mids, i)})

    for (i in 1:mice.no.imp)
    {
        for (colname in colnames(data.new))
        {
            attr(data.new[[i]][[colname]], "contrasts") = NULL
        }

        data.new[[i]] = cbind(data.new[[i]], data[ncol(data)])
    }

    data.new
}
