imputation.median.mode = function(data, seed)
{
    colnames.ord.factor = names(which(sapply(data, is.ordered)))

    mlr::impute(data,
                target  = tail(colnames(data), 1),
                classes = list(numeric   = mlr::imputeMedian(),
                               integer   = mlr::imputeMedian(),
                               factor    = mlr::imputeMode()),
                cols    = sapply(colnames.ord.factor,
                                 function(x){ x = mlr::imputeMode() },
                                 simplify = F))$data
}

imputation.random.forest = function(data, seed)
{
    set.seed(seed)

    suppressWarnings(
        capture.output(
            data.new <- missForest::missForest(droplevels(data)[, -ncol(data)])$ximp
        ))

    cbind(data.new, data[ncol(data)])
}

imputation.mice = function(data, seed)
{
    methods.for.predictors =
        sapply(head(colnames(data), ncol(data) - 1),
            function(x){
                y <- data[[x]]
                if (is.numeric(y))
                    return("pmm")
                else if (nlevels(y) == 2)
                    return("logreg")
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

    data.new =
        mice::complete(mice::mice(droplevels(data)[, -ncol(data)],
                                  m = 1, maxit = 5,
                                  printFlag = FALSE,
                                  method = methods.for.predictors,
                                  defaultMethod =
                                      c("pmm", # numeric
                                        "logreg", # binary, factor with 2 lvls
                                        "cart", # unordered factor with > 2 lvls
                                        "cart"), # ordered factor with > 2 lvls
                                  seed = seed),
                       action = 1)

    for (colname in colnames(data.new))
    {
        attr(data.new[[colname]], "contrasts") = NULL
    }

    cbind(data.new, data[ncol(data)])
}
