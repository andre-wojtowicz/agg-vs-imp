imputationMedianMode = function(data)
{
    colnames.ord.factor =
        names(which(sapply(colnames(data),
                           function(x){ all(class(data[[x]])
                                            == c("ordered", "factor"))})
                    == TRUE))

    mlr::impute(data,
                target  = tail(colnames(data), 1),
                classes = list(numeric   = mlr::imputeMedian(),
                               integer   = mlr::imputeMedian(),
                               factor    = mlr::imputeMode()),
                cols    = sapply(colnames.ord.factor,
                                 function(x){ x = mlr::imputeMode() },
                                 simplify = F))$data
}

imputationRandomForest = function(data)
{
    suppressWarnings(
        capture.output(
            data.new <- missForest::missForest(data[, -ncol(data)])$ximp
        ))

    cbind(data.new, data[ncol(data)])
}

imputationMice = function(data)
{
    data.imputed = mice::complete(mice::mice(data[, -ncol(data)],
                                             m = 1, maxit = 10,
                                             printFlag = FALSE),
                                  action = 1)

    for (colname in colnames(data.imputed))
    {
        attr(data.imputed[[colname]], "contrasts") = NULL
    }

    cbind(data.imputed, data[ncol(data)])
}
