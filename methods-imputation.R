imputation.median.mode = function(data, .random.seed)
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

imputation.random.forest = function(data, .random.seed)
{
    assign(".Random.seed", .random.seed, envir = .GlobalEnv)

    mf.maxiter = 10
    mf.ntree   = 100
    mf.data    = droplevels(data)[, -ncol(data)]

    par.seeds = sample.int(10000, mf.ntree)

    suppressWarnings(
        capture.output(
            data.new <- missForest.custom(xmis        = mf.data,
                                          maxiter     = mf.maxiter,
                                          ntree       = mf.ntree,
                                          par.seeds   = par.seeds,
                                          parallelize = "forests")$ximp
        ))

    data.new = cbind(data.new, data[ncol(data)])

    list(data.new)
}

imputation.mice = function(data, .random.seed)
{
    assign(".Random.seed", .random.seed, envir = .GlobalEnv)

    mice.no.imp = 5
    mice.maxit  = 5
    mice.data   = droplevels(data)[, -ncol(data)]
    mice.default.methods = c("pmm",    # numeric
                             "logreg", # binary, factor with 2 lvls
                             "cart",   # unordered factor with > 2 lvls
                             "cart")   # ordered factor with > 2 lvls

    if ((mice.no.imp %% 2) == 0)
    {
        stop.script("Number of imputations must be an odd number")
    }

    mice.max.attempts = 10
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

# ------------------------------------------------------------------------------

# customized version of missForest::missForest() in order to reproduce
# parallelized forests with no restriction on number of parallel workers;
# note: apparently randomForest::randomForest() is unable to produce the same
# results in sequential and parallel computing
missForest.custom = function(xmis, maxiter = 10, ntree = 100, variablewise = FALSE,
                             decreasing = FALSE, verbose = FALSE, mtry = floor(sqrt(ncol(xmis))),
                             par.seeds = NULL,
                             replace = TRUE, classwt = NULL, cutoff = NULL, strata = NULL,
                             sampsize = NULL, nodesize = NULL, maxnodes = NULL, xtrue = NA,
                             parallelize = "forests")
{
    n <- nrow(xmis)
    p <- ncol(xmis)
    if (!is.null(classwt))
        stopifnot(length(classwt) == p, typeof(classwt) == "list")
    if (!is.null(cutoff))
        stopifnot(length(cutoff) == p, typeof(cutoff) == "list")
    if (!is.null(strata))
        stopifnot(length(strata) == p, typeof(strata) == "list")
    if (!is.null(nodesize))
        stopifnot(length(nodesize) == 2)
    if (any(apply(is.na(xmis), 2, sum) == n)) {
        indCmis <- which(apply(is.na(xmis), 2, sum) == n)
        xmis <- xmis[, -indCmis]
        p <- ncol(xmis)
        cat("  removed variable(s)", indCmis, "due to the missingness of all entries\n")
    }
    parallelize <- match.arg(parallelize)

    ximp <- xmis
    xAttrib <- lapply(xmis, attributes)
    varType <- character(p)
    for (t.co in 1:p) {
        if (is.null(xAttrib[[t.co]])) {
            varType[t.co] <- "numeric"
            ximp[is.na(xmis[, t.co]), t.co] <- mean(xmis[, t.co],
                                                    na.rm = TRUE)
        }
        else {
            varType[t.co] <- "factor"
            max.level <- max(table(ximp[, t.co]))
            class.assign <- sample(names(which(max.level == summary(ximp[,
                                                                         t.co]))), 1)
            if (class.assign != "NA's") {
                ximp[is.na(xmis[, t.co]), t.co] <- class.assign
            }
            else {
                while (class.assign == "NA's") {
                    class.assign <- sample(names(which(max.level ==
                                                           summary(ximp[, t.co]))), 1)
                }
                ximp[is.na(xmis[, t.co]), t.co] <- class.assign
            }
        }
    }
    NAloc <- is.na(xmis)
    noNAvar <- apply(NAloc, 2, sum)
    sort.j <- order(noNAvar)
    if (decreasing)
        sort.j <- rev(sort.j)

    Ximp <- vector("list", maxiter)
    iter <- 0
    k <- length(unique(varType))
    convNew <- rep(0, k)
    convOld <- rep(Inf, k)
    OOBerror <- numeric(p)
    names(OOBerror) <- varType
    if (k == 1) {
        if (unique(varType) == "numeric") {
            names(convNew) <- c("numeric")
        }
        else {
            names(convNew) <- c("factor")
        }
        convergence <- c()
        OOBerr <- numeric(1)
    }
    else {
        names(convNew) <- c("numeric", "factor")
        convergence <- matrix(NA, ncol = 2)
        OOBerr <- numeric(2)
    }
    stopCriterion <- function(varType, convNew, convOld, iter,
                              maxiter) {
        k <- length(unique(varType))
        if (k == 1) {
            (convNew < convOld) & (iter < maxiter)
        }
        else {
            ((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) &
                (iter < maxiter)
        }
    }
    while (stopCriterion(varType, convNew, convOld, iter, maxiter)) {
        if (iter != 0) {
            convOld <- convNew
            OOBerrOld <- OOBerr
        }
        cat("  missForest iteration", iter + 1, "in progress...")
        t.start <- proc.time()
        ximp.old <- ximp

        for (s in 1:p) {
            varInd <- sort.j[s]
            if (noNAvar[[varInd]] != 0) {
                obsi <- !NAloc[, varInd]
                misi <- NAloc[, varInd]
                obsY <- ximp[obsi, varInd]
                obsX <- ximp[obsi, seq(1, p)[-varInd]]
                misX <- ximp[misi, seq(1, p)[-varInd]]
                typeY <- varType[varInd]
                if (typeY == "numeric") {
                    xntree <- NULL

                    RF <- foreach(xntree = 1:ntree,
                                  .combine = "combine", .multicombine = TRUE,
                                  .packages = "randomForest") %dopar% {
                                      set.seed(par.seeds[xntree])
                                      randomForest(x = obsX, y = obsY, ntree = 1,
                                                   mtry = mtry, replace = replace, sampsize = if (!is.null(sampsize))
                                                       sampsize[[varInd]]
                                                   else if (replace)
                                                       nrow(obsX)
                                                   else ceiling(0.632 * nrow(obsX)), nodesize = if (!is.null(nodesize))
                                                       nodesize[1]
                                                   else 1, maxnodes = if (!is.null(maxnodes))
                                                       maxnodes
                                                   else NULL)
                                  }
                    OOBerror[varInd] <- mean((predict(RF) -
                                                  RF$y) ^ 2, na.rm = TRUE)

                    misY <- predict(RF, misX)
                }
                else {
                    obsY <- factor(obsY)
                    summarY <- summary(obsY)
                    if (length(summarY) == 1) {
                        misY <- factor(rep(names(summarY), sum(misi)))
                    }
                    else {
                        RF <- foreach(xntree = 1:ntree,
                                      .combine = "combine", .multicombine = TRUE,
                                      .packages = "randomForest") %dopar%
                                      {
                                          set.seed(par.seeds[xntree])
                                          randomForest(x = obsX, y = obsY,
                                                       ntree = 1, mtry = mtry, replace = replace,
                                                       classwt = if (!is.null(classwt))
                                                           classwt[[varInd]]
                                                       else rep(1, nlevels(obsY)), cutoff = if (!is.null(cutoff))
                                                           cutoff[[varInd]]
                                                       else rep(1/nlevels(obsY), nlevels(obsY)),
                                                       strata = if (!is.null(strata))
                                                           strata[[varInd]]
                                                       else obsY, sampsize = if (!is.null(sampsize))
                                                           sampsize[[varInd]]
                                                       else if (replace)
                                                           nrow(obsX)
                                                       else ceiling(0.632 * nrow(obsX)),
                                                       nodesize = if (!is.null(nodesize))
                                                           nodesize[2]
                                                       else 5, maxnodes = if (!is.null(maxnodes))
                                                           maxnodes
                                                       else NULL)
                                      }
                        ne <- as.integer(predict(RF)) != as.integer(RF$y)
                        ne <- ne[!is.na(ne)]
                        OOBerror[varInd] <- sum(ne)/length(ne)

                        misY <- predict(RF, misX)
                    }
                }
                ximp[misi, varInd] <- misY
            }
        }

        cat("done!\n")
        iter <- iter + 1
        Ximp[[iter]] <- ximp
        t.co2 <- 1
        for (t.type in names(convNew)) {
            t.ind <- which(varType == t.type)
            if (t.type == "numeric") {
                convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[,
                                                                t.ind]) ^ 2)/sum(ximp[, t.ind] ^ 2)
            }
            else {
                dist <- sum(as.character(as.matrix(ximp[, t.ind])) !=
                                as.character(as.matrix(ximp.old[, t.ind])))
                convNew[t.co2] <- dist/(n * sum(varType == "factor"))
            }
            t.co2 <- t.co2 + 1
        }
        if (!variablewise) {
            NRMSE <- sqrt(mean(OOBerror[varType == "numeric"])/var(as.vector(as.matrix(xmis[,
                                                                                            varType == "numeric"])), na.rm = TRUE))
            PFC <- mean(OOBerror[varType == "factor"])
            if (k == 1) {
                if (unique(varType) == "numeric") {
                    OOBerr <- NRMSE
                    names(OOBerr) <- "NRMSE"
                }
                else {
                    OOBerr <- PFC
                    names(OOBerr) <- "PFC"
                }
            }
            else {
                OOBerr <- c(NRMSE, PFC)
                names(OOBerr) <- c("NRMSE", "PFC")
            }
        }
        else {
            OOBerr <- OOBerror
            names(OOBerr)[varType == "numeric"] <- "MSE"
            names(OOBerr)[varType == "factor"] <- "PFC"
        }
        if (any(!is.na(xtrue))) {
            err <- suppressWarnings(missForest::mixError(ximp, xmis, xtrue))
        }
        if (verbose) {
            delta.start <- proc.time() - t.start
            if (any(!is.na(xtrue))) {
                cat("    error(s):", err, "\n")
            }
            cat("    estimated error(s):", OOBerr, "\n")
            cat("    difference(s):", convNew, "\n")
            cat("    time:", delta.start[3], "seconds\n\n")
        }
    }
    if (iter == maxiter) {
        if (any(is.na(xtrue))) {
            out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr)
        }
        else {
            out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr,
                        error = err)
        }
    }
    else {
        if (any(is.na(xtrue))) {
            out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld)
        }
        else {
            out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld,
                        error = suppressWarnings(missForest::mixError(Ximp[[iter -
                                                                                1]], xmis, xtrue)))
        }
    }
    class(out) <- "missForest"
    return(out)
}
