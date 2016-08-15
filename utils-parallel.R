make.psock.cluster = function(names, connection.timeout, ...)
{
    if (is.numeric(names)) {
        names <- as.integer(names[1L])
        #if (is.na(names) || names < 1L)
        #    stop("numeric 'names' must be >= 1")
        names <- rep("localhost", names)
    }
    parallel:::.check_ncores(length(names))
    options <- parallel:::addClusterOptions(parallel:::defaultClusterOptions,
                                            list(...))
    cl <- vector("list", length(names))
    for (i in seq_along(cl))
    {

        flog.info(paste0("[", i, "/", length(cl), "] Connecting to ",
                         names[[i]], " ... "))

        options.copy     = parallel:::addClusterOptions(options, NULL)
        options.out.file = parallel:::getClusterOption("outfile", options)
        if (class(options.out.file) == "lazy")
        {
            options.copy = parallel:::addClusterOptions(options,
                list("outfile" = lazy_eval(options.out.file,
                                           list(worker.id   = i,
                                                worker.name = names[i]))))
        }

        tryCatch({
            cl.node =
                evalWithTimeout(parallel:::newPSOCKnode(names[[i]],
                                                        options = options.copy,
                                                        rank = i),
                                timeout = connection.timeout,
                                onTimeout = "error")
            cl[[i]] = cl.node
            flog.info("OK")},
            error = function(e) {flog.warn("Timeout")}
        )
    }

    cl.filtered = list()
    i = 1
    for (j in seq_along(cl))
    {
        if (!is.null(cl[[j]]))
        {
            cl.filtered[[i]] = cl[[j]]
            i = i + 1
        }
    }

    if (length(cl) != length(cl.filtered))
    {
        flog.warn(paste("Unable to connect to", length(cl) - length(cl.filtered),
                        "nodes"))
    }

    if (length(cl.filtered) == 0)
    {
        flog.fatal("No remote workers")
        stop()
    } else {
        flog.info(paste("Working on", length(cl.filtered), "nodes"))
    }

    class(cl.filtered) <- c("SOCKcluster", "cluster")
    cl.filtered
}

stop.cluster = function(cl.to.stop = NULL)
{
    flog.info("Workers shut down")

    clusterEvalQ(cl, {
        flog.info("Worker shut down")
    })
    stopCluster(cl.to.stop)
}
