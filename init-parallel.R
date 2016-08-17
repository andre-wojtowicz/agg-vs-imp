# ---- init-parallel ----

# load setup variables

source("config-parallel.R")
source("utils-parallel.R")

# create cluster

cl = if (PARALLEL.USED.METHOD == "LOCAL")
{
    if (PARALLEL.LOCAL.METHOD == "PSOCK")
    {
        flog.info("Creating local PSOCK cluster")
        make.psock.cluster(
            names              = PARALLEL.LOCAL.NODES,
            connection.timeout = PARALLEL.LOCAL.CONNECTION.TIMEOUT,
            outfile            = PARALLEL.LOCAL.SLAVE.OUT.FILE)
    } else if (PARALLEL.LOCAL.METHOD == "FORK")
    {
        flog.info("Creating local FORK cluster")
        makeForkCluster(PARALLEL.LOCAL.NODES)
    } else {
        stop.script(paste("Unknown local parallel cluster method:",
                          PARALLEL.USED.METHOD))
    }
} else if (PARALLEL.USED.METHOD == "REMOTE")
{
    if (PARALLEL.REMOTE.METHOD)
    {
        flog.info("Creating remote PSOCK cluster")
        make.psock.cluster(
            names              = PARALLEL.REMOTE.MASTER.SLAVES.LIST,
            connection.timeout = PARALLEL.REMOTE.MASTER.CONNECTION.TIMEOUT,
            master             = PARALLEL.REMOTE.MASTER.IP,
            port               = PARALLEL.REMOTE.MASTER.PORT,
            rshcmd             = PARALLEL.REMOTE.MASTER.SHELL.CMD,
            outfile            = PARALLEL.REMOTE.SLAVE.OUT.FILE,
            user               = PARALLEL.REMOTE.SLAVE.SSH.USER,
            rscript            = PARALLEL.REMOTE.SLAVE.RSCRIPT.PATH,
            homogeneous        = PARALLEL.REMOTE.SLAVE.HOMOGENEOUS,
            methods            = PARALLEL.REMOTE.SLAVE.METHODS,
            useXDR             = PARALLEL.REMOTE.SLAVE.USEXDR)
    }
    else {
        stop.script(paste("Unknown remote parallel cluster method:",
                          PARALLEL.REMOTE.METHOD))
    }
} else {
    stop.script(paste("Unknown used parallel method:", PARALLEL.USED.METHOD))
}


flog.info("Exporting checkpoint constants")
clusterExport(cl, c("CHECKPOINT.QUICK.LOAD", "CHECKPOINT.MRAN.URL",
                    "CHECKPOINT.SNAPSHOT.DATE", "LOGGER.LEVEL"))

clusterEvalQ(cl, {
    library(checkpoint)

    if (CHECKPOINT.QUICK.LOAD) # approx. x10 faster checkpoint library loading
    {
        # assume https
        options(checkpoint.mranUrl = CHECKPOINT.MRAN.URL)
        # disable url checking
        assignInNamespace("is.404", function(mran, warn = TRUE) { FALSE },
                          "checkpoint")
    }

    checkpoint(CHECKPOINT.SNAPSHOT.DATE, verbose = TRUE, scanForPackages = TRUE)

    library(futile.logger)

    flog.threshold(LOGGER.LEVEL)

    flog.info("Logging configured")
})

flog.info("Setting cluster RNG strean")
clusterSetRNGStream(cl, SEED)

flog.info("Registering cluster")
registerDoParallel(cl)

# perform additional custom init

if (file.exists("init-parallel.R.user"))
    source("init-parallel.R.user")