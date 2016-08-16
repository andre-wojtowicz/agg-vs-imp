# ---- config-parallel ----

PARALLEL.USED.METHOD = "LOCAL" # LOCAL or REMOTE

# local
PARALLEL.LOCAL.METHOD             = switch(Sys.info()[['sysname']],
                                           Windows = "PSOCK",
                                           Linux   = "FORK",
                                           Darwin  = "FORK")

PARALLEL.LOCAL.NODES              = parallel::detectCores(logical = TRUE)
PARALLEL.LOCAL.CONNECTION.TIMEOUT = 5
PARALLEL.LOCAL.SLAVE.OUT.FILE     =
    lazy(file.path(LOGGER.OUTPUT.DIR,
                   paste0("worker-local-", worker.name, "-", worker.id, ".log")))

# remote
PARALLEL.REMOTE.METHOD                    = "PSOCK"

PARALLEL.REMOTE.MASTER.IP                 = "192.168.0.1" # ip accessible from slaves
PARALLEL.REMOTE.MASTER.PORT               = 11000
PARALLEL.REMOTE.MASTER.CONNECTION.TIMEOUT = 10
PARALLEL.REMOTE.MASTER.SSH.KEY            = file.path("ssh", "rsa.key")
PARALLEL.REMOTE.MASTER.SHELL.CMD          = paste0(
    "ssh -q",
    " -o ConnectTimeout=", PARALLEL.REMOTE.MASTER.CONNECTION.TIMEOUT,
    " -o UserKnownHostsFile=/dev/null",
    " -o StrictHostKeyChecking=no",
    " -i ", PARALLEL.REMOTE.MASTER.SSH.KEY)
PARALLEL.REMOTE.MASTER.SLAVES.IPS.FILE.PATH = "remote-hosts-ips.txt"
PARALLEL.REMOTE.MASTER.SLAVES.IPS           =
    readLines(PARALLEL.REMOTE.MASTER.SLAVES.IPS.FILE.PATH)

PARALLEL.REMOTE.SLAVE.OUT.FILE     = lazy(paste0("worker-remote-", worker.name,
                                                 "-", worker.id, ".log"))
PARALLEL.REMOTE.SLAVE.SSH.USER     = "root"
PARALLEL.REMOTE.SLAVE.RSCRIPT.PATH = "/usr/bin/Rscript"
PARALLEL.REMOTE.SLAVE.HOMOGENEOUS  = TRUE
PARALLEL.REMOTE.SLAVE.METHODS      = TRUE
PARALLEL.REMOTE.SLAVE.USEXDR       = TRUE

# perform additional custom config

if (file.exists("config-parallel.R.user"))
    source("config-parallel.R.user")
