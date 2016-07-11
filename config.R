# ---- config ----

# randomization

SEED                  = 1337

# extra user configuration and init

USER.CONFIG.FILE      = "config.R.user"
USER.INIT.FILE        = "init.R.user"

# checkpoint library

CHECKPOINT.QUICK.LOAD = TRUE

# logging system

LOGGER.OUTPUT.S1.FILE  = "s1.log"
LOGGER.OUTPUT.S2.FILE  = "s2.log"
LOGGER.OUTPUT.S3.FILE  = "s3.log"
LOGGER.OUTPUT.S4.FILE  = "s4.log"
LOGGER.OUTPUT.S5.FILE  = "s5.log"
LOGGER.OUTPUT.S6A.FILE = "s6a.log"
LOGGER.OUTPUT.S6B.FILE = "s6b.log"
LOGGER.OUTPUT.S7.FILE  = "s7.log"
LOGGER.LEVEL           = 6 # futile.logger::INFO
LOGGER.APPEND          = FALSE

# datasets used in the experiment

DATASETS.DIR                    = "datasets"
DATASETS.NAMES                  = c("bank-marketing",
                                    "magic",
                                    "wine-quality")
DATASETS.SIZE.FEATURE.SELECTION = 150 # TODO:
DATASETS.SIZE.CLASSIFICATION    = 300 #  450
DATASETS.SIZE.OBSCURATION       = 300 #  450
DATASETS.SIZE.PER.CLASS         = (DATASETS.SIZE.FEATURE.SELECTION +
                                   DATASETS.SIZE.CLASSIFICATION +
                                   DATASETS.SIZE.OBSCURATION) / 2

# nested cross-validation setup

NCV.FOLDS                  = 10
NCV.PREPROCESSING.METHODS  = "range"
NCV.PERFORMANCE.SELECTOR   = "Accuracy"
NCV.PERFORMANCE.MAXIMIZE   = TRUE

# classifiers used in the experiment

# list of classifiers used in caret:
#   https://topepo.github.io/caret/bytag.html
#   https://topepo.github.io/caret/modelList.html

CLASSIFIERS.DIR      = "classifiers"

CLASSIFIERS.BASELINE = "OneR"
CLASSIFIERS.LIST     = c("svmLinear",
                         "C5.0",
                         "knn")

CLASSIFIERS.FEATURE.SELECTION.METHOD = list( # NULL means internal method
    svmLinear = "rfFuncs",
    knn       = "treebagFuncs"
)

CLASSIFIERS.TUNING.PARAMS = list( # NULL means no tuning parameters

    svmLinear = expand.grid(C = 10 ^ seq(-5, 2)),

    C5.0      = expand.grid(trials = c(1, 5, 10, 15, 20),
                            model  = c("tree", "rules"),
                            winnow = TRUE),

    knn       = expand.grid(k = 1:10)
)

CLASSIFIERS.BASIC.ATTRIBUTES = list( # NULL means no need to set extra attributes
    svmLinear = list(scaled = FALSE)
)

# optimization setup when calculating interval predictions

# possible standard numeric optimization methods:
# * L-BFGS-B
# * nlminb
# * spg
# * bobyqa

OPTIMIZATION.NUMERIC.METHOD         = "nlminb"
OPTIMIZATION.NUMERIC.REPS           = 10 # standard number of iterations

OPTIMIZATION.NUMERIC.BF.CLASSIFIERS = c("C5.0", "knn") # classifiers which need
                                                       # to have brute-force
                                                       # numeric optimization
OPTIMIZATION.NUMERIC.BF.REPS        = 100 # number of iterations for brute-force
                                          # numeric optimization

# load custom config

if (file.exists(USER.CONFIG.FILE))
{
    source(USER.CONFIG.FILE)
}
