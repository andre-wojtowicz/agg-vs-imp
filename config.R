# ---- config ----

# randomization and output files

SEED                   = 1337
OVERWRITE.OUTPUT.FILES = TRUE # overwrite created datasets and classifiers

# extra user configuration and init

USER.CONFIG.FILE      = "config.R.user"
USER.INIT.FILE        = "init.R.user"

# checkpoint library

CHECKPOINT.QUICK.LOAD = TRUE

# logging system

LOGGER.OUTPUT.S1.FILE           = "output-s1.log"
LOGGER.OUTPUT.S2.FILE           = "output-s2.log"
LOGGER.OUTPUT.S3.FILE           = "output-s3.log"
LOGGER.OUTPUT.S4.FILE           = "output-s4.log"
LOGGER.OUTPUT.S5.FILE           = "output-s5.log"
LOGGER.OUTPUT.S6A.FILE          = "output-s6a.log"
LOGGER.OUTPUT.S6B.FILE          = "output-s6b.log"
LOGGER.OUTPUT.S7.FILE           = "output-s7.log"
LOGGER.LEVEL                    = 6 # futile.logger::INFO
LOGGER.OVERWRITE.EXISTING.FILES = TRUE

# datasets used in the experiment

DATASETS.DIR                      = "datasets"
DATASETS.NAME.PATTERN             = "DATASET-NAME"
DATASETS.ORIGIN            =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, ".rds"))
DATASETS.FEATURE.SELECTION =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-feature-selection.rds"))
DATASETS.CLASSIFICATION    =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-classification.rds"))
DATASETS.OBSCURATION       =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-obscuration.rds"))
DATASETS.OBSCURED          =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-obscured.rds"))
DATASETS.INTERVAL          =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-interval.rds"))

DATASETS.NAMES                    = c("bank-marketing",
                                      "magic",
                                      "wine-quality")
DATASETS.SIZE.FEATURE.SELECTION   = 150 # TODO:
DATASETS.SIZE.CLASSIFICATION      = 300 #  450
DATASETS.SIZE.OBSCURATION         = 300 #  450
DATASETS.SIZE.PER.CLASS           = (DATASETS.SIZE.FEATURE.SELECTION +
                                     DATASETS.SIZE.CLASSIFICATION +
                                     DATASETS.SIZE.OBSCURATION) / 2
DATASETS.OVERWRITE.EXISTING.FILES = TRUE

# obscuration step

OBSCURATION.NO.NAS.FRACTION = 1/3

# nested cross-validation setup

NCV.FOLDS                  = 10
NCV.PREPROCESSING.METHODS  = "range"
NCV.PERFORMANCE.SELECTOR   = "Accuracy"
NCV.PERFORMANCE.MAXIMIZE   = TRUE

# classifiers used in the experiment

# list of classifiers used in caret:
#   https://topepo.github.io/caret/bytag.html
#   https://topepo.github.io/caret/modelList.html

CLASSIFIERS.DIR                      = "classifiers"
CLASSIFIERS.NAME.PATTERN             = "CLASSIFIER-NAME"
CLASSIFIERS.LEARNED =
    file.path(CLASSIFIERS.DIR, paste0(DATASETS.NAME.PATTERN, "-",
                                      CLASSIFIERS.NAME.PATTERN, ".rds"))
CLASSIFIERS.IMPUTATION.BASELINE =
    file.path(CLASSIFIERS.DIR, paste0(DATASETS.NAME.PATTERN, "-imputation-baseline.rds"))
CLASSIFIERS.IMPUTATION.MODEL =
    file.path(CLASSIFIERS.DIR, paste0(DATASETS.NAME.PATTERN, "-imputation-model.rds"))
CLASSIFIERS.OVERWRITE.EXISTING.FILES = TRUE

CLASSIFIERS.BASELINE                 = "OneR"
CLASSIFIERS.LIST                     = c("svmLinear",
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
