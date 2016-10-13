# ---- config ----

# randomization and output files

SEED                   = 1337
OVERWRITE.OUTPUT.FILES = TRUE # overwrite created datasets and classifiers

# extra user configuration and init

USER.CONFIG.FILE      = "config.R.user"
USER.INIT.FILE        = "init.R.user"

# checkpoint library

CHECKPOINT.QUICK.LOAD    = FALSE # if TRUE then skip testing https and checking url
CHECKPOINT.MRAN.URL      = "http://mran.microsoft.com/"
CHECKPOINT.SNAPSHOT.DATE = "2016-07-01"

# logging system

LOGGER.OUTPUT.DIR               = "logs"
LOGGER.OUTPUT.S0.FILE           = "output-s0.log"
LOGGER.OUTPUT.S1.FILE           = "output-s1.log"
LOGGER.OUTPUT.S2.FILE           = "output-s2.log"
LOGGER.OUTPUT.S3.FILE           = "output-s3.log"
LOGGER.OUTPUT.S4.1.FILE         = "output-s4-1.log"
LOGGER.OUTPUT.S4.2.FILE         = "output-s4-2.log"
LOGGER.OUTPUT.S5.FILE           = "output-s5.log"
LOGGER.OUTPUT.S6.1.FILE         = "output-s6-1.log"
LOGGER.OUTPUT.S6.2.FILE         = "output-s6-2.log"
LOGGER.OUTPUT.S6.3.FILE         = "output-s6-3.log"
LOGGER.OUTPUT.S7.FILE           = "output-s7.log"
LOGGER.LEVEL                    = 6 # futile.logger::INFO
LOGGER.OVERWRITE.EXISTING.FILES = TRUE

# regex patterns

CLASSIFIERS.NAME.PATTERN        = "CLASSIFIER-NAME"
DATASETS.NAME.PATTERN           = "DATASET-NAME"

# classifiers used in the experiment

# list of classifiers used in caret:
#   https://topepo.github.io/caret/bytag.html
#   https://topepo.github.io/caret/modelList.html

CLASSIFIERS.DIR                 = "classifiers"
CLASSIFIERS.LEARNED =
    file.path(CLASSIFIERS.DIR, paste0(DATASETS.NAME.PATTERN, "-",
                                      CLASSIFIERS.NAME.PATTERN, ".rds"))
CLASSIFIERS.PERFORMANCE.ORIGINAL =
    file.path(CLASSIFIERS.DIR, paste0(DATASETS.NAME.PATTERN, "-",
                                      CLASSIFIERS.NAME.PATTERN,  "-performance-original.rds"))
CLASSIFIERS.PERFORMANCE.INTERVAL =
    file.path(CLASSIFIERS.DIR, paste0(DATASETS.NAME.PATTERN, "-",
                                      CLASSIFIERS.NAME.PATTERN, "-performance-interval.rds"))
CLASSIFIERS.IMPUTATION.BASELINE =
    file.path(CLASSIFIERS.DIR, paste0(DATASETS.NAME.PATTERN, "-imputation-baseline.rds"))
CLASSIFIERS.IMPUTATION.MODEL =
    file.path(CLASSIFIERS.DIR, paste0(DATASETS.NAME.PATTERN, "-imputation-model.rds"))
CLASSIFIERS.OVERWRITE.EXISTING.FILES = TRUE

CLASSIFIERS.BASELINE = "OneR"
CLASSIFIERS.LIST     = c("glm",
                         "nnet",
                         "svmLinear",
                         "rpart",
                         "knn")

CLASSIFIERS.FEATURE.SELECTION.METHOD = list( # NULL means internal method
    glm       = "rfFuncs",
    nnet      = "rfFuncs",
    svmLinear = "rfFuncs",
    knn       = "rfFuncs"
)

CLASSIFIERS.TUNING.PARAMS = list( # NULL means no tuning parameters

    nnet      = expand.grid(size  = seq(0, 10, 2),
                            decay = c(0, 10 ^ seq(-5, 5, 2))),

    svmLinear = expand.grid(C = 10 ^ seq(-5, 2)),

    rpart     = expand.grid(cp = c(0.001, 0.01, 0.1, 0.25, 0.5, 0.75, 0.9)),

    knn       = expand.grid(k = 1:10)
)

CLASSIFIERS.BASIC.ATTRIBUTES = list( # NULL means no need to set extra attributes
    nnet      = list(trace = FALSE),
    svmLinear = list(scaled = FALSE)
)

# datasets used in the experiment

DATASETS.URL =
    "https://github.com/andre-wojtowicz/uci-ml-to-r/releases/download/v1.2/data-collection.zip"

DATASETS.NAMES = c("bank-marketing",
                   "census-income",
                   "credit-card",
                   "magic",
                   "wine-quality")

DATASETS.ALWAYS.AVAILABLE.PREDICTORS = list(
    "bank-marketing" = c("job", "marital", "education", "day", "month",
                         "pdays", "pdays.bin", "poutcome"),
    "census-income"  = c("workclass", "education", "marital.status", "occupation",
                         "race", "sex")
)

DATASETS.DIR               = "datasets"
DATASETS.ORIGIN            =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, ".rds"))
DATASETS.FEATURE.SELECTION =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-",
                                   CLASSIFIERS.NAME.PATTERN, "-feature-selection.rds"))
DATASETS.CLASSIFICATION    =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-",
                                   CLASSIFIERS.NAME.PATTERN, "-classification.rds"))
DATASETS.OBSCURATION       =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-obscuration.rds"))
DATASETS.OBSCURED          =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-obscured.rds"))

DATASETS.AGG.FOLDS.RAW     =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-agg-folds-raw.rds"))
DATASETS.INTERVAL          =
    file.path(DATASETS.DIR, paste0(DATASETS.NAME.PATTERN, "-agg-folds-interval.rds"))


DATASETS.SIZE.FEATURE.SELECTION   =  150
DATASETS.SIZE.CLASSIFICATION      =  450
DATASETS.SIZE.OBSCURATION         = 1000
DATASETS.SIZE.PER.CLASS =
    ((DATASETS.SIZE.FEATURE.SELECTION + DATASETS.SIZE.CLASSIFICATION) *
         (length(CLASSIFIERS.LIST) + 1) + DATASETS.SIZE.OBSCURATION) / 2

# aggregation strategies

AGGREGATION.DIR                 = "aggregation-ops"
AGGREGATION.LEARNED =
    file.path(AGGREGATION.DIR, paste0(DATASETS.NAME.PATTERN, "-agg.rds"))

# obscuration step

OBSCURATION.NO.NAS.FRACTION = 1/3

# feature selection setup

FEATURE.SELECTION.FOLDS     = 10

# nested cross-validation setup

NCV.FOLDS                  = 10
NCV.PREPROCESSING.METHODS  = "range"
NCV.PERFORMANCE.SELECTOR   = "Accuracy"
NCV.PERFORMANCE.MAXIMIZE   = TRUE

# imputation

IMPUTATION.METHODS = c("median/mode",
                       "random forest",
                       "chained equations (classic)",
                       "chained equations (vote)")

# optimization setup when calculating interval predictions

OPTIMIZATION.NUMERIC.REPS           = 10 # standard number of iterations

# load custom config

if (file.exists(USER.CONFIG.FILE))
{
    source(USER.CONFIG.FILE)
}
