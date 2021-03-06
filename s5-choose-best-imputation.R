# ---- step-5-choose-best-imputation ----

source("init.R")
source("methods-imputation.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S5.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 5: choose best imputation")

source("init-parallel.R")

seeds = get.seeds(SEED, c(length(DATASETS.NAMES), 2))

imputation.methods =
    sapply(IMPUTATION.METHODS, function(m){
        switch(m, "median/mode"       = imputation.median.mode,
                  "random forest"     = imputation.random.forest,
                  "chained equations" = imputation.mice)},
        simplify = FALSE, USE.NAMES = TRUE)

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURED)

    dataset.obscured = readRDS(dataset.obscured.file.path)

    dataset.num.missing.attributes = rowSums(is.na(dataset.obscured))

    flog.info(paste("Baseline model:", CLASSIFIERS.BASELINE))

    baseline.imputation.model.file.path =
        replace.strings(DATASETS.NAME.PATTERN,
                        dataset.name, CLASSIFIERS.IMPUTATION.BASELINE)

    if (!file.exists(baseline.imputation.model.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        baseline.model.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, CLASSIFIERS.BASELINE),
                            CLASSIFIERS.LEARNED)

        baseline.model = readRDS(baseline.model.file.path)

        seed.cv = extract.seed(seeds,
                               c(which(dataset.name == DATASETS.NAMES), 1))

        baseline.imputation.model =
            nested.cross.validation.for.imputation(
                dataset.obscured,
                dataset.num.missing.attributes,
                list(baseline.model),
                imputation.methods,
                NCV.FOLDS,
                NCV.PERFORMANCE.SELECTOR,
                NCV.PERFORMANCE.MAXIMIZE,
                seed.cv)

        saveRDS(baseline.imputation.model, baseline.imputation.model.file.path)

    } else {
        flog.warn("Baseline model exists, skipping learning")

        baseline.imputation.model = readRDS(baseline.imputation.model.file.path)
    }

    flog.info(paste("Choosed imputation:", baseline.imputation.model$imputation.name))

    folds.performances = baseline.imputation.model$folds.performances

    flog.info("Overall:")
    flog.info(paste0("  Accuracy:    ", round(folds.performances %>%
        filter(is.na(Missing.attributes)) %>% select(Accuracy) %>% unlist %>% mean, 3)))
    flog.info(paste0("  Sensitivity: ", round(folds.performances %>%
        filter(is.na(Missing.attributes)) %>% select(Sensitivity) %>% unlist %>% mean, 3)))
    flog.info(paste0("  Specificity: ", round(folds.performances %>%
        filter(is.na(Missing.attributes)) %>% select(Specificity) %>% unlist %>% mean, 3)))

    for (num.missing.attr in 0:max(dataset.num.missing.attributes))
    {
        flog.info(paste("Missing attributes:", num.missing.attr))
        flog.info(paste0("  Accuracy:    ", round(mean(folds.performances %>%
            filter(Missing.attributes == num.missing.attr) %>% select(Accuracy) %>%
                unlist, na.rm = TRUE), 3)))
        flog.info(paste0("  Sensitivity: ", round(mean(folds.performances %>%
            filter(Missing.attributes == num.missing.attr) %>% select(Sensitivity) %>%
                unlist, na.rm = TRUE), 3)))
        flog.info(paste0("  Specificity: ", round(mean(folds.performances %>%
            filter(Missing.attributes == num.missing.attr) %>% select(Specificity) %>%
                unlist, na.rm = TRUE), 3)))
    }

    flog.info(paste(rep("*", 10), collapse = ""))

    flog.info("Nested cross validation: classifiers and imputation methods")

    classifier.imputation.model.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, CLASSIFIERS.IMPUTATION.MODEL)

    if (!file.exists(classifier.imputation.model.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        models = list()

        for (model.name in CLASSIFIERS.LIST)
        {
            model.file.path =
                replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                                c(dataset.name, model.name),
                                CLASSIFIERS.LEARNED)

            models[[model.name]] = readRDS(model.file.path)

            flog.info(paste("Loaded model:", model.name))
        }

        seed.cv = extract.seed(seeds,
                               c(which(dataset.name == DATASETS.NAMES), 2))

        classifier.imputation.model =
            nested.cross.validation.for.imputation(
                dataset.obscured,
                dataset.num.missing.attributes,
                models,
                imputation.methods,
                NCV.FOLDS,
                NCV.PERFORMANCE.SELECTOR,
                NCV.PERFORMANCE.MAXIMIZE,
                seed.cv)

        saveRDS(classifier.imputation.model, classifier.imputation.model.file.path)

    } else {
        flog.warn("Classifier imputation model exists, skipping learning")

        classifier.imputation.model =
            readRDS(classifier.imputation.model.file.path)
    }

    flog.info(paste("Choosed classifier:", classifier.imputation.model$model$method))
    flog.info(paste("Choosed imputation:", classifier.imputation.model$imputation.name))

    folds.performances = classifier.imputation.model$folds.performances

    flog.info(paste0("  Accuracy:    ", round(folds.performances %>%
        filter(is.na(Missing.attributes)) %>% select(Accuracy) %>% unlist %>% mean, 3)))
    flog.info(paste0("  Sensitivity: ", round(folds.performances %>%
        filter(is.na(Missing.attributes)) %>% select(Sensitivity) %>% unlist %>% mean, 3)))
    flog.info(paste0("  Specificity: ", round(folds.performances %>%
        filter(is.na(Missing.attributes)) %>% select(Specificity) %>% unlist %>% mean, 3)))

    for (num.missing.attr in 0:max(dataset.num.missing.attributes))
    {
        flog.info(paste("Missing attributes:", num.missing.attr))
        flog.info(paste0("  Accuracy:    ", round(mean(folds.performances %>%
            filter(Missing.attributes == num.missing.attr) %>% select(Accuracy) %>%
                unlist, na.rm = TRUE), 3)))
        flog.info(paste0("  Sensitivity: ", round(mean(folds.performances %>%
            filter(Missing.attributes == num.missing.attr) %>% select(Sensitivity) %>%
                unlist, na.rm = TRUE), 3)))
        flog.info(paste0("  Specificity: ", round(mean(folds.performances %>%
            filter(Missing.attributes == num.missing.attr) %>% select(Specificity) %>%
                unlist, na.rm = TRUE), 3)))
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}

stop.cluster()
