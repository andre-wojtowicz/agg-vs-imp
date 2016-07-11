# ---- step-5-choose-best-imputation ----

source("init.R")
source("methods-imputation.R")

setup.logger(LOGGER.OUTPUT.S5.FILE)

flog.info("Step 5: choose best imputation")

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURED)

    dataset.obscured = readRDS(dataset.obscured)

    imputation.methods = list("median/mode"       = imputation.median.mode,
                              "random forest"     = imputation.random.forest,
                              "chained equations" = imputation.mice)

    flog.info(paste("Baseline model:", CLASSIFIERS.BASELINE))

    baseline.imputation.model.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, CLASSIFIERS.IMPUTATION.BASELINE)

    if (!file.exists(baseline.imputation.model.file.path))
    {
        baseline.model.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, CLASSIFIERS.BASELINE),
                            CLASSIFIERS.LEARNED)

        baseline.model = readRDS(baseline.model.file.path)

        preproc.scheme = attr(baseline.model, "preproc.scheme")
        dataset.obscured.preprocessed = stats::predict(preproc.scheme,
                                                       dataset.obscured)

        datasets.imputed = list(lapply(names(imputation.methods), function(name){
            flog.info(paste("Imputation:", name))
            set.seed(SEED)
            imputation.methods[[name]](dataset.obscured.preprocessed) }))

        names(datasets.imputed[[1]]) = names(imputation.methods)

        baseline.imputation.model =
            cross.validation.for.imputation(datasets.imputed,
                                            list(baseline.model),
                                            NCV.FOLDS)

        saveRDS(baseline.imputation.model, baseline.imputation.model.file.path)

    } else {
        flog.info("Baseline model exists, skipping learning")

        baseline.imputation.model = readRDS(baseline.imputation.model.file.path)

        folds.performances = baseline.imputation.model$folds.performances

        flog.info(paste0("Estimated ", NCV.PERFORMANCE.SELECTOR, ":    ",
                         round(mean(folds.performances[[NCV.PERFORMANCE.SELECTOR]]), 3)))
        flog.info(paste0("Estimated Sensitivity: ",
                         round(mean(folds.performances[["Sensitivity"]]), 3)))
        flog.info(paste0("Estimated Specificity: ",
                         round(mean(folds.performances[["Specificity"]]), 3)))
    }

    flog.info(paste(rep("*", 10), collapse = ""))

    flog.info("Grid search: classifiers and imputation methods")

    classifier.imputation.model.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, CLASSIFIERS.IMPUTATION.MODEL)

    if (!file.exists(classifier.imputation.model.file.path))
    {
        models = list()
        datasets.imputed = list()

        for (model.name in CLASSIFIERS.LIST)
        {
            flog.info(paste("Model:", model.name))

            model.file.path =
                replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                                c(dataset.name, model.name),
                                CLASSIFIERS.LEARNED)

            model = readRDS(model.file.path)

            models = merge(models, list(model))

            preproc.scheme = attr(model, "preproc.scheme")
            dataset.obscured.preprocessed = stats::predict(preproc.scheme,
                                                           dataset.obscured)

            model.datasets.imputed = lapply(names(imputation.methods), function(name){
                flog.info(paste("Imputation:", name))
                set.seed(SEED)
                imputation.methods[[name]](dataset.obscured.preprocessed) })

            names(model.datasets.imputed) = names(imputation.methods)

            datasets.imputed[[model.name]] = model.datasets.imputed
        }

        classifier.imputation.model =
            cross.validation.for.imputation(datasets.imputed, models, NCV.FOLDS)

        saveRDS(classifier.imputation.model, classifier.imputation.model.file.path)

    } else {
        flog.info("Classifier imputation model exists, skipping learning")

        classifier.imputation.model =
            readRDS(classifier.imputation.model.file.path)

        folds.performances = classifier.imputation.model$folds.performances

        flog.info(paste0("Estimated ", NCV.PERFORMANCE.SELECTOR, ":    ",
                         round(mean(folds.performances[[NCV.PERFORMANCE.SELECTOR]]), 3)))
        flog.info(paste0("Estimated Sensitivity: ",
                         round(mean(folds.performances[["Sensitivity"]]), 3)))
        flog.info(paste0("Estimated Specificity: ",
                         round(mean(folds.performances[["Specificity"]]), 3)))
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}
