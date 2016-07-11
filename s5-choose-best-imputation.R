# ---- step-5-choose-best-imputation ----

source("init.R")
source("methods-imputation.R")

setupLogger(LOGGER.OUTPUT.S5.FILE)

flog.info("Step 5: choose best imputation")

for (dataset.name in datasets.names)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured =
        readRDS(file.path("datasets", paste0(dataset.name, "-obscured.rds")))

    imputation.methods = list("median/mode"       = imputationMedianMode,
                              "random forest"     = imputationRandomForest,
                              "chained equations" = imputationMice)

    flog.info(paste("Baseline model:", classifiers.baseline))

    baseline.model.path = file.path("models",
                                    paste0(dataset.name, "-imputation-baseline.rds"))

    if (!file.exists(baseline.model.path))
    {
        model = readRDS(file.path("models",
                                  paste0(dataset.name, "-",
                                         classifiers.baseline, ".rds")))

        preproc.scheme = attr(model, "preproc.scheme")
        dataset.obscured.preprocessed = stats::predict(preproc.scheme,
                                                       dataset.obscured)

        datasets.imputed = list(lapply(names(imputation.methods), function(name){
            flog.info(paste("Imputation:", name))
            set.seed(SEED)
            imputation.methods[[name]](dataset.obscured.preprocessed) }))

        names(datasets.imputed[[1]]) = names(imputation.methods)

        baseline.model.with.imputation =
            crossValidationForImputation(datasets.imputed, list(model), ncv.folds)

        saveRDS(baseline.model.with.imputation, baseline.model.path)

    } else {
        flog.info("Baseline model exists, skipping learning")

        baseline.model.with.imputation = readRDS(baseline.model.path)

        folds.performances = baseline.model.with.imputation$folds.performances

        flog.info(paste0("Estimated ", ncv.performance.selector, ":    ",
                         round(mean(folds.performances[[ncv.performance.selector]]), 3)))
        flog.info(paste0("Estimated Sensitivity: ",
                         round(mean(folds.performances[["Sensitivity"]]), 3)))
        flog.info(paste0("Estimated Specificity: ",
                         round(mean(folds.performances[["Specificity"]]), 3)))
    }

    flog.info(paste(rep("*", 10), collapse = ""))

    flog.info("Grid search: classifiers and imputation methods")

    classifier.model.path = file.path("models",
                                      paste0(dataset.name, "-imputation-classifier.rds"))

    if (!file.exists(classifier.model.path))
    {
        models = list()
        datasets.imputed = list()

        for (model.name in classifiers.list)
        {
            flog.info(paste("Model:", model.name))

            model = readRDS(file.path("models",
                                      paste0(dataset.name, "-", model.name, ".rds")))

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

        classifier.model.with.imputation =
            crossValidationForImputation(datasets.imputed, models, ncv.folds)

        saveRDS(classifier.model.with.imputation, classifier.model.path)

    } else {
        flog.info("Classifier model exists, skipping learning")

        classifier.model.with.imputation = readRDS(classifier.model.path)

        folds.performances = classifier.model.with.imputation$folds.performances

        flog.info(paste0("Estimated ", ncv.performance.selector, ":    ",
                         round(mean(folds.performances[[ncv.performance.selector]]), 3)))
        flog.info(paste0("Estimated Sensitivity: ",
                         round(mean(folds.performances[["Sensitivity"]]), 3)))
        flog.info(paste0("Estimated Specificity: ",
                         round(mean(folds.performances[["Specificity"]]), 3)))
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}
