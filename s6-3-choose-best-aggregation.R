# ---- step-6-3-choose-best-aggregation ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S6.3.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 6-3: choose best aggregation")

#source("init-parallel.R")
source("aggregation-operators.R")

if (!dir.exists(AGGREGATION.DIR))
{
    dir.create(AGGREGATION.DIR)
}

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    agg.model.file.path = replace.strings(DATASETS.NAME.PATTERN, dataset.name,
                                          AGGREGATION.LEARNED)

    if (!file.exists(agg.model.file.path))# | OVERWRITE.OUTPUT.FILES)
    {

        dataset.agg.folds.file.path =
            replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.INTERVAL)

        dataset.agg.folds =
            readRDS(dataset.agg.folds.file.path)

        # TODO: del begin

        tmp.x = foreach(i = 1:length(CLASSIFIERS.LIST), .combine = cbind) %do%
        {cbind(runif(nrow(dataset.agg.folds), 0, 0.5), runif(nrow(dataset.agg.folds), 0.5, 1))}
        colnames(tmp.x) = grep("\\.(upper)|(lower)$", colnames(dataset.agg.folds), value = TRUE)
        tmp.x = data.frame(tmp.x)
        setDT(tmp.x)

        dataset.agg.folds[
            , grep("\\.(upper)|(lower)$", colnames(dataset.agg.folds), value = TRUE) := tmp.x,
            with = FALSE]

        # TODO: del end

        if (any(is.na(dataset.agg.folds[
            , grep("\\.(upper)|(lower)$", colnames(dataset.agg.folds), value = TRUE),
            with = FALSE])))
        {
            stop.script("Some intervals equal to NA")
        }

        aggregation.strategies = get.grouped.aggregation.operators()

        agg.model = nested.cross.validation.for.aggregation(aggregation.strategies,
                                                            dataset.agg.folds,
                                                            NCV.FOLDS,
                                                            NCV.PERFORMANCE.SELECTOR,
                                                            NCV.PERFORMANCE.MAXIMIZE)

        saveRDS(agg.model, agg.model.file.path)
    } else {
        flog.warn("Aggregation model exists, skipping learning")

        agg.model = readRDS(agg.model.file.path)
    }

    folds.performance = attr(agg.model, "folds.performance")

    flog.info(paste0("Estimated ", NCV.PERFORMANCE.SELECTOR, ": ",
                     round(mean(folds.performance[[NCV.PERFORMANCE.SELECTOR]]), 3)))

    flog.info(paste(rep("*", 25), collapse = ""))
}

#stop.cluster()
