# ---- step-6-3-choose-best-aggregation ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S6.3.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 6-3: choose best aggregation")

source("init-parallel.R")
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

    folds.performances = attr(agg.model, "folds.performances")

    flog.info(paste0("Choosed aggregation: ",
                           attr(agg.model, "aggregation.code"),
                     " [", attr(agg.model, "aggregation.group"),
                     " ",  attr(agg.model, "aggregation.subgroup"), "]"))

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

    flog.info(paste(rep("*", 25), collapse = ""))
}

stop.cluster()
