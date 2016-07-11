# ---- step-1-divide-data ----

source("init.R")

setupLogger(LOGGER.OUTPUT.S1.FILE)

flog.info("Step 1: divide data")

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    set.seed(SEED)
    dataset = readRDS(file.path(DATASETS.DIR, paste0(dataset.name, ".rds")))

    dataset.levels    = levels(dataset[, ncol(dataset)])
    dataset.classname = tail(colnames(dataset), 1)

    dataset.class.1 = dataset %>%
        filter_(interp(quote(a == b),
                       a = as.name(dataset.classname),
                       b = dataset.levels[1])) %>%
        sample_n(datasets.size.per.class)

    dataset.class.2 = dataset %>%
        filter_(interp(quote(a == b),
                       a = as.name(dataset.classname),
                       b = dataset.levels[2])) %>%
        sample_n(datasets.size.per.class)


    dataset.class.1.feature.selection = dataset.class.1 %>%
        filter(row_number() <= datasets.size.feature.selection / 2)

    dataset.class.1.classification = dataset.class.1 %>%
        filter(between(row_number(),
                       datasets.size.feature.selection / 2 + 1,
                       (datasets.size.feature.selection +
                            datasets.size.classification) / 2))

    dataset.class.1.obscuration = dataset.class.1 %>%
        filter(row_number() >= (datasets.size.feature.selection +
                                    datasets.size.classification) / 2 + 1)


    dataset.class.2.feature.selection = dataset.class.2 %>%
        filter(row_number() <= datasets.size.feature.selection / 2)

    dataset.class.2.classification = dataset.class.2 %>%
        filter(between(row_number(),
                       datasets.size.feature.selection / 2 + 1,
                       (datasets.size.feature.selection +
                            datasets.size.classification) / 2))

    dataset.class.2.obscuration = dataset.class.2 %>%
        filter(row_number() >= (datasets.size.feature.selection +
                                    datasets.size.classification) / 2 + 1)


    saveRDS(rbind(dataset.class.1.feature.selection, dataset.class.2.feature.selection),
            file.path("datasets", paste0(dataset.name, "-feature-selection.rds")))
    saveRDS(rbind(dataset.class.1.classification, dataset.class.2.classification),
            file.path("datasets", paste0(dataset.name, "-classification.rds")))
    saveRDS(rbind(dataset.class.1.obscuration, dataset.class.2.obscuration),
            file.path("datasets", paste0(dataset.name, "-obscuration.rds")))

}
