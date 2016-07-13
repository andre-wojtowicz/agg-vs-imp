# ---- step-1-divide-data ----

source("init.R")

setup.logger(LOGGER.OUTPUT.S1.FILE)

flog.info("Step 1: divide data")

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    set.seed(SEED)

    dataset.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.ORIGIN)

    dataset = readRDS(dataset.file.path)

    dataset.levels    = levels(dataset[, ncol(dataset)])
    dataset.classname = tail(colnames(dataset), 1)

    dataset.class.1 = dataset %>%
        filter_(interp(quote(a == b),
                       a = as.name(dataset.classname),
                       b = dataset.levels[1])) %>%
        sample_n(DATASETS.SIZE.PER.CLASS)

    dataset.class.2 = dataset %>%
        filter_(interp(quote(a == b),
                       a = as.name(dataset.classname),
                       b = dataset.levels[2])) %>%
        sample_n(DATASETS.SIZE.PER.CLASS)

    dataset.feature.selection.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.FEATURE.SELECTION)
    dataset.classification.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.CLASSIFICATION)
    dataset.obscuration.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURATION)

    if (!file.exists(dataset.feature.selection.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        flog.info("Saving feature selection dataset")

        dataset.class.1.feature.selection = dataset.class.1 %>%
            filter(row_number() <= DATASETS.SIZE.FEATURE.SELECTION / 2)
        dataset.class.2.feature.selection = dataset.class.2 %>%
            filter(row_number() <= DATASETS.SIZE.FEATURE.SELECTION / 2)

        saveRDS(rbind(dataset.class.1.feature.selection, dataset.class.2.feature.selection),
                dataset.feature.selection.file.path)
    } else {
        flog.warn("Feature selection dataset exists, skipping")
    }

    if (!file.exists(dataset.classification.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        flog.info("Saving classification dataset")

        dataset.class.1.classification = dataset.class.1 %>%
            filter(between(row_number(),
                           DATASETS.SIZE.FEATURE.SELECTION / 2 + 1,
                           (DATASETS.SIZE.FEATURE.SELECTION +
                                DATASETS.SIZE.CLASSIFICATION) / 2))
        dataset.class.2.classification = dataset.class.2 %>%
            filter(between(row_number(),
                           DATASETS.SIZE.FEATURE.SELECTION / 2 + 1,
                           (DATASETS.SIZE.FEATURE.SELECTION +
                                DATASETS.SIZE.CLASSIFICATION) / 2))

        saveRDS(rbind(dataset.class.1.classification, dataset.class.2.classification),
                dataset.classification.file.path)
    } else {
        flog.warn("Classification dataset exists, skipping")
    }

    if (!file.exists(dataset.obscuration.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        flog.info("Saving obscuration dataset")

        dataset.class.1.obscuration = dataset.class.1 %>%
            filter(row_number() >= (DATASETS.SIZE.FEATURE.SELECTION +
                                        DATASETS.SIZE.CLASSIFICATION) / 2 + 1)
        dataset.class.2.obscuration = dataset.class.2 %>%
            filter(row_number() >= (DATASETS.SIZE.FEATURE.SELECTION +
                                        DATASETS.SIZE.CLASSIFICATION) / 2 + 1)

        saveRDS(rbind(dataset.class.1.obscuration, dataset.class.2.obscuration),
                dataset.obscuration.file.path)
    } else {
        flog.warn("Obscuration dataset exists, skipping")
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}
