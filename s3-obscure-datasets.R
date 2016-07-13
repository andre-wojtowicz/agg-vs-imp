# ---- step-3-obsucre-datasets ----

source("init.R")

setup.logger(LOGGER.OUTPUT.S3.FILE)

flog.info("Step 3: obscure datasets")

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.obscured.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURED)

    if (!file.exists(dataset.obscured.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        set.seed(SEED)

        dataset.obscuration.file.path =
            replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURATION)

        dataset.obscuration = readRDS(dataset.obscuration.file.path)

        dataset.used.predictors = set()

        for (model.name in c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST))
        {
            model.file.path =
                replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                                c(dataset.name, model.name),
                                CLASSIFIERS.LEARNED)

            model = readRDS(model.file.path)

            dataset.used.predictors =
                dataset.used.predictors | attr(model, "used.predictors")
        }

        flog.info(paste("Used predictors:", length(dataset.used.predictors),
                        "of", ncol(dataset.obscuration) - 1))

        dataset.class.levels = levels(dataset.obscuration[, ncol(dataset.obscuration)])
        dataset.class.name   = tail(colnames(dataset.obscuration), 1)

        dataset.1 = dataset.obscuration %>%
            filter_(interp(quote(a == b),
                           a = as.name(dataset.class.name),
                           b = dataset.class.levels[1]))

        dataset.2 = dataset.obscuration %>%
            filter_(interp(quote(a == b),
                           a = as.name(dataset.class.name),
                           b = dataset.class.levels[2]))

        dataset.obscured = data.frame()

        for (dataset in list(dataset.1, dataset.2))
        {
            no.nas.pos = round(nrow(dataset) * OBSCURATION.NO.NAS.FRACTION)

            dataset.obscured = rbind(dataset.obscured, dataset[1:no.nas.pos, ])

            dataset.nas.idx = caret::createFolds(dataset[(no.nas.pos + 1):nrow(dataset),
                                                         ncol(dataset)],
                                                 k = length(dataset.used.predictors) - 1)

            for (i in 1:(length(dataset.used.predictors) - 1))
            {
                chunk = dataset[dataset.nas.idx[[i]] + no.nas.pos, ]
                chunk.not.used = chunk[, !(colnames(chunk) %in%
                                               c(as.character(dataset.used.predictors),
                                                 dataset.class.name))]
                chunk.used     = chunk[,   colnames(chunk) %in%
                                           c(as.character(dataset.used.predictors),
                                             dataset.class.name)]

                nas.matrix = matrix(FALSE,
                                    nrow = nrow(chunk.used),
                                    ncol = ncol(chunk.used) - 1)
                nas.matrix = t(apply(nas.matrix, 1, function(row) {
                    row[sample(1:length(row), i)] = TRUE;
                    row}))

                for (pos in as.data.frame(t(which(nas.matrix == TRUE, arr.ind = TRUE))))
                {
                    chunk.used[pos[1], pos[2]] = NA
                }

                chunk = cbind(chunk.used, chunk.not.used)
                chunk = chunk[, colnames(dataset)]

                dataset.obscured = rbind(dataset.obscured, chunk)
            }
        }

        saveRDS(dataset.obscured, dataset.obscured.file.path)
    } else {
        flog.warn("Obscured dataset exists, skipping")
    }
}
