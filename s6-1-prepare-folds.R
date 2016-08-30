# ---- step-6-1-prepare-folds ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S6.1.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 6-1: prepare folds")

seeds = get.seeds(SEED, length(DATASETS.NAMES))

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.aggregation.folds.raw.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.AGG.FOLDS.RAW)

    if (!file.exists(dataset.aggregation.folds.raw.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        dataset.obscured.file.path =
            replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.OBSCURED)
        dataset.obscured = readRDS(dataset.obscured.file.path)

        dataset.aggregation.folds.raw = cbind(
            dataset.obscured[0, ], # columns from dataset
            data.frame(agg.outer.id   = integer(0),   # number of CV iteration
                   agg.inner.id       = integer(0),   # number of CV iteration, NA if outer-cv
                   agg.type           = character(0), # training or testing
                   agg.no.missing.att = numeric(0))  # number of missing attributes
            )

        data.table::setDT(dataset.aggregation.folds.raw)

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

        dataset.obscurable.predictors =
            as.character(dataset.used.predictors -
                         as.set(DATASETS.ALWAYS.AVAILABLE.PREDICTORS[[dataset.name]]))

        .Random.seed = extract.seed(seeds, which(dataset.name == DATASETS.NAMES))

        idx.outer.y =
            paste(dataset.obscured[, ncol(dataset.obscured)],
                  rowSums(is.na(dataset.obscured[, as.character(dataset.used.predictors)])),
                  sep = ".")
        idx.outer = caret::createFolds(idx.outer.y,
                                       k = NCV.FOLDS)

        flog.info("Phase 1")
        for (i in 1:NCV.FOLDS)
        {
            flog.info(paste("Outer fold", i))

            folds.inner                = idx.outer[setdiff(1:NCV.FOLDS, i)]
            dataset.obscured.inner     = dataset.obscured[as.numeric(unlist(folds.inner)), ]

            idx.inner.y = paste(
                dataset.obscured.inner[, ncol(dataset.obscured)],
                rowSums(is.na(dataset.obscured.inner[, as.character(dataset.used.predictors)])),
                sep = ".")
            idx.inner = caret::createFolds(idx.inner.y,
                                           k = NCV.FOLDS)

            for (j in 1:NCV.FOLDS)
            {
                flog.info(paste("Inner fold", j))
                training.folds = idx.inner[setdiff(1:NCV.FOLDS, j)]
                testing.fold   = idx.inner[j]
                dataset.obscured.training =
                    dataset.obscured.inner[as.numeric(unlist(training.folds)), ]
                dataset.obscured.testing  =
                    dataset.obscured.inner[as.numeric(unlist(testing.fold)), ]

                data.table::setDT(dataset.obscured.training)
                data.table::setDT(dataset.obscured.testing)

                dataset.obscured.training.no.miss.att =
                    rowSums(is.na(dataset.obscured.training)[,
                        as.character(dataset.used.predictors), drop = FALSE])

                dataset.obscured.training.gen = data.table()
                n = length(unique(dataset.obscured.training.no.miss.att))

                for (k in 0:(n - 1))
                {
                    # take (n-k)-times 0-level cases
                    new.cases =
                        rbindlist(replicate(n - k,
                        dataset.obscured.training[
                            which(dataset.obscured.training.no.miss.att == 0)],
                        simplify = FALSE))

                    if (k > 0)
                    {
                        for (l in 1:k)
                        {
                            # take 1-time l-level cases
                            new.cases =
                                rbindlist(list(new.cases,
                                    dataset.obscured.training[
                                        which(dataset.obscured.training.no.miss.att == l)]))
                        }
                    }

                    # do nothing with k-level cases
                    # obscure other cases to become k-level

                    obscuration.func = function(row, k, l)
                    {
                        ret = as.list(row)
                        ret[sample(colnames(row)[!is.na(row)], k - l)] = NA_integer_
                        ret
                    }

                    if (k >= 1)
                    {
                        new.cases.is.na.rs = rowSums(is.na(new.cases))

                        for (l in 0:(k - 1))
                        {
                            # filter new.cases to l-level
                            row.ids = which(new.cases.is.na.rs == l)

                            # for each row:
                            # - filter used predictors without always available columns
                            # - filter non-NA columns
                            # - randomly set NA to k-l columns
                            new.cases[row.ids,
                                      dataset.obscurable.predictors :=
                                          obscuration.func(.SD, k, l),
                                      by = row.ids,
                                      .SDcols = dataset.obscurable.predictors,
                                      with = FALSE]
                        }
                    }

                    dataset.obscured.training.gen =
                        rbindlist(list(dataset.obscured.training.gen, new.cases))
                }

                training.agg.no.missing.att =
                    rowSums(is.na(dataset.obscured.training.gen)[,
                                  as.character(dataset.used.predictors), drop = FALSE])

                dataset.aggregation.folds.raw =
                    rbindlist(list(dataset.aggregation.folds.raw,
                                   dataset.obscured.training.gen[,
                                        c("agg.outer.id", "agg.inner.id",
                                          "agg.type", "agg.no.missing.att") :=
                                            .(i, j, "training", training.agg.no.missing.att)]))

                testing.agg.no.missing.att =
                    rowSums(is.na(dataset.obscured.testing)[,
                                  as.character(dataset.used.predictors), drop = FALSE])

                dataset.aggregation.folds.raw =
                    rbindlist(list(dataset.aggregation.folds.raw,
                                   dataset.obscured.testing[,
                                         c("agg.outer.id", "agg.inner.id",
                                           "agg.type", "agg.no.missing.att") :=
                                             .(i, j, "testing", testing.agg.no.missing.att)]))
            }
        }

        flog.info("Phase 2")
        for (i in 1:NCV.FOLDS)
        {
            flog.info(paste("Fold", i))

            training.folds = idx.outer[setdiff(1:NCV.FOLDS, i)]
            test.fold      = idx.outer[i]

            dataset.obscured.training = dataset.obscured[as.numeric(unlist(training.folds)), ]
            dataset.obscured.testing  = dataset.obscured[as.numeric(unlist(test.fold)), ]

            data.table::setDT(dataset.obscured.training)
            data.table::setDT(dataset.obscured.testing)

            # copy-paste from phase 1

            dataset.obscured.training.no.miss.att =
                rowSums(is.na(dataset.obscured.training)[,
                              as.character(dataset.used.predictors), drop = FALSE])

            dataset.obscured.training.gen = data.table()
            n = length(unique(dataset.obscured.training.no.miss.att))

            for (k in 0:(n - 1))
            {
                # take (n-k)-times 0-level cases
                new.cases =
                    rbindlist(replicate(n - k,
                                        dataset.obscured.training[
                                            which(dataset.obscured.training.no.miss.att == 0)],
                                        simplify = FALSE))

                if (k > 0)
                {
                    for (l in 1:k)
                    {
                        # take 1-time l-level cases
                        new.cases =
                            rbindlist(list(new.cases,
                                           dataset.obscured.training[
                                            which(dataset.obscured.training.no.miss.att == l)]))
                    }
                }

                # do nothing with k-level cases
                # obscure other cases to become k-level

                obscuration.func = function(row, k, l)
                {
                    ret = as.list(row)
                    ret[sample(colnames(row)[!is.na(row)], k - l)] = NA_integer_
                    ret
                }

                if (k >= 1)
                {
                    new.cases.is.na.rs = rowSums(is.na(new.cases))

                    for (l in 0:(k - 1))
                    {
                        # filter new.cases to l-level
                        row.ids = which(new.cases.is.na.rs == l)

                        # for each row:
                        # - filter used predictors without always available columns
                        # - filter non-NA columns
                        # - randomly set NA to k-l columns
                        new.cases[row.ids,
                                  dataset.obscurable.predictors :=
                                      obscuration.func(.SD, k, l),
                                  by = row.ids,
                                  .SDcols = dataset.obscurable.predictors,
                                  with = FALSE]
                    }
                }

                dataset.obscured.training.gen =
                    rbindlist(list(dataset.obscured.training.gen, new.cases))
            }


            training.agg.no.missing.att =
                rowSums(is.na(dataset.obscured.training.gen)[,
                              as.character(dataset.used.predictors), drop = FALSE])

            dataset.aggregation.folds.raw =
                rbindlist(list(dataset.aggregation.folds.raw,
                               dataset.obscured.training.gen[,
                                 c("agg.outer.id", "agg.inner.id",
                                   "agg.type", "agg.no.missing.att") :=
                                     .(i, NA_integer_,
                                       "training", training.agg.no.missing.att)]))

            testing.agg.no.missing.att =
                rowSums(is.na(dataset.obscured.testing)[,
                              as.character(dataset.used.predictors), drop = FALSE])

            dataset.aggregation.folds.raw =
                rbindlist(list(dataset.aggregation.folds.raw,
                               dataset.obscured.testing[,
                                c("agg.outer.id", "agg.inner.id",
                                  "agg.type", "agg.no.missing.att") :=
                                    .(i, NA_integer_,
                                      "testing", testing.agg.no.missing.att)]))
        }

        saveRDS(dataset.aggregation.folds.raw, dataset.aggregation.folds.raw.file.path)
    } else {
        flog.warn("Interval predictions exists, skipping calculations")
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}
