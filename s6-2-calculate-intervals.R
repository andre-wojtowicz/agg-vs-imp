# ---- step-6-2-calculate-intervals ----

source("init.R")
source("methods-interval-predictions.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S6.2.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 6-2: calculate intervals")

source("init-parallel.R")

seeds = get.seeds(SEED, c(length(DATASETS.NAMES), length(CLASSIFIERS.LIST)))


for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.aggregation.folds.raw.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.AGG.FOLDS.RAW)

    dataset.aggregation.folds.raw =
        readRDS(dataset.aggregation.folds.raw.file.path)

    #dataset.aggregation.folds.raw = dataset.aggregation.folds.raw[1:21]
    #sample(nrow(dataset.aggregation.folds.raw), 100)] # TODO: del

    flog.info(paste("No. cases:", nrow(dataset.aggregation.folds.raw)))


    dataset.aggregation.folds.cols.agg =
        grep("^agg\\.", names(dataset.aggregation.folds.raw), value = TRUE)
    dataset.aggregation.folds.cols.class =
        tail(setdiff(names(dataset.aggregation.folds.raw),
                     dataset.aggregation.folds.cols.agg),
             1)
    dataset.aggregation.folds.cols.preds = setdiff(
        setdiff(names(dataset.aggregation.folds.raw),
                dataset.aggregation.folds.cols.agg),
        dataset.aggregation.folds.cols.class
    )


    dataset.aggregation.folds.interval.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.INTERVAL)

    if (!file.exists(dataset.aggregation.folds.interval.file.path))# | OVERWRITE.OUTPUT.FILES)
    {
        flog.info("New calculations")

        dataset.aggregation.folds.interval =
            data.table(dataset.aggregation.folds.raw[, dataset.aggregation.folds.cols.agg,
                                                     with = FALSE])

        dataset.aggregation.folds.interval[
            , agg.class := as.integer(dataset.aggregation.folds.raw[[
                                      dataset.aggregation.folds.cols.class]]) - 1]

        dataset.aggregation.folds.interval[
            , as.vector(sapply(CLASSIFIERS.LIST,
                               function(x){paste0(x, c(".lower", ".upper"))})) :=
                NA_real_,
            with = FALSE]

    } else {
        flog.info("Restoring calculations")

        dataset.aggregation.folds.interval =
            readRDS(dataset.aggregation.folds.interval.file.path)
    }

    for (model.name in CLASSIFIERS.LIST)
    {
        .Random.seed =
            extract.seed(
                seeds,
                c(which(dataset.name == DATASETS.NAMES),
                  which(model.name == CLASSIFIERS.LIST)))

        flog.info(paste("Model:", model.name))

        model.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            CLASSIFIERS.LEARNED)

        model = readRDS(model.file.path)

        used.predictors = attr(model, "used.predictors")
        preproc.scheme  = attr(model, "preproc.scheme")

        dataset.aggregation.folds.raw.preprocessed =
            stats::predict(preproc.scheme, dataset.aggregation.folds.raw)

        par.seeds = sample.int(10000, nrow(dataset.aggregation.folds.raw.preprocessed),
                               replace = TRUE)


        start.vec = is.na(dataset.aggregation.folds.interval[[paste0(model.name, ".lower")]])
        if (any(start.vec))
        {
            start.id = min(which(start.vec))
        } else {
            flog.info("All cases calculated, skipping")
            next
        }

        flog.info(paste("Starting from case no.:", start.id))

        no.full.jobs = (nrow(dataset.aggregation.folds.raw.preprocessed) - start.id + 1) %/%
            (foreach::getDoParWorkers() * PARALLEL.NO.JOBS.PER.CHUNK)
        no.rem.jobs  = (nrow(dataset.aggregation.folds.raw.preprocessed) - start.id + 1) %%
            (foreach::getDoParWorkers() * PARALLEL.NO.JOBS.PER.CHUNK)

        seq.jobs.partition =
            split(start.id:nrow(dataset.aggregation.folds.raw.preprocessed),
                  rep(1:(no.full.jobs + ifelse(no.rem.jobs > 0, 1, 0)),
                      if (no.rem.jobs > 0)
                      {
                        c(rep(foreach::getDoParWorkers() *
                                  PARALLEL.NO.JOBS.PER.CHUNK, no.full.jobs), no.rem.jobs)
                      } else {
                        rep(foreach::getDoParWorkers() * PARALLEL.NO.JOBS.PER.CHUNK,
                            no.full.jobs)
                      }
                  )
            )

        for (seq.job.id in 1:length(seq.jobs.partition))
        {
            flog.info(paste("Parallel job:", seq.job.id, "/", length(seq.jobs.partition)))

            par.jobs.partition = caret::createFolds(
                seq.jobs.partition[[seq.job.id]],
                foreach::getDoParWorkers())

            interval.predictions = foreach::foreach(
                no.job         = 1:length(par.jobs.partition),
                .multicombine  = TRUE,
                .maxcombine    = length(par.jobs.partition),
                .packages      = c("optimx", "data.table", "foreach")) %dopar%
            {
                job.results = data.table(id    = numeric(0),
                                         lower = numeric(0),
                                         upper = numeric(0))

                for (i in seq.jobs.partition[[seq.job.id]][par.jobs.partition[[no.job]]])
                {
                    set.seed(par.seeds[i])

                    case.predictors.all = dataset.aggregation.folds.raw.preprocessed[
                        i, dataset.aggregation.folds.cols.preds, with = FALSE]
                    case.class = dataset.aggregation.folds.raw.preprocessed[
                        i, dataset.aggregation.folds.cols.class, with = FALSE]

                    case.predictors.used = case.predictors.all[
                        , as.character(used.predictors), with = FALSE]

                    interval.lower = NULL
                    interval.upper = NULL

                    if (all(!is.na(case.predictors.used)))
                    {
                        # [1] All features are present
                        flog.info(paste("Case:", i, "- no optimization"))

                        predicted.value =
                            suppressWarnings(
                                stats::predict(model, case.predictors.all,
                                               type = "prob", na.action = NULL)[1, 2])

                        interval.lower = predicted.value
                        interval.upper = predicted.value

                    } else {

                        features.factors =
                            names(which(sapply(colnames(case.predictors.used),
                                               function(x){
                                                   is.factor(case.predictors.used[[x]])})
                                        == TRUE))

                        features.factors.nas =
                            features.factors[is.na(case.predictors.used[
                                , features.factors, with = FALSE])]

                        features.numeric =
                            colnames(case.predictors.used)[!colnames(case.predictors.used)
                                                           %in% features.factors]

                        features.numeric.nas =
                            features.numeric[is.na(case.predictors.used[
                                , features.numeric, with = FALSE])]

                        if (length(features.factors.nas) > 0)
                        {
                            factors.configs =
                                expand.grid(sapply(features.factors.nas,
                                                   function(x){levels(case.predictors.used[[x]])},
                                                   simplify = FALSE))

                            colnames(factors.configs) = features.factors.nas

                            if (length(features.numeric.nas) == 0)
                            {
                                # [2] Only factor features are not present
                                flog.info(paste("Case:", i, "- factor grid search"))

                                predicted.values =
                                    optim.factor.grid.search(model,
                                                             case.predictors.all,
                                                             factors.configs)

                                interval.lower = min(predicted.values)
                                interval.upper = max(predicted.values)

                            } else {
                                # [3] Factor and numeric features are not present
                                if (model.name %in% OPTIMIZATION.NUMERIC.BF.CLASSIFIERS)
                                {
                                    # [3.1] Non-smooth, derivative-free optimization
                                    flog.info(paste("Case:", i,
                                                    "- factor-numeric ns/df optimization"))

                                    predicted.values =
                                        optim.factor.numeric.nsdf(model,
                                                                  case.predictors.all,
                                                                  factors.configs,
                                                                  features.numeric.nas,
                                                                  OPTIMIZATION.NUMERIC.BF.REPS)

                                    interval.lower = min(predicted.values)
                                    interval.upper = max(predicted.values)

                                } else {
                                    # [3.2] Classic optimization
                                    flog.info(paste("Case:", i,
                                                    "- classic factor-numeric optimization"))

                                    predicted.values =
                                        optim.factor.numeric.classic(
                                            model,
                                            case.predictors.all,
                                            factors.configs,
                                            features.numeric.nas,
                                            OPTIMIZATION.NUMERIC.REPS,
                                            OPTIMIZATION.NUMERIC.METHOD
                                        )

                                    interval.lower = min(predicted.values[, 1])
                                    interval.upper = max(predicted.values[, 2])
                                }
                            }

                        } else {

                            # [4] Only numeric features are not present
                            if (model.name %in% OPTIMIZATION.NUMERIC.BF.CLASSIFIERS)
                            {
                                # [4.1] Non-smooth, derivative-free optimization
                                flog.info(paste("Case:", i, "- numeric ns/df optimization"))

                                predicted.values =
                                    optim.numeric.nsdf(model,
                                                       case.predictors.all,
                                                       features.numeric.nas,
                                                       OPTIMIZATION.NUMERIC.BF.REPS)

                                interval.lower = min(predicted.values)
                                interval.upper = max(predicted.values)

                            } else {
                                # [4.2] Classic optimization
                                flog.info(paste("Case:", i, "- classic numeric optimization"))

                                predicted.values =
                                    optim.numeric.classic(model,
                                                          case.predictors.all,
                                                          features.numeric.nas,
                                                          OPTIMIZATION.NUMERIC.REPS,
                                                          OPTIMIZATION.NUMERIC.METHOD)

                                interval.lower = min(predicted.values[1, ])
                                interval.upper = max(predicted.values[2, ])
                            }
                        }
                    }


                    if (any(is.na(  c(interval.lower, interval.upper))) ||
                        any(is.null(c(interval.lower, interval.upper))))
                    {
                        stop.script("Lower and/or upper bound is NA/NULL")
                    }

                    if (interval.lower > interval.upper)
                    {
                        stop.script("Lower bound greater than upper bound")
                    }

                    flog.info(paste0("Predicted interval: [",
                                     round(interval.lower, 4), ", ",
                                     round(interval.upper, 4), "]"))

                    job.results = rbindlist(list(job.results,
                        data.table(id = i, lower = interval.lower, upper = interval.upper)
                    ))
                }

                job.results
            }

            if (any(class(interval.predictions) == "list"))
            {
                interval.predictions = rbindlist(interval.predictions)
            }

            interval.predictions = interval.predictions[order(id)]

            dataset.aggregation.folds.interval[
                seq.jobs.partition[[seq.job.id]],
                paste0(model.name, c(".lower", ".upper")) :=
                    list(interval.predictions[, lower],
                         interval.predictions[, upper]),
                with = FALSE]

            saveRDS(dataset.aggregation.folds.interval,
                    dataset.aggregation.folds.interval.file.path)
        }
    }

    flog.info(paste(rep("*", 25), collapse = ""))
}

stop.cluster()
