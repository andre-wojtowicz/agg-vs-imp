# ---- step-7-compare-results ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S7.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 7: compare results")

factor.levels = c("Original classifiers", "Uncertaintified classifiers",
                  "Imputation", "Aggregation strategy")

palette.fill = c("Original classifiers"        = "#0078B8", # color blind friendly palette
                 "Uncertaintified classifiers" = "#F26522",
                 "Imputation"                  = "#00A574",
                 "Aggregation strategy"        = "#F3E737")

palette.shapes = c("Original classifiers"        = NA,
                   "Uncertaintified classifiers" = NA,
                   "Imputation"                  = 22,
                   "Aggregation strategy"        = 25)

performance.measures = c("Accuracy", "Decisiveness", "Sensitivity", "Specificity")

get.barplot = function(data)
{
    # data: Model | Value.min | Value.max | Measure

    data$Model = with(data, factor(Model, levels = rev(levels(Model))))

    p = ggplot(data,
           aes(x    = Model,
               y    = Value.min,
               fill = Model)) +
        geom_hline(yintercept = 0.25, color = "grey90", linetype = 2) +
        geom_hline(yintercept = 0.50, color = "grey90", linetype = 2) +
        geom_hline(yintercept = 0.75, color = "grey90", linetype = 2) +
        geom_hline(yintercept = 1.00, color = "grey90", linetype = 2) +
        geom_bar(stat  = "identity",
                 color = "grey60",
                 width = 0.75) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, 1)) +
        theme_classic() +
        geom_hline(yintercept = 0,   color = "grey") +
        geom_vline(xintercept = ifelse(nlevels(data$Model) > 1, 0.4, 0.5), color = "grey") +
        theme(axis.title.x    = element_blank(),
              axis.title.y    = element_blank(),
              axis.text.x     = element_text(color = "grey50"),
              axis.text.y     = element_text(color = "black"),
              axis.ticks.x    = element_line(color = "grey50"),
              axis.ticks.y    = element_line(color = "grey50"),
              legend.position = "none",
              plot.margin     = unit(c(0.5, 1.5, 0.5, 0.5), "cm")) +
        coord_flip() +
        geom_segment(aes(x = Model, xend = Model,
                         y = Value.min,
                         yend = ifelse(!is.na(Value.max), Value.max, Value.min)),
                     color = "grey60") +
        geom_segment(aes(x = as.numeric(Model) - 0.25, xend = as.numeric(Model) + 0.25,
                         y = ifelse(!is.na(Value.max), Value.max, Value.min),
                         yend = ifelse(!is.na(Value.max), Value.max, Value.min)),
                     color = "grey60") +
        geom_text(aes(x = Model,
                      y = ifelse(!is.na(Value.max), Value.max, Value.min),
                      label = format(ifelse(!is.na(Value.max), Value.max, Value.min),
                                     digits = 3, nsmall = 3)),
                  hjust  = -0.25,
                  size   = 3,
                  colour = "grey45") +
        scale_fill_manual(values = palette.fill)

    if (nrow(filter(data, !is.na(Value.max) & Value.min > 0.2)) > 0)
    {
        p = p +
            geom_text(data = data %>% filter(!is.na(Value.max) & Value.min > 0.2),
                      aes(x = Model,
                          y = Value.min,
                          label = format(Value.min, digits = 3, nsmall = 3),
                          hjust  = 1.25),
                      colour = "white",
                      size   = 3)
    }

    if (nrow(data %>% filter(!is.na(Value.max) & Value.min < 0.2)) > 0)
    {
        p = p + geom_text(data = data %>% filter(!is.na(Value.max) & Value.min < 0.2),
                          aes(x = as.numeric(Model) + 0.225,
                              y = Value.min,
                              label = format(Value.min, digits = 3, nsmall = 3),
                              hjust  = -0.25),
                          colour = "grey45",
                          size   = 3)
    }


    # disable clipping:
    gt = ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] = "off"
    gt
}

get.lineplot = function(data, data.loess = NULL)
{
    # data: Model | Level | Value.min | Value.max | Value | Measure

    p = ggplot() +
        geom_hline(yintercept = 1.00, color = "grey90", linetype = 2) +
        geom_hline(yintercept = 0.75, color = "grey90", linetype = 2) +
        geom_hline(yintercept = 0.50, color = "grey90", linetype = 2) +
        geom_hline(yintercept = 0.25, color = "grey90", linetype = 2) +
        geom_line(data = data %>% filter(!is.na(Value)),
                  aes(x     = Level,
                      y     = Value,
                      shape = Model,
                      color = Model,
                      fill  = Model),
                  size = 1) +
        geom_point(data = data %>% filter(!is.na(Value)),
                   aes(x     = Level,
                       y     = Value,
                       shape = Model,
                       color = Model,
                       fill  = Model),
                   color = "grey30",
                   size  = 2) +
        scale_x_continuous(expand = c(0, 0),
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.1)) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, 1.05)) +
        theme_classic() +
        geom_hline(yintercept = 0, color = "grey") +
        geom_vline(xintercept = 0, color = "grey") +
        theme(axis.title.x    = element_blank(),
              axis.title.y    = element_blank(),
              axis.text.x     = element_text(color = "grey50"),
              axis.text.y     = element_text(color = "grey50"),
              axis.ticks.x    = element_line(color = "grey50"),
              axis.ticks.y    = element_line(color = "grey50"),
              legend.position = "right",
              legend.title    = element_blank(),
              plot.margin     = unit(c(0.5, 0, 0.5, 0.5), "cm")) +
        scale_shape_manual(values = palette.shapes, breaks = levels(data$Model)) +
        scale_fill_manual(values  = palette.fill,   breaks = levels(data$Model)) +
        scale_color_manual(values = palette.fill,   breaks = levels(data$Model))

    if (is.null(data.loess))
    {
        p = p +
            geom_ribbon(data = data %>% filter(is.na(Value)),
                        aes(x     = Level,
                            ymin  = Value.min,
                            ymax  = Value.max,
                            shape = Model,
                            color = Model,
                            fill  = Model),
                        alpha = 0.15)
    } else {
        p = p + geom_smooth(data = data.loess,
                            aes(x     = Level,
                                y     = Value,
                                shape = Model,
                                color = Model,
                                fill  = Model),
                            na.rm = TRUE,
                            alpha = 0.15,
                            se = FALSE,
                            method = "loess")

        ribbon.data =
            foreach::foreach(model.name = c("Original classifiers",
                                            "Uncertaintified classifiers"),
                             .combine   = rbind) %do%
            {
                loess.p1.data = data.loess %>% filter(Model == model.name)

                suppressWarnings(
                    loess.p1 <-
                      with(loess.p1.data,
                          predict(loess(Value ~ Level), se = TRUE,
                                  newdata = data.frame(Level = seq(0, max(Level), 0.025)))))

                loess.p1.min = pmin(pmax(loess.p1$fit - qt(0.975, loess.p1$df) * loess.p1$se, 0), 1)
                loess.p1.max = pmax(pmin(loess.p1$fit + qt(0.975, loess.p1$df) * loess.p1$se, 1), 0)

                data.frame(Level = seq(0, max(loess.p1.data$Level), 0.025),
                           Value.min = loess.p1.min,
                           Value.max = loess.p1.max,
                           Model = model.name)
            }

        p = p +
            geom_ribbon(data = ribbon.data,
                        aes(x     = Level,
                            ymin  = Value.min,
                            ymax  = Value.max,
                            shape = Model,
                            color = Model,
                            fill  = Model),
                        alpha    = 0.15,
                        linetype = 0)
    }

    return(p)

    # disable clipping:
    # gt = ggplot_gtable(ggplot_build(p))
    # gt$layout$clip[gt$layout$name == "panel"] = "off"
    # print(grid::grid.draw(gt))
}

# ----------------------

#df = data.frame(Model = c("Original classifiers", "Uncertaintified classifiers",
#                         "Imputation", "Aggregation strategy"),
#               Value.min = c(0.04, 0.47, 0.62, 0.99),
#               Value.max = c(0.49, 0.55, NA, NA),
#               Measure   = "Accuracy",
#               stringsAsFactors = FALSE)
#df$Model = with(df, factor(Model, levels = Model))
#
#p1 = get.barplot(df)
#capture.output(print(grid::grid.draw(p1)))

# ----------------------

# df2 = data.frame(Model = c(rep("Original classifiers", 5),
#                            rep("Uncertaintified classifiers", 5),
#                            rep("Imputation", 5),
#                            rep("Aggregation strategy", 5)),
#                  Level = rep(0:4, 4),
#                  Value.min = c(c(0.43, 0.41, 0.35, 0.31, 0.27),
#                                c(0.49, 0.47, 0.42, 0.37, 0.33),
#                                rep(NA, 5),
#                                rep(NA, 5)),
#                  Value.max = c(c(0.53, 0.49, 0.40, 0.33, 0.30),
#                                c(0.56, 0.51, 0.44, 0.42, 0.39),
#                                rep(NA, 5),
#                                rep(NA, 5)),
#                  Value = c(rep(NA, 5),
#                            rep(NA, 5),
#                            c(0.60, 0.55, 0.53, 0.51, 0.44),
#                            c(0.71, 0.62, 0.54, 0.51, 0.41)),
#                  Measure   = "Accuracy",
#                  stringsAsFactors = FALSE)
# df2$Model = with(df2, factor(Model, levels = unique(Model)))
#
# p2 = get.lineplot(df2)
# print(p2)

for (dataset.name in DATASETS.NAMES)
{
    df.barplot =
        data.frame(Model     = character(0),
                   Value.min = numeric(0),
                   Value.max = numeric(0),
                   Measure   = character(0),
                   stringsAsFactors = FALSE)

    df.lineplot =
        data.frame(Model     = character(0),
                   Level     = integer(0),
                   Value.min = numeric(0),
                   Value.max = numeric(0),
                   Value     = numeric(0),
                   Measure   = character(0),
                   stringsAsFactors = FALSE)


    # original classifiers

    df.orig.cls =
        foreach::foreach(model.name = CLASSIFIERS.LIST,
                         .combine   = rbind) %do%
    {
        classifier.performance.original.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            CLASSIFIERS.PERFORMANCE.ORIGINAL)

        performance.df = readRDS(classifier.performance.original.file.path)

        performance.df %>% mutate(Classifier = model.name)
    }

    df.orig.cls.bp.measures = df.orig.cls %>%
        filter(is.na(Missing.attributes)) %>%
        select(-Missing.attributes, -Classifier)

    df.barplot = df.barplot %>%
        rbind(
            cbind(
                data.frame(Model = "Original classifiers",
                           stringsAsFactors = FALSE),
                join(df.orig.cls.bp.measures %>%
                         summarise_each(funs(min(., na.rm = TRUE))) %>%
                         melt(id.vars = NULL) %>%
                         rename(Measure = variable, Value.min = value),
                     df.orig.cls.bp.measures %>%
                         summarise_each(funs(max(., na.rm = TRUE))) %>%
                         melt(id.vars = NULL) %>%
                         rename(Measure = variable, Value.max = value),
                     by = "Measure"))
            %>% select(Model, Value.min, Value.max, Measure))

    df.orig.cls.lp.measures = df.orig.cls %>%
        filter(!is.na(Missing.attributes)) %>%
        select(-Classifier)


    df.orig.cls.lp.measures.processed =
        foreach::foreach(missing.lvl = 0:max(df.orig.cls.lp.measures$Missing.attributes),
                         .combine = rbind) %do%
    {
        df = df.orig.cls.lp.measures %>%
            filter(Missing.attributes == missing.lvl) %>%
            select(-Missing.attributes)

        cbind(
            data.frame(Model = "Original classifiers",
                       Level = missing.lvl,
                       Value = NA,
                       stringsAsFactors = FALSE),
            join(df %>%
                     summarise_each(funs(min(., na.rm = TRUE))) %>%
                     melt(id.vars = NULL) %>%
                     rename(Measure = variable, Value.min = value),
                 df %>%
                     summarise_each(funs(max(., na.rm = TRUE))) %>%
                     melt(id.vars = NULL) %>%
                     rename(Measure = variable, Value.max = value),
                 by = "Measure")
        )
    } %>% select(Model, Level, Value.min, Value.max, Value, Measure)

    df.lineplot = df.lineplot %>%
        rbind(df.orig.cls.lp.measures.processed)

    df.lineplot.loess = df.orig.cls.lp.measures %>%
                        melt(measure.vars = performance.measures) %>%
                        rename(Measure = variable, Value = value,
                               Level = Missing.attributes) %>%
                        mutate(Model = "Original classifiers")

    # uncertaintified classifiers

    df.unc.cls =
        foreach::foreach(model.name = CLASSIFIERS.LIST,
                         .combine   = rbind) %do%
    {
        classifier.performance.original.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(dataset.name, model.name),
                            CLASSIFIERS.PERFORMANCE.INTERVAL)

        performance.df = readRDS(classifier.performance.original.file.path)

        performance.df %>% mutate(Classifier = model.name)
    }

    df.unc.cls.bp.measures = df.unc.cls %>%
        filter(is.na(Missing.attributes)) %>%
        select(-Missing.attributes, -Classifier)

    df.barplot = df.barplot %>%
        rbind(
            cbind(
                data.frame(Model = "Uncertaintified classifiers",
                           stringsAsFactors = FALSE),
                join(df.unc.cls.bp.measures %>%
                         summarise_each(funs(min(., na.rm = TRUE))) %>%
                         melt(id.vars = NULL) %>%
                         rename(Measure = variable, Value.min = value),
                     df.unc.cls.bp.measures %>%
                         summarise_each(funs(max(., na.rm = TRUE))) %>%
                         melt(id.vars = NULL) %>%
                         rename(Measure = variable, Value.max = value),
                     by = "Measure"))
            %>% select(Model, Value.min, Value.max, Measure))

    df.unc.cls.lp.measures = df.unc.cls %>%
        filter(!is.na(Missing.attributes)) %>%
        select(-Classifier)


    df.unc.cls.lp.measures.processed =
        foreach::foreach(missing.lvl = 0:max(df.unc.cls.lp.measures$Missing.attributes),
                         .combine = rbind) %do%
    {
        df = df.unc.cls.lp.measures %>%
            filter(Missing.attributes == missing.lvl) %>%
            select(-Missing.attributes)

        cbind(
            data.frame(Model = "Uncertaintified classifiers",
                       Level = missing.lvl,
                       Value = NA,
                       stringsAsFactors = FALSE),
            join(df %>%
                     summarise_each(funs(min(., na.rm = TRUE))) %>%
                     melt(id.vars = NULL) %>%
                     rename(Measure = variable, Value.min = value),
                 df %>%
                     summarise_each(funs(max(., na.rm = TRUE))) %>%
                     melt(id.vars = NULL) %>%
                     rename(Measure = variable, Value.max = value),
                 by = "Measure")
        )
    } %>% select(Model, Level, Value.min, Value.max, Value, Measure)

    df.lineplot = df.lineplot %>%
        rbind(df.unc.cls.lp.measures.processed)

    df.lineplot.loess =
        rbind(df.lineplot.loess,
              df.unc.cls.lp.measures %>%
              melt(measure.vars = performance.measures) %>%
              rename(Measure = variable, Value = value,
                     Level = Missing.attributes) %>%
              mutate(Model = "Uncertaintified classifiers"))

    # imputation

    df.imp = readRDS(replace.strings(DATASETS.NAME.PATTERN, dataset.name,
                                     CLASSIFIERS.IMPUTATION.MODEL))

    df.imp.bp.measures = df.imp$folds.performances %>% filter(is.na(Missing.attributes))
    df.imp.lp.measures = df.imp$folds.performances %>% filter(!is.na(Missing.attributes))

    df.barplot = df.barplot %>%
        rbind(df.imp.bp.measures %>%
                  select(-Fold, -Missing.attributes) %>%
                  summarise_each(funs(mean)) %>%
                  melt(id.vars = NULL) %>%
                  rename(Measure = variable, Value.min = value) %>%
                  rbind(data.frame(Measure = "Decisiveness", Value.min = 1)) %>%
                  mutate(Value.max = NA, Model = "Imputation"))

    df.lineplot = df.lineplot %>% rbind(
        foreach::foreach(missing.lvl = 0:max(df.imp.lp.measures$Missing.attributes),
                         .combine    = rbind) %do%
        {
            df.imp.lp.measures %>% filter(Missing.attributes == missing.lvl) %>%
                select(-Fold, -Missing.attributes) %>%
                summarise_each(funs(mean)) %>%
                melt(id.vars = NULL) %>%
                rename(Measure = variable, Value = value) %>%
                rbind(data.frame(Measure = "Decisiveness", Value = 1)) %>%
                mutate(Value.min = NA, Value.max = NA, Model = "Imputation",
                       Level = missing.lvl)
        } %>% select(Model, Level, Value.min, Value.max, Value, Measure))

    # aggregation strategies

    df.agg = readRDS(replace.strings(DATASETS.NAME.PATTERN, dataset.name,
                                     AGGREGATION.LEARNED))

    df.agg.bp.measures = df.agg$folds.performances %>% filter(is.na(Missing.attributes))
    df.agg.lp.measures = df.agg$folds.performances %>% filter(!is.na(Missing.attributes))

    df.barplot = df.barplot %>%
        rbind(df.agg.bp.measures %>%
                  select(-Fold, -Missing.attributes) %>%
                  summarise_each(funs(mean)) %>%
                  melt(id.vars = NULL) %>%
                  rename(Measure = variable, Value.min = value) %>%
                  rbind(data.frame(Measure = "Decisiveness", Value.min = 1)) %>%
                  mutate(Value.max = NA, Model = "Aggregation strategy"))

    df.lineplot = df.lineplot %>% rbind(
        foreach::foreach(missing.lvl = 0:max(df.agg.lp.measures$Missing.attributes),
                         .combine    = rbind) %do%
        {
            df.agg.lp.measures %>% filter(Missing.attributes == missing.lvl) %>%
                select(-Fold, -Missing.attributes) %>%
                summarise_each(funs(mean)) %>%
                melt(id.vars = NULL) %>%
                rename(Measure = variable, Value = value) %>%
                rbind(data.frame(Measure = "Decisiveness", Value = 1)) %>%
                mutate(Value.min = NA, Value.max = NA, Model = "Aggregation strategy",
                       Level = missing.lvl)
        } %>% select(Model, Level, Value.min, Value.max, Value, Measure))

    # ---

    df.barplot$Model =
        with(df.barplot, factor(Model, levels = intersect(factor.levels, unique(Model))))
    df.lineplot$Model =
        with(df.lineplot, factor(Model, levels = intersect(factor.levels, unique(Model))))

    df.lineplot$Level = with(df.lineplot, Level / (max(Level) + 1))
    df.lineplot.loess$Level = with(df.lineplot.loess, Level / (max(Level) + 1))

    for (performance.measure in performance.measures)
    {
        grid::grid.newpage()
        capture.output(
            print(grid::grid.draw(get.barplot(df.barplot %>%
                                                  filter(Measure == performance.measure)))))

        print(get.lineplot(df.lineplot %>% filter(Measure == performance.measure),
                           df.lineplot.loess %>%
                               filter(Measure == performance.measure)))
    }
}
