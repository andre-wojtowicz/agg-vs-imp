# ---- step-7-compare-results ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S7.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 7: compare results")

# ---- classifiers-original ----

classifiers.original.show.plot = function()
{
    ds.names = expand.grid(DATASETS.NAMES, c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST),
                           stringsAsFactors = FALSE)

    ds.combined = foreach::foreach(i = 1:nrow(ds.names),
                                   .combine = rbind) %do%
    {
        classifier.performance.original.file.path =
            replace.strings(c(DATASETS.NAME.PATTERN, CLASSIFIERS.NAME.PATTERN),
                            c(ds.names[i, 1], ds.names[i, 2]),
                            CLASSIFIERS.PERFORMANCE.ORIGINAL)

        readRDS(classifier.performance.original.file.path) %>%
            mutate(Dataset = ds.names[i, 1], Model = ds.names[i, 2])
    }

    for (dataset.name in DATASETS.NAMES)
    {
        ds.results = ds.combined %>% filter(Dataset == dataset.name)

        for (measure in c("Accuracy", "Sensitivity", "Specificity", "Decisiveness"))
        {
            # barplot
            ds.barplot = ds.results %>% filter(is.na(Missing.attributes)) %>%
                select(-Missing.attributes, -Dataset) %>%
                reshape2::melt(id.vars = "Model") %>%
                filter(variable == measure)  %>%
                mutate(Model = factor(Model,
                                      levels = rev(c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST)),
                                      ordered = TRUE))

            ggplot(ds.barplot,
                   aes(x    = Model,
                       y    = value,
                       fill = Model)) +
                geom_bar(stat = "identity") +
                scale_y_continuous(expand = c(0, 0),
                                   limits = c(0, 1)) +
                theme_classic() +
                geom_hline(yintercept = 0,   color = "grey") +
                geom_vline(xintercept = 0.4, color = "grey") +
                theme(axis.title.x    = element_blank(),
                      axis.title.y    = element_blank(),
                      axis.text.x     = element_text(color = "grey50"),
                      axis.text.y     = element_text(color = "black"),
                      axis.ticks.x    = element_line(color = "grey50"),
                      axis.ticks.y    = element_line(color = "grey50"),
                      legend.position = "none",
                      plot.margin     = unit(c(0.5, 1.5, 0.5, 0.5), "cm")) +
                coord_flip() +
                geom_text(aes(x = Model,
                              y = value,
                              label = format(value, digits = 3, nsmall = 3)),
                          hjust  = -0.25,
                          size   = 3,
                          colour = "grey45")

            # lineplot


            ds.lineplot = ds.results %>% filter(!is.na(Missing.attributes)) %>%
                select(-Dataset) %>%
                reshape2::melt(id.vars = c("Model", "Missing.attributes")) %>%
                filter(variable == measure) %>%
                filter(!is.na(value) & value > 0) %>%
                mutate(Model = factor(Model,
                                      levels = c(CLASSIFIERS.BASELINE, CLASSIFIERS.LIST),
                                      ordered = TRUE))

            ggplot(ds.lineplot, aes(x = Missing.attributes,
                                    y     = value,
                                    fill  = Model,
                                    color = Model,
                                    shape = Model,
                                    order = Model)) +
                geom_line(size = 1) +
                geom_point(size = 2, color = "grey30") +
                scale_x_continuous(expand = c(0, 0),
                                 limits = c(0, max(ds.lineplot$Missing.attributes)),
                                 breaks = 0:max(ds.lineplot$Missing.attributes)) +
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
                scale_shape_manual(values = c(21,24,22,25,23,21))

            browser()
        }

    }
}

classifiers.original.show.plot()
