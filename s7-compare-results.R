# ---- step-7-compare-results ----

source("init.R")

library(grid)

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
        theme(axis.title.x    = element_text(color = "grey50", size = 10),
              axis.title.y    = element_blank(),
              axis.text.x     = element_text(color = "grey50"),
              axis.text.y     = element_text(color = "black"),
              axis.ticks.x    = element_line(color = "grey50"),
              axis.ticks.y    = element_line(color = "grey50"),
              legend.position = "none",
              plot.margin     = unit(c(0.5, 1.5, 0.5, 0.5), "cm")) +
        labs(y = "Measure") +
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
        theme(axis.title.x    = element_text(color = "grey50", size = 10),
              axis.title.y    = element_text(color = "grey50", size = 10),
              axis.text.x     = element_text(color = "grey50"),
              axis.text.y     = element_text(color = "grey50"),
              axis.ticks.x    = element_line(color = "grey50"),
              axis.ticks.y    = element_line(color = "grey50"),
              legend.position = "right",
              legend.title    = element_blank(),
              plot.margin     = unit(c(0.5, 0, 0.5, 0.5), "cm")) +
        labs(x = "Missing data level", y = "Measure") +
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
