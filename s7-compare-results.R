# ---- step-7-compare-results ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S7.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 7: compare results")

# ---- classifiers-original ----

palette.fill = c("Original classifiers"        = "#0078B8", # color blind friendly palette
                 "Uncertaintified classifiers" = "#F26522",
                 "Imputation"                  = "#00A574",
                 "Aggregation strategy"        = "#F3E737")

palette.shapes = c("Original classifiers"        = 21,
                   "Uncertaintified classifiers" = 24,
                   "Imputation"                  = 22,
                   "Aggregation strategy"        = 25)

get.barplot = function(data)
{
    # data: Model | Value.min | Value.max | Measure

    data$Model = with(data, factor(Model, levels = rev(levels(Model))))

    ggplot(data,
           aes(x    = Model,
               y    = Value.min,
               fill = Model)) +
        geom_hline(yintercept = 0.5, color = "grey90", linetype = 2) +
        geom_hline(yintercept = 1.0, color = "grey90", linetype = 2) +
        geom_bar(stat  = "identity",
                 color = "grey60",
                 width = 0.75) +
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
        geom_text(aes(x = Model,
                      y = ifelse(is.na(Value.max), 1, Value.min),
                      label = ifelse(is.na(Value.max), "",
                                     format(Value.min, digits = 3, nsmall = 3))),
                  hjust  = 1.25,
                  size   = 3,
                  colour = "white") +
        scale_fill_manual(values = palette.fill)


}

get.lineplot = function()
{
    # ggplot(ds.lineplot, aes(x = Missing.attributes,
    #                         y     = value,
    #                         fill  = Model,
    #                         color = Model,
    #                         shape = Model,
    #                         order = Model)) +
    #     geom_line(size = 1) +
    #     geom_point(size = 2, color = "grey30") +
    #     scale_x_continuous(expand = c(0, 0),
    #                        limits = c(0, max(ds.lineplot$Missing.attributes)),
    #                        breaks = 0:max(ds.lineplot$Missing.attributes)) +
    #     scale_y_continuous(expand = c(0, 0),
    #                        limits = c(0, 1.05)) +
    #     theme_classic() +
    #     geom_hline(yintercept = 0, color = "grey") +
    #     geom_vline(xintercept = 0, color = "grey") +
    #     theme(axis.title.x    = element_blank(),
    #           axis.title.y    = element_blank(),
    #           axis.text.x     = element_text(color = "grey50"),
    #           axis.text.y     = element_text(color = "grey50"),
    #           axis.ticks.x    = element_line(color = "grey50"),
    #           axis.ticks.y    = element_line(color = "grey50"),
    #           legend.position = "right",
    #           legend.title    = element_blank(),
    #           plot.margin     = unit(c(0.5, 0, 0.5, 0.5), "cm")) +
    #     scale_shape_manual(values = c(21,24,22,25,23,21))
}

# ----------------------

df = data.frame(Model = c("Original classifiers", "Uncertaintified classifiers",
                          "Imputation", "Aggregation strategy"),
                Value.min = c(0.44, 0.47, 0.62, 0.79),
                Value.max = c(0.49, 0.55, NA, NA),
                Measure   = "Accuracy",
                stringsAsFactors = FALSE)
df$Model = with(df, factor(Model, levels = Model))

p1 = get.barplot(df)
print(p1)
