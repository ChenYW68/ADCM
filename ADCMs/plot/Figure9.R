######################################################################
source("./code/1_Cross_Validation/LangFang_CI.R")
# size <- c(100.5, 0.8, 40)
ls <- c(1.3, 1.3, 1.3)
######################################################################
p <- ggplot(Da, aes(x = DATE_TIME, group  = Model)) +
  geom_ribbon(
    aes(
      ymin = Fit.L25,
      ymax = Fit.U95,
      linetype = Model,
      fill = Model
    ),
    alpha = 0.3,
    size = 0.5
  ) +
  geom_line(aes(
    y = Fit.Median,
    linetype = Model,
    col = Model,
    alpha = alpha,
    size = size
  )) +
  theme_bw() +
  scale_colour_manual(
                      name = '',
                      values = c("black", "gray50", "#43a2ca"),
                      labels = label
                    ) +
  scale_fill_manual(
                    name = '',
                  values = c("transparent",
                             "transparent",
                             "#43a2ca"),
                  labels = label
                ) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_size_manual(values = ls) +
  scale_linetype_manual(
                          name = '',
                          values = c("dashed", "dotdash",
                                     "solid"),
                          labels = label
                        ) +
  scale_x_continuous(
    # expand = c(1e-5, 0),
    breaks  = unique(Da$DATE_TIME)[seq(1, num, 10, )]
                                  , labels = c(
                                    "Nov 01, 2015",
                                    "Nov 11, 2015",
                                    "Nov 21, 2015",
                                    "Dec 01, 2015",
                                    "Dec 11, 2015",
                                    "Dec 21, 2015"
                                  )
                                ) +
  scale_y_continuous(
    limits = c(0, UP),
    breaks  = seq(0, UP, scal),
    labels = seq(0, UP, scal)
  ) +
  labs(
    x = "Date",
    fill = "",
    y = latex2exp::TeX("PM$_{2.5}$ $( \\mu g/m^3 )$")
  ) +
  theme(
    axis.text = element_text(size = 20, colour = "black")
    ,
    axis.text.x = element_text(
      hjust = 0.6,
      size = 20,
      colour = "black"
    )
    ,
    axis.title = element_text(size = 22, colour = "black")
    ,
    legend.title = element_text(size = 20, colour = "black")
    ,
    legend.text = element_text(size = 20, colour = "black")
    # , legend.title = element_blank()
    ,
    legend.background = element_rect(colour = 'transparent'
                                     , fill = 'transparent')
    ,
    legend.key.width = unit(8.5, "line")
    ,
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    legend.position = c(0.5, 1)
    # , legend.margin = margin(t = -0.1, unit='cm')
    , strip.text =  element_text(size = 16, colour = "black")
  ) +
  guides(
    col = guide_legend(
      override.aes = list(linewidth = c(1.5, 1.5, 1.5)),
      nrow = 1,
      byrow = TRUE
    ), alpha = "none", size = "none"
  )
# p
ggsave(
  plot = p,
  file = "./figure/Fig9.pdf",
  width  = 18,
  height = 5#,
  # dpi = 500
)
