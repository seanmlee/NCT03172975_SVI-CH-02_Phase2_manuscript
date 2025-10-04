

# libraries --------------------------------------------------------------------
library(tidyverse)
library(scales)


# format -----------------------------------------------------------------------
dose_cols <- c(
  placebo = "#F8766D", 
  dose5 = "#7CAE00",
  dose100 = "#00BFC4", 
  dose500 = "#C77CFF"
  )

dose_labels <- c(
  'Placebo',
  expression(paste(italic('Na'), '-GST-1/Alhydrogel/5', mu, 'g AP 10-701')),
  expression(paste(100*mu*g, italic(' Na'), '-GST-1/Alhydrogel')),
  expression(paste(italic('Na'), '-GST-1/Alhydrogel/500', mu, 'g CpG 10104'))
)

dose_levels <- c("placebo", "dose5", "dose100", "dose500")

igg_long_average <- igg_long %>%
  
  mutate(dose = factor(dose, levels = dose_levels)) %>%
  
  group_by(dose, visit) %>%
  
  summarize(
    n = sum(!is.na(igg)),
    mean_log = mean(log(igg + 0.1), na.rm = TRUE),
    se_log = sd(log(igg + 0.1), na.rm = TRUE) / sqrt(n),
    crit = qt(0.975, df = pmax(n - 1, 1)),
    geomean = exp(mean_log) - 0.1,
    lower_CI = pmax(0, exp(mean_log - crit * se_log) - 0.1),
    upper_CI = exp(mean_log + crit * se_log) - 0.1,
    .groups = "drop"
  )


# plot -------------------------------------------------------------------------
p <- igg_long_average %>%
  
  ggplot(
    aes(
      x = visit, 
      y = geomean, 
      color = dose, 
      fill = dose
      )
    ) +
  
  geom_line(
    linewidth = 0.5, 
    position = position_dodge(width = 7), 
    show.legend = FALSE
    ) +
  
  geom_errorbar(
    aes(
      ymin = lower_CI, 
      ymax = upper_CI
      ),
    width = 12, 
    alpha = 0.5, 
    linewidth = 0.5,
    position = position_dodge(width = 7), 
    show.legend = FALSE
    ) +
  
  geom_point(
    shape = 21, 
    size = 5, 
    stroke = 0.6, 
    color = "black",
    position = position_dodge(width = 7), 
    show.legend = TRUE
    ) +
  
  labs(
    x = "Study Day",
    y = expression(paste("Geometric mean anti-", italic('Na'), "-GST-1 IgG (AU)"))
  ) +
  
  scale_x_continuous(
    limits = c(-5, 385),
    breaks = c(0, 56, 112, 140, 380),
    labels = c("0\nV1", "56\nV2", "112\nV3", "140\nCHHI", "380")
  ) +
  
  scale_y_continuous(
    trans = log1p_trans(),
    breaks = c(0, 1, 10, 100),
    limits = c(0, 190)
  ) +
  
  scale_color_manual(values = dose_cols, guide = "none") +
  
  scale_fill_manual(values  = dose_cols, labels = dose_labels, name = NULL) +
  
  guides(
    fill = guide_legend(
      ncol = 1, 
      byrow = TRUE,
      override.aes = list(shape = 21, size = 5, linetype = 0, colour = "black")
      )
    ) +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20, margin = margin(t = 10)),
    axis.title.y = element_text(size = 20, margin = margin(r = 10)),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.position  = "bottom",
    legend.direction = "vertical",
    legend.text = element_text(size = 20)
  )

p


# write ------------------------------------------------------------------------
ggsave(
  "out/fig5.tiff", 
  p, 
  dpi = 300, 
  width = 12, 
  height = 8
  )
