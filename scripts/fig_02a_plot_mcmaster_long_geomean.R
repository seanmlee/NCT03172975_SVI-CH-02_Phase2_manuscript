

# libraries --------------------------------------------------------------------
library(tidyverse)


# geomean ----------------------------------------------------------------------
dv_long_mcmaster_average <- dv_long_mcmaster %>%
  
  group_by(
    VISIT,
    dose
  ) %>%
  
  summarize(

    n = sum(!is.na(LBSTRESN)),
    mean_log = mean(log(LBSTRESN + 0.1), na.rm = TRUE),
    se_log   = sd(log(LBSTRESN + 0.1), na.rm = TRUE) / sqrt(n),
    crit     = qt(0.975, df = pmax(n - 1, 1)),
    geomean  = exp(mean_log) - 0.1,
    lower_CI = pmax(0, exp(mean_log - crit * se_log) - 0.1),
    upper_CI =          exp(mean_log + crit * se_log) - 0.1,
    .groups = "drop"
    
  )


# plot -------------------------------------------------------------------------
p_a <-
dv_long_mcmaster_average %>%

  ggplot(
    aes(
      x = VISIT,
      y = geomean,
      fill = dose
    )
  ) +
  
  geom_line(
    aes(
      color = dose
    ),
    linewidth = 0.5,
    position = position_dodge(width = 7)
  ) +
  
  geom_errorbar(
    aes(
      ymin = lower_CI, 
      ymax = upper_CI,
      color = dose
    ),
    width = 12,
    alpha = 0.5,
    linewidth = 0.25,
    position = position_dodge(width = 7)
  ) +
  
  geom_point(
    size = 3, 
    pch = 21,
    color = "black",
    position = position_dodge(width = 7)
  ) +
  
  scale_x_continuous(
    limits = c(100, 300),
    breaks = c(100, 140, 200, 300),
    labels = c("100", "140\nCHHI", "200", "300")
  ) +
  
  scale_y_continuous(
    trans = scales::log1p_trans(),
    breaks = c(0, 1, 10, 100, 1000),
    limits = c(0, 1000)
  ) +
  
  labs(
    title = "a)",
    x = "Study Day",
    y = "Geometric mean\neggs per gram"
  ) +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.text.align = 0,
    legend.position = "none"
  ) + 
  
  scale_color_discrete(
    labels = c(
      'Placebo', 
      expression(paste(italic('Na'), '-GST-1/Alhydrogel/5', mu, 'g AP 10-701')), 
      expression(paste(100*mu*g, italic(' Na'), '-GST-1/Alhydrogel')), 
      expression(paste(italic('Na'), '-GST-1/Alhydrogel/500', mu, 'g CpG 10104'))
    )
  ) +
  
  guides(
    color = guide_legend(ncol = 1)
  )

print(p_a)

