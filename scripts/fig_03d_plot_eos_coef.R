

# libraries --------------------------------------------------------------------
library(sjPlot)
library(parameters)
library(dplyr)
library(ggplot2)


# fit model --------------------------------------------------------------------
mod_eos_dose <- glm(
  log(max_eos_count) ~ dose,
  data = dv_max
)

sjPlot::tab_model(
  mod_eos_dose,
  transform = "exp",
  show.r2   = FALSE,
  digits    = 3
)


# tidy rate ratios from GLM ----------------------------------------------------
prm_eos <- parameters::model_parameters(
  mod_eos_dose,
  exponentiate = TRUE,
  effects      = "fixed"
)

df_eos <- prm_eos %>%
  dplyr::filter(!Parameter %in% c("(Intercept)", "Intercept")) %>%
  dplyr::mutate(
    Group = dplyr::recode(
      Parameter,
      "dosedose500" = "Na-GST-1/Alhydrogel/500µg CpG 10104",
      "dosedose100" = "100µg Na-GST-1/Alhydrogel",
      "dosedose5"   = "Na-GST-1/Alhydrogel/5µg AP 10-701"
    )
  ) %>%

  dplyr::mutate(Group = factor(
    Group,
    levels = c(
      "Na-GST-1/Alhydrogel/500µg CpG 10104",
      "100µg Na-GST-1/Alhydrogel",
      "Na-GST-1/Alhydrogel/5µg AP 10-701"
    )
  )) %>%
  dplyr::transmute(
    Group,
    rr      = Coefficient,
    rr_low  = CI_low,
    rr_high = CI_high
  )

# same colors and label expressions as McMaster plot ---------------------------
dose_cols <- c(
  "Na-GST-1/Alhydrogel/500µg CpG 10104" = "#C77CFF",  # purple
  "100µg Na-GST-1/Alhydrogel"           = "#00B8E7",  # blue
  "Na-GST-1/Alhydrogel/5µg AP 10-701"   = "#7CAE00"   # green
)

y_labs <- c(
  "Na-GST-1/Alhydrogel/500µg CpG 10104" =
    expression(paste(italic('Na'), '-GST-1/Alhydrogel/500', mu, 'g CpG 10104')),
  "100µg Na-GST-1/Alhydrogel" =
    expression(paste(100*mu*g, italic(' Na'), '-GST-1/Alhydrogel')),
  "Na-GST-1/Alhydrogel/5µg AP 10-701" =
    expression(paste(italic('Na'), '-GST-1/Alhydrogel/5', mu, 'g AP 10-701'))
)

# truncate to match 0–2 range used for McMaster --------------------------------
df_eos <- df_eos %>%
  dplyr::mutate(
    rr_plot      = pmax(rr, 0),
    rr_low_plot  = pmax(rr_low, 0),
    rr_high_plot = pmin(rr_high, 2)
  )

# plot (panel d) ---------------------------------------------------------------
plot_eos_coef <-
  
  ggplot(
    df_eos, 
    aes(
      x = rr_plot, 
      y = Group, 
      color = Group
    )
  ) +
  
  geom_vline(
    xintercept = 1, 
    linetype = "solid", 
    alpha = 0.25
  ) +
  
  geom_errorbarh(
    aes(
      xmin = rr_low_plot, 
      xmax = rr_high_plot
    ),
    height = 0.5, 
    size = 0.75
  ) +
  
  geom_point(size = 8) +
  
  geom_text(
    aes(label = sprintf("%.2f", rr_plot)),
    height = 0.5, 
    size = 1,
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = dose_cols, guide = "none") +
  
  scale_x_continuous(
    "Geomeric Mean Ratio",
    limits = c(-0.1, 2),
    breaks = c(0, 1, 2)
  ) +
  
  scale_x_log10(
    name = expression("Geometric Mean Ratio (log"[10]*")"),
    breaks = c(0.001, 0.01, 0.1, 1, 3),
    labels = c("0.001", "0.01", "0.1", "1", "3"),
    limits = c(0.001, 3)
  ) +
  
  scale_y_discrete(
    position = "right",
    labels = y_labs
  ) +
  
  ggtitle("d)") +
  
  coord_cartesian(clip = "off") +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 20),
    axis.title.x     = element_text(size = 20),
    axis.title.y     = element_blank(),
    axis.text.x      = element_text(size = 20),
    axis.text.y      = element_text(
      size  = 20,
      hjust = 0,
      margin = margin(l = 5)
    ),
    axis.ticks.y     = element_blank(),
    legend.position  = "none"
  )

print(plot_eos_coef)
