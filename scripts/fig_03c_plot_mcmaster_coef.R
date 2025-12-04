

# libraries --------------------------------------------------------------------
library(brms)
library(parameters)
library(dplyr)
library(ggplot2)


# fit model --------------------------------------------------------------------
mod_mcmaster_dose <- brms::brm(
  round(max_egg_count) ~ dose,
  family  = "zero_inflated_negbinomial",
  control = list(adapt_delta = 0.9),
  iter    = 4000,
  data    = dv_max
)


# extract conditional rate ratios from brms ZINB model -------------------------
prm_rr <- parameters::model_parameters(
  mod_mcmaster_dose,
  exponentiate = TRUE,        # rate ratios
  effects      = "fixed",
  component    = "conditional"
)


# tidy for plotting ------------------------------------------------------------
df_plot <- prm_rr %>%
  
  filter(!Parameter %in% c("(Intercept)", "Intercept", "b_Intercept")) %>%
  
  mutate(
    Group = recode(
      Parameter,
      "b_dosedose500" = "Na-GST-1/Alhydrogel/500µg CpG 10104",
      "b_dosedose100" = "100µg Na-GST-1/Alhydrogel",
      "b_dosedose5"   = "Na-GST-1/Alhydrogel/5µg AP 10-701"
      )
    ) %>%
  
  mutate(Group = factor(
    Group,
    levels = c(
      "Na-GST-1/Alhydrogel/500µg CpG 10104",
      "100µg Na-GST-1/Alhydrogel",
      "Na-GST-1/Alhydrogel/5µg AP 10-701"
      )
    )
  ) %>%
  
  transmute(
    Group,
    rr      = Median,
    rr_low  = CI_low,
    rr_high = CI_high
  )


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


eps <- 1e-3

df_plot <- df_plot %>%
  mutate(
    rr_plot      = pmax(rr, eps),
    rr_low_plot  = pmax(rr_low, eps),
    rr_high_plot = pmin(rr_high, 2)
  )

# plot (panel c) ---------------------------------------------------------------
plot_mcmaster_coef <-
  
  ggplot(
    df_plot, 
    aes(
      x = rr_plot, 
      y = Group, 
      color = Group
      )
    ) +
  
  geom_vline(xintercept = 1, linetype = "solid", alpha = 0.25) +
  
  geom_errorbarh(
    aes(
      xmin = rr_low_plot, 
      xmax = rr_high_plot
      ),
    height = 0.5, 
    size = 0.75
    ) +
  
  geom_point(size = 8) +
  
#  geom_text(
#    aes(
#      label = sprintf("%.2f", rr_plot)
#      ),
#    hjust = -0.2,
#    vjust = -0.2,
#    size  = 10,
#    show.legend = FALSE
#  ) +
  
  scale_color_manual(values = dose_cols, guide = "none") +
  
  scale_x_continuous(
    "Rate Ratio",
    limits = c(-0.1, 2),
    breaks = c(0, 1, 2)
  ) +
  
  scale_x_log10(
    name = expression("Rate Ratio (log"[10]*")"),
    breaks = c(0.001, 0.01, 0.1, 1, 3),
    labels = c("0.001", "0.01", "0.1", "1", "3"),
    limits = c(0.001, 3)
  ) +
  
  scale_y_discrete(labels = y_labs) +
  
  ggtitle("c)") +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 20),
    axis.title       = element_text(size = 20),
    axis.text        = element_text(size = 20),
    legend.position  = "none"
  )


print(plot_mcmaster_coef)
