

# libraries --------------------------------------------------------------------
library(tidyverse)
library(mgcv)


# fit model --------------------------------------------------------------------
mod_mcmaster_gam <- gam(
  LBSTRESN + 0.1 ~
    s(VISIT, by = dose, k = 4),
  family = nb(theta = NULL, link = "log"),
  data = dv_long_mcmaster
)


# preds ------------------------------------------------------------------------
VISIT <- seq(
  100,
  300,
  length.out = 1000
)

critval <- 1.96

newdata <- expand.grid(
  dose = c("placebo", "dose5", "dose100", "dose500"),
  VISIT = VISIT
)

preds <- 
  predict(
    mod_mcmaster_gam, 
    newdata = newdata, 
    type = "response", 
    se.fit = TRUE
  )

fit_response <-
  preds$fit

upr_response <-
  preds$fit + preds$se.fit

lwr_response <- 
  preds$fit - preds$se.fit

newdata <-
  as.data.frame(
    cbind(
      newdata,
      fit_response,
      upr_response, 
      lwr_response
    )
  )


# plot -------------------------------------------------------------------------
p_b <-
newdata %>%
  
  ggplot(
    aes(
      x = VISIT, 
      y = fit_response,
      fill = dose,
      color = dose
    )
  ) +
  
  scale_x_continuous(
    limits = c(100, 300),
    breaks = c(100, 140, 200, 300),
    labels = c("100", "140\nCHHI", "200", "300")
  ) +
  
  scale_y_continuous(
    limits = c(0, 150),
    breaks = c(0, 150)
  ) +
  
  geom_line(
    linewidth = 0.75
  ) +
  
  geom_ribbon(
    aes(
      ymin = lwr_response,
      ymax = upr_response,
      color = NULL
    ),
    alpha = 0.15
  ) +
  
  labs(
    title = "b)",
    x = "Study Day",
    y = "Predicted mean\neggs per gram"
  ) +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text.align = 0,
    legend.position = "none"
  ) +
  
  # legend and override alpha
  scale_color_manual(
    
    values = c("placebo" = "#FF5733", 
               "dose5" = "#7CAE00",
               "dose100" = "#00B8E7",
               "dose500" = "#C77CFF"),
    
    labels = c("placebo",
               "dose5",
               "dose100",
               "dose500"),
    
    name = "Procedure"
    
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(alpha = 1)
    )
  )

