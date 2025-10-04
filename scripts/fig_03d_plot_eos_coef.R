

# libraries --------------------------------------------------------------------
library(sjPlot)
library(sjlabelled)
library(sjmisc)


# fit model --------------------------------------------------------------------
mod_eos_dose <- glm(
  log(max_eos_count) ~
    dose,
  data = dv_max
)
sjPlot::tab_model(mod_eos_dose, transform = "exp", show.r2 = FALSE, digits = 3)


# plot -------------------------------------------------------------------------
eos_coef <-
  
  plot_model(
    mod_eos_dose, 
    transform = "exp",
    dot.size = 5,
    line.size = 0.75,
    value.size = 10,
    value.offset = 0.35,
    show.values = TRUE,
    group.terms = c(1, 2, 3), 
    colors = c("#7CAE00", "#00B8E7", "#C77CFF")
  ) 

eos_coef$data <- eos_coef$data %>%
  
  mutate(
    conf.high = if_else(conf.high > 2, 2, conf.high)
  )


plot_eos_coef <-
eos_coef +
  
  theme_bw()  +
  
  theme(
    
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "none"
    
  ) +
  
  scale_y_continuous(
    limits = c(-0.1, 2),
    breaks = c(0, 2, 1)
  ) +
  
  geom_hline(
    yintercept = 1,
    linetype = "solid",
    alpha = 0.25
  )+
  
  ggtitle(
    "d)"
  ) +
  
  labs(
    y = "Rate Ratio"
  ) +
  
  scale_x_discrete(
    
    position = "top",
    
    labels = c(
      expression(paste(italic('Na'), '-GST-1/Alhydrogel/500', mu, 'g CpG 10104')),
      expression(paste(100*mu*g, italic(' Na'), '-GST-1/Alhydrogel')),
      expression(paste(italic('Na'), '-GST-1/Alhydrogel/5', mu, 'g AP 10-701'))
    )
    
  )

print(plot_eos_coef)

