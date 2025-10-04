

# libraries --------------------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)
library(ggsignif)


# plot -------------------------------------------------------------------------
plot_eos <-
dv_max %>%
  
  mutate(
    float = case_when(
      float == 0 ~ "Negative",
      float == 1 ~ "Positive"
    )
  ) %>%
  
  ggplot(
    
    aes(
      x = dose, 
      y = max_eos_count,
      color = dose,
      fill = dose
    )
    
  ) + 
  
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.1,
    size = 0.5
  ) +
  
  geom_beeswarm(
    
    size = 6,
    alpha = 0.5,
    color = "black",
    method = "hex",
    stroke = 1,
    aes(
      shape = float,
      color = dose
    )
    
  ) +
  
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 5),
    labels = scales::number_format(accuracy = 1, trim = TRUE)
  ) +
  
  scale_shape_manual(values = c(21, 23)) +
  
  theme_bw() +
  
  theme(
    
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 5, b = 0, l = 0)),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 19),
    legend.text.align = 0
    
  ) +
  
  guides(
    fill = "none",
    color = guide_legend(ncol = 1),
    shape = guide_legend(ncol = 1)
  ) +
  
  scale_color_discrete(
    
    labels = c(
      'Placebo', 
      expression(paste(italic('Na'), '-GST-1/Alhydrogel/5', mu, 'g AP 10-701')), 
      expression(paste(100*mu*g, italic(' Na'), '-GST-1/Alhydrogel')), 
      expression(paste(italic('Na'), '-GST-1/Alhydrogel/500', mu, 'g CpG 10104'))
    )
    
  ) +
  
  ggtitle("b)") +
  
  labs(
    shape = "Float",
    fill = "",
    color = "",
    y = "Maximum eosinophil x 10³/μL"
  )  +
  
  geom_signif(
    
    y_position = 9, 
    xmin = 1,
    xmax = 4,
    annotation = "*", 
    tip_length = c(0.36, 0.08),
    color = "black",
    size = 0.25,
    textsize = 10,
    vjust = 0.55
    
  )  +
  
  stat_summary(
    fun  = median,
    geom = "text",
    aes(label = after_stat(sprintf("%.1f", y))),
    vjust = -0.25,
    hjust = -0.4,
    size  = 6,
    color = "black",
    fontface = "bold",
    show.legend = FALSE
  )

print(plot_eos)

