

# libraries --------------------------------------------------------------------
library(patchwork)


# combine ----------------------------------------------------------------------
p_a <- plot_mcmaster + theme(legend.position = "none")

p_b <- plot_eos +
  
  theme(
    legend.position = c(1.002, 1),
    legend.justification = c(0, 1),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.margin = margin(5.5, 40, 5.5, 5.5)
    ) +
  
  coord_cartesian(clip = "off")

p_c <- plot_mcmaster_coef + 
  theme(
    legend.position = "none",
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
    )

p_d <- plot_eos_coef + 
  theme(legend.position = "none")

uniform_margins <- theme(plot.margin = margin(15, 15, 15, 15))
p_a <- p_a + uniform_margins
p_b <- p_b + uniform_margins
p_c <- p_c + uniform_margins
p_d <- p_d + uniform_margins

right_col <- (p_b / p_d)
left_col  <- (p_a / p_c)

final_plot <- left_col | right_col
final_plot

# write ------------------------------------------------------------------------
ggsave(
  "out/fig3.tiff",
  dpi = 300,
  width = 18,
  height = 10
)
