

# libraries --------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)
library(grid)


# combine ----------------------------------------------------------------------
dose_labels <- c(
  'Placebo',
  expression(paste(italic('Na'), '-GST-1/Alhydrogel/5', mu, 'g AP 10-701')),
  expression(paste(100*mu*g, italic(' Na'), '-GST-1/Alhydrogel')),
  expression(paste(italic('Na'), '-GST-1/Alhydrogel/500', mu, 'g CpG 10104'))
)

p_a <- p_a + guides(color = "none", fill = "none", linetype = "none", shape = "none", alpha = "none")
p_c <- p_c + guides(color = "none", fill = "none", linetype = "none", shape = "none", alpha = "none")
p_d <- p_d + guides(color = "none", fill = "none", linetype = "none", shape = "none", alpha = "none")

p_b <- p_b +
  
  scale_fill_manual(
    values = c(placebo = "#FF5733", dose5 = "#7CAE00", dose100 = "#00B8E7", dose500 = "#C77CFF"),
    labels = dose_labels,
    breaks = c("placebo","dose5","dose100","dose500"),
    name   = NULL
  ) +
  
  guides(color = "none", fill = guide_legend(ncol = 1, byrow = TRUE)) +
  theme(legend.position = "bottom")

panel <- (p_a | p_b) / (p_c | p_d) / patchwork::guide_area() +
  
  plot_layout(guides = "collect", heights = c(1, 1, 0.18)) &
  theme(
    legend.position      = "bottom",
    legend.direction     = "vertical",
    legend.justification = "center",
    legend.box.just      = "center",
    legend.title         = element_blank(),
    legend.text          = element_text(size = 16),
    legend.key.width     = unit(18, "pt"),
    plot.margin          = margin(10, 16, 24, 16)
  )

panel


# write ------------------------------------------------------------------------
ggsave(
  "out/fig2.tiff",
  dpi = 300,
  width = 15,
  height = 10
)
