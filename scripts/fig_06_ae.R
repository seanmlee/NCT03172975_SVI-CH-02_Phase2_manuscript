

# libraries --------------------------------------------------------------------
library(haven)
library(tidyverse)
library(patchwork)


# read AEPTCD ------------------------------------------------------------------
AEPTCD <- read.csv("data/AEPTCD.csv", header = TRUE)


# define vaccine ae ------------------------------------------------------------
term_levels <- c(
  "Any",
  "Injection site pain",
  "Injection site tenderness",
  "Injection site erythema",
  "Injection site swelling",
  "Pyrexia",
  "Headache",
  "Nausea",
  "Vomiting",
  "Myalgia",
  "Arthralgia"
)

relevant_ae <- c(
  10022086, # "Injection site pain", 
  10022102, # "Injection site tenderness",
  10022061, # "Injection site erythema", "Injection site redness",
  10053425, # "Injection site swelling",
  10037660, # "Fever", Pyrexia
  10019211, # "Headache",
  10028813, # "Nausea",
  10047700, # "Vomiting",
  10028411, # "Myalgia",
  10003239  # "Arthralgia"
)

EXDOSE <- c("Group 1","Group 2","Group 3","Group 4")

severity_lvls <- c("MILD","MODERATE","SEVERE")

arm_levels <- c("Group 4","Group 3","Group 1","Group 2")

pal <- c("Group 4"="#F8766D","Group 3"="#7CAE00","Group 1"="#00B8E7","Group 2"="#C77CFF")

lab <- c(
  'Placebo',
  expression(paste(italic('Na'), '-GST-1/Alhydrogel/5', mu,'g AP 10-701')),
  expression(paste(100*mu*g, italic(' Na'), '-GST-1/Alhydrogel')),
  expression(paste(italic('Na'), '-GST-1/Alhydrogel/500', mu,'g CpG 10104'))
)

grid <- expand.grid(
  AEPTCD = relevant_ae, 
  EXDOSE = EXDOSE, 
  AESEV = severity_lvls
  ) %>%
  
  left_join(
    AEPTCD, 
    by="AEPTCD"
    ) %>% 
  
  dplyr::select(-AEPTCD)

grid_any <- grid %>% select(-preferred_term) %>% distinct()


# function ---------------------------------------------------------------------
build_dose_df <- function(seq_num, n_per_group) {
  
  sev <- ae_vacc_solicited %>%
    
    filter(AELLT != "Knee pain", AELLT != "Myalgia of lower extremities") %>%
    
    mutate(AEPTCD = ifelse(AELLT == "Injection site tenderness", 10022102, AEPTCD)) %>%
    
    filter(EXSEQ == seq_num, AEPTCD %in% relevant_ae) %>%
    
    mutate(AESEV = factor(AESEV, levels = severity_lvls)) %>%
    
    dplyr::select(USUBJID, AESEV, EXDOSE, preferred_term) %>%
    
    distinct() %>% select(-USUBJID) %>%
    
    group_by(EXDOSE, AESEV, preferred_term) %>%
    
    summarise(n = n(), .groups = "drop")
  
  sev <- merge(grid, sev, by = c("EXDOSE", "preferred_term", "AESEV"), all.x = TRUE)
  
  sev$n[is.na(sev$n)] <- 0
  
  any <- ae_vacc_solicited %>%
    
    filter(EXSEQ == seq_num, AEPTCD %in% relevant_ae) %>%
    
    mutate(AESEV = factor(AESEV, levels = severity_lvls)) %>%
    
    select(USUBJID, AESEV, EXDOSE) %>% distinct() %>% select(-USUBJID) %>%
    
    group_by(EXDOSE, AESEV) %>% summarise(n = n(), .groups="drop")
  
  any <- merge(grid_any, any, by = c("EXDOSE","AESEV"), all.x = TRUE)
  
  any$n[is.na(any$n)] <- 0
  
  any$preferred_term <- "Any"
  
  sev <- bind_rows(sev, any) %>%
    
    mutate(
      n_dose = case_when(
        EXDOSE == "Group 1" ~ n_per_group["Group 1"],
        EXDOSE == "Group 2" ~ n_per_group["Group 2"],
        EXDOSE == "Group 3" ~ n_per_group["Group 3"],
        EXDOSE == "Group 4" ~ n_per_group["Group 4"]
      ),
      n = pmin(n, n_dose),
      percentage = (n / n_dose) * 100,
      AESEV = recode(AESEV, MILD = "Mild", MODERATE = "Moderate", SEVERE = "Severe"),
      preferred_term = factor(preferred_term, levels = term_levels) |> fct_rev(),
      EXDOSE = factor(EXDOSE, levels = arm_levels, ordered = TRUE)
    )
  
  sev
}


severity_dose1 <- build_dose_df(1, c("Group 1" = 10, "Group 2" = 9, "Group 3" = 10, "Group 4" = 10))
severity_dose2 <- build_dose_df(2, c("Group 1" = 9, "Group 2" = 8, "Group 3" = 10, "Group 4" = 10))
severity_dose3 <- build_dose_df(3, c("Group 1" = 9, "Group 2" = 7, "Group 3" = 10, "Group 4" = 10))

make_fig <- function(df, title_txt){
  
  ggplot(
    df, 
    aes(
      x = percentage/100, 
      y = preferred_term, 
      fill = EXDOSE, 
      colour = EXDOSE
      )
    ) +
    
    geom_col(
      position = position_dodge2(
        width = 0.7, 
        preserve = "single", 
        reverse = TRUE
        ),
      width = 0.7,
      alpha = 0.5,
      orientation = "y"
    ) +
    
    scale_fill_manual(
      values = pal[arm_levels], 
      breaks = arm_levels, 
      limits = arm_levels,
      labels = lab, 
      name = NULL
      ) +
    
    scale_color_manual(
      values = pal[arm_levels], 
      breaks = arm_levels, 
      limits = arm_levels,
      guide = "none"
      ) +
    
    scale_x_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", "0.25", "0.5", "0.75", "1"),
      minor_breaks = NULL
      
    ) + 
    
    labs(
      x = "Relative Proportion of Subjects", 
      y = NULL, 
      title = title_txt
      ) +
    
    facet_grid(~ AESEV) +
    
    theme_bw() +
    
    theme(
      panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15),
      strip.text.x = element_text(size = 15),
      strip.background = element_rect(fill = "white", colour = "black"),
      panel.spacing.x = unit(0.15, "lines"),
      legend.position = "right",
      legend.text = element_text(hjust = 0, size = 15),
      legend.title = element_blank()
    ) +
    
    guides(fill = guide_legend(ncol = 1, byrow = TRUE))
  
}

fig_dose_1 <- make_fig(severity_dose1, "Dose 1")
fig_dose_2 <- make_fig(severity_dose2, "Dose 2")
fig_dose_3 <- make_fig(severity_dose3, "Dose 3")

# ============================== combine (3 rows + right legend) ===============
panel <- ((fig_dose_1) /
            (fig_dose_2) /
            (fig_dose_3) | patchwork::guide_area()) +
  
  plot_layout(guides = "collect",
              heights = c(1, 1, 1),
              widths  = c(0.78, 0.375)) &
  
  guides(fill = guide_legend(ncol = 1, byrow = TRUE)) &
  
  theme(legend.position = "right", legend.direction = "vertical")

panel

# write ------------------------------------------------------------------------
ggsave(
  "out/fig6.tiff", 
  panel, 
  dpi = 300, 
  width = 15, 
  height = 15
  )
