

# libraries --------------------------------------------------------------------
library(tidyverse)
library(survival)
library(survminer)
library(ggfortify)
library(kableExtra)
library(patchwork)
library(grid)


# read -------------------------------------------------------------------------
lb <- 
  read_xpt(
    "data/lb.xpt",
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  )

ex <- 
  read_xpt(
    "data/ex.xpt",
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  )

ds <- 
  read_xpt(
    "data/ds.xpt",
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  )


# exclude ----------------------------------------------------------------------
lb <- lb %>%
  
  filter(
    USUBJID != "SVI-CH-02-001-8002",
    USUBJID != "SVI-CH-02-001-8012",
    USUBJID != "SVI-CH-02-001-8015"
  )


# LIPLAN/PROCCUR == "Y" --------------------------------------------------------
pr <- 
  read_xpt(
    "data/pr.xpt",
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  ) %>%
  
  dplyr::select(
    USUBJID, 
    PROCCUR,
    PRSTDTC   # start date/time of challenge
  ) %>%
  
  filter(
    PROCCUR == "Y"
  )

lb <- inner_join(lb, pr)

rm(pr)


# DSTERM == "COMPLETED" --------------------------------------------------------
ds <- ds %>%
  
  dplyr::select(
    USUBJID, 
    DSTERM
  ) %>%
  
  filter(
    DSTERM == "COMPLETED"
  )

lb <- inner_join(lb, ds)

rm(ds)


# subset vaccination data ------------------------------------------------------
vacc <- ex %>%
  
  dplyr::select(
    USUBJID,
    EXTRT,
    EXDOSE
  ) %>%
  
  distinct(
    USUBJID, 
    EXDOSE
  ) %>%
  
  mutate(
    dose = ifelse(
      EXDOSE == 0,
      "placebo",
      ifelse(
        EXDOSE == 5,
        "dose5",
        ifelse(
          EXDOSE == 100,
          "dose100",
          "dose500"
        )
      )
    )
  )

vacc$dose <- 
  factor(
    vacc$dose,
    levels = c(
      "placebo", 
      "dose5", 
      "dose100", 
      "dose500"
    )
  )

rm(ex)


# float ########################################################################
float <- lb %>%
  
  dplyr::select(
    LBSEQ,
    USUBJID,   # unique subject id
    PRSTDTC,   # start date/time of challenge
    LBDTC,     # date/time of specimen collection
    LBTEST,    # lab test or exam name
    LBSTAT,    # completion status
    LBORRES,   # flotation result
  ) %>%
  
  filter(
    LBTEST == "Stool Examination" & LBSTAT != "NOT DONE"
  ) %>%
  
  mutate(
    PRSTDTC = as.Date(sub("T.*", "", PRSTDTC)),
    LBDTC = as.Date(LBDTC),
    time_until_result = LBDTC - PRSTDTC
  ) %>%
  
  filter(
    time_until_result >= 0
  )

float <- float %>%
  
  group_by(USUBJID, LBORRES) %>%

  summarise(
    result_time = if(any(LBORRES == "Positive")) {
      min(time_until_result[LBORRES == "Positive"])
    } else {
      max(time_until_result)
    },
    .groups = 'drop'
    
  ) %>%
  
  arrange(desc(LBORRES)) %>%
  
  distinct(USUBJID, .keep_all = TRUE)

float <- inner_join(float, vacc)


# float km ---------------------------------------------------------------------
float$survival_time <- as.numeric(float$result_time)
float$status <- ifelse(float$LBORRES == "Positive", 1, 0)
km_fit <- survfit(Surv(survival_time, status) ~ dose, data = float)


# plot float -------------------------------------------------------------------
km_float <-
ggsurvplot(
  km_fit, 
  data = float, 
  pval = TRUE, 
  conf.int = FALSE, 
  risk.table = FALSE, 
  size = 0.5,
  censor.size = 3,
  censor.shape = "o",
  legend = "right",
  legend.labs = levels(float$dose),
  title = "a)",
  xlab = "Days Post-CHHI",
  xlim = c(0, 220),
  break.x.by = 30,
  ylab = "Probability of Negative Test Result",
  
  ggtheme = 
    
    theme_bw() + 
    
    theme(
      panel.grid = element_blank(),
      legend.title = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
      )
  
  )


# table
km_fit
summary(km_fit)
km_fit_df <- fortify(km_fit)
km_fit_df %>% kbl()


# mcmaster #####################################################################
mcmaster <- lb %>%
  
  dplyr::select(
    LBSEQ,
    USUBJID,   # unique subject id
    PRSTDTC,   # start date/time of procedure
    LBDTC,     # date/time of specimen collection
    LBTEST,    # lab test or exam name
    LBSTAT,    # completion status
    LBORRES,   # flotation result
    LBSTRESN,  # numerical result/finding in standard units
  ) %>%
  
  filter(
    LBTEST == "Stool Examination" & LBSTAT != "NOT DONE"
  ) %>%
  
  replace(
    is.na(.),
    0
  ) %>%
  
  mutate(
    PRSTDTC = as.Date(sub("T.*", "", PRSTDTC)),
    LBDTC = as.Date(LBDTC),
    time_until_result = LBDTC - PRSTDTC
  ) %>%
  
  filter(
    time_until_result >= 0
  ) %>%
  
  mutate(
    LBSTRESN = ifelse(
      LBSTRESN > 0,
      "Positive",
      "Negative"
    )
  )

mcmaster <- mcmaster %>%
  
  group_by(USUBJID, LBSTRESN) %>%
  
  summarise(
    result_time = if(any(LBSTRESN == "Positive")) {
      min(time_until_result[LBSTRESN == "Positive"])
    } else {
      max(time_until_result)
    },
    .groups = 'drop'
    
  ) %>%
  
  arrange(desc(LBSTRESN)) %>%
  
  distinct(USUBJID, .keep_all = TRUE)

mcmaster <- inner_join(mcmaster, vacc)


# mcmaster km ------------------------------------------------------------------
mcmaster$survival_time <- as.numeric(mcmaster$result_time)
mcmaster$status <- ifelse(mcmaster$LBSTRESN == "Positive", 1, 0)
km_fit <- survfit(Surv(survival_time, status) ~ dose, data = mcmaster)


# plot mcmaster ----------------------------------------------------------------
km_mcmaster <-
ggsurvplot(
  km_fit, 
  data = mcmaster, 
  pval = TRUE, 
  conf.int = FALSE, 
  risk.table = FALSE, 
  size = 0.5,
  censor.size = 3,
  censor.shape = "o",
  legend = "right",
  legend.labs = levels(mcmaster$dose),
  title = "b)",
  xlab = "Days Post-CHHI",
  xlim = c(0, 220),
  break.x.by = 30,
  ylab = "Probability of Negative Test Result",
  
  ggtheme = 
    
    theme_bw() + 
    
    theme(
      panel.grid = element_blank(),
      legend.title = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    )
  
)


# table
km_fit
summary(km_fit)
km_fit_df <- fortify(km_fit)
km_fit_df %>% kbl()


# combine ----------------------------------------------------------------------
dose_cols <- c(
  placebo="#F8766D", 
  dose5="#7CAE00", 
  dose100="#00BFC4", 
  dose500="#C77CFF"
  )

dose_lab_map <- list(
  placebo = expression(Placebo),
  dose5   = expression(paste(italic('Na'), '-GST-1/Alhydrogel/5', mu, 'g AP 10-701')),
  dose100 = expression(paste(100*mu*g, italic(' Na'), '-GST-1/Alhydrogel')),
  dose500 = expression(paste(italic('Na'), '-GST-1/Alhydrogel/500', mu, 'g CpG 10104'))
)

strata_levels <- levels(factor(km_float$plot$data$strata))

dose_keys <- sub("^.*=", "", strata_levels)

values_for_strata <- setNames(dose_cols[dose_keys], strata_levels)

labels_for_strata <- do.call(c, unname(dose_lab_map[dose_keys]))

p1 <- km_float$plot +
  
  scale_color_manual(
    values = values_for_strata,
    breaks = strata_levels,
    labels = labels_for_strata,
    name = NULL) +
  
  guides(
    linetype = "none", 
    shape = "none"
    ) +
  
  theme(legend.position = "bottom")

p2 <- km_mcmaster$plot +
  
  scale_color_manual(
    values = values_for_strata,
    breaks = strata_levels,
    labels = labels_for_strata,
    name = NULL
    ) +
  
  guides(linetype = "none", shape = "none") +
  
  theme(legend.position = "none")

panel <- (p1 | p2) / patchwork::guide_area() +
  
  plot_layout(
    guides = "collect", 
    widths = c(1, 1), 
    heights = c(1, 0.18)
    ) &
  
  guides(
    colour = guide_legend(ncol = 1, byrow = TRUE)
    ) &
  
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.key.width = unit(16, "pt"),
    legend.key.height = unit(16, "pt")
    
  )

panel


# write ------------------------------------------------------------------------
ggsave(
  "out/fig4.tiff", 
  panel, 
  dpi = 300, 
  width = 9, 
  height = 4.5
  )
