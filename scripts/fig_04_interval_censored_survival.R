# libraries --------------------------------------------------------------------
library(tidyverse)
library(haven)
library(survival)
library(survminer)
library(ggfortify)
library(kableExtra)
library(patchwork)
library(grid)

# read -------------------------------------------------------------------------
lb <- read_xpt(
  "data/lb.xpt",
  col_select   = NULL,
  skip         = 0,
  n_max        = Inf,
  .name_repair = "unique"
)

ex <- read_xpt(
  "data/ex.xpt",
  col_select   = NULL,
  skip         = 0,
  n_max        = Inf,
  .name_repair = "unique"
)

ds <- read_xpt(
  "data/ds.xpt",
  col_select   = NULL,
  skip         = 0,
  n_max        = Inf,
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
pr <- read_xpt(
  "data/pr.xpt",
  col_select   = NULL,
  skip         = 0,
  n_max        = Inf,
  .name_repair = "unique"
) %>%
  dplyr::select(
    USUBJID,
    PROCCUR,
    PRSTDTC   # start date/time of challenge
  ) %>%
  filter(PROCCUR == "Y")

lb <- inner_join(lb, pr, by = "USUBJID")
rm(pr)

# DSTERM == "COMPLETED" --------------------------------------------------------
ds <- ds %>%
  dplyr::select(
    USUBJID,
    DSTERM
  ) %>%
  filter(DSTERM == "COMPLETED")

lb <- inner_join(lb, ds, by = "USUBJID")
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
      EXDOSE == 0,   "placebo",
      ifelse(
        EXDOSE == 5,   "dose5",
        ifelse(
          EXDOSE == 100, "dose100",
          "dose500"
        )
      )
    )
  )

vacc$dose <- factor(
  vacc$dose,
  levels = c("placebo", "dose5", "dose100", "dose500")
)

rm(ex)

# FLOAT (interval-censored) ####################################################
float_long <- lb %>%
  dplyr::select(
    LBSEQ,
    USUBJID,
    PRSTDTC,   # start date/time of challenge
    LBDTC,     # date/time of specimen collection
    LBTEST,
    LBSTAT,
    LBORRES    # flotation result
  ) %>%
  filter(
    LBTEST == "Stool Examination",
    LBSTAT != "NOT DONE"
  ) %>%
  mutate(
    PRSTDTC           = as.Date(sub("T.*", "", PRSTDTC)),
    LBDTC             = as.Date(LBDTC),
    time_until_result = as.numeric(LBDTC - PRSTDTC)
  ) %>%
  filter(time_until_result >= 0)

# build interval bounds per subject (float)
float_ic <- float_long %>%
  arrange(USUBJID, time_until_result) %>%
  group_by(USUBJID) %>%
  summarise(
    has_pos   = any(LBORRES == "Positive"),
    first_pos = ifelse(
      has_pos,
      min(time_until_result[LBORRES == "Positive"]),
      NA_real_
    ),
    last_time = max(time_until_result),
    last_neg_before_pos = ifelse(
      has_pos,
      ifelse(
        any(LBORRES == "Negative" & time_until_result < first_pos),
        max(time_until_result[LBORRES == "Negative" &
                                time_until_result < first_pos]),
        0
      ),
      last_time
    ),
    timeL  = ifelse(has_pos, last_neg_before_pos, last_time),
    timeR  = ifelse(has_pos, first_pos, Inf),
    status = as.integer(has_pos),
    .groups = "drop"
  ) %>%
  mutate(
    timeL = as.numeric(timeL),
    timeR = as.numeric(timeR)
  )

float_ic <- inner_join(float_ic, vacc, by = "USUBJID") %>%
  filter(!is.na(timeL), !is.na(timeR), !is.na(dose))

# Turnbull estimator by dose (float)
km_float_ic <- survfit(
  Surv(timeL, timeR, type = "interval2") ~ dose,
  data = float_ic
)

# plot float (WITH RISK TABLE) -------------------------------------------------
km_float_plot <- ggsurvplot(
  km_float_ic,
  data              = float_ic,
  pval              = FALSE,
  conf.int          = FALSE,
  risk.table        = TRUE,
  risk.table.col    = "strata",
  risk.table.title  = "Number at risk",
  risk.table.height = 0.25,
  size              = 0.5,
  censor.size       = 3,
  censor.shape      = "o",
  legend            = "right",
  legend.labs       = levels(float_ic$dose),
  title             = "a)",
  xlab              = "Days Post-CHHI",
  xlim              = c(0, 220),
  break.x.by        = 30,
  ylab              = "Probability of Negative Test Result",
  ggtheme =
    theme_bw() +
    theme(
      panel.grid   = element_blank(),
      legend.title = element_blank(),
      axis.text    = element_text(size = 12),
      axis.title   = element_text(size = 12)
    )
)

# MCMASTER (interval-censored, with right-censored fallback) ###################

mcmaster_long <- lb %>%
  dplyr::select(
    LBSEQ,
    USUBJID,
    PRSTDTC,   # start date/time of procedure
    LBDTC,     # date/time of specimen collection
    LBTEST,
    LBSTAT,
    LBSTRESN   # numeric result
  ) %>%
  filter(
    LBTEST == "Stool Examination",
    LBSTAT != "NOT DONE"
  ) %>%
  mutate(
    PRSTDTC           = as.Date(sub("T.*", "", PRSTDTC)),
    LBDTC             = as.Date(LBDTC),
    time_until_result = as.numeric(LBDTC - PRSTDTC)
  ) %>%
  filter(time_until_result >= 0) %>%
  mutate(
    LBSTRESN = ifelse(is.na(LBSTRESN), 0, LBSTRESN),
    POSNEG   = ifelse(LBSTRESN > 0, "Positive", "Negative")
  )

mcmaster_ic <- mcmaster_long %>%
  arrange(USUBJID, time_until_result) %>%
  group_by(USUBJID) %>%
  summarise(
    has_pos   = any(POSNEG == "Positive", na.rm = TRUE),
    first_pos = ifelse(
      has_pos,
      min(time_until_result[POSNEG == "Positive"], na.rm = TRUE),
      NA_real_
    ),
    last_time = max(time_until_result, na.rm = TRUE),
    last_neg_before_pos = ifelse(
      has_pos,
      ifelse(
        any(POSNEG == "Negative" & time_until_result < first_pos, na.rm = TRUE),
        max(time_until_result[POSNEG == "Negative" &
                                time_until_result < first_pos], na.rm = TRUE),
        0
      ),
      last_time
    ),
    timeL  = ifelse(has_pos, last_neg_before_pos, last_time),
    timeR  = ifelse(has_pos, first_pos, Inf),
    status = as.integer(has_pos),
    .groups = "drop"
  ) %>%
  mutate(
    timeL = as.numeric(timeL),
    timeR = as.numeric(timeR)
  )

mcmaster_ic <- inner_join(mcmaster_ic, vacc, by = "USUBJID") %>%
  filter(!is.na(timeL), !is.na(timeR), !is.na(dose))

cat("nrow(mcmaster_ic) =", nrow(mcmaster_ic), "\n")

if (nrow(mcmaster_ic) > 0) {
  km_mcmaster_ic <- survfit(
    Surv(timeL, timeR, type = "interval2") ~ dose,
    data = mcmaster_ic
  )
  
  km_mcmaster_plot <- ggsurvplot(
    km_mcmaster_ic,
    data              = mcmaster_ic,
    pval              = FALSE,
    conf.int          = FALSE,
    risk.table        = TRUE,
    risk.table.col    = "strata",
    risk.table.title  = "Number at risk",
    risk.table.height = 0.25,
    size              = 0.5,
    censor.size       = 3,
    censor.shape      = "o",
    legend            = "right",
    legend.labs       = levels(mcmaster_ic$dose),
    title             = "b)",
    xlab              = "Days Post-CHHI",
    xlim              = c(0, 220),
    break.x.by        = 30,
    ylab              = "Probability of Negative Test Result",
    ggtheme =
      theme_bw() +
      theme(
        panel.grid   = element_blank(),
        legend.title = element_blank(),
        axis.text    = element_text(size = 12),
        axis.title   = element_text(size = 12)
      )
  )
  
} else {
  message("No eligible McMaster interval-censored observations; using original right-censored KM.")
  
  mcmaster_rc <- lb %>%
    dplyr::select(
      LBSEQ,
      USUBJID,
      PRSTDTC,
      LBDTC,
      LBTEST,
      LBSTAT,
      LBORRES,
      LBSTRESN
    ) %>%
    filter(
      LBTEST == "Stool Examination",
      LBSTAT != "NOT DONE"
    ) %>%
    replace(is.na(.), 0) %>%
    mutate(
      PRSTDTC           = as.Date(sub("T.*", "", PRSTDTC)),
      LBDTC             = as.Date(LBDTC),
      time_until_result = LBDTC - PRSTDTC
    ) %>%
    filter(time_until_result >= 0) %>%
    mutate(
      LBSTRESN = ifelse(LBSTRESN > 0, "Positive", "Negative")
    )
  
  mcmaster_rc <- mcmaster_rc %>%
    group_by(USUBJID, LBSTRESN) %>%
    summarise(
      result_time = if (any(LBSTRESN == "Positive")) {
        min(time_until_result[LBSTRESN == "Positive"])
      } else {
        max(time_until_result)
      },
      .groups = "drop"
    ) %>%
    arrange(desc(LBSTRESN)) %>%
    distinct(USUBJID, .keep_all = TRUE)
  
  mcmaster_rc <- inner_join(mcmaster_rc, vacc, by = "USUBJID")
  
  mcmaster_rc$survival_time <- as.numeric(mcmaster_rc$result_time)
  mcmaster_rc$status        <- ifelse(mcmaster_rc$LBSTRESN == "Positive", 1, 0)
  
  km_mcmaster <- survfit(Surv(survival_time, status) ~ dose, data = mcmaster_rc)
  
  km_mcmaster_plot <- ggsurvplot(
    km_mcmaster,
    data              = mcmaster_rc,
    pval              = FALSE,
    conf.int          = FALSE,
    risk.table        = TRUE,
    risk.table.col    = "strata",
    risk.table.title  = "Number at risk",
    risk.table.height = 0.25,
    size              = 0.5,
    censor.size       = 3,
    censor.shape      = "o",
    legend            = "right",
    legend.labs       = levels(mcmaster_rc$dose),
    title             = "b)",
    xlab              = "Days Post-CHHI",
    xlim              = c(0, 220),
    break.x.by        = 30,
    ylab              = "Probability of Negative Test Result",
    ggtheme =
      theme_bw() +
      theme(
        panel.grid   = element_blank(),
        legend.title = element_blank(),
        axis.text    = element_text(size = 12),
        axis.title   = element_text(size = 12)
      )
  )
}

# combine plots ----------------------------------------------------------------
dose_cols <- c(
  placebo = "#F8766D",
  dose5   = "#7CAE00",
  dose100 = "#00BFC4",
  dose500 = "#C77CFF"
)

dose_lab_map <- list(
  placebo = expression(Placebo),
  dose5   = expression(paste(italic('Na'), '-GST-1/Alhydrogel/5', mu, 'g AP 10-701')),
  dose100 = expression(paste(100*mu*g, italic(' Na'), '-GST-1/Alhydrogel')),
  dose500 = expression(paste(italic('Na'), '-GST-1/Alhydrogel/500', mu, 'g CpG 10104'))
)

# strata as produced by survfit/ggsurvplot (e.g., "dose=placebo")
strata_levels <- levels(factor(km_float_plot$plot$data$strata))
dose_keys     <- sub("^.*=", "", strata_levels)

values_for_strata <- setNames(dose_cols[dose_keys], strata_levels)
labels_for_strata <- do.call(c, unname(dose_lab_map[dose_keys]))

# FLOAT: plot + risk table (no strata axis labels, legend uses original labels)
p1_plot <- km_float_plot$plot +
  scale_color_manual(
    values = values_for_strata,
    breaks = strata_levels,
    labels = labels_for_strata,
    name   = NULL
  ) +
  guides(linetype = "none", shape = "none") +
  theme(legend.position = "bottom")

p1_tbl <- km_float_plot$table +
  scale_color_manual(values = values_for_strata, breaks = strata_levels) +
  scale_x_continuous(breaks = seq(0, 210, 30), limits = c(0, 220)) +
  labs(y = NULL) +
  guides(color = "none") +
  theme(
    legend.position = "none",
    axis.title.y    = element_blank(),
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank()
  )

p1 <- p1_plot / p1_tbl + plot_layout(heights = c(3, 1))

# MCMASTER: plot + risk table (no strata axis labels, no legend here)
p2_plot <- km_mcmaster_plot$plot +
  scale_color_manual(
    values = values_for_strata,
    breaks = strata_levels,
    labels = labels_for_strata,
    name   = NULL
  ) +
  guides(linetype = "none", shape = "none") +
  theme(legend.position = "none")

p2_tbl <- km_mcmaster_plot$table +
  scale_color_manual(values = values_for_strata, breaks = strata_levels) +
  scale_x_continuous(breaks = seq(0, 210, 30), limits = c(0, 220)) +
  labs(y = NULL) +
  guides(color = "none") +
  theme(
    legend.position = "none",
    axis.title.y    = element_blank(),
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank()
  )

p2 <- p2_plot / p2_tbl + plot_layout(heights = c(3, 1))

# Final combined panel with shared legend -------------------------------------
panel <- (p1 | p2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position   = "bottom",
    legend.direction  = "vertical",
    legend.title      = element_blank(),
    legend.text       = element_text(size = 11),
    legend.key.width  = unit(16, "pt"),
    legend.key.height = unit(16, "pt")
  )

panel

# write ------------------------------------------------------------------------
ggsave(
  "out/fig4.pdf",
  panel,
  dpi    = 300,
  width  = 9,
  height = 6
)
