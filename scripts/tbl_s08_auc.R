

# libraries --------------------------------------------------------------------
library(haven)
library(tidyverse)
library(pracma)
library(openxlsx)


# load -------------------------------------------------------------------------
lb <- read_xpt("data/lb.xpt", .name_repair = "unique")
ex <- read_xpt("data/ex.xpt", .name_repair = "unique") %>%
  distinct(USUBJID, EXDOSE)

dat <- lb %>%
  full_join(ex, by = "USUBJID") %>%
  filter(LBTEST == "Stool Examination", LBSTAT != "NOT DONE") %>%
  mutate(
    LBSTRESN = suppressWarnings(as.numeric(LBSTRESN)),
    LBSTRESN = coalesce(LBSTRESN, 0)
  ) %>%
  transmute(
    USUBJID = str_replace(USUBJID, "SVI-CH-02-001-", ""),
    EXDOSE,
    VISIT   = as.numeric(str_replace(VISIT, "DAY ", "")),
    LBSTRESN
  ) %>%
  group_by(USUBJID) %>%
  filter(n() > 1) %>%
  ungroup()

label_map <- c(
  `0`   = "Placebo",
  `100` = "100µg Na-GST-1/Alhydrogel",
  `500` = "100µg Na-GST-1/Alhydrogel + 500µg CpG 10104",
  `5`   = "100µg Na-GST-1/Alhydrogel + 5µg AP 10-701"
)

dat <- dat %>%
  mutate(Group = recode(as.character(EXDOSE), !!!label_map))


# subject-level AUC on original scale ------------------------------------------
subject_auc <- dat %>%
  arrange(USUBJID, VISIT) %>%
  group_by(USUBJID, Group) %>%
  summarise(
    auc_raw = trapz(VISIT, LBSTRESN),      # AUC of epg over time (original scale)
    log_auc = log(auc_raw + 1),            # log-transform for analysis
    .groups = "drop"
  )


# group summaries: geometric mean AUC ------------------------------------------
grp_stats <- subject_auc %>%
  group_by(Group) %>%
  summarise(
    gm_auc = exp(mean(log_auc, na.rm = TRUE)) - 1,  # geometric mean AUC
    N      = n(),
    .groups = "drop"
  )


# placebo geometric mean for percent-protection --------------------------------
placebo_gm <- grp_stats %>%
  filter(Group == "Placebo") %>%
  pull(gm_auc)

tbl <- grp_stats %>%
  mutate(
    `Percent Protection` = 100 * (1 - gm_auc / placebo_gm)
  )


# Welch t-tests on log AUC -----------------------------------------------------
placebo_log_auc <- subject_auc %>%
  filter(Group == "Placebo") %>%
  pull(log_auc)

pvals <- subject_auc %>%
  filter(Group != "Placebo") %>%
  group_by(Group) %>%
  summarise(
    p = tryCatch(
      t.test(log_auc, placebo_log_auc)$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

tbl <- tbl %>%
  left_join(pvals, by = "Group") %>%
  mutate(p = ifelse(Group == "Placebo", NA_real_, p))


# order rows -------------------------------------------------------------------
order_vec <- c(
  "Placebo",
  "100µg Na-GST-1/Alhydrogel + 500µg CpG 10104",
  "100µg Na-GST-1/Alhydrogel",
  "100µg Na-GST-1/Alhydrogel + 5µg AP 10-701"
)

tbl <- tbl %>%
  mutate(Group = factor(Group, levels = order_vec)) %>%
  arrange(Group)


# format for export ------------------------------------------------------------
tbl_out <- tbl %>%
  transmute(
    `Study Group`                = as.character(Group),
    `Geometric mean epg AUC`     = sprintf("%.2f", gm_auc),
    N                            = as.integer(N),
    `Percent Protection`         = ifelse(`Study Group` == "Placebo", "-",
                                          sprintf("%.2f", `Percent Protection`)),
    p                            = case_when(
      is.na(p)        ~ "-",
      p < 0.001       ~ "<0.001",
      TRUE            ~ sprintf("%.3f", p)
    )
  )


# excel table ------------------------------------------------------------------
out_path <- "out/tbl_s08_auc.xlsx"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "Table S8")

writeData(wb, "Table S8", tbl_out, startRow = 1, startCol = 1, colNames = TRUE)

hdr      <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                        border = "TopBottomLeftRight")
body_left <- createStyle(halign = "left", border = "TopBottomLeftRight")
body_cent <- createStyle(halign = "center", border = "TopBottomLeftRight")

addStyle(wb, "Table S8", hdr, rows = 1, cols = 1:ncol(tbl_out), gridExpand = TRUE)
addStyle(wb, "Table S8", body_left, rows = 2:(nrow(tbl_out)+1), cols = 1, gridExpand = TRUE)
addStyle(wb, "Table S8", body_cent, rows = 2:(nrow(tbl_out)+1), cols = 2:ncol(tbl_out), gridExpand = TRUE)

setColWidths(wb, "Table S8", cols = 1, widths = 46)
setColWidths(wb, "Table S8", cols = 2, widths = 22)
setColWidths(wb, "Table S8", cols = 3, widths = 6)
setColWidths(wb, "Table S8", cols = 4, widths = 18)
setColWidths(wb, "Table S8", cols = 5, widths = 10)

saveWorkbook(wb, out_path, overwrite = TRUE)
