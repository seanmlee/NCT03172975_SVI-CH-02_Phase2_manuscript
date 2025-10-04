

# libraries --------------------------------------------------------------------
library(haven)
library(tidyverse)
library(pracma)
library(openxlsx)


# load -------------------------------------------------------------------------
lb <- read_xpt("data/lb.xpt", .name_repair = "unique")
ex <- read_xpt("data/ex.xpt", .name_repair = "unique") |>
  distinct(USUBJID, EXDOSE)

dat <- lb |>
  full_join(ex, by = "USUBJID") |>
  filter(LBTEST == "Stool Examination", LBSTAT != "NOT DONE") |>
  mutate(
    LBSTRESN = suppressWarnings(as.numeric(LBSTRESN)),
    LBSTRESN = coalesce(LBSTRESN, 0)
  ) |>
  transmute(
    USUBJID = str_replace(USUBJID, "SVI-CH-02-001-", ""),
    EXDOSE,
    VISIT   = as.numeric(str_replace(VISIT, "DAY ", "")),
    LBSTRESN
  ) |>
  group_by(USUBJID) |>
  filter(n() > 1) |>
  ungroup()

label_map <- c(
  `0`   = "Placebo",
  `100` = "100µg Na-GST-1/Alhydrogel",
  `500` = "100µg Na-GST-1/Alhydrogel + 500µg CpG 10104",
  `5`   = "100µg Na-GST-1/Alhydrogel + 5µg AP 10-701"
)

dat <- dat |>
  mutate(Group = recode(as.character(EXDOSE), !!!label_map))


# subject-level AUC ------------------------------------------------------------
subject_auc <- dat |>
  arrange(USUBJID, VISIT) |>
  group_by(USUBJID, Group) |>
  summarise(
    auc = trapz(VISIT, log10(LBSTRESN + 1)),
    .groups = "drop"
  )


# group summaries --------------------------------------------------------------
grp_stats <- subject_auc |>
  group_by(Group) |>
  summarise(
    mean_auc = mean(auc, na.rm = TRUE),
    N        = n(),
    .groups  = "drop"
  )


# placebo mean for percent-protection ------------------------------------------
placebo_mean <- grp_stats |> filter(Group == "Placebo") |> pull(mean_auc)

tbl <- grp_stats |>
  mutate(
    `Percent Protection` = 100 * (1 - mean_auc / placebo_mean)
  )


# welch t-tests ----------------------------------------------------------------
placebo_auc <- subject_auc |> filter(Group == "Placebo") |> pull(auc)
pvals <- subject_auc |>
  filter(Group != "Placebo") |>
  group_by(Group) |>
  summarise(
    p = tryCatch(t.test(auc, placebo_auc)$p.value, error = function(e) NA_real_),
    .groups = "drop"
  )

tbl <- tbl |>
  left_join(pvals, by = "Group") |>
  mutate(p = ifelse(Group == "Placebo", NA_real_, p))


# order rows
order_vec <- c(
  "Placebo",
  "100µg Na-GST-1/Alhydrogel + 500µg CpG 10104",
  "100µg Na-GST-1/Alhydrogel",
  "100µg Na-GST-1/Alhydrogel + 5µg AP 10-701"
)
tbl <- tbl |>
  mutate(Group = factor(Group, levels = order_vec)) |>
  arrange(Group)

# format for export
tbl_out <- tbl |>
  transmute(
    `Study Group`             = as.character(Group),
    `Mean epg AUC (log10)`    = sprintf("%.2f", mean_auc),
    N                         = as.integer(N),
    `Percent Protection`      = ifelse(`Study Group` == "Placebo", "-",
                                       sprintf("%.2f", `Percent Protection`)),
    p                         = case_when(
      is.na(p)                           ~ "-",
      p < 0.001                          ~ "<0.001",
      TRUE                               ~ sprintf("%.3f", p)
    )
  )


# excel table ------------------------------------------------------------------
out_path <- "out/tbl_s08_auc.xlsx"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "Table S8")

writeData(wb, "Table S8", tbl_out, startRow = 1, startCol = 1, colNames = TRUE)

# body
hdr <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                   border = "TopBottomLeftRight")
body_left <- createStyle(halign = "left", border = "TopBottomLeftRight")
body_cent <- createStyle(halign = "center", border = "TopBottomLeftRight")

# style
addStyle(wb, "Table S8", hdr, rows = 1, cols = 1:ncol(tbl_out), gridExpand = TRUE)
addStyle(wb, "Table S8", body_left, rows = 2:(nrow(tbl_out)+1), cols = 1, gridExpand = TRUE)
addStyle(wb, "Table S8", body_cent, rows = 2:(nrow(tbl_out)+1), cols = 2:ncol(tbl_out), gridExpand = TRUE)

# col width
setColWidths(wb, "Table S8", cols = 1, widths = 46)
setColWidths(wb, "Table S8", cols = 2, widths = 18)
setColWidths(wb, "Table S8", cols = 3, widths = 6)
setColWidths(wb, "Table S8", cols = 4, widths = 18)
setColWidths(wb, "Table S8", cols = 5, widths = 10)


# write ------------------------------------------------------------------------
saveWorkbook(wb, out_path, overwrite = TRUE)

