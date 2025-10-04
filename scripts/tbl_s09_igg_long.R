

# libraries --------------------------------------------------------------------
library(haven)
library(tidyverse)
library(openxlsx)


# load -------------------------------------------------------------------------
ex <- read_xpt("data/ex.xpt") %>%
  dplyr::select(USUBJID, EXSEQ, EXDOSE) %>%
  mutate(EXSEQ = as.factor(EXSEQ))

seq4 <- ex %>%
  distinct(USUBJID, .keep_all = TRUE) %>%
  mutate(EXSEQ = case_when(EXSEQ == 1 ~ 4),
         EXSEQ = as.factor(EXSEQ))
ex <- bind_rows(ex, seq4)
rm(seq4)

igg_data <- read.csv("data/igg.csv", header = TRUE) %>%
  mutate(
    USUBJID = str_replace_all(USUBJID, c(
      "SVI-CH-02-001-" = "",
      " .*$" = "",
      "^0+" = "",
      "-" = "",
      "D0" = ""
    )),
    USUBJID = paste0("SVI-CH-02-001-", USUBJID),
    igg = as.numeric(igg),
    EXSEQ = case_when(
      visit <= 56 ~ "1",
      visit <= 112 ~ "2",
      visit <= 140 ~ "3",
      TRUE ~ "4"
    )
  ) %>%
  rename(value = igg) %>%
  filter(!is.na(value)) %>%
  filter(!visit %in% c(238, 252))

igg <- igg_data %>% filter(type == "igg")
igg <- inner_join(ex, igg, by = "USUBJID")


# stats ------------------------------------------------------------------------
igg_summary <- igg %>% 
  
  group_by(visit, EXDOSE) %>% 
  
  summarize(
    n = n_distinct(USUBJID),
    Min = min(value),
    Max = max(value),
    Mean = mean(value),
    Median = median(value),
    SD = sd(value),
    GML = exp(mean(log(value + 0.1))),
    log_se = sd(log(value + 0.1)) / sqrt(n()),
    lwr = exp(mean(log(value + 0.1)) - 1.96 * log_se),
    upr = exp(mean(log(value + 0.1)) + 1.96 * log_se),
    .groups = "drop"
  ) %>%
  
  mutate(
    n = round(n, 1),
    `(Min, Max)` = paste0("(",
                          formatC(Min, format = "f", digits = 1), ", ",
                          formatC(Max, format = "f", digits = 1), ")"),
    Mean   = formatC(Mean,   format = "f", digits = 1),
    Median = formatC(Median, format = "f", digits = 1),
    SD     = formatC(SD,     format = "f", digits = 1),
    `GML (95% CI)` = paste0(
      formatC(GML, format = "f", digits = 1), " (",
      formatC(lwr, format = "f", digits = 1), ", ",
      formatC(upr, format = "f", digits = 1), ")"
    )
  ) %>%
  
  dplyr::select(visit, EXDOSE, n, `(Min, Max)`, Mean, Median, SD, `GML (95% CI)`) %>%
  
  mutate(across(everything(), as.character))


# anova ------------------------------------------------------------------------
anova_p <- igg %>%
  
  mutate(log_value = log(value + 0.1)) %>%
  
  group_by(visit) %>%
  
  group_split() %>%
  
  map_df(function(df) {
    v <- unique(df$visit)
    p <- tryCatch({
      summary(aov(log_value ~ factor(EXDOSE), data = df))[[1]]$`Pr(>F)`[1]
    }, error = function(e) NA_real_)
    tibble(visit = as.character(v), p_raw = p)
  }) %>%
  
  mutate(
    p_adj = p.adjust(p_raw, method = "bonferroni"),
    `Overall (p-value)` = ifelse(
      is.na(p_adj), "",
      ifelse(p_adj < 0.001, "<0.001", formatC(p_adj, digits = 3, format = "f"))
    )
  ) %>%
  
  dplyr::select(visit, `Overall (p-value)`)

igg_final_anova <- igg_summary %>%
  
  pivot_longer(
    cols = c(n, `(Min, Max)`, Mean, Median, SD, `GML (95% CI)`),
    names_to = "Statistic",
    values_to = "value"
  ) %>%
  
  pivot_wider(
    names_from = EXDOSE,
    values_from = value
  ) %>%
  
  rename(
    TimePointRaw = visit,
    `100µg Na-GST-1/Alhydrogel`                 = `100`,
    `100µg Na-GST-1/Alhydrogel + 5µg AP 10-701` = `5`,
    `100µg Na-GST-1/Alhydrogel + 500µg CpG`     = `500`,
    `Saline Placebo`                            = `0`
  ) %>%
  
  left_join(anova_p, by = c("TimePointRaw" = "visit")) %>%
  
  mutate(
    `Overall (p-value)` = ifelse(Statistic == "GML (95% CI)", `Overall (p-value)`, ""),
    `Time Point` = paste0("Day ", TimePointRaw),
    `Time Point` = str_replace(`Time Point`, "Day 0",   "Day 0 (Pre-Dose 1)"),
    `Time Point` = str_replace(`Time Point`, "Day 14",  "Day 14"),
    `Time Point` = str_replace(`Time Point`, "Day 56",  "Day 56 (Pre-Dose 2)"),
    `Time Point` = str_replace(`Time Point`, "Day 70",  "Day 70 (14 Days Post-Dose 2)"),
    `Time Point` = str_replace(`Time Point`, "Day 112", "Day 112 (Pre-Dose 3)"),
    `Time Point` = str_replace(`Time Point`, "Day 126", "Day 126 (14 Days Post-Dose 3)"),
    `Time Point` = str_replace(`Time Point`, "Day 140", "Day 140 (CHHI; 28 Days Post-Dose 3)"),
    `Time Point` = str_replace(`Time Point`, "Day 147", "Day 147 (Follow-up)"),
    `Time Point` = str_replace(`Time Point`, "Day 154", "Day 154 (Follow-up)"),
    `Time Point` = str_replace(`Time Point`, "Day 175", "Day 175 (Follow-up)"),
    `Time Point` = str_replace(`Time Point`, "Day 189", "Day 189 (Follow-up)"),
    `Time Point` = str_replace(`Time Point`, "Day 231", "Day 231 (Follow-up)"),
    `Time Point` = str_replace(`Time Point`, "Day 280", "Day 280 (Albendazole Rx)"),
    `Time Point` = str_replace(`Time Point`, "Day 320", "Day 320 (Follow-up)"),
    `Time Point` = str_replace(`Time Point`, "Day 380", "Day 380 (Follow-up)")
  ) %>%
  
  dplyr::select(
    `Time Point`, Statistic,
    `100µg Na-GST-1/Alhydrogel`,
    `100µg Na-GST-1/Alhydrogel + 5µg AP 10-701`,
    `100µg Na-GST-1/Alhydrogel + 500µg CpG`,
    `Saline Placebo`,
    `Overall (p-value)`
  )


# excel table ------------------------------------------------------------------
out_path <- "out/table_s09.xlsx"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

wb  <- createWorkbook()
sh  <- "IgG ANOVA"
addWorksheet(wb, sh)

# row 1
top_header <- c("Time Point", "Statistic",
                "Anti–Na-GST-1 IgG Level (AU)", "", "", "",
                "Overall (p-value)")
writeData(wb, sh, t(top_header), startRow = 1, startCol = 1, colNames = FALSE)
mergeCells(wb, sh, cols = 3:6, rows = 1)

# row 2
col_header <- c("",
                "",
                "100µg Na-GST-1/Alhydrogel",
                "100µg Na-GST-1/Alhydrogel + 5µg AP 10-701",
                "100µg Na-GST-1/Alhydrogel + 500µg CpG",
                "Saline Placebo",
                "Overall (p-value)")
writeData(wb, sh, t(col_header), startRow = 2, startCol = 1, colNames = FALSE)

# body
writeData(wb, sh, igg_final_anova, startRow = 3, startCol = 1, colNames = FALSE)

# styles
hdrTop <- createStyle(fontSize = 11, textDecoration = "bold",
                      halign = "center", valign = "center",
                      border = "TopBottomLeftRight")
hdr2   <- createStyle(fontSize = 10, textDecoration = "bold",
                      halign = "center", valign = "center",
                      border = "TopBottomLeftRight", wrapText = TRUE)
body   <- createStyle(fontSize = 10, halign = "center", valign = "center",
                      border = "TopBottomLeftRight", wrapText = TRUE)
centerBody <- createStyle(fontSize = 10, halign = "center", valign = "center",
                          border = "TopBottomLeftRight", wrapText = TRUE)

# header styles
addStyle(wb, sh, hdrTop, rows = 1, cols = 1:7, gridExpand = TRUE)
addStyle(wb, sh, hdr2,   rows = 2, cols = 1:7, gridExpand = TRUE)

# body styles
nr <- nrow(igg_final_anova)
addStyle(wb, sh, body, rows = 3:(2 + nr), cols = 2:7, gridExpand = TRUE)

tp <- igg_final_anova$`Time Point`
run_starts <- c(1, which(tp != dplyr::lag(tp, default = tp[1])))
run_starts <- sort(unique(run_starts))
run_ends <- c(run_starts[-1] - 1, length(tp))

for (i in seq_along(run_starts)) {
  r1 <- 2 + run_starts[i]  # offset for two header rows
  r2 <- 2 + run_ends[i]
  mergeCells(wb, sh, cols = 1, rows = r1:r2)
  addStyle(wb, sh, centerBody, rows = r1:r2, cols = 1, gridExpand = TRUE)
}

# col widths/row heights
setColWidths(wb, sh, cols = 1, widths = 26)
setColWidths(wb, sh, cols = 2, widths = 16)
setColWidths(wb, sh, cols = 3:6, widths = 28)
setColWidths(wb, sh, cols = 7, widths = 18)
setRowHeights(wb, sh, rows = 1, heights = 24)
setRowHeights(wb, sh, rows = 2, heights = 38)


# write ------------------------------------------------------------------------
saveWorkbook(wb, out_path, overwrite = TRUE)

