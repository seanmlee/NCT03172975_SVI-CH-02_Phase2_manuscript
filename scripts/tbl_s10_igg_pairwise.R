

# libraries --------------------------------------------------------------------
library(haven)
library(tidyverse)
library(openxlsx)
library(emmeans)
library(stringr)
library(purrr)


# load -------------------------------------------------------------------------
ex <- read_xpt("data/ex.xpt") %>%
  
  select(USUBJID, EXSEQ, EXDOSE) %>%
  
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


# helper -----------------------------------------------------------------------
dose_label_long <- function(dose_code) {
  
  case_when(
    dose_code == 0   ~ "Saline Placebo",
    dose_code == 100 ~ "100µg Na-GST-1/Alhydrogel",
    dose_code == 5   ~ "Na-GST-1/Alhydrogel/5µg AP 10-701",
    dose_code == 500 ~ "Na-GST-1/Alhydrogel/500µg CpG 10104",
    TRUE ~ as.character(dose_code)
  )
  
}


# pairwise comparisons ---------------------------------------------------------
visits_of_interest <- c(70, 126, 140, 147)

pairwise_custom <- igg %>%
  
  filter(visit %in% visits_of_interest, EXDOSE %in% c(0, 5, 100, 500)) %>%
  
  mutate(log_value = log(value + 0.1)) %>%
  
  group_by(visit) %>%
  
  group_split() %>%
  
  map_df(function(df) {
    
    visit_val <- unique(df$visit)
    
    fit <- aov(log_value ~ factor(EXDOSE), data = df)
    
    em <- emmeans(fit, ~ factor(EXDOSE))
    
    cmp <- pairs(em, adjust = "tukey") %>% summary(infer = TRUE)
    
    tibble(
      visit = visit_val,
      contrast = cmp$contrast,
      Estimate_raw = cmp$estimate,
      SE = cmp$SE,
      t_raw = cmp$t.ratio,
      p = cmp$p.value
    )
    
  })

pairwise_final <- pairwise_custom %>%
  
  mutate(
    nums = str_extract_all(contrast, "\\d+"),
    G1 = as.numeric(map_chr(nums, 1)),
    G2 = as.numeric(map_chr(nums, 2))
  ) %>%
  
  rowwise() %>%
  
  mutate(
    
    ref = if (G1 == 0 | G2 == 0) {
      0
    } else if (G1 == 500 | G2 == 500) {
      500
    } else {
      NA_real_
    },
    
    comp = ifelse(G1 == ref, G2, G1),

        Estimate = ifelse(G1 == ref, -Estimate_raw, Estimate_raw),
    
    t = ifelse(G1 == ref, -t_raw, t_raw)
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    `Time Point` = paste0("Day ", visit),
    `Time Point` = str_replace(`Time Point`, "Day 70",  "Day 70 (14 Days Post-Dose 2)"),
    `Time Point` = str_replace(`Time Point`, "Day 126", "Day 126 (14 Days Post-Dose 3)"),
    `Time Point` = str_replace(`Time Point`, "Day 140", "Day 140 (CHHI; 28 Days Post-Dose 3)"),
    `Time Point` = str_replace(`Time Point`, "Day 147", "Day 147 (Follow-up)"),
    Reference  = dose_label_long(ref),
    Comparison = dose_label_long(comp)
  ) %>%
  
  filter(!is.na(Reference), !is.na(Comparison), !is.na(Estimate), !is.na(SE), !is.na(t), !is.na(p)) %>%

  mutate(
    Estimate = formatC(Estimate, digits = 3, format = "f"),
    SE       = formatC(SE,       digits = 2, format = "f"),
    t        = formatC(t,        digits = 2, format = "f"),
    p        = ifelse(p < 0.001, "<0.001", formatC(p, digits = 3, format = "f"))
  ) %>%
  
  dplyr::select(`Time Point`, Reference, Comparison, Estimate, SE, t, p) %>%
  
  mutate(
    `Time Point` = factor(`Time Point`,
                          levels = c("Day 70 (14 Days Post-Dose 2)",
                                     "Day 126 (14 Days Post-Dose 3)",
                                     "Day 140 (CHHI; 28 Days Post-Dose 3)",
                                     "Day 147 (Follow-up)"))
  ) %>%
  
  arrange(`Time Point`,
          factor(Reference, levels = c("Saline Placebo",
                                       "Na-GST-1/Alhydrogel/500µg CpG 10104")),
          Comparison)


# excel table ------------------------------------------------------------------
out_path_pw <- "out/table_s10.xlsx"
dir.create(dirname(out_path_pw), recursive = TRUE, showWarnings = FALSE)

wb_pw <- createWorkbook()
sh_pw <- "Pairwise"
addWorksheet(wb_pw, sh_pw)

# row 1
top_header_pw <- c("Time Point",
                   "Post-hoc Pairwise Comparisons (Comparison vs Reference)", "", "", "", "", "")
writeData(wb_pw, sh_pw, t(top_header_pw), startRow = 1, startCol = 1, colNames = FALSE)
mergeCells(wb_pw, sh_pw, cols = 2:7, rows = 1)

# row 2
col_header_pw <- c("",
                   "Reference", "Comparison", "Estimate", "SE", "t", "p")
writeData(wb_pw, sh_pw, t(col_header_pw), startRow = 2, startCol = 1, colNames = FALSE)

# body
writeData(wb_pw, sh_pw, pairwise_final, startRow = 3, startCol = 1, colNames = FALSE)

# styles
hdrTop <- createStyle(fontSize = 11, textDecoration = "bold",
                      halign = "center", valign = "center",
                      border = "TopBottomLeftRight")
hdr2   <- createStyle(fontSize = 10, textDecoration = "bold",
                      halign = "center", valign = "center",
                      border = "TopBottomLeftRight", wrapText = TRUE)
body   <- createStyle(fontSize = 10, halign = "center", valign = "center",
                      border = "TopBottomLeftRight", wrapText = TRUE)

# header styles
addStyle(wb_pw, sh_pw, hdrTop, rows = 1, cols = 1:7, gridExpand = TRUE)
addStyle(wb_pw, sh_pw, hdr2,   rows = 2, cols = 1:7, gridExpand = TRUE)

# body style
nr_pw <- nrow(pairwise_final)
addStyle(wb_pw, sh_pw, body, rows = 3:(2 + nr_pw), cols = 2:7, gridExpand = TRUE)

# merge/center time points
tp <- as.character(pairwise_final$`Time Point`)
run_starts <- c(1, which(tp != dplyr::lag(tp, default = tp[1])))
run_starts <- sort(unique(run_starts))
run_ends <- c(run_starts[-1] - 1, length(tp))

for (i in seq_along(run_starts)) {
  r1 <- 2 + run_starts[i] 
  r2 <- 2 + run_ends[i]
  mergeCells(wb_pw, sh_pw, cols = 1, rows = r1:r2)
  addStyle(wb_pw, sh_pw, body, rows = r1:r2, cols = 1, gridExpand = TRUE)
}

# col widths/row heights
setColWidths(wb_pw, sh_pw, cols = 1, widths = 36)
setColWidths(wb_pw, sh_pw, cols = 2, widths = 28)
setColWidths(wb_pw, sh_pw, cols = 3, widths = 28)
setColWidths(wb_pw, sh_pw, cols = 4, widths = 14)
setColWidths(wb_pw, sh_pw, cols = 5, widths = 10)
setColWidths(wb_pw, sh_pw, cols = 6, widths = 10)
setColWidths(wb_pw, sh_pw, cols = 7, widths = 12)

setRowHeights(wb_pw, sh_pw, rows = 1, heights = 24)
setRowHeights(wb_pw, sh_pw, rows = 2, heights = 38)


# write ------------------------------------------------------------------------
saveWorkbook(wb_pw, out_path_pw, overwrite = TRUE)

