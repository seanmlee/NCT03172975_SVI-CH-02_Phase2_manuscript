

# libraries --------------------------------------------------------------------
library(tidyverse)
library(emmeans)
library(openxlsx)
library(stringr)


# fit model --------------------------------------------------------------------
mod_ae <- glm(
  ae ~ dose,
  data = dv_ae,
  family = poisson()
)

# posthoc pairwise comparisons -------------------------------------------------
pw_raw <- pairs(emmeans(mod_ae, ~ dose), adjust = "tukey") %>% summary(infer = TRUE)

all_pw <- tibble(
  contrast = pw_raw$contrast,
  est      = pw_raw$estimate,
  SE       = pw_raw$SE,
  z        = pw_raw$z.ratio,
  p        = pw_raw$p.value
) %>%
  separate(contrast, into = c("g1", "g2"), sep = " - ", remove = FALSE)


# helper -----------------------------------------------------------------------
label_long <- function(x) {
  case_when(
    x == "placebo"  ~ "Saline Placebo",
    x == "dose5"    ~ "Na-GST-1/Alhydrogel/5µg AP 10-701",
    x == "dose100"  ~ "100µg Na-GST-1/Alhydrogel",
    x == "dose500"  ~ "Na-GST-1/Alhydrogel/500µg CpG 10104",
    TRUE ~ x
  )
}

# format table -----------------------------------------------------------------
row_100_vs_500 <- all_pw %>%
  filter((g1 == "dose100" & g2 == "dose500") | (g1 == "dose500" & g2 == "dose100")) %>%
  mutate(
    Reference_code  = "dose500",
    Comparison_code = "dose100",
    est = ifelse(g1 == "dose500", -est, est),
    z   = ifelse(g1 == "dose500", -z, z)
  ) %>%
  slice(1)

row_5_vs_500 <- all_pw %>%
  filter((g1 == "dose5" & g2 == "dose500") | (g1 == "dose500" & g2 == "dose5")) %>%
  mutate(
    Reference_code  = "dose500",
    Comparison_code = "dose5",
    est = ifelse(g1 == "dose500", -est, est),
    z   = ifelse(g1 == "dose500", -z, z)
  ) %>%
  slice(1)

row_5_vs_100 <- all_pw %>%
  filter((g1 == "dose5" & g2 == "dose100") | (g1 == "dose100" & g2 == "dose5")) %>%
  mutate(
    Reference_code  = "dose100",
    Comparison_code = "dose5",
    est = ifelse(g1 == "dose100", -est, est),
    z   = ifelse(g1 == "dose100", -z, z)
  ) %>%
  slice(1)

ae_pw_three <- bind_rows(row_100_vs_500, row_5_vs_500, row_5_vs_100) %>%
  transmute(
    Reference  = label_long(Reference_code),
    Comparison = label_long(Comparison_code),
    `β`  = formatC(est, digits = 3, format = "f"),
    `SE` = formatC(SE,  digits = 2, format = "f"),
    `z`  = formatC(z,   digits = 2, format = "f"),
    p_raw = p,
    `p`  = ifelse(p < 0.001, "<0.001", formatC(p, digits = 3, format = "f"))
  )


# excel table ------------------------------------------------------------------
out_path <- "out/table_s06.xlsx"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

wb <- createWorkbook()
sh <- "AE Pairwise (3 rows)"
addWorksheet(wb, sh)

# row 1
hdr1 <- c("Contrast", "", "Adverse Events", "", "", "")
writeData(wb, sh, t(hdr1), startRow = 1, startCol = 1, colNames = FALSE)
mergeCells(wb, sh, cols = 1:2, rows = 1)
mergeCells(wb, sh, cols = 3:6, rows = 1)

# row 2
hdr2 <- c("Reference", "Comparison", "β", "SE", "z", "p")
writeData(wb, sh, t(hdr2), startRow = 2, startCol = 1, colNames = FALSE)

# body
writeData(wb, sh, ae_pw_three %>% select(Reference, Comparison, `β`, `SE`, `z`, `p`),
          startRow = 3, startCol = 1, colNames = FALSE)

# styles
hdrTop <- createStyle(fontSize = 11, textDecoration = "bold",
                      halign = "center", valign = "center",
                      border = "TopBottomLeftRight")
hdr2s  <- createStyle(fontSize = 10, textDecoration = "bold",
                      halign = "center", valign = "center",
                      border = "TopBottomLeftRight", wrapText = TRUE)
body   <- createStyle(fontSize = 10, halign = "center", valign = "center",
                      border = "TopBottomLeftRight", wrapText = TRUE)
leftBody <- createStyle(fontSize = 10, halign = "left", valign = "center",
                        border = "TopBottomLeftRight", wrapText = TRUE)
boldP  <- createStyle(fontSize = 10, textDecoration = "bold",
                      halign = "center", valign = "center",
                      border = "TopBottomLeftRight")

addStyle(wb, sh, hdrTop, rows = 1, cols = 1:6, gridExpand = TRUE)
addStyle(wb, sh, hdr2s,  rows = 2, cols = 1:6, gridExpand = TRUE)
addStyle(wb, sh, leftBody, rows = 3:5, cols = 1:2, gridExpand = TRUE)
addStyle(wb, sh, body,     rows = 3:5, cols = 3:6, gridExpand = TRUE)

sig_rows <- which(ae_pw_three$p_raw < 0.05)
if (length(sig_rows)) {
  addStyle(wb, sh, boldP, rows = 2 + sig_rows, cols = 6, gridExpand = TRUE, stack = TRUE)
}

# col width
setColWidths(wb, sh, cols = 1, widths = 42)
setColWidths(wb, sh, cols = 2, widths = 42)
setColWidths(wb, sh, cols = 3:6, widths = 10)


# write ------------------------------------------------------------------------
saveWorkbook(wb, out_path, overwrite = TRUE)

