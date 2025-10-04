

# libraries --------------------------------------------------------------------
library(tidyverse)
library(parameters)
library(openxlsx)


# helpers ----------------------------------------------------------------------
dose_label_long <- function(x) {
  case_when(
    x %in% c("placebo","0")   ~ "Saline Placebo",
    x %in% c("dose5","5")     ~ "Na-GST-1/Alhydrogel/5µg AP 10-701",
    x %in% c("dose100","100") ~ "100µg Na-GST-1/Alhydrogel",
    x %in% c("dose500","500") ~ "Na-GST-1/Alhydrogel/500µg CpG 10104",
    TRUE ~ as.character(x)
  )
}

fmt_num  <- function(x, d=2) formatC(x, digits=d, format="f")
fmt_ci   <- function(lo, hi, d=2) paste0("(", fmt_num(lo,d), ", ", fmt_num(hi,d), ")")
fmt_p    <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, digits = 3, format = "f")))

make_dose_block <- function(model, block_title, digits = 2) {

    if ("dose" %in% names(model$model)) {
    model$model$dose <- factor(model$model$dose, levels = c("placebo","dose5","dose100","dose500"))
    model$model$dose <- stats::relevel(model$model$dose, ref = "placebo")
  }
  
  prm <- parameters::model_parameters(model, exponentiate = TRUE, verbose = FALSE) |> as_tibble()
  prm$Parameter <- as.character(prm$Parameter)
  
  ref <- prm %>% filter(Parameter == "(Intercept)") %>%
    transmute(
      Section = block_title,
      Term = "Saline Placebo (reference)",
      EstVal = Coefficient,
      Estimate = fmt_num(EstVal, digits),
      `95% CI`  = fmt_ci(CI_low, CI_high, digits),
      p = "-"
    ) %>% select(-EstVal)
  
  others <- prm %>%
    filter(grepl("^dose", Parameter)) %>%
    mutate(Level = gsub("`", "", Parameter),
           Level = sub("^dose", "", Level),
           Group = dose_label_long(Level),
           EstVal = Coefficient) %>%
    transmute(
      Section = block_title,
      Term = Group,
      Estimate = fmt_num(EstVal, digits),
      `95% CI`  = fmt_ci(CI_low, CI_high, digits),
      p = fmt_p(p)
    )
  
  out <- bind_rows(ref, others) %>%
    mutate(Term = factor(
      Term,
      levels = c("Saline Placebo (reference)",
                 "Na-GST-1/Alhydrogel/5µg AP 10-701",
                 "100µg Na-GST-1/Alhydrogel",
                 "Na-GST-1/Alhydrogel/500µg CpG 10104")
    )) %>%
    arrange(Term) %>%
    mutate(Term = as.character(Term))
  
  out
}

make_continuous_block <- function(model, block_title, coef_label, digits = 2) {
  prm <- parameters::model_parameters(model, exponentiate = TRUE, verbose = FALSE) |> as_tibble()
  prm$Parameter <- as.character(prm$Parameter)
  
  bind_rows(
    prm %>% filter(Parameter == "(Intercept)") %>%
      transmute(
        Section = block_title,
        Term = "Intercept",
        EstVal = Coefficient,
        Estimate = fmt_num(EstVal, digits),
        `95% CI`  = fmt_ci(CI_low, CI_high, digits),
        p = fmt_p(p)
      ) %>% select(-EstVal),
    prm %>% filter(Parameter != "(Intercept)") %>%
      transmute(
        Section = block_title,
        Term = coef_label,
        EstVal = Coefficient,
        Estimate = fmt_num(EstVal, digits),
        `95% CI`  = fmt_ci(CI_low, CI_high, digits),
        p = fmt_p(p)
      ) %>% select(-EstVal)
  )
}


# fit models -------------------------------------------------------------------
dv_max <- dv_max %>%
  mutate(dose = factor(dose, levels = c("placebo", "dose5", "dose100", "dose500")))
dv_ae  <- dv_ae  %>%
  mutate(dose = factor(dose, levels = c("placebo", "dose5", "dose100", "dose500")))

mod_float_dose     <- glm(float ~ dose, family = binomial(), data = dv_max)
mod_mcmaster_dose  <- glm(log(max_egg_count + 0.1) ~ dose, data = dv_max)
mod_eos_dose       <- glm(log(max_eos_count) ~ dose, data = dv_max)
mod_ae             <- glm(ae ~ dose, family = poisson(), data = dv_ae)
mod_igg_dose       <- glm(log(max_igg_count_140) ~ dose, data = dv_max)

mod_float_igg_140    <- glm(float ~ scale(max_igg_count_140, scale = FALSE), family = binomial(), data = dv_max)
mod_mcmaster_igg_140 <- glm(log(max_egg_count + 0.1) ~ scale(max_igg_count_140, scale = FALSE), data = dv_max)
mod_float_igg_147    <- glm(float ~ scale(max_igg_count_147, scale = FALSE), family = binomial(), data = dv_max)
mod_mcmaster_igg_147 <- glm(log(max_egg_count + 0.1) ~ scale(max_igg_count_147, scale = FALSE), data = dv_max)

blocks <- list(
  make_dose_block(mod_float_dose,             "Hypertonic Saline Flotation Egg Result – Study Group"),
  make_dose_block(mod_mcmaster_dose,          "McMaster Method Egg Result – Study Group"),
  make_dose_block(mod_eos_dose,               "Eosinophil Count – Study Group"),
  make_dose_block(mod_ae,                     "Number of Adverse Events – Study Group"),
  make_dose_block(mod_igg_dose,               "Anti–Na–GST-1 IgG Level – Study Group"),
  make_continuous_block(mod_float_igg_140,    "Hypertonic Saline Flotation Egg Result – Anti–Na–GST-1 IgG Level on Day 140", "Anti–Na–GST-1 IgG"),
  make_continuous_block(mod_mcmaster_igg_140, "McMaster Method Egg Result – Anti–Na–GST-1 IgG Level on Day 140",             "Anti–Na–GST-1 IgG"),
  make_continuous_block(mod_float_igg_147,    "Hypertonic Saline Flotation Egg Result – Anti–Na–GST-1 IgG Level on Day 147", "Anti–Na–GST-1 IgG"),
  make_continuous_block(mod_mcmaster_igg_147, "McMaster Method Egg Result – Anti–Na–GST-1 IgG Level on Day 147",             "Anti–Na–GST-1 IgG")
)

tbl <- bind_rows(blocks) %>%
  mutate(Section = stringr::str_replace_all(Section, " – ", " ~ "))


# excel table ------------------------------------------------------------------
out_path <- "out/table_s07.xlsx"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

wb <- createWorkbook()
sh <- "Model Summary"
addWorksheet(wb, sh)

# header
hdr <- c("Dependent Variable ~ Independent Variable", "Term", "Estimate", "95% CI", "P")
writeData(wb, sh, t(hdr), startRow = 1, startCol = 1, colNames = FALSE)

# body
writeData(wb, sh, tbl %>% select(Section, Term, Estimate, `95% CI`, p),
          startRow = 2, startCol = 1, colNames = FALSE)

# styles
hdrStyle   <- createStyle(fontSize = 10, textDecoration = "bold",
                          halign = "center", valign = "center",
                          border = "TopBottomLeftRight", wrapText = TRUE)
cellLeft   <- createStyle(fontSize = 10, halign = "left",  valign = "center",
                          border = "TopBottomLeftRight", wrapText = TRUE)
cellCent   <- createStyle(fontSize = 10, halign = "center",valign = "center",
                          border = "TopBottomLeftRight", wrapText = TRUE)
boldP      <- createStyle(fontSize = 10, textDecoration = "bold",
                          halign = "center", valign = "center",
                          border = "TopBottomLeftRight")

addStyle(wb, sh, hdrStyle, rows = 1, cols = 1:5, gridExpand = TRUE)

n_rows <- nrow(tbl)

# cols
addStyle(wb, sh, cellLeft, rows = 2:(1 + n_rows), cols = 1, gridExpand = TRUE)
addStyle(wb, sh, cellCent, rows = 2:(1 + n_rows), cols = 2:5, gridExpand = TRUE)

sec <- tbl$Section
starts <- c(1, which(sec != dplyr::lag(sec, default = sec[1])))
ends <- c(starts[-1] - 1, length(sec))
for (i in seq_along(starts)) {
  r1 <- 1 + starts[i]
  r2 <- 1 + ends[i]
  mergeCells(wb, sh, cols = 1, rows = r1:r2)
}

num_p <- suppressWarnings(as.numeric(tbl$p))
sig_rows <- which((!is.na(num_p) & num_p < 0.05) | grepl("^<0.001$", tbl$p))
if (length(sig_rows)) {
  addStyle(wb, sh, boldP, rows = 1 + sig_rows, cols = 5, gridExpand = TRUE, stack = TRUE)
}


# col width
setColWidths(wb, sh, cols = 1, widths = 58)
setColWidths(wb, sh, cols = 2, widths = 40)
setColWidths(wb, sh, cols = 3, widths = 12)
setColWidths(wb, sh, cols = 4, widths = 20)
setColWidths(wb, sh, cols = 5, widths = 8)


# write ------------------------------------------------------------------------
saveWorkbook(wb, out_path, overwrite = TRUE)
