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

fmt_num  <- function(x, d = 2) formatC(x, digits = d, format = "f")
fmt_ci   <- function(lo, hi, d = 2) paste0("(", fmt_num(lo, d), ", ", fmt_num(hi, d), ")")
fmt_p    <- function(p) ifelse(
  is.na(p), "",
  ifelse(p < 0.001, "<0.001", formatC(p, digits = 3, format = "f"))
)

# robustly pick estimate column from model_parameters() ------------------------
get_estval <- function(prm) {
  candidates <- c("Coefficient", "Estimate", "Odds_Ratio", "Risk_Ratio",
                  "Median", "Mean")
  col_found <- candidates[candidates %in% names(prm)][1]
  if (is.na(col_found)) {
    stop(
      "No suitable estimate column found in model_parameters() output. Columns were: ",
      paste(names(prm), collapse = ", ")
    )
  }
  prm |> mutate(EstVal = .data[[col_found]])
}

# dose-block helper (glm + brms) ----------------------------------------------
# NOTE: takes `data` so we can reliably map non-reference doses in order.
make_dose_block <- function(model, block_title, data, digits = 2) {
  
  # standardize dose factor in data
  data <- data %>%
    mutate(dose = factor(dose, levels = c("placebo","dose5","dose100","dose500")))
  dose_levels <- levels(data$dose)
  
  prm <- parameters::model_parameters(
    model,
    exponentiate = TRUE,
    verbose      = FALSE,
    effects      = "fixed",
    component    = "conditional"   # only the count/mean part (for ZINB)
  ) |>
    as_tibble()
  
  prm$Parameter <- as.character(prm$Parameter)
  prm <- get_estval(prm)
  
  # p-values: glm has them, brms typically does not
  if ("p" %in% names(prm)) {
    prm <- prm |> mutate(p_out = fmt_p(p))
  } else {
    prm <- prm |> mutate(p_out = "-")
  }
  
  # intercept: handle glm ("(Intercept)") and brms ("b_Intercept", sometimes "Intercept")
  intercept_idx <- prm$Parameter %in% c("(Intercept)", "Intercept", "b_Intercept")
  
  prm_int    <- prm[intercept_idx, , drop = FALSE]
  prm_others <- prm[!intercept_idx, , drop = FALSE]
  
  # reference row: placebo
  if (nrow(prm_int) == 0) {
    stop("No intercept row found in model_parameters() output for block: ", block_title)
  }
  
  ref <- prm_int %>%
    transmute(
      Section  = block_title,
      Term     = "Saline Placebo (reference)",
      Estimate = fmt_num(EstVal, digits),
      `95% CI` = fmt_ci(CI_low, CI_high, digits),
      p        = p_out
    )
  
  # non-reference doses: map in order to dose levels beyond placebo
  dose_nonref <- dose_levels[-1]
  if (nrow(prm_others) > 0) {
    n_map <- min(nrow(prm_others), length(dose_nonref))
    prm_others <- prm_others[seq_len(n_map), , drop = FALSE]
    
    prm_others <- prm_others %>%
      mutate(
        dose_level = dose_nonref[seq_len(n())],
        Group      = dose_label_long(dose_level)
      ) %>%
      transmute(
        Section  = block_title,
        Term     = Group,
        Estimate = fmt_num(EstVal, digits),
        `95% CI` = fmt_ci(CI_low, CI_high, digits),
        p        = p_out
      )
  }
  
  out <- bind_rows(ref, prm_others) %>%
    mutate(Term = factor(
      Term,
      levels = c(
        "Saline Placebo (reference)",
        "Na-GST-1/Alhydrogel/5µg AP 10-701",
        "100µg Na-GST-1/Alhydrogel",
        "Na-GST-1/Alhydrogel/500µg CpG 10104"
      )
    )) %>%
    arrange(Term) %>%
    mutate(Term = as.character(Term))
  
  out
}

# continuous-block helper (glm + brms) ----------------------------------------
make_continuous_block <- function(model, block_title, coef_label, digits = 2) {
  
  prm <- parameters::model_parameters(
    model,
    exponentiate = TRUE,
    verbose      = FALSE,
    effects      = "fixed",
    component    = "conditional"
  ) |>
    as_tibble()
  
  prm$Parameter <- as.character(prm$Parameter)
  prm <- get_estval(prm)
  
  # p-values: glm has them, brms does not
  if ("p" %in% names(prm)) {
    prm <- prm |> mutate(p_out = fmt_p(p))
  } else {
    prm <- prm |> mutate(p_out = "-")
  }
  
  intercept_idx <- prm$Parameter %in% c("(Intercept)", "Intercept", "b_Intercept")
  
  bind_rows(
    # intercept row
    prm %>%
      filter(intercept_idx) %>%
      transmute(
        Section  = block_title,
        Term     = "Intercept",
        Estimate = fmt_num(EstVal, digits),
        `95% CI` = fmt_ci(CI_low, CI_high, digits),
        p        = p_out
      ),
    # continuous effect row(s)
    prm %>%
      filter(!intercept_idx) %>%
      transmute(
        Section  = block_title,
        Term     = coef_label,
        Estimate = fmt_num(EstVal, digits),
        `95% CI` = fmt_ci(CI_low, CI_high, digits),
        p        = p_out
      )
  )
}

# fit models -------------------------------------------------------------------
dv_max <- dv_max %>% mutate(dose = factor(dose, levels = c("placebo", "dose5", "dose100", "dose500")))
dv_ae  <- dv_ae  %>% mutate(dose = factor(dose, levels = c("placebo", "dose5", "dose100", "dose500")))

mod_float_dose        <- glm(float ~ dose, family = binomial(), data = dv_max)
mod_mcmaster_dose     <- brms::brm( round(max_egg_count) ~ dose, family = "zero_inflated_negbinomial", control = list(adapt_delta = 0.9), iter = 4000, data = dv_max)
mod_eos_dose          <- glm(log(max_eos_count) ~ dose, data = dv_max)
mod_ae                <- glm(ae ~ dose, family = poisson(), data = dv_ae)
mod_igg_dose          <- glm(log(max_igg_count_140) ~ dose, data = dv_max)
mod_float_igg_140     <- glm(float ~ scale(max_igg_count_140, scale = FALSE), family = binomial(), data = dv_max)
mod_mcmaster_igg_140  <- brms::brm(round(max_egg_count) ~ scale(max_igg_count_140, scale = FALSE), family = "zero_inflated_negbinomial", control = list(adapt_delta = 0.9), iter = 4000, data = dv_max)
mod_float_igg_147     <- glm(float ~ scale(max_igg_count_147, scale = FALSE), family = binomial(), data = dv_max)
mod_mcmaster_igg_147  <- brms::brm(round(max_egg_count) ~ scale(max_igg_count_147, scale = FALSE), family = "zero_inflated_negbinomial", control = list(adapt_delta = 0.9), iter = 4000, data = dv_max)

# build combined table ---------------------------------------------------------
blocks <- list(
  make_dose_block(mod_float_dose,    "Hypertonic Saline Flotation Egg Result – Study Group", dv_max),
  make_dose_block(mod_mcmaster_dose, "McMaster Method Egg Result – Study Group",             dv_max),
  make_dose_block(mod_eos_dose,      "Eosinophil Count – Study Group",                       dv_max),
  make_dose_block(mod_ae,            "Number of Adverse Events – Study Group",               dv_ae),
  make_dose_block(mod_igg_dose,      "Anti–Na–GST-1 IgG Level – Study Group",                dv_max),
  make_continuous_block(mod_float_igg_140,
                        "Hypertonic Saline Flotation Egg Result – Anti–Na–GST-1 IgG Level on Day 140",
                        "Anti–Na–GST-1 IgG"),
  make_continuous_block(mod_mcmaster_igg_140,
                        "McMaster Method Egg Result – Anti–Na–GST-1 IgG Level on Day 140 (ZINB)",
                        "Anti–Na–GST-1 IgG"),
  make_continuous_block(mod_float_igg_147,
                        "Hypertonic Saline Flotation Egg Result – Anti–Na–GST-1 IgG Level on Day 147",
                        "Anti–Na–GST-1 IgG"),
  make_continuous_block(mod_mcmaster_igg_147,
                        "McMaster Method Egg Result – Anti–Na–GST-1 IgG Level on Day 147 (ZINB)",
                        "Anti–Na–GST-1 IgG")
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
writeData(
  wb, sh,
  tbl %>% dplyr::select(Section, Term, Estimate, `95% CI`, p),
  startRow = 2, startCol = 1, colNames = FALSE
)

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

sec    <- tbl$Section
starts <- c(1, which(sec != dplyr::lag(sec, default = sec[1])))
ends   <- c(starts[-1] - 1, length(sec))

for (i in seq_along(starts)) {
  r1 <- 1 + starts[i]
  r2 <- 1 + ends[i]
  mergeCells(wb, sh, cols = 1, rows = r1:r2)
}

num_p    <- suppressWarnings(as.numeric(tbl$p))
sig_rows <- which((!is.na(num_p) & num_p < 0.05) | grepl("^<0.001$", tbl$p))
if (length(sig_rows)) {
  addStyle(
    wb, sh, boldP,
    rows = 1 + sig_rows, cols = 5,
    gridExpand = TRUE, stack = TRUE
  )
}

# col widths
setColWidths(wb, sh, cols = 1, widths = 58)
setColWidths(wb, sh, cols = 2, widths = 40)
setColWidths(wb, sh, cols = 3, widths = 12)
setColWidths(wb, sh, cols = 4, widths = 20)
setColWidths(wb, sh, cols = 5, widths = 8)

# write ------------------------------------------------------------------------
saveWorkbook(wb, out_path, overwrite = TRUE)
