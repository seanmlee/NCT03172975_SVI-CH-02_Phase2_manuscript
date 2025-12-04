

# libraries --------------------------------------------------------------------
library(brms)
library(dplyr)
library(tibble)
library(purrr)
library(openxlsx)


extract_brms_diagnostics <- function(model, model_label) {
  
  sm    <- summary(model)
  fixed <- as.data.frame(sm$fixed)
  fixed$Parameter <- rownames(fixed)
  
  nd <- brms::ndraws(model)
  
  keep_cols <- intersect(
    c("Estimate", "Est.Error", "l-95% CI", "u-95% CI",
      "Rhat", "Bulk_ESS", "Tail_ESS"),
    names(fixed)
  )
  
  cols_to_keep <- c("Parameter", keep_cols)
  cols_to_keep <- cols_to_keep[cols_to_keep %in% names(fixed)]
  
  fixed_tbl <- fixed %>%
    as_tibble() %>%
    { .[, cols_to_keep, drop = FALSE] }
  
  if ("Bulk_ESS" %in% names(fixed_tbl)) {
    fixed_tbl <- fixed_tbl %>%
      mutate(Bulk_ESS_ratio = Bulk_ESS / nd)
  } else {
    fixed_tbl <- fixed_tbl %>%
      mutate(Bulk_ESS_ratio = NA_real_)
  }
  
  if ("Tail_ESS" %in% names(fixed_tbl)) {
    fixed_tbl <- fixed_tbl %>%
      mutate(Tail_ESS_ratio = Tail_ESS / nd)
  } else {
    fixed_tbl <- fixed_tbl %>%
      mutate(Tail_ESS_ratio = NA_real_)
  }
  
  fixed_tbl %>%
    mutate(
      Model        = model_label,
      ndraws_total = nd
    ) %>%
    relocate(Model, Parameter)
}

bayes_models <- list(
  "McMaster Method Egg Count ~ dose (ZINB, brms)" =
    mod_mcmaster_dose,
  "McMaster Egg Count ~ IgG Day 140 (ZINB, brms)" =
    mod_mcmaster_igg_140,
  "McMaster Egg Count ~ IgG Day 147 (ZINB, brms)" =
    mod_mcmaster_igg_147
)

bayes_diag_tbl <- purrr::imap_dfr(
  bayes_models,
  ~ extract_brms_diagnostics(model = .x, model_label = .y)
)

bayes_diag_tbl <- bayes_diag_tbl %>%
  mutate(
    Rhat           = if ("Rhat" %in% names(.)) round(Rhat, 3) else Rhat,
    Bulk_ESS_ratio = round(Bulk_ESS_ratio, 3),
    Tail_ESS_ratio = round(Tail_ESS_ratio, 3)
  )

out_path <- "out/table_bayesian_diagnostics.xlsx"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

wb <- createWorkbook()
sh <- "Bayes Diagnostics"
addWorksheet(wb, sh)

writeData(wb, sh, bayes_diag_tbl, startRow = 1, startCol = 1)

hdrStyle <- createStyle(
  fontSize = 10, textDecoration = "bold",
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", wrapText = TRUE
)
bodyStyle <- createStyle(
  fontSize = 10,
  halign  = "center", valign = "center",
  border  = "TopBottomLeftRight", wrapText = TRUE
)

addStyle(wb, sh, hdrStyle, rows = 1, cols = 1:ncol(bayes_diag_tbl), gridExpand = TRUE)
addStyle(wb, sh, bodyStyle, rows = 2:(nrow(bayes_diag_tbl) + 1),
         cols = 1:ncol(bayes_diag_tbl), gridExpand = TRUE)

setColWidths(wb, sh, cols = 1, widths = 55)
setColWidths(wb, sh, cols = 2, widths = 30)
setColWidths(wb, sh, cols = 3:ncol(bayes_diag_tbl), widths = 12)

saveWorkbook(wb, out_path, overwrite = TRUE)

cat("Bayesian diagnostics table written to:", out_path, "\n")
