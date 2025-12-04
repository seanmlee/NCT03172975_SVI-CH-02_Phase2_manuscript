## ---------------------------------------------------------------
## Basic model diagnostics (no extra packages beyond brms)
## ---------------------------------------------------------------
## Assumes the following models already exist in the workspace:
##   mod_float_dose
##   mod_mcmaster_dose     # brms ZINB
##   mod_eos_dose          # Gaussian on log scale
##   mod_ae                # Poisson
##   mod_igg_dose          # Gaussian on log scale
##   mod_float_igg_140     # Binomial
##   mod_mcmaster_igg_140  # brms ZINB (IgG Day 140)
##   mod_float_igg_147     # Binomial
##   mod_mcmaster_igg_147  # brms ZINB (IgG Day 147)
## ---------------------------------------------------------------

## --------- helpers -------------------------------------------------

check_gaussian_glm <- function(model, name = deparse(substitute(model))) {
  cat("\n==================================================\n")
  cat("Diagnostics for", name, "(Gaussian GLM on transformed outcome)\n")
  cat("==================================================\n")
  
  r  <- residuals(model)          # raw residuals
  fv <- fitted(model)
  
  ## Normality: Shapiro–Wilk
  sh <- shapiro.test(r)
  cat("\nShapiro–Wilk test for normality of residuals:\n")
  print(sh)
  
  ## Simple homoskedasticity check: residuals vs fitted
  op <- par(mfrow = c(1, 2))
  plot(fv, r,
       xlab = "Fitted values",
       ylab = "Residuals",
       main = paste(name, "- Residuals vs Fitted"))
  abline(h = 0, col = "grey")
  
  ## Q-Q plot
  qqnorm(r, main = paste(name, "- Normal Q-Q plot"))
  qqline(r, col = "red")
  par(op)
  
  invisible(sh)
}

check_glm_poisson <- function(model, name = deparse(substitute(model))) {
  cat("\n==================================================\n")
  cat("Diagnostics for", name, "(Poisson GLM)\n")
  cat("==================================================\n")
  
  ## Pearson residual-based dispersion
  rp   <- residuals(model, type = "pearson")
  disp <- sum(rp^2) / df.residual(model)
  cat("\nPearson-based dispersion estimate:", disp, "\n")
  if (disp > 1.5) {
    cat(">>> Suggests overdispersion relative to Poisson assumption.\n")
  } else {
    cat("No strong evidence of overdispersion.\n")
  }
  
  ## Residuals vs fitted
  fv <- fitted(model)
  op <- par(mfrow = c(1, 1))
  plot(fv, rp,
       xlab = "Fitted values",
       ylab = "Pearson residuals",
       main = paste(name, "- Pearson residuals vs Fitted"))
  abline(h = 0, col = "grey")
  par(op)
  
  invisible(disp)
}

check_glm_binomial <- function(model, name = deparse(substitute(model))) {
  cat("\n==================================================\n")
  cat("Diagnostics for", name, "(Binomial GLM)\n")
  cat("==================================================\n")
  
  ## Pearson residual-based dispersion
  rp   <- residuals(model, type = "pearson")
  disp <- sum(rp^2) / df.residual(model)
  cat("\nPearson-based dispersion estimate:", disp, "\n")
  if (disp > 1.5) {
    cat(">>> Possible overdispersion / lack of fit.\n")
  } else {
    cat("No strong evidence of overdispersion.\n")
  }
  
  ## Deviance residuals vs fitted
  rd <- residuals(model, type = "deviance")
  fv <- fitted(model)
  op <- par(mfrow = c(1, 1))
  plot(fv, rd,
       xlab = "Fitted values",
       ylab = "Deviance residuals",
       main = paste(name, "- Deviance residuals vs Fitted"))
  abline(h = 0, col = "grey")
  par(op)
  
  invisible(disp)
}

check_brms_zinb_basic <- function(model, name = deparse(substitute(model))) {
  cat("\n==================================================\n")
  cat("Diagnostics for", name, "(brms zero-inflated negative binomial)\n")
  cat("==================================================\n")
  
  ## Basic summary: includes Rhat, n_eff, etc.
  sm <- summary(model)
  cat("\nFixed-effects summary:\n")
  print(sm$fixed)
  
  ## Show Rhat range for fixed effects
  if ("Rhat" %in% colnames(sm$fixed)) {
    rhat_range <- range(sm$fixed[,"Rhat"], na.rm = TRUE)
    cat("\nRhat range (fixed effects):", paste(rhat_range, collapse = " – "), "\n")
  }
  
  ## Optional simple posterior predictive check:
  if (requireNamespace("brms", quietly = TRUE)) {
    cat("\nCalling brms::pp_check(model) for a basic posterior predictive plot...\n")
    brms::pp_check(model)
  }
  
  invisible(sm)
}

## --------- run diagnostics for each model --------------------------

## 1) Binary: float ~ dose
diag_float_dose <- check_glm_binomial(
  mod_float_dose,
  "Hypertonic Saline Flotation Egg Result ~ dose"
)

## 2) ZINB (Bayesian): McMaster eggs ~ dose
diag_mcmaster_dose <- check_brms_zinb_basic(
  mod_mcmaster_dose,
  "McMaster Method Egg Count ~ dose (ZINB, brms)"
)

## 3) Gaussian (log-scale): log eosinophils ~ dose
diag_eos_dose <- check_gaussian_glm(
  mod_eos_dose,
  "Eosinophil Count (log) ~ dose"
)

## 4) Poisson: AE count ~ dose
diag_ae <- check_glm_poisson(
  mod_ae,
  "Number of Adverse Events ~ dose (Poisson)"
)

## 5) Gaussian (log-scale): log IgG ~ dose
diag_igg_dose <- check_gaussian_glm(
  mod_igg_dose,
  "Anti–Na–GST-1 IgG (log) ~ dose"
)

## 6) Float ~ continuous IgG (Day 140) – Binomial GLM
diag_float_igg_140 <- check_glm_binomial(
  mod_float_igg_140,
  "Float result ~ Anti–Na–GST-1 IgG Day 140"
)

## 7) McMaster eggs ~ IgG (Day 140) – ZINB, brms
diag_mcmaster_igg_140 <- check_brms_zinb_basic(
  mod_mcmaster_igg_140,
  "McMaster egg count ~ Anti–Na–GST-1 IgG Day 140 (ZINB, brms)"
)

## 8) Float ~ continuous IgG (Day 147) – Binomial GLM
diag_float_igg_147 <- check_glm_binomial(
  mod_float_igg_147,
  "Float result ~ Anti–Na–GST-1 IgG Day 147"
)

## 9) McMaster eggs ~ IgG (Day 147) – ZINB, brms
diag_mcmaster_igg_147 <- check_brms_zinb_basic(
  mod_mcmaster_igg_147,
  "McMaster egg count ~ Anti–Na–GST-1 IgG Day 147 (ZINB, brms)"
)

cat("\nAll basic diagnostics completed.\n")
