

# libraries --------------------------------------------------------------------
library(haven)
library(tidyverse)


# read -------------------------------------------------------------------------
lb <- 
  read_xpt(
    "data/lb.xpt",
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  )

ex <- 
  read_xpt(
    "data/ex.xpt",
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  )

ds <- 
  read_xpt(
    "data/ds.xpt",
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  )

igg <- 
  read.csv(
    "data/igg.csv"
    )
  

# exclude ----------------------------------------------------------------------
lb <- lb %>%
  
  filter(
    USUBJID != "SVI-CH-02-001-8002",
    USUBJID != "SVI-CH-02-001-8012",
    USUBJID != "SVI-CH-02-001-8015"
  )


# DSTERM == "COMPLETED" --------------------------------------------------------
ds <- ds %>%
  
  dplyr::select(
    USUBJID, 
    DSTERM
  ) %>%
  
  filter(
    DSTERM == "COMPLETED"
  ) %>%
  
  dplyr::select(
    -DSTERM
  ) %>%
  
  inner_join(lb)

rm(ds)


# float ------------------------------------------------------------------------
float <- lb %>%
  
  dplyr::select(
    LBSEQ,
    USUBJID,   # unique subject id
    LBTEST,    # lab test or exam name
    LBSTAT,    # completion status
    LBORRES,   # flotation result
  ) %>%
  
  filter(
    LBTEST == "Stool Examination" & LBSTAT != "NOT DONE"
  ) %>%
  
  group_by(
    USUBJID
  ) %>%
  
  summarize(
    float = ifelse(
      any(LBORRES == "Positive"), 
      first(LBORRES[LBORRES == "Positive"]), 
      first(LBORRES[LBORRES == "Negative"])),
    .groups = 'drop'
  )


# mcmaster ---------------------------------------------------------------------
mcmaster <- lb %>%
  
  dplyr::select(
    LBSEQ,
    USUBJID,   # unique subject id
    LBTEST,    # lab test or exam name
    LBSTAT,    # completion status
    LBSTRESN   # numerical result/finding in standard units
  ) %>%
  
  filter(
    LBTEST == "Stool Examination" & LBSTAT != "NOT DONE"
  ) %>%
  
  replace(
    is.na(.),
    0
  ) %>%
  
  group_by(
    USUBJID
  ) %>%
  
  summarize(
    max_egg_count = max(LBSTRESN)
  )


# igg_140 ----------------------------------------------------------------------
igg_140 <- igg %>%
  
  filter(
    type == "igg"
  ) %>%
  
  dplyr::select(
    USUBJID, 
    visit, 
    igg
  ) %>%
  
  filter(
    visit == 140
  ) %>%
  
  mutate(
    igg = as.numeric(igg)
  ) %>%
  
  group_by(
    USUBJID
  ) %>%
  
  summarize(
    max_igg_count_140 = max(igg)
  )


# igg_147 ----------------------------------------------------------------------
igg_147 <- igg %>%
  
  filter(
    type == "igg"
  ) %>%
  
  dplyr::select(
    USUBJID, 
    visit, 
    igg
  ) %>%
  
  filter(
    visit == 147
  ) %>%
  
  mutate(
    igg = as.numeric(igg)
  ) %>%
  
  group_by(
    USUBJID
  ) %>%
  
  summarize(
    max_igg_count_147 = max(igg)
  )


# eosinophil -------------------------------------------------------------------
eos <- lb %>%
  
  dplyr::select(
    LBSEQ,
    USUBJID,   # unique subject id
    LBTEST,    # lab test or exam name
    LBSTAT,    # completion status
    LBSTRESN   # numerical result/finding in standard units (eosinophil count)
  ) %>%
  
  filter(
    LBTEST == "Eosinophils" & LBSTAT != "NOT DONE"
  ) %>%
  
  group_by(
    USUBJID
  ) %>%
  
  summarize(
    max_eos_count = max(LBSTRESN)
  ) %>%
  
  dplyr::select(
    USUBJID,
    max_eos_count
  )


# neutrophil -------------------------------------------------------------------
neutro <- lb %>%
  
  dplyr::select(
    LBSEQ,
    USUBJID,   # unique subject id
    LBTEST,    # lab test or exam name
    LBSTAT,    # completion status
    LBSTRESN   # numerical result/finding in standard units (neutroinophil count)
  ) %>%
  
  filter(
    LBTEST == "Neutrophils" & LBSTAT != "NOT DONE"
  ) %>%
  
  group_by(
    USUBJID
  ) %>%
  
  summarize(
    max_neutro_count = max(LBSTRESN)
  ) %>%
  
  dplyr::select(
    USUBJID,
    max_neutro_count
  )


# vaccine group ----------------------------------------------------------------
ex <- ex %>%
  
  distinct(
    USUBJID,   # unique subject id
    EXDOSE     # dose
  ) %>%
  
  mutate(
    dose = case_when(
      EXDOSE == 0 ~ "placebo",
      EXDOSE == 5 ~ "dose5",
      EXDOSE == 100 ~ "dose100",
      EXDOSE == 500 ~ "dose500",
      TRUE ~ NA
    )
  ) %>%
  
  mutate(
    dose = factor(
      dose,
      levels = c(
        "placebo", 
        "dose5", 
        "dose100", 
        "dose500"
      )
    )
  ) %>%
  
  dplyr::select(
    -EXDOSE
  )


# join -------------------------------------------------------------------------
dv_max <- inner_join(mcmaster, float)

dv_max <- inner_join(dv_max, ex)

dv_max <- inner_join(dv_max, igg_140)

dv_max <- full_join(dv_max, igg_147)

dv_max <- inner_join(dv_max, eos)

dv_max <- inner_join(dv_max, neutro)


# format -----------------------------------------------------------------------
dv_max <- dv_max %>%
  
  dplyr::select(
    USUBJID,
    dose,
    float,
    max_egg_count,
    max_igg_count_140,
    max_igg_count_147,
    max_eos_count,
    max_neutro_count
  ) %>%
  
  mutate(
    float = ifelse(
      float == "Negative",
      0,
      1
    )
  ) %>%
  
  mutate(
    float = as.factor(float) 
  )


# rm ---------------------------------------------------------------------------
rm(lb)
rm(ex)
rm(float)
rm(mcmaster)
rm(eos)
rm(neutro)
rm(igg)
rm(igg_140)
rm(igg_147)
