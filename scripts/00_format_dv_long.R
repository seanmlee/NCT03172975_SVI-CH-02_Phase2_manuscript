

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

igg_long <- 
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
  
  inner_join(lb)

rm(ds)



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


# dv_long_mcmaster --------------------------------------------------------
dv_long_mcmaster <- lb %>%
  
  select(
    LBSEQ,
    USUBJID,   # unique subject id
    LBTEST,    # lab test or exam name
    LBSTAT,    # completion status
    LBSTRESN,  # numerical result/finding in standard units,
    VISIT
  ) %>%
  
  filter(
    LBTEST == "Stool Examination" & LBSTAT != "NOT DONE"
  ) %>%
  
  replace(
    is.na(.),
    0
  ) %>% 
  
  left_join(ex)

dv_long_mcmaster$VISIT <- gsub(
  "DAY ", 
  "", 
  dv_long_mcmaster$VISIT
)

dv_long_mcmaster$VISIT <- as.numeric(dv_long_mcmaster$VISIT)


# dv_long_eos -------------------------------------------------------------
dv_long_eos <- lb %>%
  
  select(
    LBSEQ,
    USUBJID,   # unique subject id
    LBTEST,    # lab test or exam name
    LBSTAT,    # completion status
    LBSTRESN,  # numerical result/finding in standard units (eosinophil count)
    VISIT
  ) %>%
  
  filter(
    LBTEST == "Eosinophils" & LBSTAT != "NOT DONE"
  ) %>%
  
  left_join(ex)

dv_long_eos$VISIT <- gsub(
  "DAY ", 
  "", 
  dv_long_eos$VISIT
)

dv_long_eos$VISIT <- as.numeric(dv_long_eos$VISIT)


# igg_long ---------------------------------------------------------------------
igg_long <- igg_long %>%
  
  filter(
    type == "igg"
  ) %>%
  
  dplyr::select(
    USUBJID, 
    visit, 
    igg
  ) %>%
  
  left_join(ex) %>%
  
  mutate(
    igg = as.numeric(igg)
  )


# rm ---------------------------------------------------------------------------
rm(lb)
rm(ex)
