

# load libraries ---------------------------------------------------------------
library(haven)
library(tidyverse)


# read -------------------------------------------------------------------------
AEPTCD <- read.csv("data/AEPTCD.csv", header = TRUE)


# total patients who received any vacc -----------------------------------------
total_vacc <- data.frame(
  EXDOSE = c("Group 1", "Group 2", "Group 3", "Group 4"),
  total_vacc = c(10, 9, 10, 10)
)

# total patients who received dose 1 -------------------------------------------
total_vacc_dose_1 <- data.frame(
  EXDOSE = c("Group 1", "Group 2", "Group 3", "Group 4"),
  total_vacc_dose_1 = c(10, 9, 10, 10)
)

# total patients who received dose 2 -------------------------------------------
total_vacc_dose_2 <- data.frame(
  EXDOSE = c("Group 1", "Group 2", "Group 3", "Group 4"),
  total_vacc_dose_2 = c(9, 8, 10, 10)
)

# total patients who received dose 3 -------------------------------------------
total_vacc_dose_3 <- data.frame(
  EXDOSE = c("Group 1", "Group 2", "Group 3", "Group 4"),
  total_vacc_dose_3 = c(9, 7, 10, 10)
)


# define vaccine ae ------------------------------------------------------------
relevant_ae <- c(
  10022086, # "Injection site pain", 
  10022102, # "Injection site tenderness",
  10022061, # "Injection site erythema", "Injection site redness",
  10053425, # "Injection site swelling",
  10037660, # "Fever", Pyrexia
  10019211, # "Headache",
  10028813, # "Nausea",
  10047700, # "Vomiting",
  10028411, # "Myalgia",
  10003239  # "Arthralgia"
)


# load ae.xpt ------------------------------------------------------------------
ae_vacc_raw <- 
  
  read_xpt(
    "data/ae.xpt",
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  ) %>%
  
  filter(
    EPOCH == "VACCINE"
  ) %>%
  
  dplyr::select(
    USUBJID,
    AESEQ,
    AESTDTC,  # start date/time ae
    AEENDTC,  # end date/time ae
    AESEV,    # severity
    AELLT,    # lowest-level term
    AEPTCD,   # preferred term
    AEREL,    # causality
    AEBODSYS, # body system or organ class
    AEOUT     # outcome of ae
  ) %>%
  
  mutate(
    AEREL = case_when(
      AEREL == "Definite" ~ "Related",
      AEREL == "Probable" ~ "Related",
      AEREL == "Possible" ~ "Related",
      AEREL == "Unlikely" ~ "Not Related",
      AEREL == "Not related" ~ "Not Related",
      TRUE ~ NA
    )
  ) 


# load -------------------------------------------------------------------------
ex <- 
  
  read_xpt(
    "data/ex.xpt",
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  ) %>%
  
  dplyr::select(
    USUBJID,
    EXSEQ,
    EXDOSE,
    EXSTDTC
  )


# join -------------------------------------------------------------------------
ae_vacc_raw <- ae_vacc_raw %>%
  
  full_join(ex) %>%
  
  mutate(
    EXDOSE = case_when(
      EXDOSE == 100 ~ "Group 1",
      EXDOSE == 500 ~ "Group 2",
      EXDOSE == 5 ~ "Group 3",
      EXDOSE == 0 ~ "Group 4"
    )
  ) %>%
  
  mutate(
    Days = as.Date(AESTDTC) - as.Date(EXSTDTC)
  ) %>%
  
  mutate(
    AEPTCD = ifelse(
      AELLT == "Injection site tenderness",
      10022102,
      AEPTCD
    )
  ) %>%
  
  left_join(AEPTCD)
  
  
# solicited --------------------------------------------------------------------
ae_vacc_solicited <- ae_vacc_raw %>%
  
  filter(
    AEPTCD %in% relevant_ae & Days >= 0 & Days <= 14
  ) %>%
  
  distinct(
    USUBJID,
    AESEQ,
    .keep_all = TRUE
  ) %>%
  
  mutate(
    code = paste(USUBJID, AESEQ)
  )


# unsolicited ------------------------------------------------------------------
Knee_pain <- ae_vacc_solicited %>% filter(AELLT == "Knee pain") %>% dplyr::select(-code)

Myalgia_of_lower_extremities <- ae_vacc_solicited %>% filter(AELLT == "Myalgia of lower extremities") %>% dplyr::select(-code)

ae_vacc_unsolicited <- anti_join(ae_vacc_raw, ae_vacc_solicited)

ae_vacc_unsolicited <- rbind(ae_vacc_unsolicited, Knee_pain)

ae_vacc_unsolicited <- rbind(ae_vacc_unsolicited, Myalgia_of_lower_extremities)

ae_vacc_unsolicited_dose_1 <- ae_vacc_unsolicited %>%
  
  filter(
    EXSEQ == 1 & Days >= 0 & Days <= 28
  ) %>%
  
  mutate(
    soc = paste(AEBODSYS, preferred_term, sep = "-")
  )

ae_vacc_unsolicited_dose_2 <- ae_vacc_unsolicited %>%
  
  filter(
    EXSEQ == 2 & Days >= 0 & Days <= 28
  ) %>%
  
  mutate(
    soc = paste(AEBODSYS, preferred_term, sep = "-")
  )

ae_vacc_unsolicited_dose_3 <- ae_vacc_unsolicited %>%
  
  filter(
    EXSEQ == 3 & Days >= 0 & Days <= 28
  ) %>%
  
  mutate(
    soc = paste(AEBODSYS, preferred_term, sep = "-")
  )


# format ae --------------------------------------------------------------------
dv_ae <- ae_vacc_solicited %>%
  
  group_by(
    USUBJID,
    EXDOSE
  ) %>%
  
  summarize(
    ae = n()
  ) %>%
  
  dplyr::select(
    -EXDOSE
  )

ex_3 <- ex %>% filter(EXSEQ == 3) %>% dplyr::select(USUBJID, EXDOSE)

dv_ae <- dv_ae %>% right_join(ex_3) %>%
  
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

dv_ae$ae[is.na(dv_ae$ae)] <- 0


