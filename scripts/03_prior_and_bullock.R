# Bullock et al. 2015
# Prior et al. 2015

# Set dir
setwd(dropboxdir)
setwd("partisan_gap/pgap/rep_files/")

# Load libs
library(tidyverse)
library(haven)
library(broom)
library(infer)

# Function
new_lm <- function(item_name, survey, study, year, data, item, rd, wt) {
        prefix         <- c(item_name, survey, study, year)
        lm_weight      <- rep(1, nrow(data))
        if(wt %in% names(data)) {
            lm_weight      <- unlist(data[, wt])
        }
        coef_mod       <- coef(summary(lm(data[[item]] ~ data[[rd]], weights = lm_weight)))
        base_layer     <- c(coef_mod[1, 1], coef_mod[1, 1] + coef_mod[2, 1], unname(coef_mod[2, 1:4]))
        prop_correct   <- coef(summary(lm(data[[item]] ~ 1, weights = lm_weight)))[1]
        nona_df        <- data[!is.na(data[[item]]) & !is.na(data[[rd]]), rd]
        n_rd           <- c(sum(nona_df[[rd]] == 1), sum(nona_df[[rd]] == 0))

        # no lean
        rd_nolean        <- paste0(rd, "_nolean")
        coef_nolean_mod  <- coef(summary(lm(data[[item]] ~ data[[rd_nolean]], weights = lm_weight)))        
        nolean_layer     <- c(coef_nolean_mod[1, 1], coef_nolean_mod[1, 1] + coef_nolean_mod[2, 1], unname(coef_nolean_mod[2, 1:4]))

        c(prefix, base_layer, prop_correct, n_rd, nolean_layer)
}

# Open results data frame
res_df        <- data.frame(matrix(ncol = 19, nrow = 0))
item_meta     <-  c("item_name", "survey", "study", "year")
base_layer    <- c("prop_r_correct", "prop_d_correct", "dem_minus_rep", "std.error", "statistic", "p.value")
nolean        <- paste0(base_layer, "_nolean")
names(res_df) <- c(item_meta, base_layer, "prop_correct_all", "n_dem", "n_rep", nolean)

#**************************
#***Bullock ET AL (2015)***
#**************************

#***2008 CCES STUDY***

# cces
cces <- haven::read_dta("data/bullock_et_al_2015/CCES2008/CCES2008PublicReplicationDataset.dta")
cces <- cces %>% filter(treatment != 1)

# cces %>% group_by(questionid) %>% summarize(n_dem = sum(dem, na.rm = T), n_rep = sum(dem == 0, na.rm = T))

#*recoding PID with leaner variable - 1 = Dem, 0 = Rep

cces  <- cces %>% mutate(dem = case_when(
      withlean_pid2 == 1 ~ 1,
      withlean_pid2 == 0 ~ 0
))

#*creating PID variable to separate out leaners

cces  <- cces %>% mutate(dem_nolean = case_when(
      pid2 == 1 ~ 1,
      pid2 == 0 ~ 0
))

#*Iraq 07 to 08 casualties

cces <- cces %>%
    mutate(iraq_cas_corr = case_when(
      directionalresponse == 0 & questionid == 1 ~ 1,
      (directionalresponse==.5 | directionalresponse==1) & questionid == 1 ~ 0
))

res_df[1, ] <- new_lm(item_name = "iraq_change_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 1, ], item = "iraq_cas_corr", rd = "dem", wt = "noweight")

#*Bush inflation change

cces <- cces %>%
    mutate(bush_inf_corr = case_when(
      directionalresponse == 1 & questionid == 2 ~ 1,
     (directionalresponse == 0 | directionalresponse == .5) & questionid == 2 ~ 0
))

res_df[2, ] <- new_lm(item_name = "gwb_infl_change_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 2, ], item = "bush_inf_corr", rd = "dem", wt = "noweight")

#*Bush unemployment change

cces <- cces %>%
    mutate(bush_emp_corr = case_when(
      directionalresponse == 1 & questionid == 3 ~ 1,
     (directionalresponse == 0 | directionalresponse == .5) & questionid == 3 ~ 0
))

res_df[3, ] <- new_lm(item_name = "gwb_unemp_change_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 3, ], item = "bush_emp_corr", rd = "dem", wt = "noweight")

#*Est. Bush approval

cces <- cces %>%
    mutate(bush_app = case_when(
      directionalresponse == 1 & questionid == 4 ~ 1,
     (directionalresponse == 0 | directionalresponse == .25 | directionalresponse == .5 | directionalresponse == .75) & questionid == 4 ~ 0
))

res_df[4, ] <- new_lm(item_name = "gwb_approval_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 4, ], item = "bush_app", rd = "dem", wt = "noweight")

#*Iraq total casualties

cces <- cces %>%
    mutate(iraq_total_corr = case_when(
      directionalresponse == 0 & questionid == 5 ~ 1,
     (directionalresponse == .25 | directionalresponse == .5 | directionalresponse == .75 | directionalresponse == 1) & questionid == 5 ~ 0
))

res_df[5, ] <- new_lm(item_name = "iraq_total_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 5, ], item = "iraq_total_corr", rd = "dem", wt = "noweight")

#*Est. Bush approval among Reps.

cces <- cces %>%
    mutate(bush_app_rep_corr = case_when(
      directionalresponse == .5 & questionid == 6 ~ 1,
     (directionalresponse == 0 | directionalresponse == .25 | directionalresponse == .75 | directionalresponse == 1) & questionid == 6 ~ 0
))

res_df[6, ] <- new_lm(item_name = "gwb_app_reps_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 6, ], item = "bush_app_rep_corr", rd = "dem", wt = "noweight")

#*Obama age

cces <- cces %>%
    mutate(obama_age_corr = case_when(
      response == 3 & questionid == 7 ~ 1,
     (response == 1 | response == 2 | response == 4) & questionid == 7 ~ 0
))

res_df[7, ] <- new_lm(item_name = "obama_age_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 7, ], item = "obama_age_corr", rd = "dem", wt = "noweight")

#*McCain age

cces <- cces %>%
    mutate(mccain_age_corr = case_when(
      response == 3 & questionid == 8 ~ 1,
     (response == 1 | response == 2 | response == 4) & questionid == 8 ~ 0
))

res_df[8, ] <- new_lm(item_name = "mccain_age_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 8, ], item = "mccain_age_corr", rd = "dem", wt = "noweight")

#*Afghanistan 07 t0 08 casualties

cces <- cces %>%
    mutate(af_cas_corr = case_when(
      directionalresponse == .5 & questionid == 9 ~ 1,
     (directionalresponse == 0 | directionalresponse == 1) & questionid == 9 ~ 0
))

res_df[9, ] <- new_lm(item_name = "afgh_change_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 9, ], item = "af_cas_corr", rd = "dem", wt = "noweight")

#*Bush deficit change

cces <- cces %>%
    mutate(bush_def_corr = case_when(
      directionalresponse == 1 & questionid == 10 ~ 1,
     (directionalresponse == 0 | directionalresponse == .5) & questionid == 10 ~ 0
))

res_df[10, ] <- new_lm(item_name = "gwb_def_change_b", survey = "CCES", study = "Bullock et al.", year = 2008, data = cces[cces$questionid == 10, ], item = "bush_def_corr", rd = "dem", wt = "noweight")

#*Placebo - gold price 1980

#*Placebo - Bangladeshi independence date

#***2012 MTURK STUDY***

turk <- haven::read_dta("data/bullock_et_al_2015/MTurk12/MTURK2012PublicReplicationDataset.dta")
turk <- turk %>% filter(treatmentnum == 1)

turk  <- turk %>% mutate(dem = case_when(
     withlean_partisanship == 1 ~ 1,
     withlean_partisanship == 0 ~ 0
))

turk  <- turk %>% mutate(dem_nolean = case_when(
     pid7 == 1 | pid7 == 2 ~ 1,
     pid7 == 6 | pid7 == 7 ~ 0
))

#*creating pure Ind variable
#gen pure_ind=0
#replace pure_ind=1 if pid7==4
#tab pure_ind

#*Defense Spending
#gen defense_corr=isresponsecorrect if questionid==109
#prtest defense_corr, by(democrat)
#prtest defense_corr, by(dem_nolean)
#sum defense_corr if pure_ind==1
#sum defense_corr

turk <- turk %>%
    mutate(defense_corr = case_when(
      isresponsecorrect == 1 & questionid == 109 ~ 1,
      isresponsecorrect == 0 & questionid == 109 ~ 0
))

res_df[11, ] <- new_lm(item_name = "def_spend_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 109, ], item = "defense_corr", rd = "dem", wt = "noweight")

#*Iraq deaths

turk <- turk %>%
    mutate(iraq_corr = case_when(
      isresponsecorrect == 1 & questionid == 110 ~ 1,
      isresponsecorrect == 0 & questionid == 110 ~ 0
))

res_df[12, ] <- new_lm(item_name = "iraq_death_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 110, ], item = "iraq_corr", rd = "dem", wt = "noweight")

#*Iraq deaths % black

turk <- turk %>%
    mutate(iraq_black_corr = case_when(
      isresponsecorrect == 1 & questionid == 111 ~ 1,
      isresponsecorrect == 0 & questionid == 111 ~ 0
))

res_df[13, ] <- new_lm(item_name = "iraq_deaths_black_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 111, ], item = "iraq_black_corr", rd = "dem", wt = "noweight")

#*Bush II unemployment

turk <- turk %>%
    mutate(bush_emp_corr = case_when(
      isresponsecorrect == 1 & questionid == 112 ~ 1,
      isresponsecorrect == 0 & questionid == 112 ~ 0
))

res_df[14, ] <- new_lm(item_name = "gwb_emp_change_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 112, ], item = "bush_emp_corr", rd = "dem", wt = "noweight")

#*Obama vote

turk <- turk %>%
    mutate(obama_vote_corr = case_when(
      isresponsecorrect == 1 & questionid == 113 ~ 1,
      isresponsecorrect == 0 & questionid == 113 ~ 0
))

res_df[15, ] <- new_lm(item_name = "obama_vote_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 113, ], item = "obama_vote_corr", rd = "dem", wt = "noweight")

#*Global warming amount

turk <- turk %>%
    mutate(gw_corr = case_when(
      isresponsecorrect == 1 & questionid == 114 ~ 1,
      isresponsecorrect == 0 & questionid == 114 ~ 0
))

res_df[16, ] <- new_lm(item_name = "amount_warm_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 114, ], item = "gw_corr", rd = "dem", wt = "noweight")

#*Mantle home runs 1961
# Placebo

#*Medicaid spending

turk <- turk %>%
    mutate(med_spend_corr = case_when(
      isresponsecorrect == 1 & questionid == 116 ~ 1,
      isresponsecorrect == 0 & questionid == 116 ~ 0
))

res_df[17, ] <- new_lm(item_name = "medicaid_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 116, ], item = "med_spend_corr", rd = "dem", wt = "noweight")

#*Debt service spending

turk <- turk %>%
    mutate(debt_spend_corr = case_when(
      isresponsecorrect == 1 & questionid == 117 ~ 1,
      isresponsecorrect == 0 & questionid == 117 ~ 0
))

res_df[18, ] <- new_lm(item_name = "debt_service_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 117, ], item = "debt_spend_corr", rd = "dem", wt = "noweight")

#*Obama unemployment

turk <- turk %>%
    mutate(obama_emp_corr = case_when(
      isresponsecorrect == 1 & questionid == 118 ~ 1,
      isresponsecorrect == 0 & questionid == 118 ~ 0
))

res_df[19, ] <- new_lm(item_name = "obama_unemp_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 118, ], item = "obama_emp_corr", rd = "dem", wt = "noweight")

#*TARP % paid back

turk <- turk %>%
    mutate(tarp_corr = case_when(
      isresponsecorrect == 1 & questionid == 119 ~ 1,
      isresponsecorrect == 0 & questionid == 119 ~ 0
))

res_df[20, ] <- new_lm(item_name = "tarp_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 119, ], item = "tarp_corr", rd = "dem", wt = "noweight")

#*Foreign born % 

turk <- turk %>%
    mutate(foreign_corr = case_when(
      isresponsecorrect == 1 & questionid == 120 ~ 1,
      isresponsecorrect == 0 & questionid == 120 ~ 0
))

res_df[21, ] <-  new_lm(item_name = "foreign_b", survey = "MTurk", study = "Bullock et al.", year = 2012, data = turk[turk$questionid == 120, ], item = "foreign_corr", rd = "dem", wt = "noweight")

#************************
#***PRIOR ET AL (2015)***
#************************

#psk_wide_recode_pid_by_CR.dta
#*why use this? because the recode replication file doesn't have a measure of 7 point PID
#*so I went back to the source file (prior to recode) and made sure it was included
#*this new file is the same as the previous recode, but just keeps a pid7 t000
#keep if rd==0

psk <- haven::read_dta("data/prior_et_al_2015/psk_wide_recode_pid_by_CR.dta")
psk <- psk %>% filter(rd == 0)

#*"correct" coding based on trichotomized variable in PSK_Recodes.do; //
#*"under," "over," "missing" coded as "incorrect"

#*generating PID variable without leaners

psk  <- psk %>% mutate(dem_nolean = case_when(
     pid_7_standard == 1 | pid_7_standard == 2 ~ 1,
     pid_7_standard == 6 | pid_7_standard == 7 ~ 0
))

#*Public Debt (2008)
psk <- psk %>%
    mutate(l1_corr = case_when(
      l1 == 5 ~ 1,
      l1 == -1 | l1 == 1 | l1 == 2 | l1 == 3 | l1 == 4 | l1 == 6 | l1 == 7 ~ 0
))

res_df[22, ] <- new_lm(item_name = "debt_2008_p", survey = "KN", study = "Prior et al.", year = 2008, data = psk[psk$pollid == 2008, ], item = "l1_corr", rd = "dem", wt = "noweight")

#*Unemployment (2008)

psk <- psk %>%
    mutate(l2_corr = case_when(
      l2 >= 4.2 & l2 <= 5.4 ~ 1,
      (l2 >= -1 & l2 < 4.2) | (l2 > 5.4 & l2 <= 100) ~ 0
))

res_df[23, ] <- new_lm(item_name = "unemp_2008_p", survey = "KN", study = "Prior et al.", year = 2008, data = psk[psk$pollid == 2008, ], item = "l2_corr", rd = "dem", wt = "noweight")

#*Uninsured (2008)

psk <- psk %>%
    mutate(l3_corr = case_when(
      l3 >= 12.6 & l3 <= 19 ~ 1,
      (l3 >= -1 & l3 < 12.6)  | (l3 > 19 & l3 <= 91) ~ 0
))

res_df[24, ] <- new_lm(item_name = "uninsur_2008_p", survey = "KN", study = "Prior et al.", year = 2008, data = psk[psk$pollid == 2008, ], item = "l3_corr", rd = "dem", wt = "noweight")

#*Estate Tax (2008)

psk <- psk %>%
    mutate(l4_corr = case_when(
      l4 == 1 ~ 1,
      l4 != 1 ~ 0
))

res_df[25, ] <- new_lm(item_name = "estate_2008_p", survey = "KN", study = "Prior et al.", year = 2008, data = psk[psk$pollid == 2008, ], item = "l4_corr", rd = "dem", wt = "noweight")

#*Gas Price (2008)

psk$l5r <- ifelse(psk$l5 >= 100, psk$l5/100, psk$l5)
psk <- psk %>%
    mutate(l5_corr = case_when(
      l5r >= 3.22 & l5r <= 3.32 ~ 1,
      l5r < 3.22  | l5r >  3.32 ~ 0
))

res_df[26, ] <- new_lm(item_name = "gas_2008_p", survey = "KN", study = "Prior et al.", year = 2008, data = psk[psk$pollid == 2008, ], item = "l5_corr", rd = "dem", wt = "noweight")

#*Unemployment (2004)

psk <- psk %>%
    mutate(l6_corr = case_when(
      l6 == 2 ~ 1,
      (l6 >= -2 & l6 == 1) | (l6 >= 3 & l6 <= 5) ~ 0
))

res_df[27, ] <- new_lm(item_name = "unemp_2004_p", survey = "KN", study = "Prior et al.", year = 2004, data = psk[psk$pollid == 2004, ], item = "l6_corr", rd = "dem", wt = "noweight")

#*Estate Tax (2004)

psk <- psk %>%
    mutate(l7_corr = case_when(
      l7 == 1 ~ 1,
      (l7 >= -2 & l7 <= -1) | (l7 >= 2 & l7 <= 5) ~ 0
))

res_df[28, ] <- new_lm(item_name = "estate_2004_p", survey = "KN", study = "Prior et al.", year = 2004, data = psk[psk$pollid == 2004, ], item = "l7_corr", rd = "dem", wt = "noweight")

#*Debt (2004)

psk <- psk %>%
    mutate(l8_corr = case_when(
      l8 == 5 ~ 1,
      (l8 >= -1 & l8 <= 4) | (l8 >= 6 & l8 <= 7) ~ 0
))

res_df[29, ] <- new_lm(item_name = "debt_2004_p", survey = "KN", study = "Prior et al.", year = 2004, data = psk[psk$pollid == 2004, ], item = "l8_corr", rd = "dem", wt = "noweight")

#*Uninsured (2004)

psk <- psk %>%
    mutate(l9_corr = case_when(
      l9 >= 2 & l9 <= 29.2 ~ 1,
      (l9 < 3.22 & l9 >= -1) | (l9 > 29.2 & l9 <= 770) ~ 0
))

res_df[30, ] <- new_lm(item_name = "uninsur_2004_p", survey = "KN", study = "Prior et al.", year = 2004, data = psk[psk$pollid == 2004, ], item = "l9_corr", rd = "dem", wt = "noweight")

#*Poverty (2004)

psk <- psk %>%
    mutate(l10_corr = case_when(
      l10 >= 4.5  & l10 <= 20.5 ~ 1,
      (l10 > -2  & l10 < 4.5) | (l10 > 20.5 & l10 < 86) ~ 0
))

res_df[31, ] <-  new_lm(item_name = "pov_2004_p", survey = "KN", study = "Prior et al.", year = 2004, data = psk[psk$pollid == 2004, ], item = "l10_corr", rd = "dem", wt = "noweight")

# Write out results
write_csv(res_df, file = "tabs/bullock_prior_results.csv")
