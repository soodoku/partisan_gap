# ANES

# Set dir
setwd(githubdir)
setwd("partisan_gap/")

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


# ANES Cum.
anes <- haven::read_dta("data/anes/anes_cumulative_through_2012.dta")

# weights
#*full sample weight: VCF0009z
anes <- rename(anes, weight = VCF0009z)

# gen dem=1 if VCF0303==1 
# replace dem=0 if VCF0303==3
anes  <- anes %>% mutate(dem = case_when(
      VCF0303 == 1 ~ 1,
      VCF0303 == 3 ~ 0
))

anes  <- anes %>% mutate(dem_nolean = case_when(
      VCF0301 == 1 | VCF0301 == 2 ~ 1,
      VCF0301 == 6 | VCF0301 == 7 ~ 0
))

# recode VCF0870 (0 = .), gen(economy)
# label define economyl 1 "Better" 3 "Stayed the same" 5 "Worse" 8 "DK"
# label values economy economyl
anes <- rename(anes, economy = VCF0870)

#anes$economy <- car::recode(as.character(anes$VCF0870), "1 = 'better'; 3 = 'Stayed the same'; 5 = 'Worse'; 8 = 'DK'; 0 = NA")
# table(anes$economy, anes$VCF0870, useNA = "always")
# rename VCF0004 year
anes <- rename(anes, year = VCF0004)

# //CODING SCHEMA 1 
# *"correct" answers based off of difference between
# //real GDP growth from Q3 of previous year to Q3 of election year
# *negative numbers = got worse; positive numbers = got better
# *anything less than a difference of abs($500) coded as 'stayed about the same'
# *the direction of the >abs($500) also marked as a correct answer
# *example: from 1979-1980, diff is -335 - "stayed about the same" and "gotten worse"
# //are both correct answers

# *1980 - stayed about the same OR got worse
# *1982 - worse
# *1984 - better
# *1986 - better
# *1988 - better
# *1990 - stayed about the same OR got worse
# *1992 - better
# *1994 - better
# *1996 - better
# *1998 - better
# *2000 - better
# *2002 - stayed about the same OR got better
# *2004 - better
# *2008 - worse
# *2012 - stayed about the same OR got better
# *2016 - better

anes <- anes %>%
    mutate(ec_1980_corr = case_when(
      year == 1980 & (economy == 3 | economy == 5) ~ 1,
      year == 1980 & (economy == 1 | economy == 8) ~ 0
))

res_df[1, ] <- new_lm(item_name = "ec_1980_corr", survey = "ANES", study = "ANES", year = 1980, data = anes, item = "ec_1980_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_1982_corr = case_when(
      year == 1982 & (economy == 5) ~ 1,
      year == 1982 & (economy == 1 | economy == 3 | economy == 8) ~ 0
))

res_df[2, ] <- new_lm(item_name = "ec_1982_corr", survey = "ANES", study = "ANES", year = 1982, data = anes, item = "ec_1982_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_1984_corr = case_when(
      year == 1984 & (economy == 1) ~ 1,
      year == 1984 & (economy == 3 | economy == 5 | economy == 8) ~ 0
))

res_df[3, ] <- new_lm(item_name = "ec_1984_corr", survey = "ANES", study = "ANES", year = 1984, data = anes, item = "ec_1984_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_1986_corr = case_when(
      year == 1986 & (economy == 1) ~ 1,
      year == 1986 & (economy == 3 | economy == 5 | economy == 8) ~ 0
))

res_df[4, ] <- new_lm(item_name = "ec_1986_corr", survey = "ANES", study = "ANES", year = 1986, data = anes, item = "ec_1986_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_1988_corr = case_when(
      year == 1988 & (economy == 1) ~ 1,
      year == 1988 & (economy == 3 | economy == 5 | economy == 8) ~ 0
))

res_df[5, ] <- new_lm(item_name = "ec_1988_corr", survey = "ANES", study = "ANES", year = 1988, data = anes, item = "ec_1988_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_1990_corr = case_when(
      year == 1990 & (economy == 3 | economy == 5) ~ 1,
      year == 1990 & (economy == 1 | economy == 8) ~ 0
))

res_df[6, ] <- new_lm(item_name = "ec_1990_corr", survey = "ANES", study = "ANES", year = 1990, data = anes, item = "ec_1990_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_1992_corr = case_when(
      year == 1992 & (economy == 1) ~ 1,
      year == 1992 & (economy == 3 | economy == 5 | economy == 8) ~ 0
))

res_df[7, ] <- new_lm(item_name = "ec_1992_corr", survey = "ANES", study = "ANES", year = 1992, data = anes, item = "ec_1992_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_1994_corr = case_when(
      year == 1994 & (economy == 1) ~ 1,
      year == 1994 & (economy == 3 | economy == 5 | economy == 8) ~ 0
))

res_df[8, ] <- new_lm(item_name = "ec_1994_corr", survey = "ANES", study = "ANES", year = 1994, data = anes, item = "ec_1994_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_1996_corr = case_when(
      year == 1996 & (economy == 1) ~ 1,
      year == 1996 & (economy == 3 | economy == 5 | economy == 8) ~ 0
))

res_df[9, ] <- new_lm(item_name = "ec_1996_corr", survey = "ANES", study = "ANES", year = 1996, data = anes, item = "ec_1996_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_1998_corr = case_when(
      year == 1998 & (economy == 1) ~ 1,
      year == 1998 & (economy == 3 | economy == 5 | economy == 8) ~  0
))

res_df[10, ] <- new_lm(item_name = "ec_1998_corr", survey = "ANES", study = "ANES", year = 1998, data = anes, item = "ec_1998_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_2000_corr = case_when(
      year == 2000 & (economy == 1) ~ 1,
      year == 2000 & (economy == 3 | economy == 5 | economy == 8) ~  0
))

res_df[11, ] <- new_lm(item_name = "ec_2000_corr", survey = "ANES", study = "ANES", year = 2000, data = anes, item = "ec_2000_corr", rd = "dem", wt = "weight")

anes <- anes %>%
    mutate(ec_2002_corr = case_when(
      year == 2002 & (economy == 1 | economy == 3) ~ 1,
      year == 2002 & (economy == 5 | economy == 8) ~ 0
))

res_df[12, ] <- new_lm(item_name = "ec_2002_corr", survey = "ANES", study = "ANES", year = 2002, data = anes, item = "ec_2002_corr", rd = "dem", wt = "weight") 

anes <- anes %>%
    mutate(ec_2004_corr = case_when(
      year == 2004 & (economy == 1) ~ 1,
      year == 2004 & (economy == 3 | economy == 5 | economy == 8) ~ 0
))

res_df[13, ] <- new_lm(item_name = "ec_2004_corr", survey = "ANES", study = "ANES", year = 2004, data = anes, item = "ec_2004_corr", rd = "dem", wt = "weight") 

anes <- anes %>%
    mutate(ec_2008_corr = case_when(
      year == 2008 & (economy == 5) ~ 1,
      year == 2008 & (economy == 1 | economy == 3 | economy == 8) ~ 0
))

res_df[14, ] <- new_lm(item_name = "ec_2008_corr", survey = "ANES", study = "ANES", year = 2008, data = anes, item = "ec_2008_corr", rd = "dem", wt = "weight") 

anes <- anes %>%
    mutate(ec_2012_corr = case_when(
      year == 2012 & (economy == 1 | economy == 3) ~ 1,
      year == 2012 & (economy == 5 | economy == 8) ~ 0
))

res_df[15, ] <- new_lm(item_name = "ec_2012_corr", survey = "ANES", study = "ANES", year = 2012, data = anes, item = "ec_2012_corr", rd = "dem", wt = "weight")  

#***ANES 1982***
#***************
#*note: there is another question re: party control of Senate post-election (820526), but:
#*note from codebook: 
# THE PRINCIPAL INVESTIGATORS RECOMMEND THAT DATA FROM THIS 
# VARIABLE NOT BE USED IN ANALYSIS.

#*note: there are no weights associated with this study


#***************
#***ANES 1984***
#***************
#*note: there are no weights associated with this study

anes_1984 <- haven::read_dta("data/anes/anes_1984/NES1984.dta")

anes_1984 <- anes_1984 %>%
    mutate(dem = case_when(
      V840318 == 0 | V840318 == 1 | V840318 == 2 ~ 1,
      V840318 == 4 | V840318 == 5 | V840318 == 6 ~ 0
))

anes_1984 <- anes_1984 %>%
    mutate(dem_nolean = case_when(
      V840318 == 0 | V840318 == 1 ~ 1,
      V840318 == 5 | V840318 == 6 ~ 0
))


#*ec. condition of women better/worse than last year - 
#//based on unemployment data: better (see supp_factual_data_for_anes.xls)

#  table(anes_1984$V840199, anes_1984$women_cond_corr, useNA = "always")
anes_1984 <- anes_1984 %>%
    mutate(women_cond_corr = case_when(
      V840199 == 3 | V840199 == 5 | V840199 == 8 ~ 0,
      V840199 == 1 ~ 1
))

res_df[16, ] <- new_lm(item_name = "women_cond_corr", survey = "ANES", study = "ANES", year = 1984, data = anes_1984, item = "women_cond_corr", rd = "dem", wt = "weight") 

#*ec. condition of blacks better/worse than last year - 
#//based on unemployment data: better

anes_1984 <- anes_1984 %>%
    mutate(blacks_cond_corr = case_when(
      V840200 == 3 | V840200 == 5 | V840200 == 8 ~ 0,
      V840200 == 1 ~ 1
))

res_df[17, ] <- new_lm(item_name = "blacks_cond_corr", survey = "ANES", study = "ANES", year = 1984, data = anes_1984, item = "blacks_cond_corr", rd = "dem", wt = "weight")

#***************
#***ANES 1986***
#***************
#*note: there are no weights associated with this study

anes_1986 <- haven::read_dta("data/anes/anes_1986/nes1986.dta")

anes_1986 <- anes_1986 %>%
    mutate(dem = case_when(
      V860300 == 0 | V860300 == 1 | V860300 == 2 ~ 1,
      V860300 == 5 | V860300 == 6 | V860300 == 7 ~ 0
))

anes_1986 <- anes_1986 %>%
    mutate(dem_nolean = case_when(
      V860300 == 1 | V860300 == 2 ~ 1,
      V860300 == 5 | V860300 == 6 ~ 0
))

anes_1986 <- anes_1986 %>%
    mutate(pure_ind = case_when(
      V860300 == 3 ~ 1
))

#*party control of House pre-election = Democratic

anes_1986 <- anes_1986 %>%
    mutate(house_pre_corr = case_when(
      V860349 == 5 ~ 1,
      V860349 == 1| V860349 == 8 ~ 0
))

res_df[18, ] <- new_lm(item_name = "house_pre_corr", survey = "ANES", study = "ANES", year = 1986, data = anes_1986, item = "house_pre_corr", rd = "dem", wt = "weight")

#*party control of Senate pre-election = Republican

anes_1986 <- anes_1986 %>%
    mutate(senate_pre_corr = case_when(
      V860350 == 1 ~ 1,
      V860350 == 5| V860350 == 8 ~ 0
))

res_df[19, ] <- new_lm(item_name = "senate_pre_corr", survey = "ANES", study = "ANES", year = 1986, data = anes_1986, item = "senate_pre_corr", rd = "dem", wt = "weight")

#*national unemployment rate over the past year = stayed the same

anes_1986 <- anes_1986 %>%
    mutate(unemploy_corr = case_when(
      V860377 == 3 ~ 1,
      V860377 == 1 | V860377 == 5 | V860377 == 8 ~ 0
))

res_df[20, ] <- new_lm(item_name = "unemp_1986_anes", survey = "ANES", study = "ANES", year = 1986, data = anes_1986, item = "unemploy_corr", rd = "dem", wt = "weight")

#*national inflation rate over the past year = stayed the same
#*note: V860370 asks about state's economy 

anes_1986 <- anes_1986 %>%
    mutate(inflation_corr = case_when(
      V860379 == 3 ~ 1,
      V860379 == 1 | V860379 == 5 | V860379 == 8 ~ 0
))

res_df[21, ] <- new_lm(item_name = "infl_1986_anes", survey = "ANES", study = "ANES", year = 1986, data = anes_1986, item = "inflation_corr", rd = "dem", wt = "weight")


#***************
#***ANES 1988***
#***************
#*note: there are no weights associated with this study

anes_1988 <- haven::read_dta("data/anes/anes_1988/NES1988.dta")

anes_1988 <- anes_1988 %>%
    mutate(dem = case_when(
      V880274 == 0 | V880274 == 1 | V880274 == 2 ~ 1,
      V880274 == 4 | V880274 == 5 | V880274 == 6 ~ 0
))

anes_1988 <- anes_1988 %>%
    mutate(dem_nolean = case_when(
      V880274 == 0 | V880274 == 1 ~ 1,
      V880274 == 5 | V880274 == 6 ~ 0
))

#*Unemployment - better

anes_1988 <- anes_1988 %>%
    mutate(emp_corr = case_when(
      V880239 == 1 ~ 1,
      V880239 == 3 | V880239 == 5 | V880239 == 8 ~ 0
))

res_df[22, ] <- new_lm(item_name = "unemp_1988_anes", survey = "ANES", study = "ANES", year = 1988, data = anes_1988, item = "emp_corr", rd = "dem", wt = "weight")

#*Inflation - stayed about the same

anes_1988 <- anes_1988 %>%
    mutate(infl_corr = case_when(
      V880241 == 3 ~ 1,
      V880241 == 1 | V880241 == 5 | V880241 == 8 ~ 0
))

res_df[23, ] <-  new_lm(item_name = "infl_1988_anes", survey = "ANES", study = "ANES", year = 1988, data = anes_1988, item = "infl_corr", rd = "dem", wt = "weight")

#*Federal Budget Deficit since 1980 - larger

anes_1988 <- anes_1988 %>%
    mutate(deficit_corr = case_when(
      V881036 == 5 ~ 1,
      V881036 == 1 | V881036 == 3 | V881036 == 8 ~ 0
))

res_df[24, ] <- new_lm(item_name = "deficit_since80_1988_anes", survey = "ANES", study = "ANES", year = 1988, data = anes_1988, item = "deficit_corr", rd = "dem", wt = "weight")

#*Fed. efforts on environment since 1980 - decreased

anes_1988 <- anes_1988 %>%
    mutate(envir_corr = case_when(
      V880899 == 5 ~ 1,
      V880899 == 0 | V880899 == 1 | V880899 == 3 | V880899 == 8 ~ 0
))

res_df[25, ] <- new_lm(item_name = "envir_corr", survey = "ANES", study = "ANES", year = 1988, data = anes_1988, item = "envir_corr", rd = "dem", wt = "weight")

#*Social Security benefits since 1980 - increased

anes_1988 <- anes_1988 %>%
    mutate(ss_corr = case_when(
      V880904 == 1 ~ 1,
      V880904 == 0 | V880904 == 3 | V880904 == 5 | V880904 == 8 ~ 0
))

res_df[26, ] <- new_lm(item_name = "ss_since80_1988_anes", survey = "ANES", study = "ANES", year = 1988, data = anes_1988, item = "ss_corr", rd = "dem", wt = "weight")

#*Defense spending since 1980 - increase

anes_1988 <- anes_1988 %>%
    mutate(def_corr = case_when(
      V880909 == 1 ~ 1,
      V880909 == 0 | V880909 == 3 | V880909 == 5 | V880909 == 8 ~ 0
))

res_df[27, ] <- new_lm(item_name = "def_corr", survey = "ANES", study = "ANES", year = 1988, data = anes_1988, item = "def_corr", rd = "dem", wt = "weight")

#*Federal assistance to poor since 1980 - decreased

anes_1988 <- anes_1988 %>%
    mutate(poor_corr = case_when(
      V880914 == 5 ~ 1,
      V880914 == 0 | V880914 == 1 | V880914 == 3 | V880914 == 8 ~ 0
))

res_df[28, ] <- new_lm(item_name = "poor_corr", survey = "ANES", study = "ANES", year = 1988, data = anes_1988, item = "poor_corr", rd = "dem", wt = "weight")

#*Spending on public schools since 1980 - increased
#*note: V880250 asks about state's economy 

anes_1988 <- anes_1988 %>%
    mutate(schools_corr = case_when(
      V880919 == 1 ~ 1,
      V880919 == 0 | V880919 == 3 | V880919 == 5 | V880919 == 8 ~ 0
))

res_df[29, ] <- new_lm(item_name = "school_since80_1988_anes", survey = "ANES", study = "ANES", year = 1988, data = anes_1988, item = "schools_corr", rd = "dem", wt = "weight")

#***************
#***ANES 1990***
#***************

#*no additional retrospection items

#***************
#***ANES 1992***
#***************

anes_1992 <- haven::read_dta("data/anes/anes_1992/NES1992.dta")
anes_1992 <- rename(anes_1992, weight = V923008)

anes_1992 <- anes_1992 %>%
    mutate(dem = case_when(
      V923634 == 0 | V923634 == 1 | V923634 == 2 ~ 1,
      V923634 == 4 | V923634 == 5 | V923634 == 6 ~ 0
))

anes_1992 <- anes_1992 %>%
    mutate(dem_nolean = case_when(
      V923634 == 0 | V923634 == 1 ~ 1,
      V923634 == 5 | V923634 == 6 ~ 0
))

#*Unemployment - worse

anes_1992 <- anes_1992 %>%
    mutate(unemploy_corr = case_when(
      V923527 == 5 ~ 1,
      V923527 == 1 | V923527 == 3 | V923527 == 8 ~ 0
))

res_df[30, ] <- new_lm(item_name = "unemp_1992_anes", survey = "ANES", study = "ANES", year = 1992, data = anes_1992, item = "unemploy_corr", rd = "dem", wt = "weight")

#*Inflation - better

anes_1992 <- anes_1992 %>%
    mutate(infl_corr = case_when(
      V923529 == 1 ~ 1,
      V923529 == 3 | V923529 == 5 | V923529 == 8 ~ 0
))

res_df[31, ] <- new_lm(item_name = "infl_1992_anes", survey = "ANES", study = "ANES", year = 1992, data = anes_1992, item = "infl_corr", rd = "dem", wt = "weight")

#*Economy since the 4th of July - better	OR stayed about the same
anes_1992 <- anes_1992 %>%
    mutate(econ_4th_corr = case_when(
      V923533 == 1 | V923533 == 3 ~ 1,
      V923533 == 5 | V923533 == 8 ~ 0
))

res_df[32, ] <- new_lm(item_name = "econ_4th_corr", survey = "ANES", study = "ANES", year = 1992, data = anes_1992, item = "econ_4th_corr", rd = "dem", wt = "weight")

#*Economy compared to 4 years ago - worse
#*note: V923543 asks about state's economy

anes_1992 <- anes_1992 %>%
    mutate(econ_4yrs_corr = case_when(
      V923535 == 5 ~ 1,
      V923535 == 1 | V923535 == 3 | V923535 == 8 ~ 0
))

res_df[33, ] <- new_lm(item_name = "econ_4yrs_corr", survey = "ANES", study = "ANES", year = 1992, data = anes_1992, item = "econ_4yrs_corr", rd = "dem", wt = "weight")

#***************
#***ANES 1994***
#***************

#*no additional retrospection items

#***************
#***ANES 1996***
#***************

anes_1996 <- haven::read_dta("data/anes/anes_1996/nes96.dta")
anes_1996 <- rename(anes_1996, weight = V960003)

anes_1996 <- anes_1996 %>%
    mutate(dem = case_when(
      V960420 == 0 | V960420 == 1 | V960420 == 2 ~ 1,
      V960420 == 4 | V960420 == 5 | V960420 == 6 ~ 0
))

anes_1996 <- anes_1996 %>%
    mutate(dem_nolean = case_when(
      V960420 == 0 | V960420 == 1 ~ 1,
      V960420 == 5 | V960420 == 6 ~ 0
))

#*deficit increase/decrease under Clinton - decrease

anes_1996 <- anes_1996 %>%
    mutate(deficit_corr = case_when(
      V960392 == 2 ~ 1,
      V960392 == 1 | V960392 == 3 | V960392 == 8 ~ 0
))

res_df[34, ] <- new_lm(item_name = "deficit_clinton_1996_anes", survey = "ANES", study = "ANES", year = 1996, data = anes_1996, item = "deficit_corr", rd = "dem", wt = "weight")

#*income tax paid by average person increase/decrease under Clinton - increase

anes_1996 <- anes_1996 %>%
    mutate(income_corr = case_when(
      V960394 == 1 ~ 1,
      V960394 == 2 | V960394 == 3 | V960394 == 8 ~ 0
))

res_df[35, ] <- new_lm(item_name = "taxes_clinton_1996_anes", survey = "ANES", study = "ANES", year = 1996, data = anes_1996, item = "income_corr", rd = "dem", wt = "weight")

#*note: V961265 asks about personal tax burden increase/decrease under Clinton

#***************
#***ANES 1998***
#***************

anes_1998 <- haven::read_dta("data/anes/anes_1998/nes1998.dta")
anes_1998 <- rename(anes_1998, weight = V980002)

anes_1998 <- anes_1998 %>%
    mutate(dem = case_when(
      V980339 == 0 | V980339 == 1 | V980339 == 2 ~ 1,
      V980339 == 4 | V980339 == 5 | V980339 == 6 ~ 0
))

anes_1998 <- anes_1998 %>%
    mutate(dem_nolean = case_when(
      V980339 == 0 | V980339 == 1 ~ 1,
      V980339 == 5 | V980339 == 6 ~ 0
))

#*economy better/worse since Clinton took office - better

anes_1998 <- anes_1998 %>%
    mutate(econ_clint_corr = case_when(
      V980422 == 1 ~ 1,
      V980422 == 3 | V980422 == 5 | V980422 == 8 ~ 0
))

res_df[36, ] <- new_lm(item_name = "econ_clint_corr", survey = "ANES", study = "ANES", year = 1998, data = anes_1998, item = "econ_clint_corr", rd = "dem", wt = "weight")

#***************
#***ANES 2000***
#***************
#*note: question about moral climate, place in the world, etc. 
# not analyzed bc no objectively correct answer

anes_2000 <- haven::read_dta("data/anes/anes_2000/anes2000TS.dta")
anes_2000 <- rename(anes_2000, weight = V000002)

anes_2000 <- anes_2000 %>%
    mutate(dem = case_when(
      V000523 == 0 | V000523 == 1 | V000523 == 2 ~ 1,
      V000523 == 4 | V000523 == 5 | V000523 == 6 ~ 0
))

anes_2000 <- anes_2000 %>%
    mutate(dem_nolean = case_when(
      V000523 == 0 | V000523 == 1 ~ 1,
      V000523 == 5 | V000523 == 6 ~ 0
))

#*deficit compared to 1992 - smaller

anes_2000 <- anes_2000 %>%
    mutate(deficit_1992_corr = case_when(
      V000807 == 1 | V001590 == 1 ~ 1,
      (V000807 == 3 | V000807 == 5 | V000807 == 8) ~ 0,
     (V001590 == 3 | V001590 == 5 | V001590 == 8) ~ 0,
))

res_df[37, ] <- new_lm(item_name = "deficit_92_2000_anes", survey = "ANES", study = "ANES", year = 2000, data = anes_2000, item = "deficit_1992_corr", rd = "dem", wt = "weight")

#*economy compared to 1992 - better

anes_2000 <- anes_2000 %>%
    mutate(econ_1992_corr = case_when(
      V000814 == 1 | V001596 == 1 ~ 1,
      V000814 == 3 | V000814 == 5 | V000814 == 8 ~ 0,
      V001596 == 3 | V001596 == 5 | V001596 == 8 ~ 0
))

res_df[38, ] <- new_lm(item_name = "econ_1992_corr", survey = "ANES", study = "ANES", year = 2000, data = anes_2000, item = "econ_1992_corr", rd = "dem", wt = "weight")

#*crime rate compared to 1992 - better

anes_2000 <- anes_2000 %>%
    mutate(crime_1992_corr = case_when(
      V000831 == 1 | V001613 == 1 ~ 1,
      V000831 == 3 | V000831 == 5 | V000831 == 8 ~ 0,
      V001613 == 3 | V001613 == 5 | V001613 == 8 ~ 0
))

res_df[39, ] <- new_lm(item_name = "crime_92_2000_anes", survey = "ANES", study = "ANES", year = 2000, data = anes_2000, item = "crime_1992_corr", rd = "dem", wt = "weight")

#*federal spending on poor since 1992 - increased

anes_2000 <- anes_2000 %>%
    mutate(poor_1992_corr = case_when(
      V001593 == 1 | V001596 == 1 ~ 1,
      V001593 == 3 | V001593 == 5 | V001593 == 8 ~ 0,
      V000811 == 3 | V000811 == 5 | V000811 == 8 ~ 0
))

res_df[40, ] <- new_lm(item_name = "poor_1992_corr", survey = "ANES", study = "ANES", year = 2000, data = anes_2000, item = "poor_1992_corr", rd = "dem", wt = "weight")

#***************
#***ANES 2002***
#***************
#*note: weight is the pre-election weight (Q from pre-election survey)

anes_2002 <- haven::read_dta("data/anes/anes_2002/anes2002TS.dta")
anes_2002 <- rename(anes_2002, weight = V020101)

anes_2002 <- anes_2002 %>%
    mutate(dem = case_when(
      V023038x == 0 | V023038x == 1 | V023038x == 2 ~ 1,
      V023038x == 4 | V023038x == 5 | V023038x == 6 ~ 0
))

anes_2002 <- anes_2002 %>%
    mutate(dem_nolean = case_when(
      V023038x == 0 | V023038x == 1 ~ 1,
      V023038x == 5 | V023038x == 6 ~ 0
))

#*income inequality over past 20 years - larger

anes_2002 <- anes_2002 %>%
    mutate(ineq_corr = case_when(
      V023060 == 1 ~ 1,
      V023060 == 3 | V023060 == 5 | V023060 == 8 ~ 0
))

res_df[41, ] <- new_lm(item_name = "ineq_corr", survey = "ANES", study = "ANES", year = 2002, data = anes_2002, item = "ineq_corr", rd = "dem", wt = "weight")

#***************
#***ANES 2004***
#***************

anes_2004 <- haven::read_dta("data/anes/anes_2004/anes2004TS.dta")
anes_2004 <- rename(anes_2004, weight = V040102)

anes_2004 <- anes_2004 %>%
    mutate(dem = case_when(
      V043116 == 0 | V043116 == 1 | V043116 == 2 ~ 1,
      V043116 == 4 | V043116 == 5 | V043116 == 6 ~ 0
))

anes_2004 <- anes_2004 %>%
    mutate(dem_nolean = case_when(
      V043116 == 0 | V043116 == 1 ~ 1,
      V043116 == 5 | V043116 == 6 ~ 0
))

#*unemployment over past year - better

anes_2004 <- anes_2004 %>%
    mutate(unemp_corr = case_when(
      V043101 == 1 ~ 1,
      V043101 == 3 | V043101 == 5 | V043101 == 8 ~ 0
))

res_df[42, ] <- new_lm(item_name = "unemp_2004_anes", survey = "ANES", study = "ANES", year = 2004, data = anes_2004, item = "unemp_corr", rd = "dem", wt = "weight")

#*inflation over past year - worse

anes_2004 <- anes_2004 %>%
    mutate(inf_corr = case_when(
      V043104 == 5 ~ 1,
      V043104 == 1 | V043104 == 3 | V043104 == 8 ~ 0
))

res_df[43, ] <- new_lm(item_name = "infl_2004_anes", survey = "ANES", study = "ANES", year = 2004, data = anes_2004, item = "inf_corr", rd = "dem", wt = "weight")

#*income tax by average working person - decrease 

anes_2004 <- anes_2004 %>%
    mutate(tax_corr = case_when(
      V043211 == 3 ~ 1,
      V043211 == 1 | V043211 == 5 | V043211 == 8 ~ 0
))

res_df[44, ] <- new_lm(item_name = "taxes_gwb_2004_anes", survey = "ANES", study = "ANES", year = 2004, data = anes_2004, item = "tax_corr", rd = "dem", wt = "weight")

#*income inequality ("gap btwn rich and poor") over past 20 years - larger

anes_2004 <- anes_2004 %>%
    mutate(ineq_corr = case_when(
      V045113 == 1 ~ 1,
      V045113 == 3 | V045113 == 5 | V045113 == 8 ~ 0
))

res_df[45, ] <-  new_lm(item_name = "ineq_corr", survey = "ANES", study = "ANES", year = 2004, data = anes_2004, item = "ineq_corr", rd = "dem", wt = "weight")

#***************
#***ANES 2008***
#***************

anes_2008 <- haven::read_dta("data/anes/anes_2008/anes_timeseries_2008_stata12.dta")
anes_2008 <- rename(anes_2008, weight = V080102)

anes_2008 <- anes_2008 %>%
    mutate(dem = case_when(
      V083098x == 0 | V083098x == 1 | V083098x == 2 ~ 1,
      V083098x == 4 | V083098x == 5 | V083098x == 6 ~ 0
))

anes_2008 <- anes_2008 %>%
    mutate(dem_nolean = case_when(
      V083098x == 0 | V083098x == 1 ~ 1,
      V083098x == 5 | V083098x == 6 ~ 0
))

#*unemployment over past year - worse

anes_2008 <- anes_2008 %>%
    mutate(unemp_corr = case_when(
      V083087 == 5 ~ 1,
      V083087 == 1 | V083087 == 3 | V083087 == 8 ~ 0
))

res_df[46, ] <- new_lm(item_name = "unemp_2008_anes", survey = "ANES", study = "ANES", year = 2008, data = anes_2008, item = "unemp_corr", rd = "dem", wt = "weight")

#*inflation over past year - stayed the same

anes_2008 <- anes_2008 %>%
    mutate(infl_corr = case_when(
      V083089 == 3 ~ 1,
      V083089 == 1 | V083089 == 5 | V083089 == 8 ~ 0
))

res_df[47, ] <- new_lm(item_name = "infl_2008_anes", survey = "ANES", study = "ANES", year = 2008, data = anes_2008, item = "infl_corr", rd = "dem", wt = "weight")

#*income inequality ("gap btwn rich and poor") over the past 20 years - larger
anes_2008 <- anes_2008 %>%
    mutate(ineq_corr = case_when(
      V085080 == 1 ~ 1,
      V085080 == 3 | V085080 == 5 | V085080 == 8 ~ 0
))

res_df[48, ] <- new_lm(item_name = "ineq_corr", survey = "ANES", study = "ANES", year = 2008, data = anes_2008, item = "ineq_corr", rd = "dem", wt = "weight")

#***************
#***ANES 2012***
#***************
# *note: there is also a question about average health care costs since the ACA:  HLTHLAW_AMCOST

anes_2012 <- haven::read_dta("data/anes/anes_2012/anes_timeseries_2012_Stata12.dta")
anes_2012 <- rename(anes_2012, weight = weight_full)

anes_2012 <- anes_2012 %>%
    mutate(dem = case_when(
      pid_x == 1 | pid_x == 2 | pid_x == 3 ~ 1,
      pid_x == 5 | pid_x == 6 | pid_x == 7 ~ 0
))

anes_2012 <- anes_2012 %>%
    mutate(dem_nolean = case_when(
      pid_x == 1 | pid_x == 2 ~ 1,
      pid_x == 6 | pid_x == 7 ~ 0
))

anes_2012 <- anes_2012 %>%
    mutate(ineq_corr = case_when(
      ineq_incgap == 1 ~ 1,
      ineq_incgap == 2 | ineq_incgap == 3 | ineq_incgap == -8 ~ 0
))

res_df[49, ] <- new_lm(item_name = "ineq_corr", survey = "ANES", study = "ANES", year = 2012, data = anes_2012, item = "ineq_corr", rd = "dem", wt = "weight")

#*unemployment over the past year - better 

anes_2012 <- anes_2012 %>%
    mutate(unemp_corr = case_when(
      econ_unpast == 1 ~ 1,
      econ_unpast == 2 | econ_unpast == 3 | econ_unpast == -8 ~ 0
))

res_df[50, ] <- new_lm(item_name = "unemp_2012_anes", survey = "ANES", study = "ANES", year = 2012, data = anes_2012, item = "unemp_corr", rd = "dem", wt = "weight")

#*Obama born in U.S.
anes_2012 <- anes_2012 %>%
    mutate(born_correct = case_when(
      nonmain_born == 1 ~ 1,
      nonmain_born == -8 | nonmain_born == 2 | nonmain_born == 3 | nonmain_born == 4 ~ 0
))

res_df[51, ] <- new_lm(item_name = "birther", survey = "ANES", study = "ANES", year = 2012, data = anes_2012, item = "born_correct", rd = "dem", wt = "weight")

#*death panels

anes_2012 <- anes_2012 %>%
    mutate(death_correct = case_when(
      nonmain_endlife == 4 ~ 1,
      nonmain_endlife == -8 | nonmain_endlife == 1 | nonmain_endlife == 2 | nonmain_endlife == 3 ~ 0
))

res_df[52, ] <- new_lm(item_name = "death_panels", survey = "ANES", study = "ANES", year = 2012, data = anes_2012, item = "death_correct", rd = "dem", wt = "weight")

#*9/11
anes_2012 <- anes_2012 %>%
    mutate(correct_911 = case_when(
      nonmain_govt911 == 4 ~ 1,
      nonmain_govt911 == -8 | nonmain_govt911 == 1 | nonmain_govt911 == 2 | nonmain_govt911 == 3 ~ 0
))

res_df[53, ] <- new_lm(item_name = "9_11_2012", survey = "ANES", study = "ANES", year = 2012, data = anes_2012, item = "correct_911", rd = "dem", wt = "weight")

#*Katrina flooding
anes_2012 <- anes_2012 %>%
    mutate(hurr_corr = case_when(
      nonmain_hurric == 4 ~ 1,
      nonmain_hurric == -8 | nonmain_hurric == 1 | nonmain_hurric == 2 | nonmain_hurric == 3 ~ 0
))

res_df[54, ] <- new_lm(item_name = "katrina", survey = "ANES", study = "ANES", year = 2012, data = anes_2012, item = "hurr_corr", rd = "dem", wt = "weight")

anes_2012 <- anes_2012 %>%
    mutate(health_corr = case_when(
      hlthlaw_num == 1 ~ 1,
      hlthlaw_num == 2 | hlthlaw_num == 3 | hlthlaw_num == -8 ~ 0
))

res_df[55, ] <- new_lm(item_name = "health_corr", survey = "ANES", study = "ANES", year = 2012, data = anes_2012, item = "health_corr", rd = "dem", wt = "weight")

#***************
#***ANES 2016***
#***************

anes_2016 <- haven::read_dta("data/anes/anes_2016/anes_timeseries_2016_Stata13.dta")
anes_2016 <- rename(anes_2016, weight = V160102)

anes_2016 <- anes_2016 %>%
    mutate(dem = case_when(
      V161158x == 1 | V161158x == 2 | V161158x == 3 ~ 1,
      V161158x == 5 | V161158x == 6 | V161158x == 7 ~ 0
))

anes_2016 <- anes_2016 %>%
    mutate(dem_nolean = case_when(
      V161158x == 1 | V161158x == 2 ~ 1,
      V161158x == 6 | V161158x == 7 ~ 0
))

anes_2016 <- anes_2016 %>%
    mutate(econ_corr = case_when(
      V161140 == 1 ~ 1,
      V161140 == 2 | V161140 == 3 | V161140 == -8 ~ 0
))

res_df[56, ] <- new_lm(item_name = "econ_corr", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "econ_corr", rd = "dem", wt = "weight")

anes_2016 <- anes_2016 %>%
    mutate(ineq_corr = case_when(
      V161137 == 2 ~ 1,
      V161137 == 2 | V161137 == 3 | V161137 == -8 ~ 0
))

res_df[57, ] <- new_lm(item_name = "ineq_corr", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "ineq_corr", rd = "dem", wt = "weight")

anes_2016 <- anes_2016 %>%
    mutate(unemp_corr = case_when(
      V161142 == 1 ~ 1,
      V161142 == 2 | V161142 == 3 | V161142 == -8 ~ 0
))

res_df[58, ] <- new_lm(item_name = "unemp_2016_anes", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "unemp_corr", rd = "dem", wt = "weight")

anes_2016 <- anes_2016 %>%
    mutate(glob_corr = case_when(
      V161221 == 1 ~ 1,
      V161221 == 2 | V161221 == -8 ~ 0
))

res_df[59, ] <- new_lm(item_name = "exist_warm_2016_anes", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "glob_corr", rd = "dem", wt = "weight")

anes_2016 <- anes_2016 %>%
    mutate(glcause_corr = case_when(
      V161222 == 1 ~ 1,
      V161222 == 2 | V161222 == 3 | V161222 == -8 ~ 0
))

res_df[60, ] <- new_lm(item_name = "cause_warm_2016_anes", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "glcause_corr", rd = "dem", wt = "weight")

anes_2016 <- anes_2016 %>%
    mutate(ec2008_corr = case_when(
      V161235 == 1 ~ 1,
      V161235 == 2 | V161235 == 3 | V161235 == -8 ~ 0
))

res_df[61, ] <- new_lm(item_name = "ec2008_corr", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "ec2008_corr", rd = "dem", wt = "weight")

anes_2016 <- anes_2016 %>%
    mutate(ecmob_corr = case_when(
      V162135 == 2 ~ 1,
      V162135 == 1 | V162135 == 3 | V162135 == -8 ~ 0
))

res_df[62, ] <- new_lm(item_name = "ecmob_corr", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "ecmob_corr", rd = "dem", wt = "weight")

anes_2016 <- anes_2016 %>%
    mutate(vacc_corr = case_when(
      V162161 == 1 ~ 1,
      V162161 == 2 | V162161 == 3 | V162161 == -8 ~ 0
))

res_df[63, ] <- new_lm(item_name = "vacc_corr", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "vacc_corr", rd = "dem", wt = "weight")

anes_2016 <- anes_2016 %>%
    mutate(corr_911_2016 = case_when(
      V162254 == 4 ~ 1,
      V162254 == -8 | V162254 == 1 | V162254 == 2 | V162254 == 3 ~ 0
))

res_df[64, ] <- new_lm(item_name = "9_11_2016", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "corr_911_2016", rd = "dem", wt = "weight")

anes_2016 <- anes_2016 %>%
    mutate(muslim_corr = case_when(
      V162255 == 2 ~ 1,
      V162255 == -8 | V162255 == 1 ~ 0
))

res_df[65, ] <- new_lm(item_name = "obama_muslim", survey = "ANES", study = "ANES", year = 2016, data = anes_2016, item = "muslim_corr", rd = "dem", wt = "weight")

# Write out the results df
write_csv(res_df, file = "tabs/anes_results.csv")
