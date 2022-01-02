# JnJ

# Set dir
setwd(dropboxdir)
setwd("partisan_gap/pgap/rep_files/")

# Load libs
# Loading the required libraries
library(tidyverse)
library(magrittr)
library(plm)
library(lmtest)
library(multiwayvcov)
library(broom)

# Function
tidy_lm <- function(item_name, survey, study, year, out) {
    do.call(cbind.data.frame, 
        c(item_name = item_name, 
          survey = survey,
          study = study,
          year = year,
          out[1, 2], out[1, 2] + out[2, 2], out[2, 2:5]))
}

# load data
jnj <- foreign::read.dta("data/jerit_barabas/JeritBarabas_JOP_Data/PB1.dta")

# 1. Replication
# (Successfully) Replicating Table 1 numbers
# 1st col
round(mean(jnj$know[jnj$dem == 1]), 2)
round(mean(jnj$know[jnj$rep == 1]), 2)

# 2nd col
round(mean(jnj$know[jnj$dempos == 1 & jnj$dem == 1]), 2)
round(mean(jnj$know[jnj$reppos == 1 & jnj$rep == 1]), 2)

# 3rd col
round(mean(jnj$know[jnj$demneg == 1 & jnj$dem == 1]), 2)
round(mean(jnj$know[jnj$repneg == 1 & jnj$rep == 1]), 2)

t.test(jnj$know[jnj$dempos == 1 & jnj$dem == 1], jnj$know[jnj$reppos == 1 & jnj$rep == 1])
t.test(jnj$know[jnj$demneg == 1 & jnj$dem == 1], jnj$know[jnj$repneg == 1 & jnj$rep == 1])

# 2: Remove 2 irrelevant items + Subsets of partisans with only partisan relevant items
# 
# Only Partisan relevant items
jnj_partisan <- subset(jnj, dempos == 1 | reppos == 1 | demneg == 1 | repneg == 1)
length(unique(jnj_partisan$IEqid))

# 2. Removing incorrectly coded items
jnj_small <- subset(jnj, !(IEqid == 32 | IEqid == 40))
length(unique(jnj_small$IEqid))

jnj_partisan_small <- subset(jnj_partisan, !(IEqid == 32 | IEqid == 40))
length(unique(jnj_partisan_small$IEqid))

# Only partisans
jnj_partisan_small_p_only <- subset(jnj_partisan_small, dem == 1 | rep == 1)
length(unique(jnj_partisan_small_p_only$IEqid))

# 3. Results: item wise means

# Item wise R/D
item_rd <- jnj_partisan_small_p_only[, c("IEqid", "dem", "know")] %>% 
	group_by(IEqid) %>%
	do(fitl = lm(know ~ dem, data = .)) %>% 
	ungroup %>%
	mutate(modresults = map(fitl, tidy))

item_rd[, c("prop_r_correct", "prop_d_correct", "dem_minus_rep", "std.error", "statistic", "p.value")] <- NA 

for(i in 1:nrow(item_rd)){
	item_rd[i, c("prop_r_correct", "prop_d_correct", "dem_minus_rep", "std.error", "statistic", "p.value")] <- c(item_rd$modresults[i][[1]][1, 2], 
		                                                												   item_rd$modresults[i][[1]][1, 2] + item_rd$modresults[i][[1]][2, 2], 
		                                                                                                   item_rd$modresults[i][[1]][2, 2:5])
}

# item wise prop correct
item_means <- jnj_partisan_small_p_only[, c("IEqid", "dem", "know")] %>% 
	group_by(IEqid) %>%
	do(fitl = lm(know ~ 1, data = .)) %>% 
	ungroup %>%
	mutate(modresults = map(fitl, tidy))

item_means[, "prop_correct_all"] <- unlist(sapply(item_means$modresults, function(x) x[2]))

# get n_reps and n_dems
item_metadata <- jnj_partisan_small_p_only[, c("IEqid", "dem")] %>%
					   group_by(IEqid) %>%
					   summarize(n_dem = sum(dem == 1), n_rep = sum(dem == 0))

# Join all
item_all <- item_rd %>% 
			left_join(item_metadata, by = "IEqid") %>%
			left_join(item_means[, c("IEqid", "prop_correct_all")], by = "IEqid")

# Create dummy cols
item_all$survey <- "jnj"
item_all$study  <- "Jerit & Barabas"
item_all$year   <- -1 

# Join ready item name
item_all$item_name <- item_all$IEqid
item_all$item_name <- paste0("jnj_", item_all$item_name)

# Filter to relevant cols
item_means_small <- item_all[, c("item_name", "survey", "study", "year", "prop_r_correct", "prop_d_correct", "dem_minus_rep", "std.error", "statistic", "p.value", "n_dem", "n_rep", "prop_correct_all")]

# Write
write_csv(item_means_small, file = "tabs/jnj.csv")
