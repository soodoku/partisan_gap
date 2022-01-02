"								
Partisan Gaps

  1. Density Plot of Partisan Gaps
  2. Summary stats
"

# Set dir 
setwd(githubdir)
setwd("partisan_gap/")

# Load libs
library(devtools)
devtools::install_github("soodoku/goji")
library(goji)
library(dplyr)
library(readr)
library(xtable)
library(sandwich)
library(lmtest)
library(stargazer)

# Colnames
item_meta     <-  c("item_name", "survey", "study", "year")
base_layer    <- c("prop_r_correct", "prop_d_correct", "dem_minus_rep", "std.error", "statistic", "p.value")
nolean        <- paste0(base_layer, "_nolean")
rest          <- c("prop_correct_all", "n_dem", "n_rep")

# Load dat
anes <- read_csv("tabs/anes_results.csv") %>% select(!contains("_nolean"))
bullock_prior <- read_csv("tabs/bullock_prior_results.csv") %>% select(!contains("_nolean"))
jnj <- read_csv("tabs/jnj.csv")

# Load metadata 
metadata <- read_csv("data/roush_sood/item_metadata.csv") %>%  mutate(survey = as.character(survey))

# Filter dat
anes_filter <- read_csv("data/roush_sood/anes_filter_list.csv")
anes_fin    <- anes %>% left_join(anes_filter, by = c("item_name", "year"), keep = FALSE) %>% filter(filter == 1) 
anes_fin2   <- subset(anes_fin, select = -c(filter, econ_retro))

# Combine
all_items = rbind(anes_fin2, bullock_prior, jnj)
all_fin = all_items %>% left_join(metadata, by = "item_name")

# Sign Gap
all_fin$part_gap <- -1*all_fin$dem_minus_rep
all_fin$sign_gap <- -1*all_fin$dem_minus_rep*all_fin$sign

# 
# Write out initial dataset
write.csv(all_fin, "tabs/pgap_dataset_master_rep.csv", row.names = F)

# Load dat
p_gap <- read.csv("tabs/pgap_dataset_master_rep.csv")

# 1. Figure
# ----------------------
ggplot(p_gap, aes(sign_gap)) +
  geom_density(aes(x = sign_gap)) +
  theme_bw() +
  xlab("Signed Partisan Gap") +
  ylab("") +
  geom_vline(aes(xintercept = mean(sign_gap), color = "#330000"), linetype = "solid") + 
  annotate("text", x=mean(p_gap$sign_gap)+.05, label="Mean = .065", y=4, size = 3.5) + 
  theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.position = "none")

ggsave("figs/partisan_gap_density.pdf", width = 7, height = 7)

# 2. SI 3: Absolute Partisan Gap

p_gap$abs_sign_gap <- abs(p_gap$sign_gap)

ggplot(p_gap, aes(abs_sign_gap)) +
  geom_density(aes(x = abs_sign_gap)) +
  theme_bw() +
  xlab("Absolute Partisan Gap") +
  ylab("") +
  geom_vline(aes(xintercept = mean(abs_sign_gap), color = "#330000"), linetype = "solid") + 
  annotate("text", x=mean(p_gap$abs_sign_gap)+.05, label="Mean = .092", y=4, size = 3.5) + 
  theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.position = "none")

ggsave("figs/abs_value_p_gap.pdf", width = 7, height = 7)


# 2. Summary stats
# ------------------

# Number of items
nrow(all_fin)
# 187

# Total number of responses
sum(all_fin$n_rep) + sum(all_fin$n_dem)
# [1] 162083

# Number of surveys
length(unique(all_fin$survey.y))
# [1] 47

# Range of years
range(p_gap$year.y)
# [1] 1986 2016

# Proportion <= 0
mean(p_gap$sign_gap <= 0)
# [1] 0.2834225

# Mean signed gap
mean(p_gap$sign_gap)
# [1] 0.06495512

# Median signed gap
median(p_gap$sign_gap)
# [1] 0.04581734

# SD of signed gap
sd(p_gap$sign_gap)
# [1] 0.1132083

# Range of signed gap
range(p_gap$sign_gap)
#  [1] -0.1625473  0.4916665

# Proportion of partisan gaps > 10%
mean(p_gap$sign_gap >= .1)
# [1] 0.2941176

# Proportion of 'statistically significant' partisan gaps
mean(p_gap$p.value <= .05)
# [1] 0.459893

# Proportion of signed gaps in the right direction that are significant
mean(p_gap$p.value[p_gap$sign_gap > 0] <= .05)
# [1] 0.5522388

# Mean sample size on ^ questions
mean(p_gap$n_rep[p_gap$sign_gap > 0] + p_gap$n_dem[p_gap$sign_gap > 0])
# [1] 939.1269

# Proportion of signed gaps in the right direction that are significant at 90%
mean(p_gap$p.value[p_gap$sign_gap > 0] <= .1)
# [1] 0.5895522

# Proportion of sample where n_rep and n_dem > 100 each
mean(p_gap$n_rep > 100 & p_gap$n_dem > 100)
# [1] 0.8877005

# Proportion with n > 100 sample size significant
mean(p_gap$p.value[p_gap$n_rep > 100 & p_gap$n_dem > 100] <= .05)
# [1] 0.4939759

# Mean absolute gap
mean(p_gap$abs_sign_gap)
# [1] 0.09238132

# Median absolute gap
median(p_gap$abs_sign_gap)
# [1] 0.06519015

# Median absolute gap
sd(p_gap$abs_sign_gap)
# [1] 0.09207491

# Actual partisan questions
length(p_gap$sign_gap[p_gap$actual_partisan == 1])
mean(p_gap$sign_gap[p_gap$actual_partisan == 1])
# [1] 0.06746802

# Mean R/D correct
mean(p_gap$prop_r_correct)
# [1] 0.4162878
mean(p_gap$prop_d_correct)
# [1] 0.415894

## no lean
# Load dat
anes_nolean <- read_csv("tabs/anes_results.csv") %>% select(item_name, dem_minus_rep_nolean) %>% rename(dem_minus_rep = dem_minus_rep_nolean)
anes_fin_nolean  <- anes_nolean %>% left_join(anes_filter, by = c("item_name"), keep = FALSE) %>% filter(filter == 1) %>% select(item_name, dem_minus_rep)
bullock_prior_nolean <- read_csv("tabs/bullock_prior_results.csv") %>% select(item_name, dem_minus_rep_nolean) %>% rename(dem_minus_rep = dem_minus_rep_nolean)
jnj_nolean <- read_csv("tabs/jnj.csv") %>% select(item_name, dem_minus_rep)
all_items_nolean <-  rbind(anes_fin_nolean, bullock_prior_nolean, jnj_nolean)
all_fin_nolean <- all_items_nolean %>% left_join(metadata, by = "item_name")
all_fin_nolean$sign_gap <- -1*all_fin_nolean$dem_minus_rep*all_fin_nolean$sign
mean(all_fin_nolean$sign_gap)
# [1] 0.0622685
median(all_fin_nolean$sign_gap)
# [1] 0.04576809
sd(all_fin_nolean$sign_gap)
# [1] 0.124097

# 3. Item-wise Appendix Table
# ----------------------------

# Remove year from the description and sanitize for latex
#p_gap$description <-  gsub(" -.*$", "", p_gap$description)
p_gap$description <-  sanitize(p_gap$description, type = "latex")
p_gap$study.x     <-  sanitize(p_gap$study.x, type = "latex")

# Remove 0 before decimal
#numeric_cols <- c("prop_r_correct", "prop_d_correct", "sign_gap", "p.value")
#p_gap[, numeric_cols] <- sapply(p_gap[, numeric_cols], function(x) nolead0s(round(x, 3)))

# A DF for all items and misinfo each
p_misinfo  <- p_gap[p_gap$address_misinfo == 1, ]

# Subset on Relevant Cols.
select_cols = c("description", "study.x", "year.y", "prop_r_correct", "n_rep", "prop_d_correct", "n_dem", "sign_gap", "p.value")
p_gap_small    <- subset(p_gap, select = select_cols)
p_misinfo_small <- subset(p_misinfo, select = select_cols)

# Assign human readable names
names(p_gap_small) <- names(p_misinfo_small) <- c("Item", "Study", "Year", "R", "n(R)", "D", "n(D)", "Signed Gap", "$\\textit{p(Gap)}$")

## Appendix Table 1/Knowledge Items
print(
        xtable(p_gap_small,
          digits = 3,
          align = "llllcccccc",
          caption = "Partisan Knowledge Gaps by Item", 
          label = "tab:indiv_items"), 
        include.rownames = FALSE,
        include.colnames = TRUE, 
        size="\\tiny", 
        tabular.environment  = "longtable",
        type = "latex", 
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        floating = FALSE,
        table.placement = "!htb",
        file = "tabs/append_table_1_all_partisan_gap_by_item.tex")

## Appendix Table 2/Misinformation Items
print(
        xtable(p_misinfo_small,
          digits = 3,
          align = "llllcccccc",
          caption = "Partisan Knowledge Gaps on Misinformation Items", 
          label = "tab:misinfo_items"), 
        include.rownames = FALSE,
        include.colnames = TRUE, 
        size="\\tiny", 
        type = "latex", 
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        table.placement = "!htb",
        file = "tabs/append_table_2_misinfo_partisan_gap_by_item.tex")

## Appendix Table 5/Economic Retrospection Items
anes_retro  <- anes %>% left_join(anes_filter, by = c("item_name", "year"), keep = FALSE) %>% filter(econ_retro == 1) 
anes_retro2 <- subset(anes_retro, select = -c(filter, econ_retro))

# Combine
econ_metadata <- read_csv("data/roush_sood/anes_econ_items_metadata.csv")
retro_fin = anes_retro2 %>% left_join(econ_metadata, by = "item_name")

# Sign Gap
retro_fin$part_gap <- -1*retro_fin$dem_minus_rep
retro_fin$sign_gap <- -1*retro_fin$dem_minus_rep*retro_fin$sign

#retro_fin$description <-  gsub(" -.*$", "", retro_fin$description)
retro_fin$description <-  sanitize(retro_fin$description, type = "latex")
retro_fin$study.x     <-  sanitize(retro_fin$study.x, type = "latex")

p_retro_small <- subset(retro_fin, select = select_cols)
p_retro_small$year.y <- as.integer(p_retro_small$year.y)
p_retro_small$n_dem <- as.integer(p_retro_small$n_dem)
p_retro_small$n_rep <- as.integer(p_retro_small$n_rep)

# Mean gap on econ. retrospection items
mean(p_retro_small$sign_gap)
# [1] 0.1493458

names(p_retro_small) <- c("Item", "Study", "Year", "R", "n(R)", "D", "n(D)", "Signed Gap", "$\\textit{p(Gap)}$")

print(
        xtable(p_retro_small,
          digits = 3,
          align = "llllcccccc",
          caption = "Partisan Gap on Economic Retrospection Items", 
          label = "tab:econ"), 
        include.rownames = FALSE,
        include.colnames = TRUE, 
        size="\\tiny", 
        type = "latex", 
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        table.placement = "!htb",
        file = "tabs/append_table_3_retro_partisan_gap_by_item.tex")

## Main text Table 1: Regression

# Recoding
## Difficulty
p_gap$diff         <- zero1(-p_gap$prop_correct_all)
p_gap$dk           <- zero1(p_gap$exp_dk + p_gap$exp_ns)
p_gap$ambiguous    <- zero1(p_gap$tbyk + p_gap$afayk + p_gap$dyt + p_gap$other_enc_wording)

lm_mod    <- lm(sign_gap ~ partisan_cue + response_options + dk + ambiguous + response_open_int + address_misinfo + diff + asked_election_year + as.factor(study.x), data = p_gap)
rob_mod   <- coeftest(lm_mod, vcovHC, cluster= ~survey.y, type = 'HC2')

stargazer(rob_mod, 
       covariate.labels = c("Partisan Cue", "Number of response options", "Don't know/not sure", "Encourages guessing", "Response open to interpretation", 
                              "Addresses misinformation", "Question difficulty", "Asked in fall of election year", "Bullock et al.", "Jerit and Barabas", "Prior et al.", "Constant"),

       label = "tab:tab1",
       dep.var.labels = "Absolute Partisan Gap",
       title = "Predicting Absolute Value of the Partisan Gap",
       out   = "tabs/table_1.tex")

# By Topic
p_gap$topicl <- recode(p_gap$topic, "1" = "Economic", "2" = "Health Care", "3" = "Foreign policy/nat sec", 
                                     "4"  ="Office/cand rec", "5" = "Enviro", "6" = "Soc Security", "7" = "Guns", "8" = "Education", "9"  = "Misc")

lm_mod_topic <- lm(sign_gap ~ partisan_cue + response_options + dk + ambiguous + response_open_int + address_misinfo + diff + asked_election_year + as.factor(study.x) + topicl, data = p_gap)
coeftest(lm_mod_topic, vcovHC, cluster= ~survey.y, type = 'HC2')
