Updated: September 2, 2015

This folder contains the replication data and code for the QJPS article “You Cannot Be Serious: The Impact of Accuracy Incentives on Partisan Bias in Reports of Economic Perceptions” by Prior, Sood, and Khanna.

Table of Contents
- This File (README.txt)

- Study 1 Questionnaire (Study1Questionnaire.doc)

- Study 2 Questionnaire (Study2Questionnaire.doc)

- Stata Datasets
	- Data in Wide Format (psk_wide.dta)
	- Data in Long Format (psk_long.dta)
	- Recoded Data in Wide Format (ps_wide_recode.dta)
	- Recoded Data in Long Format (ps_wide_recode.dta)

- Stata Scripts
	- Recode Script (PSK_Recodes.do): applies recodes to psk_wide.dta and psk_long.dta, producing ask psk_wide_recode.dta and psk_long_recode.dta, respectively
	- Functions Script (PSK_Functions.do): defines functions needed in the bootstrap commands for Table 1 (starting on line 30 of PSK_Analysis.do)
	- Analysis Script (PSK_Analysis.do): produces all the analyses, tables, and figures in the paper, using psk_wide_recode.dta and psk_long_recode.dta

- GLLAMM Package (to run hierarchical models with ordinal dependent variable)
	- gllam_ll.ado
	- gllamm.ado
	- gllapred.ado
	- gllarob.ado
	- gllas_yu.ado
	- gllasim.ado
	- remcor.ado
	- glamm.hlp
	- gllapred.hlp
	- gllasim.hlp

- GLLAMM Add-Ons (to set prior on random effects variance in GLLAMM)
	- calc_prior.ado
	- init_prior.ado

- JAGS Scripts and Output (to calculate confidence intervals of predicted probabilities)
	- PSK_JAGS.R
	- study12hologit.jag
	- study2ologit.jag
	- study2hologit.jag
	- study12jagsresults.csv
	- study2jagsresults.csv


Code for Each Table and Figure (in PSK_Analysis.do)

- Tables
	- Table 1: lines 17 - 108 (Note: run PSK_Functions.do before bootstrapping)
	- Table 2: lines 111 - 131
	- Table 3: lines 134 - 143
	- Table 4: lines 146 - 165

- Figures
	- Figure 1A: lines 170 - 205
	- Figure 1B: lines 208 - 247
	- Figure 2A: lines 252 - 265
	- Figure 2B: lines 268 - 286
	- Figure 2C: lines 289 - 295 (Note: first run lines 253-254)
	- Figure 2D: lines 298 - 304 (Note: first run lines 269-275)

- Main Text and Footnotes: lines 307 - 434

- Appendix
	- Appendix A: lines 438 - 450
	- Table B1: lines 456 - 475
	- Table B2: lines 493 - 526
	- Table B3: lines 529 - 545


Base Software Dependencies (Used by Authors)
- OSX 10.9 or Windows 7/8
— R 2.13.1/3.0.1
- Stata 12/13 (Note: Stata 14 may result in slightly different bootstrapped standard errors)

Additional Dependencies
- GLLAMM Package: 7 .ado files and 3 .hlp files (http://fmwww.bc.edu/repec/bocode/g)
- calc_prior.do (http://www.gllamm.org/bayes.html)
- init_prior.do (http://www.gllamm.org/bayes.html)

Seed Locations: default Stata seed of 123456 is used in lines 30 - 91 of PSK_Analysis.do to calculate bootstrapped standard errors for Table 1.

Note: In order to open the Stata .dta files in R, they must be saved as v.12 or earlier.