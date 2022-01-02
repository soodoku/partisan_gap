# Replication Materials For "A Gap in Our Understanding? Reconsidering the Evidence for Partisan Knowledge Gaps"

## Replicating Figure 1

### Study Wise Files

1. [Item wise results from Jerit and Barabas](scripts/01_j_and_j.R)
	* Input  = data/jerit_barabas/JeritBarabas_JOP_Data/PB1.dta
	* Output = tabs/jnj.csv

2. [Item wise results from ANES](scripts/02_anes.R)
	* Inputs = data/anes/
	* Output = tabs/anes.csv

3. [Item wise results from Prior et al. and Bullock et al.](scripts/03_prior_and_bullock.R)
	* Inputs
		* data/bullock_et_al_2015/CCES2008/CCES2008PublicReplicationDataset.dta
		* data/bullock_et_al_2015/MTurk12/MTURK2012PublicReplicationDataset.dta
		* data/prior_et_al_2015/psk_wide_recode_pid_by_CR.dta
	* Output = 
		* tabs/bullock_prior_results.csv

### Combine and Graph
### Replicating in-text numbers, Tables, Figures

 Table 1, , , and Figure Figure SI 3.1

4. [Filter, Combine, and Plot](scripts/04_density_plot.R)
	* Inputs = 
		1. Study wise files: tabs/jnj.csv, tabs/anes.csv, tabs/bullock_prior_results.csv
		2. Filter anes items to relevant set using anes_filter_list.csv
		3. Join to item meta data file and codebook: item_metadata.csv and coding_question_features.txt 

	* Outputs = 
		1. Table 1: tabs/table_1.tex
		2. Appendix Table SI 1.1: tabs/append_table_1_all_partisan_gap_by_item.tex
		3. Appendix Table SI 1.2: tabs/append_table_2_misinfo_partisan_gap_by_item.tex
		4. Appendix Table SI 4.5: tabs/append_table_3_retro_partisan_gap_by_item.tex
		5. Figure 1: figs/partisan_gap_density.pdf
		6. Appendix Figure SI 3.1: figs/abs_value_p_gap.pdf

### Authors

Carrie Roush and Gaurav Sood