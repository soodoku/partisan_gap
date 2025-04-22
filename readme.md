# Replication Materials For "A Gap in Our Understanding? Reconsidering the Evidence for Partisan Knowledge Gaps"


## Manuscript

* [manuscript](ms/)

### Workflow

1. [Item wise results from Jerit and Barabas](scripts/01_j_and_j.R)
	* Input  = [jnj](data/jerit_barabas/JeritBarabas_JOP_Data/PB1.dta)
	* Output = [jnj results](tabs/jnj.csv)

2. [Item wise results from ANES](scripts/02_anes.R)
	* Inputs = [anes data](data/anes/)
	* Output = [anes results](tabs/anes.csv)

3. [Item wise results from Prior et al. and Bullock et al.](scripts/03_prior_and_bullock.R)
	* Inputs
		* [bullock et al. cces](data/bullock_et_al_2015/CCES2008/CCES2008PublicReplicationDataset.dta)
		* [bullock et al. mturk](data/bullock_et_al_2015/MTurk12/MTURK2012PublicReplicationDataset.dta)
		* [prior et al.](data/prior_et_al_2015/psk_wide_recode_pid_by_CR.dta)
	* Output = 
		* [bullock and prior results](tabs/bullock_prior_results.csv)

4. [Filter, Combine, and Plot](scripts/04_density_plot_and_table.R)
	* Inputs = 
		1. Study wise files: [jnj results](tabs/jnj.csv), [anes results](tabs/anes.csv), [bullock and prior results](tabs/bullock_prior_results.csv)
		2. Filter anes items to relevant set using [anes filer](data/roush_sood/anes_filter_list.csv)
		3. Join to [item meta data file](data/roush_sood/item_metadata.csv). [Codebook](data/roush_sood/coding_question_features.txt)
		4. [Item meta data file for partisan retrospection items](data/roush_sood/anes_econ_items_metadata.csv)

	* Outputs = 
		1. [Table 1](tabs/table_1.tex)
		2. [Table SI 1.1](tabs/append_table_1_all_partisan_gap_by_item.tex)
		3. [Table SI 1.2](tabs/append_table_2_misinfo_partisan_gap_by_item.tex)
		4. [Table SI 4.5](tabs/append_table_3_retro_partisan_gap_by_item.tex)
		5. [Figure 1](figs/partisan_gap_density.pdf
		6. [Figure SI 3.1](figs/abs_value_p_gap.pdf)

5. [Run All The Above Scripts](scripts/05_run_all.R)

## Cite

Roush, Carrie and Gaurav Sood. 2023. "A Gap in Our Understanding? Reconsidering the Evidence for Partisan Knowledge Gaps." The Quarterly Journal of Political Science. 

### Authors

Carrie Roush and Gaurav Sood
