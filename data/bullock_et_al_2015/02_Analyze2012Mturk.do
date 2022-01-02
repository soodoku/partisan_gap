**********************************************************
*Analyze.do
*
*Analyze the data files of the partisan bias survey 
*fielded on Mechanical Turk by Bullock, Gerber, Huber, and Hill 
*in March 2012.
**********************************************************

* Note: Core Analysis is among those who are Dems/Republicans (variable partisanship~=.)
set more off
use MTurk12\MTURK2012PublicReplicationDataset.dta if partisanship~=.

*Count subjects
preserve
sort userid
drop if userid==userid[_n-1]
keep if treatmentnum<=3
count
keep if  partisanship~=.
count
restore

*****************************************************************
*
* For all subsequent work, re order questions on the basis of party gaps in pre-treatment case
*
*****************************************************************

/* This code is what is used to calculate appropriate party gaps to figure out what order questions should be listed in */
gen partygappretreatment=.
levelsof questionid, local(questions)
qui foreach qstn of local questions {
 regress directed_sliderresponse partisanship if posttreatment==0 & questionid==`qstn'
 replace partygappretreatment= _b[partisanship] if questionid==`qstn'
}
table questionid, c(mean partygappretreatment)

rename questionid temp
recode temp (118=1) (112=2) (109=3) (113=4) (111=5) (116=6) (119=7) (114=8) (110=9) (117=10) (115=11) (120=12) (*=.), gen(questionid)
label define newquestlabel 1 "Obama Unemployment" 2 "Bush II Unemployment" 3 "Defense Spending" 4 "Obama Vote 08" 5 "Iraq deaths % Black" 6 "Medicaid Spending" 7 "TARP % Paid Back" 8 "Global Warming Amount" 9 "Iraq deaths" 10 "Debt Service Spending" 11 "Placebo: Mantle home runs '61" 12 "Foreign Born %" 
label values questionid newquestlabel
table questionid, c(mean partygappretreatment)
gen isplacebo=questionid==11

tab treatmentnum

****
* Summary stats: distribution of responses across scale range
****

table questionid if posttreatment==1, c(min directed_sliderresponse max directed_sliderresponse )

*****************************************************************
*Table 3: Assessing partisan divergence by question in pre-treatment condition
*****************************************************************

levelsof questionid, local(questions)
qui foreach qstn of local questions {
 ttest directed_sliderresponse if posttreatment==0 & questionid==`qstn', by(partisanship)
 local diff=`r(mu_2)'-`r(mu_1)'
 local num=`r(N_1)'+`r(N_2)'
 local avg=(`r(N_1)'*`r(mu_1)' + `r(N_2)'*`r(mu_2)')/(`r(N_1)'+`r(N_2)')
 matrix temprow=`qstn',`r(mu_2)',`r(mu_1)',`diff',`r(p_l)',`num',`avg'
 if `qstn'==1 {
  matrix dofmean=temprow
 }
 else {
  matrix dofmean=dofmean\temprow
 }
}
svmat dofmean
label values dofmean1 newquestlabel 
label var dofmean1 "Question"
label var dofmean2 "Mean Democratic Response"
label var dofmean3 "Mean Republican Response"
label var dofmean4 "Diff., Dem. - Rep."
label var dofmean5 "P-value of Diff., one-tailed"
label var dofmean6 "N"
label var dofmean7 "Overall Avg."
cl dofmean* in 1/12

preserve
keep in 1/12
keep dofmean*
export excel using "Tables/Table3_MTURK12_DifferenceInPartyMeans_PreTreatment_ByQuestion", firstrow(varlabels) replace
restore

*We also want to save the parameter estimated here, the difference in means, in each of the questions. So do it here
*(We also save whether p<.10)
gen divergenceparameter=.
gen partygapsforquestion=0
gen meanscore=.

*Based on this analysis, we have certain questions for which we don't observe partisan gaps
*Create a filter variable that is 0 if there are no gaps
forvalues ctr=1(1)12 {
 replace divergenceparameter=dofmean4[`ctr'] if questionid==dofmean1[`ctr']
 replace partygapsforquestion=1 if dofmean5[`ctr']<.10 &  questionid==dofmean1[`ctr']
 replace meanscore=dofmean7[`ctr'] if questionid==dofmean1[`ctr']
 }
drop dofmean*
label var divergenceparameter "PID gap in pre"
label var partygapsforquestion "Question has p<.10 party gap in pre"
table questionid, c(mean divergenceparameter mean partygapsforquestion)

*****************************************************************
*Takeup of don't know, discussed in text
*****************************************************************
preserve
keep if (treatmentnum==3 | treatmentnum==4) & posttreatment==1

gen propdk=assignedy
label var propdk "Proportion correct response payment for Don't Know response (0, .2, .25, 33)"

table treatmentnum if isplacebo ==0 & partygapsforquestion==1 , c(mean responsedontknow)
ttest responsedontknow if isplacebo ==0 & partygapsforquestion==1, by(treatmentnum)

keep if treatmentnum==3

table assignedy  if isplacebo ==0 & partygapsforquestion==1 , c(mean responsedontknow)
ttest responsedontknow if isplacebo ==0 & partygapsforquestion==1 & (assignedy==.2 | assignedy==.25), by(assignedy)
ttest responsedontknow if isplacebo ==0 & partygapsforquestion==1 & (assignedy==.2 | assignedy==.33), by(assignedy)
ttest responsedontknow if isplacebo ==0 & partygapsforquestion==1 & (assignedy==.25 | assignedy==.33), by(assignedy)
tab assignedy, gen(asy_)
regress responsedontknow assignedx asy_2 asy_3 quid_* if isplacebo ==0 & partygapsforquestion==1
restore

*****************************************************************
*Table 4: Comparing post-treatment polarization by treatment condition
*****************************************************************
preserve

gen x=1 
egen numtimesasked=sum(x), by(userid questionid)
drop x

keep if (posttreatment==1 & treatmentnum~=4)

replace directed_sliderresponse=meanscore if responsedontknow==1 & posttreatment==1 & (treatmentnum==3 | treatmentnum==4)

gen post_flat=posttreatment*(treatmentnum==1)
gen post_fee=posttreatment*(treatmentnum==2)
gen post_feedk=posttreatment*(treatmentnum==3)
gen post_flatdk=posttreatment*(treatmentnum==4)

gen post_fee_pid=post_fee*partisanship
gen post_feedk_pid=post_feedk*partisanship
gen post_flatdk_pid=post_flatdk*partisanship

label var partisanship "Democrat (1=Yes, 0=Republican)"

label var post_flat "Flat fee for survey"
label var post_fee "Payment for Correct Response"
label var post_feedk "Payment for DK and Correct Response"
label var post_flatdk "Flat fee for survey with DK"

label var post_fee_pid "Payment Correct * Democrat"
label var post_feedk_pid "Payment DK and Correct * Democrat"
label var post_flatdk_pid "Flat fee with DK * Democrat"

gen amountcorrect=assignedx
replace amountcorrect=0 if treatmentnum==1
label var amountcorrect "Amount paid for correct (0 to $1)"

gen amountcorrect_pid=amountcorrect*partisanship
label var amountcorrect_pid "Amount correct * Democrat"

tab amountcorrect, gen(acd_)
forvalues ctr=1(1)6 {
 summ amountcorrect if acd_`ctr'==1
 local temp= string(r(mean),"%9.2f")
 label var acd_`ctr' "Amount correct = `temp'"
 gen acd_`ctr'_pid=  acd_`ctr'*partisanship
 label var acd_`ctr'_pid "Amount correct = `temp' * Democrat"
}
drop acd_1 acd_1_pid

gen propdk=assignedy
replace propdk=0 if treatmentnum==1 | treatmentnum==2
label var propdk "Proportion correct response payment for Don't Know response (0, .2, .25, 33)"

gen propdk_pid=propdk*partisanship
label var propdk_pid "Prop. payment for DK * Democrat"

tab propdk, gen(dummyfracpay_)
drop dummyfracpay_1
label var dummyfracpay_2 "Prop. payment for DK=.20"
label var dummyfracpay_3 "Prop. payment for DK=.25"
label var dummyfracpay_4 "Prop. payment for DK=.33"

gen dummyfracpay_2_pid=dummyfracpay_2*partisanship 
gen dummyfracpay_3_pid=dummyfracpay_3*partisanship 
gen dummyfracpay_4_pid=dummyfracpay_4*partisanship 

label var dummyfracpay_2_pid "Prop. payment for DK=.20 * Democrat"
label var dummyfracpay_3_pid "Prop. payment for DK=.25 * Democrat"
label var dummyfracpay_4_pid "Prop. payment for DK=.33 * Democrat"

* Non-placebo questions with pre-treatment PID gaps
regress directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* if partygapsforquestion==1 & isplacebo==0, cluster(userid)
test post_fee_pid ==post_feedk_pid
local testa=r(p)/2
outreg partisanship post_fee_pid post_feedk_pid post_fee post_feedk using Tables\Table4_MTURK12_PooledTreatmentAnalysis.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases, all questions with partisan gap among pre-treatment cases, p<.10") addnote("Note: Source: 2012 MTURK study. Includes only Democrats and Republicans. Robust standard errors, clustered by respondent. F-test p-values are one-tailed. Question fixed effects not reported. Number of participants is `e(N_clust)'") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa') replace

* Not in table, discussed in text: Scale effects excluding Obama unemployment item.
regress directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* if questionid~=1 & partygapsforquestion==1 & isplacebo==0, cluster(userid)
test post_fee_pid ==post_feedk_pid
local testa=r(p)/2

* Non-placebo questions with pre-treatment PID gaps, tobit
tobit directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* if partygapsforquestion==1 & isplacebo==0, cluster(userid) ll(0) ul(1)
test post_fee_pid ==post_feedk_pid
local testa=r(p)/2
*test post_fee_pid ==post_feedk_pid
*local testb=r(p)/2
outreg partisanship post_fee_pid post_feedk_pid post_fee post_feedk using Tables\Table4_MTURK12_PooledTreatmentAnalysis.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases, all questions with partisan gap among pre-treatment cases, p<.10") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa') append

* Specification exploiting difference in amount paid for correct response
regress directed_sliderresponse partisanship acd_*_pid dummyfracpay_*_pid acd_2-acd_6 dummyfracpay_2-dummyfracpay_4 quid_* if partygapsforquestion==1 & isplacebo==0, cluster(userid)
outreg partisanship acd_*_pid dummyfracpay_*_pid acd_2-acd_6 dummyfracpay_2-dummyfracpay_4 using Tables\Table4_MTURK12_PooledTreatmentAnalysis.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases, all questions with partisan gap among pre-treatment cases, p<.10") append

*****************************************************************
*Appendix Table 2: Question by question analysis
*****************************************************************

* For appendix, analysis broken down by question
levelsof questionid if partygapsforquestion==1 & isplacebo==0, local(questions)
qui foreach qstn of local questions {
 local clabel : label (questionid) `qstn'
 no di "`clabel'"
 regress directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk if questionid==`qstn', robust
 test post_fee_pid ==post_feedk_pid
 local testa=r(p)/2
 *Calculate pct of partisan gap eliminated by payment for correct.
 local gapelim = 100*abs(_b[post_fee_pid]/_b[partisanship])
 if (_b[post_fee_pid] > 0) {
  *In one case, coefficient suggests payment increased gap; if so, just note that.
  local gapelim = -9999
 } 
 *Calculate pct of partisan gap eliminated by payment for DK and for correct.
 local gapelimdk = 100*abs(_b[post_feedk_pid]/_b[partisanship])

 if `qstn'==1 {
  outreg partisanship post_fee_pid post_feedk_pid post_fee post_feedk using Tables\AppendixTable2_MTurk12_PayForCorrectByQuestion_PartyDivergenceEffects.out, se bracket rdec(3) 3aster ctitle("`clabel'") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa', "Percentage of Partisan Gap Eliminated by Payment for Correct Response",`gapelim', "Percentage of Partisan Gap Eliminated by Payment for DK and Correct Response",`gapelimdk') replace

 }
 else {
  outreg partisanship post_fee_pid post_feedk_pid post_fee post_feedk using Tables\AppendixTable2_MTurk12_PayForCorrectByQuestion_PartyDivergenceEffects.out, se bracket rdec(3) 3aster ctitle("`clabel'") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa', "Percentage of Partisan Gap Eliminated by Payment for Correct Response",`gapelim', "Percentage of Partisan Gap Eliminated by Payment for DK and Correct Response",`gapelimdk') append

 }
  
}

*****************************************************************
*Online Appendix Table 5: Within Person Design
*****************************************************************

regress directed_sliderresponse lag_directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* if partygapsforquestion==1 & isplacebo==0, cluster(userid)
test post_fee_pid - post_feedk_pid = 0
local testa=`r(p)'/2

outreg lag_directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk using tables\OnlineAppendixTable5_MTURK12_WithinPersonDesign.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases asked in pre, all questions with partisan gap among pre-treatment cases, p<.10") addnote("Note: Source: 2012 MTURK study. Includes only Democrats and Republicans. Robust standard errors, clustered by respondent. F-test p-values are one-tailed. Question fixed effects not reported. Number of participants is `e(N_clust)'") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa') replace

regress directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* if partygapsforquestion==1 & isplacebo==0 & lag_directed_sliderresponse~=., cluster(userid)
test post_fee_pid - post_feedk_pid = 0
local testa=`r(p)'/2

outreg partisanship post_fee_pid post_feedk_pid post_fee post_feedk using tables\OnlineAppendixTable5_MTURK12_WithinPersonDesign.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases asked in pre, all questions with partisan gap among pre-treatment cases, p<.10") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa') append

*****************************************************************
*Online Appendix Table 6: Excluding Cheaters.
*****************************************************************
gen cheat=(lookedupanswer=="Yes")
egen anycheat=max(cheat), by(userid)

* Non-placebo questions with pre-treatment PID gaps
regress directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* if partygapsforquestion==1 & isplacebo==0, cluster(userid)
test post_fee_pid ==post_feedk_pid
local testa=r(p)/2
outreg partisanship post_fee_pid post_feedk_pid post_fee post_feedk using Tables\OnlineAppendixTable6_MTURK12_Regressionpayeffectsonpidgapscheating.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases, all questions with partisan gap among pre-treatment cases, p<.10") addnote("Note: Source: 2012 MTURK study. Includes only Democrats and Republicans. Robust standard errors, clustered by respondent. F-test p-values are one-tailed. Question fixed effects not reported. Number of participants is `e(N_clust)'") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa') replace

* Non-placebo questions with pre-treatment PID gaps
regress directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* if partygapsforquestion==1 & isplacebo==0 & anycheat==0, cluster(userid)
test post_fee_pid ==post_feedk_pid
local testa=r(p)/2
outreg partisanship post_fee_pid post_feedk_pid post_fee post_feedk using Tables\OnlineAppendixTable6_MTURK12_Regressionpayeffectsonpidgapscheating.out, se bracket rdec(3) 3aster ctitle("Excluding people who report any cheating") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa') append

*****************************************************************
*Online Appendix Table 7: A single observation per individual
*****************************************************************

*Store variable labels for after collapse
foreach v of var directed_sliderresponse  partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* {
 local l`v' : variable label `v'
 if `"`l`v''"' == "" {
  local l`v' "`v'"
 }
}

keep if partygapsforquestion==1 & isplacebo==0
collapse (mean) directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk (max) quid_*, by(userid)

foreach v of var directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* {
 label var `v' "`l`v''"
}

regress directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_*
test post_fee_pid ==post_feedk_pid
local testa=`r(p)'/2
outreg using tables\OnlineAppendixTable7_MTURK12_Regressionpayeffectsonpidgapsoneobsperperson.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases, all questions with partisan gap among pre-treatment cases, p<.10")  addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem. one-tailed", `testa') replace
restore
clear

*****************************************************************
*Online Appendix Table 4: Including leaners.
*****************************************************************

* Note: Core Analysis is among those who are Dems/Republicans (variable partisanship~=.)
* So we can use the same code as above, we just reload the raw data and swap in the partisanship measure with leaners for the old measure
use MTurk12\MTURK2012PublicReplicationDataset.dta if withlean_partisanship~=.
replace partisanship=withlean_partisanship

/* This code is what is used to calculate appropriate party gaps to figure out what order questions should be listed in */
gen partygappretreatment=.
levelsof questionid, local(questions)
qui foreach qstn of local questions {
 regress directed_sliderresponse partisanship if posttreatment==0 & questionid==`qstn'
 replace partygappretreatment= _b[partisanship] if questionid==`qstn'
}
table questionid, c(mean partygappretreatment)

rename questionid temp
recode temp (118=1) (112=2) (109=3) (113=4) (111=5) (116=6) (119=7) (114=8) (110=9) (117=10) (115=11) (120=12) (*=.), gen(questionid)
label define newquestlabel 1 "Obama Unemployment" 2 "Bush II Unemployment" 3 "Defense Spending" 4 "Obama Vote 08" 5 "Iraq deaths % Black" 6 "Medicaid Spending" 7 "TARP % Paid Back" 8 "Global Warming Amount" 9 "Iraq deaths" 10 "Debt Service Spending" 11 "Placebo: Mantle home runs '61" 12 "Foreign Born %" 
label values questionid newquestlabel
table questionid, c(mean partygappretreatment)
gen isplacebo=questionid==11

levelsof questionid, local(questions)
qui foreach qstn of local questions {
 ttest directed_sliderresponse if posttreatment==0 & questionid==`qstn', by(partisanship)
 local diff=`r(mu_2)'-`r(mu_1)'
 local num=`r(N_1)'+`r(N_2)'
 local avg=(`r(N_1)'*`r(mu_1)' + `r(N_2)'*`r(mu_2)')/(`r(N_1)'+`r(N_2)')
 matrix temprow=`qstn',`r(mu_2)',`r(mu_1)',`diff',`r(p_l)',`num',`avg'
 if `qstn'==1 {
  matrix dofmean=temprow
 }
 else {
  matrix dofmean=dofmean\temprow
 }
}
svmat dofmean
label values dofmean1 newquestlabel 
label var dofmean1 "Question"
label var dofmean2 "Mean Democratic Response"
label var dofmean3 "Mean Republican Response"
label var dofmean4 "Diff., Dem. - Rep."
label var dofmean5 "P-value of Diff., one-tailed"
label var dofmean6 "N"
label var dofmean7 "Overall Avg."

*We also want to save the parameter estimated here, the difference in means, in each of the questions. So do it here
*(We also save whether p<.10)
gen divergenceparameter=.
gen partygapsforquestion=0
gen meanscore=.

*Based on this analysis, we have certain questions for which we don't observe partisan gaps
*Create a filter variable that is 0 if there are no gaps
forvalues ctr=1(1)12 {
 replace divergenceparameter=dofmean4[`ctr'] if questionid==dofmean1[`ctr']
 replace partygapsforquestion=1 if dofmean5[`ctr']<.10 &  questionid==dofmean1[`ctr']
 replace meanscore=dofmean7[`ctr'] if questionid==dofmean1[`ctr']
 }
drop dofmean*
label var divergenceparameter "PID gap in pre"
label var partygapsforquestion "Question has p<.10 party gap in pre"
table questionid, c(mean divergenceparameter mean partygapsforquestion)

preserve

gen x=1 
egen numtimesasked=sum(x), by(userid questionid)
drop x

keep if (posttreatment==1 & treatmentnum~=4)

replace directed_sliderresponse=meanscore if responsedontknow==1 & posttreatment==1 & (treatmentnum==3 | treatmentnum==4)

gen post_flat=posttreatment*(treatmentnum==1)
gen post_fee=posttreatment*(treatmentnum==2)
gen post_feedk=posttreatment*(treatmentnum==3)
gen post_flatdk=posttreatment*(treatmentnum==4)

gen post_fee_pid=post_fee*partisanship
gen post_feedk_pid=post_feedk*partisanship
gen post_flatdk_pid=post_flatdk*partisanship

label var partisanship "Democrat (1=Yes, 0=Republican)"

label var post_flat "Flat fee for survey"
label var post_fee "Payment for Correct Response"
label var post_feedk "Payment for DK and Correct Response"
label var post_flatdk "Flat fee for survey with DK"

label var post_fee_pid "Payment Correct * Democrat"
label var post_feedk_pid "Payment DK and Correct * Democrat"
label var post_flatdk_pid "Flat fee with DK * Democrat"

gen amountcorrect=assignedx
replace amountcorrect=0 if treatmentnum==1
label var amountcorrect "Amount paid for correct (0 to $1)"

gen amountcorrect_pid=amountcorrect*partisanship
label var amountcorrect_pid "Amount correct * Democrat"

tab amountcorrect, gen(acd_)
forvalues ctr=1(1)6 {
 summ amountcorrect if acd_`ctr'==1
 local temp= string(r(mean),"%9.2f")
 label var acd_`ctr' "Amount correct = `temp'"
 gen acd_`ctr'_pid=  acd_`ctr'*partisanship
 label var acd_`ctr'_pid "Amount correct = `temp' * Democrat"
}
drop acd_1 acd_1_pid

gen propdk=assignedy
replace propdk=0 if treatmentnum==1 | treatmentnum==2
label var propdk "Proportion correct response payment for Don't Know response (0, .2, .25, 33)"

gen propdk_pid=propdk*partisanship
label var propdk_pid "Prop. payment for DK * Democrat"

tab propdk, gen(dummyfracpay_)
drop dummyfracpay_1
label var dummyfracpay_2 "Prop. payment for DK=.20"
label var dummyfracpay_3 "Prop. payment for DK=.25"
label var dummyfracpay_4 "Prop. payment for DK=.33"

gen dummyfracpay_2_pid=dummyfracpay_2*partisanship 
gen dummyfracpay_3_pid=dummyfracpay_3*partisanship 
gen dummyfracpay_4_pid=dummyfracpay_4*partisanship 

label var dummyfracpay_2_pid "Prop. payment for DK=.20 * Democrat"
label var dummyfracpay_3_pid "Prop. payment for DK=.25 * Democrat"
label var dummyfracpay_4_pid "Prop. payment for DK=.33 * Democrat"

* Non-placebo questions with pre-treatment PID gaps
regress directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* if partygapsforquestion==1 & isplacebo==0, cluster(userid)
test post_fee_pid ==post_feedk_pid
local testa=r(p)/2
outreg partisanship post_fee_pid post_feedk_pid post_fee post_feedk using Tables\OnlineAppendixTable4_MTURK12_Regressionpayeffectsonpidgapswithlean.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases, all questions with partisan gap among pre-treatment cases, p<.10") addnote("Note: Source: 2012 MTURK study. Includes only Democrats and Republicans. Robust standard errors, clustered by respondent. F-test p-values are one-tailed. Question fixed effects not reported. Number of participants is `e(N_clust)'") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa') replace

* Non-placebo questions with pre-treatment PID gaps, tobit
tobit directed_sliderresponse partisanship post_fee_pid post_feedk_pid post_fee post_feedk quid_* if partygapsforquestion==1 & isplacebo==0, cluster(userid) ll(0) ul(1)
test post_fee_pid ==post_feedk_pid
local testa=r(p)/2
*test post_fee_pid ==post_feedk_pid
*local testb=r(p)/2
outreg partisanship post_fee_pid post_feedk_pid post_fee post_feedk using Tables\OnlineAppendixTable4_MTURK12_Regressionpayeffectsonpidgapswithlean.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases, all questions with partisan gap among pre-treatment cases, p<.10") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa') append

* Specification exploiting difference in amount paid for correct response
regress directed_sliderresponse partisanship acd_*_pid dummyfracpay_*_pid acd_2-acd_6 dummyfracpay_2-dummyfracpay_4 quid_* if partygapsforquestion==1 & isplacebo==0, cluster(userid)
outreg partisanship acd_*_pid dummyfracpay_*_pid acd_2-acd_6 dummyfracpay_2-dummyfracpay_4 using Tables\OnlineAppendixTable4_MTURK12_Regressionpayeffectsonpidgapswithlean.out, se bracket rdec(3) 3aster ctitle("Post-treatment cases, all questions with partisan gap among pre-treatment cases, p<.10") append



