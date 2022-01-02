set more off

* Note, core sample (main analysis is those who are either Democrats or Republicans, that is those for whom pid2~=.)

use CCES2008\CCES2008PublicReplicationDataset.dta if pid2~=.

* Demographics for footnote
preserve
sort userid
drop if userid==userid[_n-1]
summ c_*
restore

*Gold and Bangladesh question correct rates
gen correct=0
replace correct=1 if response>=800 & response<900 & (questionid==11)
replace correct=1 if response==1971 & (questionid==12)
table questionid treatment if questionid==11 | questionid==12, c(mean correct)
ttest correct if questionid==11, by(treatment)
ttest correct if questionid==12, by(treatment)
drop correct

*****************************************************************
*Table 1: Assessing partisan divergence by question in control condition
*****************************************************************

gen controlgap=.
gen controlgaptstat=.
gen controlpval=.

levelsof questionid if directionalresponse~=., local(quests)
foreach qstn of local quests {
 ttest directionalresponse if treatment==0 & questionid==`qstn', by(pid2)
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
preserve
keep in 1/12
keep dofmean*
export excel using "Tables/Table1_CCES08_DifferenceInPartyMeans_Control_ByQuestion", firstrow(varlabels) replace
restore

*We also want to save the parameter estimated here, the difference in means, in each of the questions. So do it here
*(We also save whether p<.10)
gen divergenceparameter=.
gen partygapsforquestion=0

*Based on this analysis, we have certain questions for which we don't observe partisan gaps
*Create a filter variable that is 0 if there are no gaps
forvalues ctr=1(1)12 {
 replace divergenceparameter=dofmean4[`ctr'] if questionid==dofmean1[`ctr']
 replace partygapsforquestion=1 if dofmean5[`ctr']<.10 &  questionid==dofmean1[`ctr']
 }
drop dofmean*
label var divergenceparameter "PID gap in pre"
label var partygapsforquestion "Question has p<.10 party gap in pre"
table questionid, c(mean divergenceparameter mean partygapsforquestion)

*****************************************************************
*Table 2: difference in partisan divergence, paid for correct (no DK option) versus control.
*****************************************************************

label var treatment "Payment for Correct Response"
label var pid2 "Democrat (1=Yes, 0=Republican)"

* Now, calculate diff in diff, treatment - control for cases with t-stat>2
tab questionid, gen(qdummy_)
drop qdummy_1

regress directionalresponse pid2 treat_pid2 treatment qdummy_* if partygapsforquestion==1 & isplacebo==0, robust cluster(userid)
outreg pid2 treat_pid2 treatment using Tables\Table2_CCES08_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") addnote("Note: Source: 2008 CCES. Includes only Democrats and Republicans. Robust standard errors, clustered by respondent. Question fixed effects not reported.") replace

regress directionalresponse pid2 treat_pid2 treatment c_*  qdummy_* if partygapsforquestion==1 & isplacebo==0, robust cluster(userid)
outreg pid2 treat_pid2 treatment c_* using Tables\Table2_CCES08_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") append

* Effects by Political interest
gen pid_int=pol_int_bin*pid2
label var pid_int "Interest * Democrat"
gen treat_pid2_int=pol_int_bin*treat_pid2
label var treat_pid2_int "Pay Correct * Interest * Democrat"
gen treatment_int=pol_int_bin*treatment
label var treatment_int "Pay Correct * Interest"

regress directionalresponse pid2 pid_int treat_pid2 treat_pid2_int treatment treatment_int pol_int_bin qdummy_* if partygapsforquestion==1 & isplacebo==0, robust cluster(userid)
outreg pid2 pid_int treat_pid2 treat_pid2_int treatment treatment_int pol_int_bin using Tables\Table2_CCES08_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") append

*****************************************************************
*Appendix Table 1: difference in partisan divergence, paid for correct (no DK option) versus control.
*****************************************************************

levelsof questionid if partygapsforquestion==1 & isplacebo==0, local(questions)
qui foreach qstn of local questions {
 local clabel : label (questionid) `qstn'
 no di "`clabel'"
 regress directionalresponse pid2 treat_pid2 treatment if questionid==`qstn', robust
 *Calculate pct of partisan gap eliminated by payment.
 local gapelim = 100*abs(_b[treat_pid2]/_b[pid2])
 if `qstn'==1 {
  outreg using Tables\AppendixTable1_CCES08_PayForCorrectByQuestionNoDK_PartyDivergenceEffects.out, se bracket rdec(3) 3aster ctitle("`clabel'") addstat("Percentage of Partisan Gap Eliminated by Payment for Correct Response",`gapelim') replace
 }
 else {
  outreg using Tables\AppendixTable1_CCES08_PayForCorrectByQuestionNoDK_PartyDivergenceEffects.out, se bracket rdec(3) 3aster ctitle("`clabel'") addstat("Percentage of Partisan Gap Eliminated by Payment for Correct Response",`gapelim') append
 }
}

*****************************************************************
*Table 5: Effect of evals on voting
*****************************************************************

preserve
keep if partygapsforquestion==1 & isplacebo==0
collapse (mean) directionalresponse (count) n=directionalresponse, by(treatment PresVoteDemRep pid2 userid)
gen treat_direct=treatment* directionalresponse
label var directionalresponse "Average scale score (1= Most Dem.)"
label var treat_direct "Pay Correct * Average scale score"
label var treatment "Pay Correct condition"

regress PresVoteDemRep directionalresponse treatment treat_direct, robust
outreg directionalresponse treatment treat_direct using Tables\Table5_CCES08_ScaleScoreToVoteByTreatment.out, se bracket rdec(3) 3aster ctitle("Presidential Vote (1=Dem., 0=Rep., .=Else)") addnote("Note: Source: 2008 CCES. Includes only Democrats and Republicans. Scale score is pooled across 8 questions non-placebo questions with partisan gaps in control condition.") replace
summ PresVoteDemRep directionalresponse treatment treat_direct if e(sample)

restore

*****************************************************************
*Online Appendix Table OA1: Difference in partisan divergence, paid for correct (no DK option) versus control, weighted versus unweighted
*****************************************************************

regress directionalresponse pid2 treat_pid2 treatment qdummy_* if partygapsforquestion==1 & isplacebo==0 [aweight=weight], robust cluster(userid) 
outreg pid2 treat_pid2 treatment using Tables\OnlineAppendixTable1_CCES08_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") addnote("Note: Source: 2008 CCES. Includes only Democrats and Republicans. Robust standard errors, clustered by respondent. Question fixed effects not reported.") replace

regress directionalresponse pid2 treat_pid2 treatment c_*  qdummy_* if partygapsforquestion==1 & isplacebo==0 [aweight=weight], robust cluster(userid)
outreg pid2 treat_pid2 treatment c_* using Tables\OnlineAppendixTable1_CCES08_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") append

regress directionalresponse pid2 pid_int treat_pid2 treat_pid2_int treatment treatment_int pol_int_bin qdummy_* if partygapsforquestion==1 & isplacebo==0 [aweight=weight], robust cluster(userid)
outreg pid2 pid_int treat_pid2 treat_pid2_int treatment treatment_int pol_int_bin using Tables\OnlineAppendixTable1_CCES08_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") append

regress directionalresponse pid2 treat_pid2 treatment qdummy_* if partygapsforquestion==1 & isplacebo==0, robust cluster(userid)
outreg pid2 treat_pid2 treatment using Tables\OnlineAppendixTable1_CCES08_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") addnote("Note: Source: 2008 CCES. Includes only Democrats and Republicans. Robust standard errors, clustered by respondent. Question fixed effects not reported.") append

regress directionalresponse pid2 treat_pid2 treatment c_*  qdummy_* if partygapsforquestion==1 & isplacebo==0, robust cluster(userid)
outreg pid2 treat_pid2 treatment c_* using Tables\OnlineAppendixTable1_CCES08_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") append

regress directionalresponse pid2 pid_int treat_pid2 treat_pid2_int treatment treatment_int pol_int_bin qdummy_* if partygapsforquestion==1 & isplacebo==0, robust cluster(userid)
outreg pid2 pid_int treat_pid2 treat_pid2_int treatment treatment_int pol_int_bin using Tables\OnlineAppendixTable1_CCES08_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") append

*****************************************************************
*Online Appendix Table OA3: Difference in partisan divergence, paid for correct (no DK option) versus control, with partisan leaners
*****************************************************************

* Note, core sample (main analysis is those who are either Democrats or Republicans, that is those for whom pid2~=.)
clear
use CCES2008\CCES2008PublicReplicationDataset.dta if  withlean_pid2~=.
* to reuse the code above, I simply recreate all the relevant interactions involving PID2 with the new measure
replace pid2=withlean_pid2
replace treat_pid2=pid2*treatment
label var pid2 "Democrat (with leaners; 1=Yes, 0=Republican)"
label var treatment "Payment for Correct Response"

gen controlgap=.
gen controlgaptstat=.
gen controlpval=.

levelsof questionid if directionalresponse~=., local(quests)
foreach qstn of local quests {
 ttest directionalresponse if treatment==0 & questionid==`qstn', by(pid2)
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

*Based on this analysis, we have certain questions for which we don't observe partisan gaps
*Create a filter variable that is 0 if there are no gaps
forvalues ctr=1(1)12 {
 replace divergenceparameter=dofmean4[`ctr'] if questionid==dofmean1[`ctr']
 replace partygapsforquestion=1 if dofmean5[`ctr']<.10 &  questionid==dofmean1[`ctr']
 }
drop dofmean*
label var divergenceparameter "PID gap in pre"
label var partygapsforquestion "Question has p<.10 party gap in pre"

* Now, calculate diff in diff, treatment - control for cases with t-stat>2
tab questionid, gen(qdummy_)
drop qdummy_1

regress directionalresponse pid2 treat_pid2 treatment qdummy_* if partygapsforquestion==1 & isplacebo==0, robust cluster(userid)
outreg pid2 treat_pid2 treatment using Tables\OnlineAppendixTable3_CCES08_Regressionpayeffectsonpidgapswithlean.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") addnote("Note: Source: 2008 CCES. Includes only Democrats and Republicans. Robust standard errors, clustered by respondent. Question fixed effects not reported.") replace

regress directionalresponse pid2 treat_pid2 treatment c_*  qdummy_* if partygapsforquestion==1 & isplacebo==0, robust cluster(userid)
outreg pid2 treat_pid2 treatment c_* using Tables\OnlineAppendixTable3_CCES08_Regressionpayeffectsonpidgapswithlean.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") append

* Effects by Political interest
gen pid_int=pol_int_bin*pid2
label var pid_int "Interest * Democrat"
gen treat_pid2_int=pol_int_bin*treat_pid2
label var treat_pid2_int "Pay Correct * Interest * Democrat"
gen treatment_int=pol_int_bin*treatment
label var treatment_int "Pay Correct * Interest"

regress directionalresponse pid2 pid_int treat_pid2 treat_pid2_int treatment treatment_int pol_int_bin qdummy_* if partygapsforquestion==1 & isplacebo==0, robust cluster(userid)
outreg pid2 pid_int treat_pid2 treat_pid2_int treatment treatment_int pol_int_bin using Tables\OnlineAppendixTable3_CCES08_Regressionpayeffectsonpidgapswithlean.out, se bracket rdec(3) 3aster ctitle("All non-placebo cases with p<.10 partisan gaps in control") append

