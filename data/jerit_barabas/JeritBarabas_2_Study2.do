*Replication file for "Partisan Perceptual Bias and the Information Environment"
*J. Jerit and J. Barabas
*12-18-11
*Study 2

*****************
*VARIABLE CODING*
*****************

*pb=condition identifier (0=control, 1=dempos, 2=demneg, 3=reppos, 4=repneg)
*pb_dp= dummy variable for Democratic-Positive condition (1=dempos, 0=otherwise)
*pb_dn= dummy variable for Democratic-Negative condition (1=demneg, 0=otherwise)
*pb_rp= dummy variable for Republican-Positive condition (1=reppos, 0=otherwise)
*pb_rn= dummy variable for Republican-Negative condition (1=repneg, 0=otherwise)

*pbk_dp=partisan bias knowledge response variable, Democratic-Positive (1=correct on Democratic-Positive, 0=othwerwise)
*pbk_dn=partisan bias knowledge response variable, Democratic-Negative (1=correct on Democratic-Negative, 0=othwerwise)
*pbk_rp=partisan bias knowledge response variable, Republican-Positive (1=correct on Republican-Positive, 0=othwerwise)
*pbk_rn=partisan bias knowledge response variable, Republican-Negative (1=correct on Republican-Negative, 0=othwerwise)

*femA=gender (1=female, 0=not female)
*femNA=gender missing (1=gender not recorded in voter file, 0=gender recorded)
*blk=black (1=african-american, 0=not african-american)
*his=hispanic (1=hispanic, 0=not hispanic)
*raceNA=missing race (1=race not in voter file, 0=race in voter file)
*age=age in years (87=87 years old, 18=18 years old)
*dem=Democrat (1=Democrat, 0=not Democrat)
*rep=Republican (1=Republican, 0=not Republican)
*v10=turnout in the 2010 general election (1=voted, 0=otherwise)
*v10NA=missing data for turnout in the 2010 general election(1=turnout not in voter file, 0=otherwise)
*v10p=turnout in the 2010 primary election (1=voted, 0=otherwise)
*v10pNA=missing data for turnout in the 2010 primary  election(1=turnout not in voter file, 0=otherwise)
*v08==turnout in the 2008 general election (1=voted, 0=otherwise)
*v08NA=missing data for turnout in the 2008 general election(1=turnout not in voter file, 0=otherwise)
*v08p=turnout in the 2008 primary election (1=voted, 0=otherwise)
*v08pNA=missing data for turnout in the 2008 primary  election(1=turnout not in voter file, 0=otherwise)
*v08ppp =turnout in the 2008 presidential preference election (1=voted, 0=otherwise)
*v08pppNA =missing data in the 2008 presidential preference election (1=turnout not in voter file, 0=otherwise)

**************
*LOADING DATA*
**************

clear
set more off
version 11.2
cd "C:\PB_JeritBarabas_JOP\JeritBarabas_JOP_ReplicationFiles\JeritBarabas_JOP_Data" 
use JeritBarabas_1_LabData_Study2.dta 

*********************
*RANDOMIZATION CHECK*
*********************
tab pb  
mlogit pb femA femNA blk his raceNA  age dem rep  v10 v10NA v10p v10pNA v08 v08NA v08p v08pNA v08ppp v08pppNA, base(0)

*****************************
*AVERAGE LEVELS OF KNOWLEDGE*
*****************************

sum  pbk_dp 
sum  pbk_dn  
sum  pbk_rp  
sum  pbk_rn 

*******
*MEANS*
*******

*Table A6, Means

/*Dem pos; combined ctrl group*/
ttest pbk_dp if dem==1, by (pb_dp)

/*Dem neg; can combine 0 and 1 (not 3/4)*/
ttest pbk_dn if (pb==0 | pb==1| pb==2) & dem==1, by (pb_dn)

/*Rep pos; combined ctrl group*/
ttest pbk_rp if rep==1 , by (pb_rp)

/*Rep neg; smaller ctrl group; can't combine any of the other groups with placebo*/
ttest pbk_rn if (pb==0 | pb==4) & rep==1 , by (pb_rn)

********
*MODELS*
********

*Model version of previous t-tests 
*Note: if demographic term drops b/c of insufficient variation, it is excluded from the model
*Note: models use as many observations as possible based upon tests (at the end of the file) to see if control group can be combined with groups treated on different facts

*******************************
*Figure 2, Democratic-Positive*
*******************************
/*Dem pos; combined ctrl group*/
/*Okay to use combined control group with dp because other groups indistinguishable from placebo on pbk_dp*/

dprobit pbk_dp pb_dp age blk his femA femNA v10 v10NA v10p v10pNA v08 v08NA v08p v08pNA v08ppp v08pppNA if dem==1  /* Figure 2, DemPos (drop raceNA b/c no variation)*/
probit pbk_dp pb_dp age blk his femA femNA v10 v10NA v10p v10pNA v08 v08NA v08p v08pNA v08ppp v08pppNA if dem==1  /* Table A8, Democratic Positive (drop raceNA b/c no variation)*/
dprobit pbk_dp pb_dp  if dem==1  /* endnote 26 */

*******************************
*Figure 2, Democratic-Negative*
*******************************
/*Can combine 0 and 1 (not 3/4)*/
dprobit pbk_dn pb_dn age blk femA v10 v10p  v08  v08p v08pNA v08ppp v08pppNA if (pb==0 | pb==1| pb==2) & dem==1   /* Figure 2, Democratic-Negative */
probit pbk_dn pb_dn age blk femA v10 v10p  v08  v08p v08pNA v08ppp v08pppNA if (pb==0 | pb==1| pb==2) & dem==1    /* Table A8, Democratic-Negative */
dprobit pbk_dn pb_dn if (pb==0 | pb==1| pb==2) & dem==1    /* endnote 27 */

*******************************
*Figure 2, Republican-Positive*
*******************************
/*Okay to use combined control group with rp because other groups indistinguishable from placebo on pbk_rp*/
dprobit pbk_rp pb_rp age his femA v10 v10p v08 v08p v08pNA v08ppp v08pppNA if rep==1   /* Figure 2, Republican-Positive, drop raceNA, femNA, blk, v10NA, v10pNA, v08NA*/
probit pbk_rp pb_rp age his femA v10 v10p v08 v08p v08pNA v08ppp v08pppNA if rep==1  /* Table A8, Republican-Positive, drop raceNA, femNA, blk, v10NA, v10pNA, v08NA*/
dprobit pbk_rp pb_rp  if rep==1  /* endnote 26 */

*******************************
*Figure 2, Republican-Negative*
*******************************
/*Rep neg; smaller ctrl group; can't combine any of the other groups with placebo*/
dprobit pbk_rn pb_rn age femA v10p v08 v08p v08ppp if (pb==0 | pb==4) & rep==1   /* Figure 2, Republican-Negative */
probit pbk_rn pb_rn age femA v10p v08 v08p v08ppp if (pb==0 | pb==4) & rep==1   /* Table A8, Republican-Negative */
dprobit pbk_rn pb_rn if (pb==0 | pb==4) & rep==1   /* endnote 27 */

*****************************************************
*Analyses for combining groups for augmented control*
*****************************************************

* pb_control=pb_0
* pb_dp=pb_1
* pb_dn=pb_2
* pb_rp=pb_3
* pb_rn=pb_4

*separating effects into each condition, all relative to omitted control
probit pbk_dp pb_dp pb_dn pb_rp pb_rn  /* other T groups indistinguishable from C; can combine*/
probit pbk_dn pb_dp pb_dn pb_rp pb_rn  /* only group 1 is indistinguishable from C; cannot combine 3 or 4*/
probit pbk_rp pb_dp pb_dn pb_rp pb_rn  /* other T groups indistinguishable from C; can combine*/
probit pbk_rn pb_dp pb_dn pb_rp pb_rn  /* other T groups different from control; cannot combine*/




