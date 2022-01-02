*Replication file for "Partisan Perceptual Bias and the Information Environment"
*J.Jerit and J.Barabas
*12-19-2011
*Study 1

*****************
*VARIABLE CODING*
*****************

*IEnumb=survey identifer (n=43, 1 to 45, skips 4 and 13)
*IEqid=unique question identifier (n=205, skips questions 23 and 24 [items asked only of Democrats] and 58, 59, 60, 61, and 62 [asked only of teens and without partisanship assessed])
*masterid=unique identifer for individual

*cs=count of the correct stories for survey question
*dempos=Democratic-Positive issue (1=Democratic-Positive, 0=Not Democratic-Positive)
*demneg=Democratic-Negative issue (1=Democratic-Negative, 0=Not Democratic-Negative)
*reppos=Republican-Positive issue (1=Republican-Positive, 0=Not Republican-Positive)
*repneg=Republican-Negative issue (1=Republican-Negative, 0=Not Republican-Negative)

*policyspecific=identifier for policy-specific question (1=policy-specific; 0=otherwise)
*answerchoices=number of answer choices in knowledge question (101=open-ended)
*ac =probability of guessing rendering correct answer based upon number of answer choices in question (.01=open ended=1/100, .33=three answer choices=1/3)
*ajps09=identifer for case in Barabas and Jerit 2009, AJPS article (1=in 2009 AJPS article, 0=not in 2009 article)


*know=political knowledge (1=correct response, 0=otherwise)

*Iraq911=topic concerns Iraq or 9/11 (1=concerns Iraq/911, 0=otherwise)
*ForeignPolicy=topic concerns foreign policy (1=foreign policy, 0=otherwise)
*HealthCare=topic concerns healthcare (1=health care, 0=otherwise) 

*demXdemposXcs=interaction of dem, dempos, and cs
*demXdemnegXcs=interaction of dem, demneg, and cs
*repXrepposXcs=interaction of rep, reppos, and cs 
*repXrepnegXcs=interaction of rep, repneg, and cs 
*demXdempos=interaction of dem and dempos
*demXdemneg=interaction of dem and demneg
*repXreppos=interaction of rep and repneg
*repXrepneg=interaction of dempos and cs
*demposXcs=interaction of dempos and cs
*repposXcs=interaction of reppos and cs
*demnegXcs=interaction of demneg and cs
*repnegXcs=interactoin of repneg and cs
*demXcs=interaction of dem and cs
*repXcs=inreaction of rep and cs

*dem=dummy variable for Demcocratic identifer (1=Democrat, 0=not Democrat)
*rep=dummy variable for Republican identifier (1=Republican, 0=not Republican)

*eduAM=education (1=high;0=low)
*incAM=income (1=high, 0=low)
*ageAM=age (1=higher/oldest, 0=lower/youngest)
*blkAM=race (1=black, 0=not black)
*femAM=gender (1=female, 0=male)
*IEnumb1-IEnumb45=dummy variable for each survey (numbers not consecutive)

******************************************************
*MEDIA COVERAGE DATA (ENVIRONMENTAL DATA) FOR STUDY 1*
******************************************************
clear
version 11.2
cd "C:\PB_JeritBarabas_JOP\JeritBarabas_JOP_ReplicationFiles\JeritBarabas_JOP_Data" /* INSERT PATH TO YOUR FILE LOCATION HERE */
use JeritBarabas_1_EnvData_Study1.dta

*Obtaining threshold values from environmental data for Table 1
sum cs if dempos==1, detail /*median=8*/
sum cs if demneg==1, detail	/*median=4*/
sum cs if reppos==1, detail	/*median=68 */
sum cs if repneg==1, detail	/*median=24.5 */
sum cs if (repneg==0 & reppos==0 & dempos==0 & demneg==0), detail  /*median=5*/
sum cs, detail /*median=10*/

*Obtaining values for predicted probabilities in Figure 2 (from media file only, not from individual-level which would weight values differently depending upon the number of observations)
sum cs /*ALL ISSUES  205     44.0439    97.31726          0        762, +2sd=239.97  */
sum cs if dempos==1 /* DEMOCRATIC-POSITIVE, 51    17.21569    26.55734       0        141,  +2sd=70.32  */
sum cs if demneg==1 /* DEMOCRATIC-NEGATIVE, 29    21.2069    39.61545        0        171,  +2sd=100.44*/
sum cs if reppos==1 /* REPUBLICAN-POSITIVE, 38    117    167.0541            0        762,  2sd=451.11 */
sum cs if repneg==1 /* REPUBLICAN-NEGATIVE, 12    44.25     50.0729          0        160,  2sd=144.4 */
sum cs if (repneg==0 & reppos==0 & dempos==0 & demneg==0)  /*NONPARTISAN,  75       34.12    84.72419     0   640, 2sd=203.57*/

sum cs if repNV==1 /*  62    80.82258    140.2018          0        762, +2sd=223 */
sum cs if demNV==1 /*  86    17.60465    30.84774          0        171, +2sd=79.28 */

* Low to high coverage ranges for predicted probabilities (never going below 0 because negative count of coverage is not possible)
*Nonpartisan 0 to 203.57
*DemPos: 0 to 70.32
*DemNeg: 0 to 100.44
*RepPos: 0 to 451.11
*RepNeg: 0 to 144.40

*count models predicting media coverage by issue type,w ith fixed effects for surveys
tab IEnumb, gen(i)
nbreg cs dempos demneg reppos repneg policyspecific answerchoices ajps09 i1-i43
di (0.403+ 0.976+0.000+ 0.427)/4 /* average p value=.45) */

*correlations among individual outlets in Study 1 environmental data for non-AJPS2009 portion of sample (only portion of data where it is possible to compare across all sources)
corr cs*
di ((.9625+.7338+.8139+.8331+.9392+.9353+.9767+.7962+.6268)/9)  /* ave=.85 */

***************
*TABLE 1 MEANS*
***************

*NOTE: The variables "dem," "rep," and "know" were not imputed, so they are the same for all five imputed datasets; using PB1.dta only

clear
version 11.2
*cd "C:\JeritBarabas_IE\IEdata\"
cd "C:\PB_JeritBarabas_JOP\JeritBarabas_JOP_ReplicationFiles\JeritBarabas_JOP_Data" 
set more off
use PB1.dta
set seed 32306

ci know if dem==1 /* Table 1, Democrats, Column 1 */
ci know if rep==1 /* Table 1, Republican, Column 1 */

ci  know if dem==1 & dempos==1  /* Table 1, Democrats, Column 2 */
tab IEqid if dempos==1  /* n=51 questions */
ci  know if dem==1 & demneg==1 /* Table 1, Democrats, Column 3 */
tab IEqid if demneg==1  /* n=29 questions */

ci  know if rep==1 & reppos==1 /* Table 1, Republicans, Column 2 */
tab IEqid if reppos==1  & rep==1 /* n=38 questions */
ci  know if rep==1 & repneg==1 /* Table 1, Republicans, Column 3 */
tab IEqid if repneg==1 /* n=12 questions */

ci   know if dem==1 & cs>=10 /* Table 1, Democrats, Column 4 */
ci   know if rep==1 & cs>=10 /* Table 1, Republicans, Column 4 */
tab IEqid if cs>=10 /* n=105 questions */

ci   know if dem==1 & dempos==1 & cs>=8 /* Table 1, Democrats, Column 5 */
tab IEqid if dempos==1 & cs>=8 /* n=26 questions */
ci   know if dem==1 & demneg==1 & cs>=4 /* Table 1, Republicans, Column 5 */
tab IEqid if demneg==1 & cs>=4 /* n=16 questions */

ci   know if rep==1 & reppos==1 & cs>=68 /* Table 1, Democrats, Column 6 */
tab IEqid if reppos==1 & cs>=68 & rep==1  /* n=19 questions */
ci   know if rep==1 & repneg==1 & cs>=24.5 /* Table 1, Republicans, Column 6 */
tab IEqid if repneg==1 & cs>=24.5 & rep==1 /* n=6 questions */

/*cross-partisan patterns*/
ci   know if dem==1 & reppos==1 & cs>=68  /* In text, Democrats on Republican-Positive */
ci   know if dem==1 & repneg==1 & cs>=24.5 /* In text, Democrats on Republican-Negative */
ci   know if rep==1 & dempos==1 & cs>=8  /* In text, Republicans on Democratic-Positive */
ci   know if rep==1 & demneg==1 & cs>=4  /* In text, Republicans on Democratic-Negative */

*Other analyses dscribed in Study 1 of paper

/*Illustrative Examples from Table 1 reported in text*/

ci know if dem==1 & IEqid==124 /* 43% know Clinton proposed tax credits to help people pay for long-term health care for the elderly and disabled */ 
ci know if dem==1 & IEqid==125 /* 25% knew that Clinton was not planning to ask seniors with higher incomes to pay more for Medicare */

ci know if rep==1 & IEqid==124  /*reported in fn; 34% of Republicans answered tax credit correctly */
ci know if rep==1 & IEqid==125  /*reported in fn; 26% of Republicans answered means testing correctly */

ci know if rep==1 & IEqid==31 /* 74% of Republicans know President Bush won the approval of Congress to use military force against Iraq */
ci know if rep==1 & IEqid==33 /* 43% of Republicans could correctly state that the administration did not  the Bush Administration publicly released evidence that Iraq was involved in the planning and funding of the September 11th terrorist attacks */

ci know if dem==1& IEqid==31  /*reported in fn; 58% of Democrats know President Bush won the approval of Congress to use military force against Iraq */
ci know if dem==1 & IEqid==33  /*reported in fn; 45% of Democrats could correctly state that the administration did not  the Bush Administration publicly released evidence that Iraq was involved in the planning and funding of the September 11th terrorist attacks */

/*Number of cases coded as nonpartisan, n=75*/

tab IEqid if repneg==0 & reppos==0 &  demneg==0 &  dempos==0   /*n=75*/

/*Knowledge for non-partisan issues, in text*/

ci know if dem==1 & repneg==0 & reppos==0 &  demneg==0 &  dempos==0  /* average level of knowledge for Democrats at .38 (.38 to .39) on non-partisan issues */
ci know if rep==1 & repneg==0 &  reppos==0 &  demneg==0 &  dempos==0  /* average level of knowledge for Republicans at .42 on non-partisan issues */
ci know if rep==0 & dem==0 &  repneg==0 & reppos==0 &  demneg==0 &  dempos==0 /* average level of knowledge for Independents at .39 on non-partisan issues */

/*Knowledge for non-partisan issues, low coverage, in text*/
ci know if dem==1 & repneg==0 & reppos==0 &  demneg==0 &  dempos==0 & cs<5 /* Democrats = .32 [.31 to .33] on low coverage, non-partisan */
ci know if rep==1 & repneg==0 &  reppos==0 &  demneg==0 &  dempos==0 & cs<5 /* Republicans = .33 [.32 to .34] on low coverage, non-partisan */
ci know if rep==0 & dem==0 &  repneg==0 & reppos==0 &  demneg==0 &  dempos==0  & cs<5 /* Independents = .32 [.31 to .33] on low coverage, non-partisan */

****************************************************************************
*PROBIT MODEL USED TO PRODUCE ESTIMATES FOR FIGURE 2 (AND APPENDIX TABLE 2)*
****************************************************************************

*datasets PB1-PB5.dta, five versions of data with missing demographic responses imputed via Amelia II (see King et al. 2001)

*Main Probit model with clustered standard errors on imputed data
clear
version 11.2
cd "C:\PB_JeritBarabas_JOP\JeritBarabas_JOP_ReplicationFiles\JeritBarabas_JOP_Data" 
set more off
use PB1.dta
set seed 32306
estsimp probit know demXdemposXcs demXdemnegXcs repXrepposXcs repXrepnegXcs demXdempos demXdemneg repXreppos repXrepneg demposXcs repposXcs demnegXcs repnegXcs demXcs repXcs dempos demneg reppos repneg dem rep cs eduAM incAM ageAM blkAM femAM  ajps09 IEnumb1-IEnumb2 IEnumb5-IEnumb12 IEnumb14-IEnumb22 IEnumb24-IEnumb44 , cluster(masterid) mi(PB1 PB2 PB3 PB4 PB5)

*other specifications (delete asterisk and re-run probability simulations)
*Alternate specification 1 (abbreviated AS1 below): controlling for the differences across topics (e.g., likelihood of guessing via answer choices, health, 9/11, foreign policy)
*clear
*version 11.2
*cd "C:\PB_JeritBarabas_JOP\JeritBarabas_JOP_ReplicationFiles\JeritBarabas_JOP_Data" 
*set more off
*use PB1.dta
*set seed 32306
*estsimp probit know demXdemposXcs demXdemnegXcs repXrepposXcs repXrepnegXcs demXdempos demXdemneg repXreppos repXrepneg demposXcs repposXcs demnegXcs repnegXcs demXcs repXcs dempos demneg reppos repneg dem rep cs eduAM incAM ageAM blkAM femAM  ajps09 IEnumb1-IEnumb2 IEnumb5-IEnumb12 IEnumb14-IEnumb22 IEnumb24-IEnumb44 ac   HealthCare ForeignPolicy Iraq911 , cluster(masterid) mi(PB1 PB2 PB3 PB4 PB5)

*************************
*PREDICTED PROBABILITIES*
*************************

**********
*DEMOCRAT*
**********
*dem without coverage and on non-it issue
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 0 dem 1 rep 0 cs 0  
simqi, prval (1)  
*Pr(know=1) mean/se/95%ci:   .3608158      .004333     .3520736     .369016
*AS1:  .3622621     .0042765     .3540945    .370813

*dem without coverage and on dempos issue
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 1 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 1 demneg 0 reppos 0 repneg 0 dem 1 rep 0 cs 0  
simqi, prval (1)  
*Pr(know=1) mean/se/95%ci: .3984004     .0063196     .3861321    .4109418
*AC1:       .3852246     .0061906     .3728569    .3973349

*first difference from baseline (dem on nonpartisan/nocoverage to dempos/nocoverage)
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 0 dem 1 rep 0 cs 0  
simqi, fd(prval(1)) changex(demXdempos 0 1  dempos 0 1  ) /* first difference effect */
*first diff, mean/se/95%ci    .0375846      .007605     .0223685    .0531  */
*AC1:      .0229625     .0076299     .0091531    .038731   

*dem without coverage and on demneg issue
setx mean
setx blkAM 0 femAM 1   
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 1 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 1 reppos 0 repneg 0 dem 1 rep 0 cs 0  
simqi, prval (1) 
*Pr(know=1) mean/se/95%ci:   .3244359     .0064363     .3123289    .3377
*AC1:     .3143216     .0064004     .3016397     .326

*first difference from baseline (dem on nonpartisan/nocoverage to demneg/nocoverage)
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 0 dem 1 rep 0 cs 0  
simqi, fd(prval(1)) changex(demXdemneg 0 1  demneg 0 1  ) /* first difference effect */
*first diff, mean/se/95%ci   -.0363799      .007537    -.0508565   -.0208286    */
*AC1:    -.0479405     .0073681    -.0629118   -.0342913

*AVERAGE COVERAGE

*dem average coverage and on dempos issue (n=17.22)
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs  17.22 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 1 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs  17.22 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 17.22 repXcs 0 dempos 1 demneg 0 reppos 0 repneg 0 dem 1 rep 0 cs  17.22
simqi, prval (1)  
*Pr(know=1) mean/se/95%ci:  .4337076     .0056863     .4226224    .44474
*AC1: .4229058     .0057344     .4117498    .4341844

*dem average coverage and on demneg issue
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs  21.21 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 1 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs  21.21  repnegXcs 0 demXcs 21.21 repXcs 0 dempos 0 demneg 1 reppos 0 repneg 0 dem 1 rep 0 cs  21.21  
simqi, prval (1) 
*Pr(know=1) mean/se/95%ci:   .3339347     .0058481     .3225275    .3458797
*AC1:   .3261504     .0058165     .3149548    .33781

*DEM, POSTIVE DEM COVERAGE, n=70.32
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 70.32 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 1.0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 70.32 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 70.4 repXcs 0 dempos 1 demneg 0 reppos 0 repneg 0 dem 1 rep 0 cs 70.32 
simqi, prval (1)  
*Pr(know=1) mean/se/95%ci:  .5446913     .0114777      .522638    .5669349
*AC1:     .5420611     .0116008     .5202263    .563   

*first difference from baseline (dem on nonpartisan/nocoverage to dempos/highcoverage)
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 0 dem 1 rep 0 cs 0  
simqi, fd(prval(1)) changex( demXdemposXcs 0 70.32 demXdempos 0 1 demposXcs 0 70.32 demXcs 0 70.32  cs 0 70.32    dempos 0 1  ) /* first difference effect */
*first diff, mean/se/95%ci      .1838752     .0120558     .1605967    .208270  */
*AC1:     .1797988     .0122224     .1558301    .20277    

*DEM WITH NEGATIVE DEM COVERAGE, n=100.44
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 100.44 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 1 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 100.44 repnegXcs 0 demXcs 100.44 repXcs 0 dempos 0 demneg 1 reppos 0 repneg 0 dem 1 rep 0 cs 100.44
simqi, prval (1) 
*Pr(know=1) mean/se/95%ci:   .370427     .0150367     .3414195    .4004294
*AC1:       .3719115     .0140985     .3457819    .3989 

*first difference from no cov/neg (dem on demneg/nocoverage to dempneg/highcoverage)
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 1 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 1 reppos 0 repneg 0 dem 1 rep 0 cs 0  
simqi, fd(prval(1)) changex( demXdemnegXcs 0 100.44  demnegXcs 0 100.44 demXcs 0 100.44 cs 0 100.44 demneg 0 1     ) /* first difference effect */
*first diff, mean/se/95%ci       .0059394     .0176987    -.0273008    .0399169    */
*AC1:    .0064066     .0167392     -.025639    .038   

************
*REPUBLICAN*
************
*republican, no issue motivation or coverage
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 0 dem 0 rep 1 cs 0  
simqi, prval (1) 
*Pr(know=1) mean/se/95%ci:  .3559517      .003987      .348112    .363706
*AC1:     .3572538     .0040825     .3490939    .36558

*rep with pos issue
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 1 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 1 repneg 0 dem 0 rep 1 cs 0  
simqi, prval (1)  
*Pr(know=1) mean/se/95%ci:   .3971564     .0064041     .3839479    .4099267
*AC1:     .3932287     .0063088     .3804817    .4058492

*first difference from baseline (rep on nonpartisan/nocoverage to reppos/nocoverage)
setx mean
setx blkAM 0 femAM 1   
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 0 dem 0 rep 1 cs 0  
simqi, fd(prval(1)) changex(repXreppos 0 1  reppos 0 1  ) /* first difference effect */
*first diff, mean/se/95%ci   .0412047     .0072273     .0265746    .0552    */
*AC1:      .0359749     .0072767     .0216512    .0500    

*rep with neg issue
setx mean
setx blkAM 0 femAM 1   
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 1 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 1 dem 0 rep 1 cs 0  
simqi, prval (1) 
*Pr(know=1) mean/se/95%ci:   .3471938     .0113772     .3246864    .36950
*AC1:     .3418734     .0118142     .3195403    .366

*first difference from baseline (rep on nonpartisan/nocoverage to repneg/nocoverage)
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 0 dem 0 rep 1 cs 0  
simqi, fd(prval(1)) changex(repXrepneg 0 1  repneg 0 1  ) /* first difference effect */
*first diff, mean/se/95%ci    -.0087578     .0119487    -.0327831    .0137  */
*AC1:    -.0153804     .0121933    -.0390165    .0094 

*AVERAGE COVERAGE
*rep with pos issue, n=117
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs  117  repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 1 repXrepneg 0 demposXcs 0 repposXcs  117 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs  117  dempos 0 demneg 0 reppos 1 repneg 0 dem 0 rep 1 cs  117  
simqi, prval (1) 
*Pr(know=1) mean/se/95%ci:  .448685     .0053681      .438425    .4594216
*AC1:   .4487346     .0053925     .4375803    .458

*rep with neg issue, n=44.25
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 44.25 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 1 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 44.25 demXcs 0 repXcs 44.25 dempos 0 demneg 0 reppos 0 repneg 1 dem 0 rep 1 cs 44.25
simqi, prval (1)  
*Pr(know=1) mean/se/95%ci:   .3013223     .0090819     .2839349    .3196553
*AC1:     .2945118     .0091024     .2769129     .3125

*REPUBLICAN, POSITIVE REPUBLICAN COVERAGE, n=451.11
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 451.11 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 1 repXrepneg 0 demposXcs 0 repposXcs 451.11  demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 451.11  dempos 0 demneg 0 reppos 1 repneg 0 dem 0 rep 1 cs 451.11 
simqi, prval (1)  
*Pr(know=1) mean/se/95%ci:    .5975949     .0120247      .574684    .6207377
*AC1:     .6089985     .0124615     .5851296    .634098
    
*first difference from baseline (rep on nonpartisan/nocoverage to reppos/highcoverage)
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 0 dem 0 rep 1 cs 0  
simqi, fd(prval(1)) changex( repXrepposXcs 0 451.11 repXreppos 0 1 repXcs 0 451.11  cs 0 451.11    reppos 0 1 repposXcs 0 451.11 ) /* first difference effect */
*first diff, mean/se/95%ci         .2416433     .0130415     .2162804    .266570 *
*AC1:       .2517447     .0136457     .2259469     .278334 

*REPUBLICAN, NEGATIVE REPUBLICAN COVERAGE, n=144.4
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 144.40 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 1  demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 144.40 demXcs 0 repXcs 144.40 dempos 0 demneg 0 reppos 0 repneg 1 dem 0 rep 1 cs 144.40
simqi, prval (1) 
*Pr(know=1) mean/se/95%ci:    .2098115     .0234746     .1655586    .256552
*AC1:      .2009014     .0227682     .1569836    .2479

*first difference from baseline (rep on nonpartisan/nocoverage to repneg/highcoverage)
setx mean 
setx blkAM 0 femAM 1  
setx demXdemposXcs 0 demXdemnegXcs 0 repXrepposXcs 0 repXrepnegXcs 0 demXdempos 0 demXdemneg 0 repXreppos 0 repXrepneg 0 demposXcs 0 repposXcs 0 demnegXcs 0 repnegXcs 0 demXcs 0 repXcs 0 dempos 0 demneg 0 reppos 0 repneg 0 dem 0 rep 1 cs 0  
simqi, fd(prval(1)) changex( repXrepnegXcs 0 144.4 repXrepneg 0 1 repXcs 0 144.4  cs 0 144    repneg 0 1 repnegXcs 0 144.4 ) /* first difference effect */
*first diff, mean/se/95% ci   -.1462703     .0237509    -.1911622    -.096189    */
*AC1:      -.1565373     .0232308    -.2010463   -.1083

**************************************************************
***WITHOUT VALENCE (POSITIVE/NEGATIVE), ONLY PARTY REFERENT***
************************************************************** 

*Alternate specification 2 (abbreviated AS2 below): Using other forms of issue type without valence
clear
version 11.2
cd "C:\PB_JeritBarabas_JOP\JeritBarabas_JOP_ReplicationFiles\JeritBarabas_JOP_Data" 
set more off
use PB1.dta
set seed 32306
estsimp probit know  demXdemNVXcs repXrepNVXcs demXdemNV repXrepNV csXdemNV csXrepNV demXcs repXcs demNV repNV dem rep cs eduAM incAM ageAM blkAM femAM  ajps09 IEnumb1-IEnumb2 IEnumb5-IEnumb12 IEnumb14-IEnumb22 IEnumb24-IEnumb44  , cluster(masterid) mi(PB1 PB2 PB3 PB4 PB5)
 
*************************
*PREDICTED PROBABILITIES*
*************************
**********
*DEMOCRAT*
**********
*dem without coverage and on non-it issue
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 0 demXdemNV 0 repXrepNV 0 csXdemNV 0 csXrepNV 0 demXcs 0 repXcs 0 demNV 0 repNV 0 dem 1 rep 0 cs 0
simqi, prval (1)  
*AS2:  .3665079     .0045327     .3575458    .37519

*dem without coverage and on dem issue
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 0 demXdemNV 1 repXrepNV 0 csXdemNV 0 csXrepNV 0 demXcs 0 repXcs 0 demNV 1 repNV 0 dem 1 rep 0 cs 0
simqi, prval (1)   
*AC2:   .3702725     .0048947     .3602768    .3794243

*first difference from baseline (dem on nonpartisan/nocoverage to dempos/nocoverage)
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 0 demXdemNV 0 repXrepNV 0 csXdemNV 0 csXrepNV 0 demXcs 0 repXcs 0 demNV 0 repNV 0 dem 1 rep 0 cs 0
simqi, fd(prval(1)) changex(demXdemNV 0 1 demNV 0 1  ) /* first difference effect */
*AC2:    .0037645     .0066003    -.0087378    .0161812

*AVERAGE COVERAGE

*dem average coverage and on dem issue (n=17.6)
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 17.6 repXrepNVXcs 0 demXdemNV 1 repXrepNV 0 csXdemNV 17.6 csXrepNV 0 demXcs 17.6 repXcs 0 demNV 1 repNV 0 dem 1 rep 0 cs 17.6
simqi, prval (1)  
*AC2:  .3913496     .0045108     .3820281    .39968

*DEM, DEM COVERAGE, n=79.23
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 79.23 repXrepNVXcs 0 demXdemNV 1 repXrepNV 0 csXdemNV 79.23 csXrepNV 0 demXcs 79.23 repXcs 0 demNV 1 repNV 0 dem 1 rep 0 cs 79.23
simqi, prval (1)
*AC2:   .4673141     .0096496     .4489676    .48537

*first difference from baseline (dem on nonpartisan/nocoverage to dem/highcoverage)
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 0 demXdemNV 0 repXrepNV 0 csXdemNV 0 csXrepNV 0 demXcs 0 repXcs 0 demNV 0 repNV 0 dem 1 rep 0 cs 0
simqi, fd(prval(1)) changex(demXdemNVXcs 0 79.23 demXdemNV 0 1 csXdemNV 0 79.23 demXcs 0 79.23 demNV 0 1 cs 0 79.23) /* first difference effect */
*AC2:    .1008061     .0104555     .0806039    .1203591 

************
*REPUBLICAN*
************
*republican, no issue motivation or coverage
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 0 demXdemNV 0 repXrepNV 0 csXdemNV 0 csXrepNV 0 demXcs 0 repXcs 0 demNV 0 repNV 0 dem 0 rep 1 cs 0
simqi, prval (1)  
*AC2:    .3640453     .0045224     .3550693    .3723987

*rep with issue
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 0 demXdemNV 0 repXrepNV 1 csXdemNV 0 csXrepNV 0 demXcs 0 repXcs 0 demNV 0 repNV 1 dem 0 rep 1 cs 0
simqi, prval (1)  
*AC2:    .3683111     .0054767      .357669     .378231

*first difference from baseline (rep on nonpartisan/nocoverage to reppos/nocoverage)
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 0 demXdemNV 0 repXrepNV 1 csXdemNV 0 csXrepNV 0 demXcs 0 repXcs 0 demNV 0 repNV 1 dem 0 rep 1 cs 0
simqi, fd(prval(1)) changex(repXrepNV 0 1  repNV 0 1 ) /* first difference effect */
*AC2:     .0042659     .0065325    -.0087658    .0170378 

*AVERAGE COVERAGE
*rep with pos issue, n=62
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 62 demXdemNV 0 repXrepNV 1 csXdemNV 0 csXrepNV 62 demXcs 0 repXcs 62 demNV 0 repNV 1 dem 0 rep 1 cs 62
simqi, prval (1) 
*AC2:     .3982252     .0049261     .3885459    .407529

*REPUBLICAN, REPUBLICAN HIGH COVERAGE, n=233
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 233 demXdemNV 0 repXrepNV 1 csXdemNV 0 csXrepNV 233 demXcs 0 repXcs 233 demNV 0 repNV 1 dem 0 rep 1 cs 233
simqi, prval (1) 
*AC2:      .4833587     .0067034     .4701984    .49661

*first difference from baseline (rep on nonpartisan/nocoverage to rep/highcoverage)
setx mean 
setx blkAM 0 femAM 1 
setx demXdemNVXcs 0 repXrepNVXcs 0 demXdemNV 0 repXrepNV 0 csXdemNV 0 csXrepNV 0 demXcs 0 repXcs 0 demNV 0 repNV 0 dem 0 rep 1 cs 0
simqi, fd(prval(1)) changex(repXrepNVXcs 0 233 csXrepNV 0 233 repXcs 0 233 cs 0 233 repNV 0 1 repXrepNV 0 1) /* first difference effect */
*AC2:    .1193134     .0081664     .1031308     .136078
