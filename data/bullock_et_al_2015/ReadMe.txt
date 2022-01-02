Replication archive for Bullock, Gerber, Huber, Hill
PARTISAN BIAS IN FACTUAL BELIEFS ABOUT POLITICS
Version 1.1 (May 08, 2015)

This replication archive has 4 stata .do files and 5 folders. Analysis was originally performed in Stata version 12.1 on a Windows operating system. Those using other operating systems may have to reverse slashes to appropriately reference file locations in subfolders.
 This code uses the outreg ado file to output regression output (From within stata type "net describe sg97_3, from(http://www.stata.com/stb/stb59)")
 For purposes of replication, we also include this .ADO and the associated .HLP file in the folder "Stata Outreg Ado sg97_3 from STB59"

We distribute three public data files (with identifying information removed and with variable recodes applied) for each of the 3 surveys used in our analysis.

 These public datafiles (Stata datasets) are stored in 3 folders:
 CCES2008 has CCES2008PublicReplicationDataset.dta
 CCES2012 has CCES2012PublicReplicationDataset.dta
 MTURK12 has MTURK2012PublicReplicationDataset.dta

01_Analyze2008CCES.do uses CCES2008PublicReplicationDataset.dta to perform all analysis using the 2008 CCES experiment.

 This produces:
  Tables 1, 2, and 5
  Appendix Table A1
  Online Appendix Tables OA1 and OA3 

02_Analyze2012Mturk.do uses MTURK2012PublicReplicationDataset.dta to perform all analysis using the 2012 MTURK experiment

 This produces:
  Tables 3 and 4
  Appendix Table A2
  Online Appendix Tables OA4, OA5, OA6, OA7

03_Analyze2012CCES.do uses CCES2012PublicReplicationDataset.dta to perform all analysis using the 2012 CCES experiment

 This produces:
  Online Appendix Table OA2

All output is placed in the folder Tables
