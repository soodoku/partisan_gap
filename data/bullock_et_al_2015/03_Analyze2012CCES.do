

use CCES2012\CCES2012PublicReplicationDataset.dta

regress unemp_obama isdem isdem_correct isdem_correctdk paycorrect paycorrectdk, robust

test isdem_correct==isdem_correctdk
local testa=r(p)/2

outreg using Tables\OnlineAppendixTable2_CCES12_Regressionpayeffectsonpidgaps.out, se bracket rdec(3) 3aster addnote("Note: Source: 2012 CCES. Includes only Democrats and Republicans. Robust standard errors. F-test p-values are one-tailed.") addstat("F-test, Pay Correct * Dem. > Pay DK and Correct * Dem.", `testa') replace

