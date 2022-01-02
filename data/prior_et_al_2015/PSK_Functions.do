*Studies 1 and 2
	
	*Effect of Monetary Incentive on Proportion Congenial
	program eff_cong, rclass
		logit congenial pay if r_bush == 0
		mat b = e(b)
		return scalar diff = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))
	end

	*Effect of Monetary Incentive on Proportion Correct
	program eff_corr, rclass
		logit correct pay if r_bush == 0
		mat b = e(b)
		return scalar diff = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))
	end

	*Effect of Monetary Incentive on Proportion Uncongenial
	program eff_uncong, rclass
		logit uncongenial pay if r_bush == 0
		mat b = e(b)
		return scalar diff = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))
	end


	*Control: Proportion Congenial - Proportion Uncongenial
	program bias_pay0, rclass
		proportion uncongenial if r_bushref == 0 & pay == 0
		mat b1mat = e(b)
		scalar b1 = b1mat[1,2]
		proportion congenial if r_bushref == 0 & pay == 0
		mat b2mat = e(b)
		scalar b2 = b2mat[1,2]
		return scalar diff = b2 - b1
	end

	*Monetary Incentive: Proportion Congenial - Proportion Uncongenial
	program bias_pay1, rclass
		proportion uncongenial if r_bushref == 0 & pay == 1
		mat b1mat = e(b)
		scalar b1 = b1mat[1,2]
		proportion congenial if r_bushref == 0 & pay == 1
		mat b2mat = e(b)
		scalar b2 = b2mat[1,2]
		return scalar diff = b2 - b1
	end


	*Effect of Monetary Incentive on (Proportion Congenial - Proportion Uncongenial)
	program did, rclass
		logit uncongenial pay if r_bush == 0
		mat b = e(b)
		scalar diff1 = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))

		logit congenial pay if r_bush == 0
		mat b = e(b)
		scalar diff2 = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))
		
		return scalar diff = diff2 - diff1
	end


*Study 1
	
	*Effect of Monetary Incentive on Proportion Congenial
	program eff_cong1, rclass
		logit congenial pay if pollid == 2004
		mat b = e(b)
		return scalar diff = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))
	end

	*Effect of Monetary Incentive on Proportion Correct
	program eff_corr1, rclass
		logit correct pay if pollid == 2004
		mat b = e(b)
		return scalar diff = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))
	end

	*Effect of Monetary Incentive on Proportion Uncongenial
	program eff_uncong1, rclass
		logit uncongenial pay if pollid == 2004
		mat b = e(b)
		return scalar diff = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))
	end
	
	
	*Control: Proportion Congenial - Proportion Uncongenial
	program bias_pay01, rclass
		proportion uncongenial if pollid == 2004 & pay == 0
		mat b1mat = e(b)
		scalar b1 = b1mat[1,2]
		proportion congenial if pollid == 2004 & pay == 0
		mat b2mat = e(b)
		scalar b2 = b2mat[1,2]
		return scalar diff = b2 - b1
	end

	*Monetary Incentive: Proportion Congenial - Proportion Uncongenial
	program bias_pay11, rclass
		proportion uncongenial if pollid == 2004 & pay == 1
		mat b1mat = e(b)
		scalar b1 = b1mat[1,2]
		proportion congenial if pollid == 2004 & pay == 1
		mat b2mat = e(b)
		scalar b2 = b2mat[1,2]
		return scalar diff = b2 - b1
	end


	*Effect of Monetary Incentive on (Proportion Congenial - Proportion Uncongenial)
	program did1, rclass
		logit uncongenial pay if pollid == 2004
		mat b = e(b)
		scalar diff1 = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))

		logit congenial pay if pollid == 2004
		mat b = e(b)
		scalar diff2 = exp(b[1,1] + b[1,2]) / (1 + exp(b[1,1] + b[1,2])) - exp(b[1,2]) / (1 + exp(b[1,2]))
		
		return scalar diff = diff2 - diff1
	end


*Study 2
	
	*Effect of Accuracy Appeal on Proportion Congenial
	program app_cong, rclass
		logit congenial app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		return scalar diff = exp(b[1,1] + b[1,3]) / (1 + exp(b[1,1] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))
	end

	*Effect of Accuracy Appeal on Proportion Correct
	program app_corr, rclass
		logit correct app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		return scalar diff = exp(b[1,1] + b[1,3]) / (1 + exp(b[1,1] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))
	end

	*Effect of Accuracy Appeal on Proportion Uncongenial
	program app_uncong, rclass
		logit uncongenial app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		return scalar diff = exp(b[1,1] + b[1,3]) / (1 + exp(b[1,1] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))
	end
	

	*Effect of Monetary Incentive on Proportion Congenial
	program mon_cong, rclass
		logit congenial app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		return scalar diff = exp(b[1,2] + b[1,3]) / (1 + exp(b[1,2] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))
	end

	*Effect of Monetary Incentive on Proportion Correct
	program mon_corr, rclass
		logit correct app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		return scalar diff = exp(b[1,2] + b[1,3]) / (1 + exp(b[1,2] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))
	end

	*Effect of Monetary Incentive on Proportion Uncongenial
	program mon_uncong, rclass
		logit uncongenial app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		return scalar diff = exp(b[1,2] + b[1,3]) / (1 + exp(b[1,2] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))
	end
	
	
	*Control: Proportion Congenial - Proportion Uncongenial
	program bias_rd0, rclass
		proportion uncongenial if pollid == 2008 & r_bushref == 0 & rd == 0
		mat b1mat = e(b)
		scalar b1 = b1mat[1,2]
		proportion congenial if pollid == 2008 & r_bushref == 0 & rd == 0
		mat b2mat = e(b)
		scalar b2 = b2mat[1,2]
		return scalar diff = b2 - b1
	end

	*Accuracy Appeal: Proportion Congenial - Proportion Uncongenial
	program bias_rd1, rclass
		proportion uncongenial if pollid == 2008 & r_bushref == 0 & rd == 1
		mat b1mat = e(b)
		scalar b1 = b1mat[1,2]
		proportion congenial if pollid == 2008 & r_bushref == 0 & rd == 1
		mat b2mat = e(b)
		scalar b2 = b2mat[1,2]
		return scalar diff = b2 - b1
	end

	*Monetary Incentive: Proportion Congenial - Proportion Uncongenial
	program bias_rd2, rclass
		proportion uncongenial if pollid == 2008 & r_bushref == 0 & rd == 2
		mat b1mat = e(b)
		scalar b1 = b1mat[1,2]
		proportion congenial if pollid == 2008 & r_bushref == 0 & rd == 2
		mat b2mat = e(b)
		scalar b2 = b2mat[1,2]
		return scalar diff = b2 - b1
	end


	*Effect of Accuracy Appeal on (Proportion Congenial - Proportion Uncongenial)
	program app_bias, rclass
		logit uncongenial app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		scalar diff1 = exp(b[1,1] + b[1,3]) / (1 + exp(b[1,1] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))

		logit congenial app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		scalar diff2 = exp(b[1,1] + b[1,3]) / (1 + exp(b[1,1] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))
		
		return scalar diff = diff2 - diff1
	end

	*Effect of Monetary Incentive on (Proportion Congenial - Proportion Uncongenial)
	program mon_bias, rclass
		logit uncongenial app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		scalar diff1 = exp(b[1,2] + b[1,3]) / (1 + exp(b[1,2] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))

		logit congenial app mon if pollid == 2008 & r_bushref == 0
		mat b = e(b)
		scalar diff2 = exp(b[1,2] + b[1,3]) / (1 + exp(b[1,2] + b[1,3])) - exp(b[1,3]) / (1 + exp(b[1,3]))
		
		return scalar diff = diff2 - diff1
	end
