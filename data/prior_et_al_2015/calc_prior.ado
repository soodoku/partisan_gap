*! version 2.1 YC & SRH 4 Sep 2011

program define calc_prior
	/* 
	Calculate the log-prior and store in global HP_res
    */
    version 7.0

	/*** gamma ***/
	if $HP_gamma==1{
		tempname s s_e2 sqrts
		matrix temp=CHmat*CHmat'
		if $HP_var==0 {
			scalar `s'=sqrt(temp[1,1])
		}
		if $HP_var==1 {
			scalar `s'=temp[1,1]
			scalar `sqrts'=sqrt(`s')
		}
		global HP_res=log(gammaden($HP_shape, $HP_scale,0,`s'))
	}
end
