*! version 2.1 YC & SRH 4 Sep 2011
program define init_prior
	/* 
	Interpret the syntax of the prior option and set
	some globals for later use - call the globals HP_*
	(max 8 characters) to avoid conflicts with gllamm
	globals which are called HG_*
    
	This program should do calculations that only need
	to be done once and store the result in a global
	to be used in calc_prior

	*****SYNTAX
	1) gamma prior
	GAMMa, shape(alpha) scale(beta) rate(theta) : gamma on sigma (default)
	GAMMa, shape(alpha) scale(beta) rate(theta) var: gamma on sigma^2, penalty on sigma_e^2 

    */
	
    version 7.0
	syntax [, prior(string) loud]

    if "`loud'"!=""{ local loud "noisily"}

    * disp in re "loud is `loud'"
	if $HG_free==1{
		global HP_prior = 0
		disp in re "Prior not used: random effects are discrete"
	}
	else if $HG_tplv>2{
		global HP_prior = 0
		disp in re "Prior not used: more than one random effect"
	}
	else{
		global HP_prior = 1
		*global HP_h = `prior'
	} 
	* disp in re "HP_prior " $HP_prior

		
	if $HP_prior == 1 {
		local 0 "`prior'"
		syntax name [,  shape(real 2.0) rate(real 0.0001) scale(real 10000) VARiance ]
		
        *disp in re "variance is `variance'"
		*disp in re "`prior'"
		*disp in re "name: `namelist', shape: `shape',  rate: `rate', scale: `scale'"
      
		/* deal with name (distribution) */
		global HP_gamma = 0

        * these are not used:
        global HP_wisha = 0
		global HP_invga = 0
		global HP_invwi = 0
		global HP_foldt = 0
		global HP_logno = 0
		global HP_corre = 0
		global HP_boxco = 0
		global HP_spect = 0

		local f = lower(trim("`namelist'"))  /* make lower-case and remove spaces */
		local l = length("`f'")
		*disp in re "f is `f' and l is `l'"
		if "`f'" == substr("gamma",1,max(`l',4)) {
			global HP_gamma = 1
        }
		else{
			disp in re "In prior: distribution `f' not recognized"
			exit 198
        }

   
		/* check parameters are OK and do calculations */
	

		/*** PRIOR:GAMMA ***/
		if $HP_gamma == 1 {
			if $HG_tprf>2{
				disp in re "Gamma prior cannot be used for more than 1 random effect. "
				exit 198
			}
			if `shape'<1 | `shape'==1 {
				disp in re _n "Warning: Shape parameter should be greater than 1 to avoid boundary estimates"
			}


			if "`rate'"~="" & "`scale'"~=""{
				if `rate'~=1/`scale'{
					disp in re "In gamma prior: scale must be equal to 1/rate."
					exit 198
				}
			}

			
			global HP_shape=`shape'

			if "`scale'"=="" {
				global HP_scale=1/`rate'
			}
			else {
				global HP_scale=`scale'
			}
			if "`variance'"!="" {
				global HP_var=1
				qui `loud' disp in ye _n "gamma prior, shape(`shape') scale(`scale') variance"
			}
			else {
				global HP_var=0
				qui `loud' disp in ye _n "gamma prior, shape(`shape') scale(`scale')"
			}

		}
    }
end
		
