use "${gen}/dataset_districts.dta", replace

gen ma = dt_perc_sc_pop-Rpre
gen seats_sc_before = noofconspre*Rpre
gen dreserv = dt_sc_reserved_total-seats_sc_before


 reghdfe totalevents  c.after c.after#(c.dreserv ) , absorb(state year)

 reghdfe totalevents c.after##(c.dreserv c.ma) , absorb(state year)
 
 reghdfe totalevents c.after##(c.ma) , absorb(state year) // not a good result

 reghdfe totalevents c.after##c.ma##c.ma , absorb(state year) // not a good result

 reghdfe totalevents c.ma##c.ma if year>=2008 & after==1, absorb(state year) // not a good result


gr tw lpoly resc ma if before==1
gr tw lpoly resc ma if after==1, kernel(tri)

/*
/*----------------------------------------------------*/
   /* [>   Balance Table   <] */ 
/*----------------------------------------------------*/
use "${gen}/dataset_districts.dta", replace
keep if year==treatyear-1
gen sc_reserved = .
replace sc_reserved = 1 if diff_post>=0.25 & !mi(diff_post)
replace sc_reserved = 0 if diff_post<=-0.25 & !mi(diff_post)

replace agriculturalgdp = agriculturalgdp/dt_pop_total
qui sum lightsum 
replace lightsum = (lightsum-r(mean))/(r(sd))

keep if !mi(sc_reserved)
replace dt_pop_total = dt_pop_total/(10^6)
label var dt_pop_total "Total population (mill's)"
label var dt_perc_sc_pop "Share of SCs"
label var agriculturalgdp "Agricultural GDP/cap"
label var logagriwage "Log agricultural wage"
label var lightsum "Nightlights (normalized)"
label var rainfed "Percent rainfed"
label var rainfed "Percent rainfed"
label var irrigated "Percent irrigated"
label var cultivated "Percent cultivated"
label var ANNUAL_temp	 "Annual temperature"
label var treat "NREGA phase"
 global depvarlist   dt_pop_total totalevents agriculturalgdp logagriwage lightsum rainfed irrigated cultivated ANNUAL_temp  // treat

 
reg sc_reserved  dt_perc_sc_pop $depvarlist  if !mi(sc_reserved)
test $depvarlist
local fval: display %9.3f r(F)
local fpval: display %9.3f r(p)
// local fpval: display %9.3f Ftail(e(df_m), e(df_r), e(F))
 
balancetable  sc_reserved dt_perc_sc_pop $depvarlist  using "${output}/tables/balancetable_districts.tex", ///
	replace  varlabels pvalues ///
	groups("$\Delta$ Seats $\leq -0.25$" "$\Delta$ Seats $\geq 0.25$" "Difference", pattern(1 1 1)) ///
	ctitles("(sd)" "(sd)" "(p-value)") ///
	 prefoot("\addlinespace  \midrule  Wald test statistic (Jointly zero) & & &  `fval' \\ p-value & & & `fpval' \\ \addlinespace \midrule ") ///
	format(%9.3f) booktabs
	
	
	*/







 
/*----------------------------------------------------*/
   /* [>   Randomization inference   <] */ 
/*----------------------------------------------------*/
clear 
local runs 2000
set obs `runs'
gen run = _n 

local conflictvars " totalevents killedinjured civilianskilled securitykilled militantskilled nonlethalevents abduct arrested"
local regcounter = 0 
foreach var in `conflictvars' {
	local regcounter = `regcounter'+1
	gen orig`regcounter' = .
	gen coef`regcounter' = .
	gen orig`regcounter'_p = .
	gen coef`regcounter'_p = .
	gen orig`regcounter'_iv = .
	gen coef`regcounter'_iv = .
	}
tempfile ri 
save `ri' 


use "${gen}/dataset_districts.dta", replace




 /*
/*----------------------------------------------------*/
   /* [>   5.  Interaction with Naxalites   <] */ 
/*----------------------------------------------------*/
local conflictvars " totalevents killedinjured civilianskilled securitykilled militantskilled nonlethalevents abduct arrested"


foreach var in `conflictvars' {
	cap gen l`var' = log(`var'+1)
	local regcounter = `regcounter'+1

reghdfe l`var'  after c.diff_post#c.after c.after#c.total_nax_pre2008 c.diff_post#c.total_nax_pre2008#c.after , absorb(id year)  vce(cluster id)
qui sum `var' if e(sample)
estadd scalar lmean = r(mean)
est sto reg`regcounter'
}



#delimit ;
estout 
reg1 reg2 reg3 reg4 reg5
using "${output}/tables/reg_dist_nax_cluster.tex" , style(tex)  
wrap varwidth(85) 
varlabels(after "After delimitation" c.diff_post#c.after "After $\times$  Extra seats" 
					c.after#c.total_nax_pre2008 "After $\times$ Naxalite intensity"
					c.diff_post#c.total_nax_pre2008#c.after  " \rowcolor{LSEred!20} \textbf{After  $\times$  Extra $\times$ Naxal} \rowcolor{LSEred!20}"
  )
keep(after c.diff_post#c.after c.after#c.total_nax_pre2008 c.diff_post#c.total_nax_pre2008#c.after)
order(after c.diff_post#c.after c.after#c.total_nax_pre2008 c.diff_post#c.total_nax_pre2008#c.after)
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats( lmean N   , fmt(%9.3fc  %9.0fc ) labels( "\midrule  Mean of DV" "Observations"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	



*/


/*
/*----------------------------------------------------*/
   /* [>   6.  DiD regressions with crime   <] */ 
/*----------------------------------------------------*/
 local crimevars "cmurder crape ckidnap cdacoity carson crobbery churt poa pcr other totalcrimes"

foreach v in `crimevars' {
		qui {
	cap drop l`v'
	gen l`v' = log(`v'+1)
	reghdfe l`v' after c.after#c.diff_post  , absorb(id year) // vce(robust)
	// reghdfe `v' i.after c.diff_post##i.after if year!=2014 , absorb(id year) vce(cluster state year)
	qui sum `v' if e(sample)
	estadd scalar lmean = `r(mean)'
	est sto e`v'
}
}

#delimit ;
estout 
etotalcrimes epoa epcr eother ecmurder
using "${output}/tables/reg_crimes_did.tex" , style(tex)  
wrap varwidth(72) 
varlabels(after "After delimitation" c.after#c.diff_post "\rowcolor{LSEred!20} \textbf{After delimitation  $\times$  Extra seats} \rowcolor{LSEred!20}" 
  _cons "\addlinespace Constant")
keep(after c.after#c.diff_post )
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N r2_within  , fmt(%9.3fc %9.0fc %9.3fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations" "Within R$^2$"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	


*/





local conflictvars " totalevents killedinjured civilianskilled securitykilled militantskilled nonlethalevents abduct arrested"


drop _merge
drop if missing(year)

local regcounter = 0 
foreach var in `conflictvars' {
	cap gen l`var' = log(`var'+1)
	local regcounter = `regcounter'+1

reghdfe l`var'  after c.diff_post#c.after , absorb(id year) //  vce(robust)
qui sum `var' if e(sample)
estadd scalar lmean = r(mean)
est sto reg`regcounter'
local orig`regcounter' = _b[c.diff_post#c.after]


ppmlhdfe `var'  after c.diff_post#c.after , absorb(id year) //  vce(robust)
qui sum `var' if e(sample)
estadd scalar lmean = r(mean)
est sto ppml`regcounter'
local orig`regcounter'_p = _b[c.diff_post#c.after]


ivreghdfe l`var' after   (seats_treat = diff_treat), absorb(id year) vce(cluster id) //  robust // vce(cluster id year)
local fx = `e(widstat)'
ivreghdfe l`var' after   (seats_treat = diff_treat), absorb(id year)  //  robust // vce(cluster id year)
qui sum `var' if e(sample)
estadd scalar lmean = r(mean)
estadd scalar fstat = `fx'
est sto iv`regcounter'
local orig`regcounter'_iv = _b[seats_treat]





}



tempfile data
save `data'

collapse (firstnm) year dt_sc_seats_by_rule st_sc_reserved_total , by(id state)
drop year
gen dt_sc_seats_by_rule_floor = floor(dt_sc_seats_by_rule)
bys state : egen sc_seats_fixed = total(dt_sc_seats_by_rule_floor)
gen leftover_seats = st_sc_reserved_total - sc_seats_fixed
 preserve

forv r = 1/`runs' {
 qui {
restore , preserve
/* [> Randomization inference <] */
gen still_leftover_seats = leftover_seats
gen dt_sc_seats_randomized = dt_sc_seats_by_rule_floor

gen random = rnormal(0,1)

qui sum leftover_seats
local runs = r(max)
forv i = 1/`runs' { 
		bys state: egen rmax = max(random)
		replace dt_sc_seats_randomized = dt_sc_seats_randomized+1 if random==rmax & still_leftover_seats>=1
		replace still_leftover_seats = still_leftover_seats-1 if still_leftover_seats>=1
		replace random = . if random==rmax & still_leftover_seats>=1
		drop rmax 
	}

merge 1:m id using `data' 

// gen diff_post_ri = dt_sc_reserved_total-dt_sc_seats_randomized
gen diff_post_ri = dt_sc_seats_randomized-dt_sc_seats_by_rule

gen diff_treat_ri = diff_post_ri*after

gen seats_treat_ri = dt_sc_seats_randomized*after


local regcounter = 0 
foreach var in `conflictvars' {
	local regcounter = `regcounter'+1
reghdfe l`var'  after c.diff_post_ri#c.after , absorb(id year) vce(cluster id)
local coef`regcounter' = _b[c.diff_post_ri#c.after]


ppmlhdfe `var'  after c.diff_post_ri#c.after , absorb(id year) //  vce(robust)
local coef`regcounter'_p = _b[c.diff_post_ri#c.after]


ivreghdfe l`var' after   (seats_treat_ri = diff_treat_ri), absorb(id year) vce(cluster id year)
local coef`regcounter'_iv = _b[seats_treat_ri]



}


 } 
di "`r', `coef1'"
qui use `ri', clear
local regcounter = 0 
foreach var in `conflictvars' {
	local regcounter = `regcounter'+1
qui replace coef`regcounter' = `coef`regcounter'' if _n==`r'
qui replace orig`regcounter' = `orig`regcounter'' if _n==`r'
qui replace coef`regcounter'_p = `coef`regcounter'_p' if _n==`r'
qui replace orig`regcounter'_p = `orig`regcounter'_p' if _n==`r'
qui replace coef`regcounter'_iv = `coef`regcounter'_iv' if _n==`r'
qui replace orig`regcounter'_iv = `orig`regcounter'_iv' if _n==`r'
	}
// qui replace coef2 = `coef2' if _n==`r'

qui save `ri', replace 
 }
restore 
use `ri', replace 

save "${gen}/randomization_inference.dta", replace
*/








set graph off
use "${gen}/randomization_inference.dta", replace
local conflictvars " totalevents killedinjured civilianskilled securitykilled militantskilled nonlethalevents abduct arrested"
local regcounter = 0 
foreach var in `conflictvars' {
	local regcounter = `regcounter'+1
		qui sum orig`regcounter'
		local x = r(mean)

		qui count 
		local n = r(N)
		qui count if coef`regcounter'<=orig`regcounter' 
		local p`regcounter' =round(r(N)/`n',0.0001)
		est resto reg`regcounter'

		local rip = "["+string(round(`p`regcounter'',0.001),"%9.3f")+"]"
		estadd local ripval = "`rip'"
		est sto reg`regcounter'

		hist coef`regcounter', frac xline(`x', lwidth(thick)) note("Randomization inference p-value: `p`regcounter''") xti("Coefficients from randomized reallocation of leftover seats") // ti("Randomization inference: DiD, `var'") // `orig1'// 
		graph export "${output}/figures/ri_hdfe_`regcounter'.pdf", replace 


	qui sum orig`regcounter'_p
		local x = r(mean)
		qui count if coef`regcounter'_p<=orig`regcounter'_p
		local p`regcounter'_p =round(r(N)/`n',0.0001)
		est resto ppml`regcounter'

		local rip = "["+string(round(`p`regcounter'_p',0.001),"%9.3f")+"]"
		estadd local ripval = "`rip'"
		est sto ppml`regcounter'

		hist coef`regcounter'_p, frac xline(`x', lwidth(thick)) note("Randomization inference p-value: `p`regcounter'_p'") xti("Coefficients from randomized reallocation of leftover seats") // ti("Randomization inference: PPML, `var'") // `orig1'// 
		graph export "${output}/figures/ri_ppml_`regcounter'.pdf", replace 

	qui sum orig`regcounter'_iv
		local x = r(mean)
		qui count if coef`regcounter'_iv<=orig`regcounter'_iv
		local p`regcounter'_iv =round(r(N)/`n',0.0001)
		est resto iv`regcounter'

		local rip = "["+string(round(`p`regcounter'_iv',0.001),"%9.3f")+"]"
		estadd local ripval = "`rip'"
		est sto iv`regcounter'

		hist coef`regcounter'_iv, frac xline(`x', lwidth(thick)) note("Randomization inference p-value: `p`regcounter'_iv'") xti("Coefficients from randomized reallocation of leftover seats") // ti("Randomization inference: IV, `var'") // `orig1'// 
		graph export "${output}/figures/ri_iv_`regcounter'.pdf", replace 
		}


#delimit ;
estout 
reg1 reg2 reg3 reg4 reg5
using "${output}/tables/reg_dist_did.tex" , style(tex)  
wrap varwidth(85) 
varlabels(after "After delimitation" c.diff_post#c.after " \rowcolor{LSEred!20} \textbf{After delimitation  $\times$  Extra seats} \rowcolor{LSEred!20}"
  )
keep(after c.diff_post#c.after)
order(after c.diff_post#c.after)
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(ripval lmean N   , fmt(%9.3fc %9.3fc %9.0fc ) labels("\rowcolor{LSEred!20}"  "\midrule \addlinespace Mean of DV" "\addlinespace Observations"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	






#delimit ;
estout 
ppml1 ppml2 ppml3 ppml4 ppml5
using "${output}/tables/ppml_dist_ppml.tex" , style(tex)  
wrap varwidth(85) 
varlabels(after "After delimitation" c.diff_post#c.after " \rowcolor{LSEred!20} \textbf{After delimitation  $\times$  Extra seats} \rowcolor{LSEred!20}"
  )
keep(after c.diff_post#c.after)
order(after c.diff_post#c.after)
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(ripval lmean N   , fmt(%9.3fc %9.3fc %9.0fc ) labels("\rowcolor{LSEred!20}"  "\midrule \addlinespace Mean of DV" "\addlinespace Observations"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	






#delimit ;
estout 
iv1 iv2 iv3 iv4 iv5
using "${output}/tables/iv_dist_iv.tex" , style(tex)  
wrap varwidth(85) 
varlabels(after "After delimitation" seats_treat " \rowcolor{LSEred!20} \textbf{After delimitation  $\times$  instr. seats} \rowcolor{LSEred!20}"
  )
keep(after seats_treat)
order(after seats_treat)
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(ripval lmean N  fstat , fmt(%9.3fc %9.3fc %9.0fc %9.1fc ) labels("\rowcolor{LSEred!20}"  "\midrule \addlinespace Mean of DV" "\addlinespace Observations" "K-P F-Stat"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	






stop 




/*------------------------- 
	To Do: Heterogeneity
		 1. Naxalites (so far only states - later districts)
		 2. Election years
-------------------------*/ 

/*
set scheme mine
tw hist diff_post,  frac bin(30) fcol(%80)  xti("Difference to expected seats", size(medlarge) margin(small)) yti("Share of observations", size(medlarge)) xlab(-0.75(0.25)0.75, labsize(medlarge)) ylab(,labsize(medlarge))
graph export  "./Output/figures/hist_diff.pdf", replace

gen diff_abs = abs(diff_post)

tw hist diff_abs,  frac bin(20) fcol(%80)  xti("Absolute difference to expected seats", size(medlarge) margin(small)) yti("Share of observations", size(medlarge)) xlab(0(0.1)0.6, labsize(medlarge)) ylab(,labsize(medlarge))
graph export  "./Output/figures/hist_diff_abs.pdf", replace
*/

 
gen diff_reservations = dt_seats_perc_sc_reserved - dt_sc_perc_sc_reserved_trivediL1

gen diff_pre_trivedi  = dt_sc_perc_sc_reserved_trivediL1-Fpre
reghdfe ltotalevents  after c.diff_post#c.after c.diff_pre_trivedi#i.year  , absorb(id year) vce(cluster id)
reghdfe ltotalevents  after c.diff_post#c.after c.dt_sc_perc_sc_reserved_trivediL1#i.year  , absorb(id year) vce(cluster id)


nbreg totalevents  after  i.id i.year c.diff_post#c.after , vce(cluster id)




reghdfe ltotalevents  after  c.dt_sc_perc_sc_reserved_trivediL1#i.year c.election_year##c.after c.diff_post#c.after , absorb(id year) vce(cluster id)


// Regressions
reghdfe totalevents  c.Rpre#c.before after c.Rpost#c.after election_year, absorb(id year)


reghdfe totalevents  after c.Rpost#c.after election_year, absorb(id year)

 
/*----------------------------------------------------*/
   /* [>   Preliminary Naxalaite regressions: Very promising at district level!   <] */ 
/*----------------------------------------------------*/
reghdfe totalevents  after c.Rpost#c.after c.nax#c.after c.Rpost#c.after#c.nax , absorb(id year) vce(cluster id)
reghdfe totalevents  after c.diff_post#c.after c.any_nax_event#c.after c.diff_post#c.after#c.any_nax_event , absorb(id year) vce(cluster id)



reghdfe totalevents  after c.diff_post#c.after c.any_nax_event#c.after c.dt_diff_post#c.after#c.any_nax_event , absorb(id year) vce(cluster id)



reg diff_post any_nax_event 



/**********************************************************************/
/*  SECTION 1: Regressions for Fetzer conflict outcomes  			
    Notes: */
/**********************************************************************/
local conflictvars " totalevents killedinjured civilianskilled securitykilled militantskilled nonlethalevents abduct arrested"

 ivreghdfe ltotalevents after   (seats_treat = diff_treat), absorb(id year) robust


/*----------------------------------------------------*/
   /* [>   2.  IV regressions   <] */ 
/*----------------------------------------------------*/
   /* [> 
   Note:
	I instrument the reserved seats that a district had after delimitation
	(i.e. the interaction between seats after and time after),
	with the rounding error interacted with time after.

	(alternative would be to instrument with difference in seats)
    <] */ 
foreach v in 	`conflictvars' {
		qui {
cap drop l`v'
gen l`v' = log(`v'+1)
qui ivreghdfe l`v' after    (seats_treat = diff_treat), absorb(id year) robust

	// cap drop l`v'
	// gen l`v' = log(`v'+0.01)
	// xtreg l`v' i.after c.diff_post##i.after i.year , fe vce(cluster id)
	estadd scalar fstat = `e(widstat)'

	qui sum `v' if e(sample)
	estadd scalar lmean = `r(mean)'
	est sto iv`v'
	}
}

#delimit ;
estout 
ivtotalevents ivkilledinjured ivcivilianskilled ivsecuritykilled ivmilitantskilled 
using "${output}/tables/reg_iv.tex" , style(tex)  
wrap varwidth(45) 
varlabels(after "After delimitation" seats_treat " After delimitation  $\times$  reserved seats" 
  )
keep(after seats_treat)
order(after seats_treat)
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N fstat  , fmt(%9.3fc %9.0fc %9.1fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations" "K-P F-stat"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	








/*----------------------------------------------------*/
   /* [>   3.  RD-type regressions   <] */ 
/*----------------------------------------------------*/
/* [> 
Note:
No RD plots because change in density (fewer high-reservation districts)
introduces weird bias to these plots. But figure with seats visualized should be sufficient.

Control for rounding error (adjusted to be around 0), and also control for fractional seats,
both after delimitation.
Then the local estimate is where the RD jumps.


 <] */ 
sc dt_sc_reserved_total dt_seats_fraction_alt if year==2014, ///
	xline(0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5, lpattern(dot)) ///
	ylab(,nogrid) col(%20) xlab(0(0.5)7) ysize(5) xsize(8.5) scale(1.25) xti("Fractional reserved seats") yti("Actual reserved seats")
graph export  "./Output/figures/seats_visualized_alt.pdf", replace

sc dt_sc_reserved_total  dt_sc_seats_by_rule if year==2014, ///
	xline(0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5, lpattern(dot)) ///
	ylab(,nogrid) col(%20) xlab(0(0.5)9) ysize(5) xsize(8.5) scale(1.25) xti("Fractional reserved seats") yti("Actual reserved seats")
graph export  "./Output/figures/seats_visualized.pdf", replace


cap drop rdd 
gen rdd = .
replace rdd = dt_sc_seats_by_rule-0.5 if  dt_sc_seats_by_rule<=1
replace rdd = dt_sc_seats_by_rule-1.5 if dt_sc_seats_by_rule>=1 & dt_sc_seats_by_rule<=2
replace rdd = dt_sc_seats_by_rule-2.5 if dt_sc_seats_by_rule>=2 & dt_sc_seats_by_rule<=3
replace rdd = dt_sc_seats_by_rule-3.5 if dt_sc_seats_by_rule>=3 & dt_sc_seats_by_rule<=4
replace rdd = dt_sc_seats_by_rule-4.5 if dt_sc_seats_by_rule>=4 & dt_sc_seats_by_rule<=5
replace rdd = dt_sc_seats_by_rule-5.5 if dt_sc_seats_by_rule>=5 & dt_sc_seats_by_rule<=6
replace rdd = dt_sc_seats_by_rule-6.5 if dt_sc_seats_by_rule>=6 & dt_sc_seats_by_rule<=7
replace rdd = dt_sc_seats_by_rule-7.5 if dt_sc_seats_by_rule>=7 & dt_sc_seats_by_rule<=8

cap drop rdd1
gen rdd1 = (rdd>=0)



foreach v in 	`conflictvars' {
		qui {
cap drop l`v'
gen l`v' = log(`v'+1)
qui reghdfe l`v' after c.after#c.rdd  c.after#c.rdd1 c.after#c.dt_sc_seats_by_rule , absorb(year id) vce(cluster id)

	// cap drop l`v'
	// gen l`v' = log(`v'+0.01)
	// xtreg l`v' i.after c.diff_post##i.after i.year , fe vce(cluster id)

	qui sum `v' if e(sample)
	estadd scalar lmean = `r(mean)'
	est sto rd`v'
	}
}


#delimit ;
estout 
rdtotalevents rdkilledinjured rdcivilianskilled rdsecuritykilled rdmilitantskilled 
using "${output}/tables/reg_rdd.tex" , style(tex)  
wrap varwidth(45) 
varlabels(after "After delimitation"
					c.after#c.rdd "After delimitation $\times$  rounding error" 
					c.after#c.rdd1 " After delimitation  $\times$  running variable>0" 
					c.after#c.dt_sc_seats_by_rule " After delimitation  $\times$  fractional seats" 
  )
keep(c.after#c.rdd1 after)
order(after c.after#c.rdd1  c.after#c.rdd c.after#c.dt_sc_seats_by_rule )
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N   r2_within , fmt(%9.3fc %9.0fc %9.3fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations" "Within R-squared"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	











 
/*----------------------------------------------------*/
   /* [>   4.  Pseudo-Poisson ML regression   <] */ 
/*----------------------------------------------------*/
cap encode state, gen(sid)
foreach v in 	`conflictvars' {
		qui {
	ppmlhdfe `v' i.after c.diff_post##i.after , absorb(id year) vce(cluster id) // c.Fpre##i.year c.Fpre##i.after
	// cap drop l`v'
	// gen l`v' = log(`v'+0.01)
	// xtreg l`v' i.after c.diff_post##i.after i.year , fe vce(cluster id)

	qui sum `v' if e(sample)
	estadd scalar lmean = `r(mean)'
	est sto e`v'
	}
}

#delimit ;
estout 
etotalevents ekilledinjured ecivilianskilled esecuritykilled emilitantskilled 
using "${output}/tables/reg_fetzer.tex" , style(tex)  
wrap varwidth(45) 
varlabels(1.after "After delimitation" 1.after#c.diff_post " After delimitation  $\times$  extra seats" 
  _cons "\addlinespace Constant")
keep(1.after 1.after#c.diff_post )
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N r2_p  , fmt(%9.3fc %9.0fc %9.3fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations" "Pseudo R$^2$"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	

stop




/*----------------------------------------------------*/
   /* [>   6.  DiD regressions  <] */ 
/*----------------------------------------------------*/
use "${gen}/dataset_districts.dta", replace

	reg diff_post  total_nax_pre2008 dt_pop_total dt_pop_sc gaezcultivated rainfed irrigated cultivated if year==2007, robust

	reghdfe ltotalevents  i.after c.diff_post##i.after  , absorb(id year) vce(cluster id)

gen bl_year = totalevents if year == 2000
bys id: egen bl_events = min(bl_year)
	reghdfe ltotalevents  i.after c.bl_events#i.year c.dt_pop_total#i.year  c.dt_pop_sc#i.year c.rainfed#i.year c.irrigated#i.year c.cultivated#i.year   c.dt_seats_perc_sc_reserved##i.after, absorb(id year) vce(cluster id)



/* [> Implement Stacked DiD <] */ 

forv x = 2008/2013 { 
	preserve 
gen keepyear = 0
gen t`x'=(treatyear==`x')
gen c`x'=0
forv y=`x'/2015 {
	// Control: 
	local z = `y'+1
	replace c`x' = 1 if treatyear==`z' & year<=`y'
}
replace keepyear = 1 if (year<=`x'+4) & (year>=`x'-4)
replace keepyear = 0 if t`x'==0 & c`x'==0
keep if keepyear 

gen s`x'=1
tempfile f`x'
save `f`x'', replace 
restore 
}


use `f2008', replace 
forv x=2009/2013 {
	append using `f`x''
}
forv x=2008/2013 {
	replace s`x'=0 if mi(s`x')
}



reghdfe ltotalevents i.after c.diff_post#c.after   , absorb(i.id#i.s2008 i.id#i.s2009 i.id#i.s2010 i.id#i.s2011 i.id#i.s2012 i.id#i.s2013 i.year#i.s2008 i.year#i.s2009 i.year#i.s2010 i.year#i.s2011 i.year#i.s2012 i.year#i.s2013) vce(cluster id)
reghdfe ltotalevents i.after c.dt_seats_perc_sc_reserved#c.after   , absorb(i.id#i.s2008 i.id#i.s2009 i.id#i.s2010 i.id#i.s2011 i.id#i.s2012 i.id#i.s2013 i.year#i.s2008 i.year#i.s2009 i.year#i.s2010 i.year#i.s2011 i.year#i.s2012 i.year#i.s2013) vce(cluster id)

reghdfe ltotalevents i.after c.dt_seats_perc_sc_reserved#c.after   , absorb(i.id#i.s2* year) vce(cluster id)

local conflictvars " totalevents killedinjured civilianskilled securitykilled militantskilled nonlethalevents abduct arrested"








	// reghdfe ltotalevents    , absorb(s2008 s2009 s2010 s2011 s2012 s2013 t2008 t2009 t2010 t2011 t2012 t2013) vce(cluster id)



	reghdfe ltotalevents  i.after c.diff_post#c.after if t2010==1 | c2010==1  , absorb(id year) vce(cluster id)

	xtreg ltotalevents  i.after i.year c.diff_post#c.after if t2010==1 | c2010==1  , fe vce(cluster id)
	xtreg ltotalevents  i.after i.year c.diff_post#c.after if t2010==1 | c2010==1  , fe vce(cluster id)


/* [> Placebo with other vars <] */ 
	reghdfe ltotalevents  i.after c.cultivated##i.after  , absorb(id year) vce(cluster code)
	reghdfe ltotalevents  i.after c.rainfed##i.after  , absorb(id year) vce(cluster code)
	reghdfe ltotalevents  i.after c.irrigated##i.after  , absorb(id year) vce(cluster code)
	reghdfe ltotalevents  i.after i.nax##i.after  , absorb(id year) vce(cluster code)
	reghdfe ltotalevents  i.after c.dt_seats_perc_sc_reserved##i.after c.diff_post##i.after , absorb(id year) vce(cluster id)

	reghdfe ltotalevents  i.after i.nax##i.after##c.diff_post  , absorb(id year) vce(cluster id)





use "${gen}/dataset_districts.dta", replace



foreach v in 	`conflictvars' {
		qui {
	cap drop l`v'
	gen l`v' = log(`v'+1)
	reghdfe l`v' i.after c.diff_post##i.after  , absorb(id year) vce(cluster id)
	// reghdfe `v' i.after c.diff_post##i.after if year!=2014 , absorb(id year) vce(cluster state year)
	qui sum `v' if e(sample)
	estadd scalar lmean = `r(mean)'
	est sto e`v'
}
}

#delimit ;
estout 
etotalevents ekilledinjured ecivilianskilled esecuritykilled emilitantskilled 
using "${output}/tables/reg_conflict_linear.tex" , style(tex)  
wrap varwidth(45) 
varlabels(1.after "After delimitation" 1.after#c.diff_post " After delimitation  $\times$ extra seats" 
  _cons "\addlinespace Constant")
keep(1.after 1.after#c.diff_post )
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N r2_p  , fmt(%9.3fc %9.0fc %9.3fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations" "Pseudo R$^2$"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	



/*------------------------------------ End of SECTION 1 ------------------------------------*/

cap drop rdd 
gen rdd = .
replace rdd = dt_sc_seats_by_rule-0.5 if  dt_sc_seats_by_rule<=1
replace rdd = dt_sc_seats_by_rule-1.5 if dt_sc_seats_by_rule>=1 & dt_sc_seats_by_rule<=2
replace rdd = dt_sc_seats_by_rule-2.5 if dt_sc_seats_by_rule>=2 & dt_sc_seats_by_rule<=3
replace rdd = dt_sc_seats_by_rule-3.5 if dt_sc_seats_by_rule>=3 & dt_sc_seats_by_rule<=4
replace rdd = dt_sc_seats_by_rule-4.5 if dt_sc_seats_by_rule>=4 & dt_sc_seats_by_rule<=5
replace rdd = dt_sc_seats_by_rule-5.5 if dt_sc_seats_by_rule>=5 & dt_sc_seats_by_rule<=6
replace rdd = dt_sc_seats_by_rule-6.5 if dt_sc_seats_by_rule>=6 & dt_sc_seats_by_rule<=7
replace rdd = dt_sc_seats_by_rule-7.5 if dt_sc_seats_by_rule>=7 & dt_sc_seats_by_rule<=8

cap drop rdd1
gen rdd1 = (rdd>=0)

cap drop rdd2 rtemp
gen rtemp = dt_sc_seats_by_rule-0.5
gen rdd2 = round(rtemp)

sc  rdd2 dt_sc_seats_by_rule if year==2014, ///
	xline(0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5, lpattern(dot)) ///
	ylab(,nogrid) col(%20) xlab(0(0.5)7) ysize(5) xsize(8.5) scale(1.25) xti("Fractional reserved seats") yti("Actual reserved seats")

gsort id year
reghdfe cmurder after L1.cmurder  c.after#i.rdd2  c.after#c.rdd1 c.after#c.dt_sc_seats_by_rule  , absorb(year id)  vce(cluster id)

reg cmurder rdd2 rdd1 rdd


/**********************************************************************/
/*  SECTION 2: Crime  			
    Notes:
    */
/**********************************************************************/
 local crimevars "cmurder crape ckidnap cdacoity carson crobbery churt poa pcr other totalcrimes"
/*----------------------------------------------------*/
   /* [>   2.1.  RD type regressions   <] */ 
/*----------------------------------------------------*/
foreach v in `crimevars' {
	qui {
			cap drop l`v'
			gen l`v' = log(`v'+1)
			reghdfe l`v' after c.after#c.rdd  c.after#c.rdd1 c.after#c.dt_sc_seats_by_rule , absorb(year id) vce(cluster id)

				// cap drop l`v'
				// gen l`v' = log(`v'+0.01)
				// xtreg l`v' i.after c.diff_post##i.after i.year , fe vce(cluster id)

				qui sum `v' if e(sample)
				qui estadd scalar lmean = `r(mean)'
				est sto rd`v'
			}
		}


#delimit ;
estout 
rdtotalcrimes rdpoa rdpcr rdother rdcmurder
using "${output}/tables/reg_rdd_crimes.tex" , style(tex)  
wrap varwidth(45) 
varlabels(after "After delimitation"
					c.after#c.rdd "After delimitation $\times$  rounding error" 
					c.after#c.rdd1 " After delimitation  $\times$  rounding error>0" 
					c.after#c.dt_sc_seats_by_rule " After delimitation  $\times$  fractional seats" 
  )
keep(c.after#c.rdd1 after c.after#c.rdd c.after#c.dt_sc_seats_by_rule)
order(c.after#c.rdd1 after c.after#c.rdd c.after#c.dt_sc_seats_by_rule )
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N  , fmt(%9.3fc %9.0fc %9.1fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	



/*----------------------------------------------------*/
   /* [>   5.  IV regressions with crime   <] */ 
/*----------------------------------------------------*/

foreach v in `crimevars' {
		qui {
	cap drop l`v'
	gen l`v' = log(`v'+1)
qui ivreghdfe l`v' after    (seats_treat = diff_treat), absorb(id year) robust

	// cap drop l`v'
	// gen l`v' = log(`v'+0.01)
	// xtreg l`v' i.after c.diff_post##i.after i.year , fe vce(cluster id)
	estadd scalar fstat = `e(widstat)'
	// reghdfe `v' i.after c.diff_post##i.after if year!=2014 , absorb(id year) vce(cluster state year)
	qui sum `v' if e(sample)
	estadd scalar lmean = `r(mean)'
	est sto iv`v'
}
}

#delimit ;
estout 
ivtotalcrimes ivpoa ivpcr ivother ivcmurder
using "${output}/tables/iv_crimes.tex" , style(tex)  
wrap varwidth(45) 
varlabels(after "After delimitation" seats_treat " After delimitation  $\times$ extra seats" 
  _cons "\addlinespace Constant")
keep(after seats_treat)
order(after seats_treat)
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N fstat  , fmt(%9.3fc %9.0fc %9.1fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations" "K-P F-stat"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	





 
/*----------------------------------------------------*/
   /* [>   6.  DiD regressions with crime   <] */ 
/*----------------------------------------------------*/

foreach v in `crimevars' {
		qui {
	cap drop l`v'
	gen l`v' = log(`v'+1)
	reghdfe l`v' i.after c.diff_post##i.after  , absorb(id year) vce(cluster id)
	// reghdfe `v' i.after c.diff_post##i.after if year!=2014 , absorb(id year) vce(cluster state year)
	qui sum `v' if e(sample)
	estadd scalar lmean = `r(mean)'
	est sto e`v'
}
}

#delimit ;
estout 
etotalcrimes epoa epcr eother ecmurder
using "${output}/tables/reg_crimes_linear.tex" , style(tex)  
wrap varwidth(45) 
varlabels(1.after "After delimitation" 1.after#c.diff_post " After delimitation  $\times$ extra seats" 
  _cons "\addlinespace Constant")
keep(1.after 1.after#c.diff_post )
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N r2_within  , fmt(%9.3fc %9.0fc %9.3fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations" "Within R$^2$"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	










 
/*----------------------------------------------------*/
   /* [>   7.  PPML regressions with crime   <] */ 
/*----------------------------------------------------*/

foreach v in `crimevars' {
	qui {
	ppmlhdfe `v' i.after c.diff_post##i.after  , absorb(id year) vce(cluster id)
	// reghdfe `v' i.after c.diff_post##i.after if year!=2014 , absorb(id year) vce(cluster state year)
	qui sum `v' if e(sample)
	estadd scalar lmean = `r(mean)'
	est sto e`v'
}
}

#delimit ;
estout 
etotalcrimes epoa epcr eother ecmurder
using "${output}/tables/reg_crimes.tex" , style(tex)  
wrap varwidth(45) 
varlabels(1.after "After delimitation" 1.after#c.diff_post " After delimitation  $\times$ extra seats" 
  _cons "\addlinespace Constant")
keep(1.after 1.after#c.diff_post )
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N r2_p  , fmt(%9.3fc %9.0fc %9.3fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations" "Pseudo R$^2$"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	



/*------------------------------------ End of SECTION 2 ------------------------------------*/

 









/**********************************************************************/
/*  SECTION 3: Heterogeneity  			
    Notes:



     */
/**********************************************************************/


/*------------------------------------ End of SECTION 3 ------------------------------------*/

beep




/*
