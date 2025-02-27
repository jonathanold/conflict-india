




// Simulation

clear
cap log close
//  set seed 123


set obs 25
gen state = _n

// villages
expand 100
bysort state: gen village_fe = runiform(0,1)
label var village "Village fixed effect in state"
bysort state: gen village = runiform(0,1)
label var village "Random assignment to treatment"
forvalues i = 1/10 {
bysort state: gen dice_simul_`i' = runiform(0,1)
label var village "Random var"
}

gen village_id = _n

// years
expand 2
sort state
bysort state village: gen year = _n
gen n = year




// Treatment years
gen treated_now		= (village>=2/3)
gen treated_past	= (village<=1/3)
gen never_treated = (treated_now==0 & treated_past==0)

gen treated = 1 if year==2 & treated_now==1
replace treated = 1 if year==1 & treated_past==1
replace treated = 0 if treated==.


gen treated_observed = (treated==1 & treated_now==1)

gen group = 1 if never_treated==1
replace group = 2 if treated_past==1
replace group = 3 if treated_now==1


gen group_observed = 1 if treated_now==0
replace group_observed = 2 if treated_now==1

// DGP
// Treatment effect
gen teffect = rnormal(1, 0.2^2)
// Error
gen error 	= rnormal(0, 1^2)

// Conflict
gen conflict = village_fe + 0.15*village + 0.1*year + treated*teffect + error



xtset village_fe year
gsort village_fe year



//// Analysis

// Diff in Diff with actual data
xtreg conflict i.year treated, fe


// Generate actual differences: FD-estimator
gen dconflict = D1.conflict
gen dtreated = D1.treated
gen dyear = D1.year

reg dconflict dtreated
matrix b = e(b)
local did_true = b[1,1]
local did_year = b[1,2]
di `did_year'



// // Recoded
// gen conf_rec = conflict
// replace conf_rec = conflict - `did_year' if year==2

// gen cr = conf_rec
// replace cr = F1.conf_rec if group==2 & year==1
// replace cr = L1.conf_rec if group==2 & year==2

// gen tr = 0
// replace tr = 1 if group==2 & year==2
// replace tr = 1 if group==3 & year==2

// xtreg cr tr, fe
// matrix b = e(b)
// local did_rec = b[1,1]
// di "`did_rec'"


// di "`did_true'" 
// di "`did_rec'"





////// Recoding properly
gen conf_r = conflict
qui sum conflict if group==1 & year==1
local cnt1 = r(mean)
qui sum conflict if group==1 & year==2
local cnt2 = r(mean)

replace conf_r = conflict - `cnt1' if year==1
replace conf_r = conflict - `cnt2' if year==2

// Recode for group 2
cap drop cr
gen cr=conf_r
replace cr = F1.conf_r if group==2 & year==1
replace cr = L1.conf_r if group==2 & year==2

// Treated variable
cap drop tr
gen tr = 0
replace tr = 1 if group==2 & year==2
replace tr = 1 if group==3 & year==2


xtreg cr tr, fe
matrix b = e(b)
local did_rec = b[1,1]
di "`did_rec'"

di "`did_true'" 
di "`did_rec'"

qui sum cr if year==1 & group==1
local npr=r(mean)
qui sum cr if year==2 & group==1
local npo=r(mean)
di `npo' - `npr'

qui sum cr if year==1 & group==2
local ppr=r(mean)
qui sum cr if year==2 & group==2
local ppo=r(mean)
qui sum cr if year==1 & group==3
local tpr=r(mean)
qui sum cr if year==2 & group==3
local tpo=r(mean)

count if group==2
local two=r(N)
count if group==3
local three=r(N)

local did_new = `two'/(`two'+`three')*(`ppo' - `ppr') + `three'/(`two'+`three')*(`tpo' - `tpr')

qui sum conflict if group==1 & year==1
local n11 = r(mean)
qui sum conflict if group==1 & year==2
local n12 = r(mean)

qui sum conflict if group==2 & year==1
local p11 = r(mean)
qui sum conflict if group==2 & year==2
local p12 = r(mean)

qui sum conflict if group==3 & year==1
local p21 = r(mean)
qui sum conflict if group==3 & year==2
local p22 = r(mean)


local did_rw = 	`three'/(`two'+`three')*(`p22' - `p21') - ///
				`two'/(`two'+`three')*(`p12' - `p11') + ///
				(`two'-`three')/(`two'+`three')*(`n12' - `n11')

di "`did_true'" 
di "`did_rec'"
di "`did_new'"
di "`did_new2'"
di "`did_rw'"








gen dtmin = (dtreated==-1)
gen dtplus = (dtreated==1)


gen dtcom = dtplus - dtmin // = dtreated: Difference between the two
// We simply impose that the effect is the same
reg dconflict dtcom
matrix b = e(b)
local coef_constr = b[1,1]




reg dconflict dtmin dtplus
matrix b = e(b)
local coef_off = b[1,1]
local coef_on = b[1,2]
local coef_nv = b[1,3]

bysort dtreated: egen ybar = mean(dconflict)
reg ybar dtreated
reg ybar dtmin dtplus
reg ybar dtplus


sum dtplus
local plus = r(mean)*r(N)
di `plus'

sum dtmin
local minus = r(mean)*r(N)
di `minus'

local mwt = `minus' / (`minus' + `plus') 
local pwt = `plus' / (`minus' + `plus') 
local ttl = (`mwt' + `pwt')
di "`mwt' , `pwt', `ttl'"


local mwt1 = `minus' / 2500
local pwt1 = `plus' / 2500
local nt1 = (2500- `minus' -`plus') / 2500

local nnv =  (2500- `minus' -`plus') / (2500- `plus')
local npast = (`minus') / (2500- `plus')


local did_w = (-(`coef_off'-`coef_nv')*`mwt' + (`coef_on'-`coef_nv')*`pwt') 
local did_u = (-`coef_off' + `coef_on') /2
local did_t = (`coef_on'*`pwt1' + `coef_nv'*`nt1' -`coef_off'*`mwt1')
di "`did_t'"

 local d1 = `did_w' - `did_true'
 local d2 = `did_u' - `did_true'
 local d3 = `coef_constr' - `did_true'

di "Diff between unweigheted and true: `d2'"
di "Diff between weigheted and true: `d1'"
di "Diff from constrained regression: `d3'"


di "We take the unweighted DiD estimate `did_u'"


local dwrong = (`coef_on' + `coef_nv') - `nnv'*(`coef_nv') - `npast'*(`coef_off'+`coef_nv')
di `dwrong'

local dt1 = (`coef_on' + `coef_nv') - `nnv'*(`coef_nv') 
di `dt1'
local dt2 = (`coef_on' + `coef_nv') - `npast'*(`coef_off'+`coef_nv')
di `dt2'
// .767066170711297



















bysort group year: egen var = mean(conflict)

		tsline var if group==1, xlabel(1/2) yti("Outcome") legend(label(1 "Never treated")) || ///
		tsline var if group==2, legend(label(2 "Treated in period 1")) || ///
		tsline var if group==3, legend(label(3 "Treated in period 2"))

tab var group 

// How to recover DiD estimate here?
qui sum conflict if group==1 & year==1
local nt_pre = r(mean)
qui sum conflict if group==1 & year==2
local nt_post = r(mean)

qui sum conflict if group==2 & year==1
local t1_pre = r(mean)
qui sum conflict if group==2 & year==2
local t1_post = r(mean)

qui sum conflict if group==3 & year==1
local t2_pre = r(mean)
qui sum conflict if group==3 & year==2
local t2_post = r(mean)

local did = (`t2_post' - `t2_pre') - (`nt_post' - `nt_pre') + (`t1_post' - `t1_pre') - (`nt_post' - `nt_pre')
di "DiD estimate is `did'"




// Diff in Diff with observed data
xtreg conflict i.year treated_observed, fe

bysort group_observed year: egen var2 = mean(conflict)

		tsline var2 if group_observed==1, xlabel(1/2) yti("Outcome") legend(label(1 "Treatment unobserved")) || ///
		tsline var2 if group_observed==2, legend(label(2 "Treatment observed")) 

	tab var2 group_observed
	
// DID estimate
qui sum conflict if group_observed==1 & year==1
local untreated_pre = r(mean)
qui sum conflict if group_observed==1 & year==2
local untreated_post = r(mean)

qui sum conflict if group_observed==2 & year==1
local treated_pre = r(mean)
qui sum conflict if group_observed==2 & year==2
local treated_post = r(mean)

local did = (`treated_post' - `treated_pre') - (`untreated_post' - `untreated_pre')
di "DiD estimate is `did'"



gsort village year
xtset village year


// Other observed possibility
gen treated_observed_2 = treated_observed
replace treated_observed_2 = 1 if F1.treated==0


xtreg conflict i.year treated_observed_2, fe
matrix b = e(b)
local coef_obs_2 = b[1,3]

local did_avg =  (1/3*`did' + 2/3*`coef_obs_2')
di "Average DiD estimate is `did_avg', while actual DiD estimate is `did_true'"











/// Simulate: take one third and check

forvalues i=1/10 {
cap drop treated_simulated

gen treated_simulated = treated_observed 
replace treated_simulated = 1 if (year==1 & dice_simul_`i'<=1/2 & F1.treated_observed==0)
xtreg conflict i.year treated_simulated, fe

matrix b = e(b)
local coef_simul_`i' = b[1,3]
}
	
xtreg conflict i.year treated, fe
matrix b = e(b)
local coef_true = b[1,3]

xtreg conflict i.year treated_observed, fe
matrix b = e(b)
local coef_obs = b[1,3]		
		
di "True is `coef_true'"
di "Observed is `coef_obs'"

forvalues i=1/10 {
di "Simulated is `coef_simul_`i''"
}
		

		
