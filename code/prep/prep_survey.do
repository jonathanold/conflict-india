****************************************************************************************************
// IHDS-2: 2011
****************************************************************************************************

// Household level data
***************************************************
use "./Data/IHDS/IHDS_2/DS0002/36151-0002-Data.dta", clear
ren *, lower
gen year=2011

// Remove leading zeros
destring idhh, replace
tostring idhh, replace

gen hhid_s = string(hhid)
replace hhid_s =  "0" + hhid_s if length(hhid_s)<3
gen id_merge = string(idpsu) + hhid_s + string(hhsplitid)
order id_merge, after(idhh)

save "./Data/_gen/survey/IHDS_2_hh.dta", replace


// Village information
***************************************************
use "./Data/IHDS/IHDS_2/DS0012/36151-0012-Data.dta", clear
ren *, lower

global r "Pradhan MLA MP"
global t "pra mla mp" 
global s "17 18 19"

forvalues let=1/3 {
local n : word `let' of $r
local g : word `let' of $t
local y : word `let' of $s

gen `g'_reserved_sc_st = (vg`y'e==3)
gen `g'_reserved_woman = (vg`y'e==2)

gen `g'_is_sc_st = (vg`y'a==4 | vg`y'a==5) 
gen `g'_is_obc_lower = (vg`y'a==3 | vg`y'a==4 | vg`y'a==5) 
gen `g'_is_sc = (vg`y'a==4) 

label var `g'_reserved_sc_st "`n' seat reserved for SC/ST"
label var `g'_reserved_woman "`n' seat reserved for women"
label var `g'_is_sc_st "`n' is SC/ST member"
label var `g'_is_obc_lower "`n' is OBC/SC/ST member"
label var `g'_is_sc "`n' is SC member"
}

gen year=2011
save "./Data/_gen/survey/IHDS_2_village.dta", replace
merge 1:m stateid distid psuid year using "./Data/_gen/survey/IHDS_2_hh.dta"

drop _merge
save "./Data/_gen/survey/IHDS_2_merged.dta", replace







// Clean survey data for control variables
/////////////////////////////////////////////////////////////////////////////////////

// Define sample
bysort stateid: egen any_reserved_state = max(pra_reserved_sc_st)
gen sample = (any_reserved_state==1)


// Recode conflict variables
label define conflict 0 "not much" 1 "some" 2 "a lot"
foreach var of varlist tr1 tr3 {
replace `var'=. if `var'!=1 & `var'!=2 & `var'!=3
replace `var'=-`var'+3
label values `var' conflict
}
gen conflict_castes 	= tr3
gen conflict_village 	= tr1

label define social 0 "together" 1 "individually", replace
foreach var of varlist tr2 {
replace `var'=. if `var'!=1 & `var'!=2
replace `var'=`var'-1
label values `var' social
}


// Road
gen road = 0 if vi3==3 
replace road=1 if vi3==2
replace road=2 if vi3==1


// Rural: Same in 2005
bysort idpsu: egen rural=mean(currentrural)

label define rural 1 "Rural" 2 "Urban 2001" 3 "Urban 2011" , replace
label values rural rural

gen rural_status = 0
replace rural_status = 1 if rural==1
replace rural_status = 1 if rural==2
replace rural_status = 1 if rural==3 & year==2011


// Schools
gen p_schools = (vsa2 + vsc3)
gen m_schools = (vsa3 + vsc3)


// Caste
gen forward = (id13==1 | id13==2)
gen backward = (id13==3 | id13==5 | id13==6)

gen caste = backward + 2*forward
label define caste 0 "SC" 1 "Other backward" 2 "Forward"
label values caste caste

 
 // Castes in village
 gen sc_prop_village = vh1d / 100
 gen muslim_prop_village = vh2b / 100
 gen st_prop_village = vh1e / 100
 
 
 // Income 
 gen linc = log(income)
/////////////////////////////////////////////////////////////////////////////////////
// Run all regressions with/without controls and with/without clustered standard errors
// Set control varaibles
/*
- Percent SC: sc_prop_village

- Number of primary schools: p_schools
- Number of middle schools: m_schools

- Village accessible by road: road
*/


	
	
global vcontrols "sc_prop_village muslim_prop_village st_prop_village  linc m_schools p_schools hheduc road "
global icontrols "i.caste linc"

global samplecondition "year==2011 & vg17a!=3 & vg17a!=5 & sample==1 & !missing(pra_reserved_sc_st)"

preserve

collapse (mean) pra_reserved_sc_st hheduc state sc_prop_village muslim_prop_village st_prop_village vg17a sample p_schools m_schools road linc year, by(idpsu)
// The followign for the balance table on village level:
global depvarlist ${vcontrols} 
replace sc_prop_village = sc_prop_village*100
replace muslim_prop_village =  muslim_prop_village*100
replace st_prop_village =  st_prop_village *100 

label var muslim_prop_village "Percent Muslim population"
label var sc_prop_village "Percent SC population"
label var st_prop_village "Percent ST population"
label var p_schools "Number of primary schools"
label var m_schools "Number of middle schools"
label var road "Accessibility by road"
label var hheduc "Mean years of education in sample"
label var linc "Mean of log income in sample"

// muslim_prop_village st_prop_village


reg pra_reserved_sc_st  sc_prop_village muslim_prop_village st_prop_village     linc   m_schools p_schools hheduc  road i.state if ${samplecondition}
test muslim_prop_village st_prop_village p_schools m_schools road  linc hheduc

local fval: display %9.3f r(F)
local fpval: display %9.3f r(p)
// local fpval: display %9.3f Ftail(e(df_m), e(df_r), e(F))

 
balancetable  pra_reserved_sc_st $depvarlist if ${samplecondition} using "./Output/tables/balancetable_villages.tex", ///
	replace  ///
	varlabels ctitles("Unreserved" "Reserved" "Difference") ///
	pvalues  prefoot("\addlinespace  \midrule  Wald test statistic & & &  `fval' \\ p-value & & & `fpval' \\ \addlinespace \midrule ") ///
	format(%9.3f) booktabs
	
restore
	
	
	
	
	
global vcontrols "muslim_prop_village st_prop_village p_schools m_schools  i.road "
global icontrols "i.caste linc hheduc"
	
	
	
	
	   

// Reduced Form OLS regressions
reg tr3 pra_reserved_sc sc_prop_village $vcontrols i.state if ${samplecondition}  [pw=fwt] , cluster(idpsu)
	estadd ysumm
est store reg_vc

reg tr3 pra_reserved_sc sc_prop_village $vcontrols $icontrols i.state if ${samplecondition}  [pw=fwt] , cluster(idpsu)
	estadd ysumm
est store reg_ic


// Include interaction terms
reg tr3 c.pra_reserved_sc##c.sc_prop_village $vcontrols i.state if ${samplecondition}  [pw=fwt] , cluster(idpsu)
	estadd ysumm
est store reg_vc_int

reg tr3 c.pra_reserved_sc##c.sc_prop_village $vcontrols $icontrols i.state if ${samplecondition}  [pw=fwt] , cluster(idpsu)
	estadd ysumm
est store reg_ic_int


// Reduced Form OLS regressions
reg tr1 pra_reserved_sc sc_prop_village $vcontrols i.state if ${samplecondition}  [pw=fwt] , cluster(idpsu)
	estadd ysumm
est store reg1_vc

reg tr1 pra_reserved_sc sc_prop_village $vcontrols $icontrols i.state if ${samplecondition}  [pw=fwt] , cluster(idpsu)
	estadd ysumm
est store reg1_ic


// Include interaction terms
reg tr1 c.pra_reserved_sc##c.sc_prop_village $vcontrols i.state if ${samplecondition}  [pw=fwt] , cluster(idpsu)
	estadd ysumm
est store reg1_vc_int

reg tr1 c.pra_reserved_sc##c.sc_prop_village $vcontrols $icontrols i.state if ${samplecondition}  [pw=fwt] , cluster(idpsu)
	estadd ysumm
est store reg1_ic_int




#delimit ;
estout 
reg1_vc reg1_vc_int
reg1_ic  reg1_ic_int
reg_vc reg_vc_int
reg_ic  reg_ic_int
using "$output/tab_survey_1.tex", style(tex)  
varlabels(pra_reserved_sc_st "\textbf{Pradhan seat reserved}" sc_prop_village "Proportion SC population" c.pra_reserved_sc_st#c.sc_prop_village "\textbf{Reserved $\times$ SC pop}"
muslim_prop_village "Proportion Muslim" st_prop_village "Proportion Scheduled Tribes" p_schools "Number of primary schools" 1.caste "\addlinespace \addlinespace Respondent from backward caste" 2.caste "Respondent from forward caste"
 linc "Log(Total income)" )
drop(_cons muslim_prop_village st_prop_village p_schools m_schools hheduc *state *road 0.caste)
cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(N  N_clust r2 ymean, fmt(%9.0fc %9.0fc %9.2f %9.2f) labels("\midrule \addlinespace N" "Number of villages" "R$^2$" "Mean of DV"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none) ; 
#delimit cr





// Randomization inference: Do not report.


// ritest pra_reserved_sc_st _b[pra_reserved_sc_st], reps(1000) force cluster(idpsu) strata(distid) seed(546): reg tr3  pra_reserved_sc_st $vcontrols i.state if ${samplecondition}  [pw=fwt] 
// ritest pra_reserved_sc_st _b[pra_reserved_sc_st], reps(1000) force cluster(idpsu) strata(distid) seed(546): reg tr3  pra_reserved_sc_st $icontrols $vcontrols i.state if ${samplecondition}  [pw=fwt] 







///// With normal robust standard errors
*********************************************************************************************************
	   

// Reduced Form OLS regressions
reg tr3 pra_reserved_sc sc_prop_village $vcontrols i.state if ${samplecondition}  [pw=fwt] , robust
	estadd ysumm
est store reg_vc

reg tr3 pra_reserved_sc sc_prop_village $vcontrols $icontrols i.state if ${samplecondition}  [pw=fwt] , robust
	estadd ysumm
est store reg_ic


// Include interaction terms
reg tr3 c.pra_reserved_sc##c.sc_prop_village $vcontrols i.state if ${samplecondition}  [pw=fwt] , robust
	estadd ysumm
est store reg_vc_int

reg tr3 c.pra_reserved_sc##c.sc_prop_village $vcontrols $icontrols i.state if ${samplecondition}  [pw=fwt] , robust
	estadd ysumm
est store reg_ic_int


// Reduced Form OLS regressions
reg tr1 pra_reserved_sc sc_prop_village $vcontrols i.state if ${samplecondition}  [pw=fwt] , robust
	estadd ysumm
est store reg1_vc

reg tr1 pra_reserved_sc sc_prop_village $vcontrols $icontrols i.state if ${samplecondition}  [pw=fwt] , robust
	estadd ysumm
est store reg1_ic


// Include interaction terms
reg tr1 c.pra_reserved_sc##c.sc_prop_village $vcontrols i.state if ${samplecondition}  [pw=fwt] , robust
	estadd ysumm
est store reg1_vc_int

reg tr1 c.pra_reserved_sc##c.sc_prop_village $vcontrols $icontrols i.state if ${samplecondition}  [pw=fwt] , robust
	estadd ysumm
est store reg1_ic_int




#delimit ;
estout 
reg1_vc reg1_vc_int
reg1_ic  reg1_ic_int
reg_vc reg_vc_int
reg_ic  reg_ic_int
using "$output/tab_survey_robust.tex", style(tex)  
varlabels(pra_reserved_sc_st "\textbf{Pradhan seat reserved}" sc_prop_village "Proportion SC population" c.pra_reserved_sc_st#c.sc_prop_village "\textbf{Reserved $\times$ SC pop}"
muslim_prop_village "Proportion Muslim" st_prop_village "Proportion Scheduled Tribes" p_schools "Number of primary schools" 1.caste "\addlinespace \addlinespace Respondent from backward caste" 2.caste "Respondent from forward caste"
 linc "Log(Total income)" )
drop(_cons muslim_prop_village st_prop_village p_schools m_schools hheduc *state *road 0.caste)
cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(N   r2 ysumm , fmt(%9.0fc %9.2f %9.2f) labels("\midrule \addlinespace N" "R$^2$" "Mean of DV"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none) ; 
#delimit cr












































******************************************
// Read in Panel
// Household level
// For "bootstrapped estimates" at the village level
use "./Data/IHDS/IHDS_Panel/DS0011/37382-0011-Data.dta", clear
ren *, lower


// Survey year
replace hs3y = cd3y if missing(hs3y)
gen year = 2005 if hs3y<=2006
replace year = 2011 if hs3y>=2007 & !missing(hs3y)

// Merge with village information
merge m:1 stateid distid psuid year using "./Data/_gen/survey/IHDS_2_village.dta"
drop if _merge==2
drop _merge


// Set as panel
xtset hhbase2 year
gsort hhbase2 year

// Clean
******************************************



// Clean and recode conflict variables
label define conflict 0 "not much" 1 "some" 2 "a lot", replace
foreach var of varlist tr1 tr3 {
replace `var'=. if `var'!=1 & `var'!=2 & `var'!=3
replace `var'=-`var'+3
label values `var' conflict
}

label define social 0 "together" 1 "individually", replace
foreach var of varlist tr2 {
replace `var'=. if `var'!=1 & `var'!=2
replace `var'=`var'-1
label values `var' social
}


// Remove villages that were reserved for Scheduled tribes or others
bysort idpsu: egen st = mean(vg17a)
bysort idpsu: egen ever_reserved = max(pra_reserved_sc_st)
drop if st==3 
drop if st==5 


// Sample excludes states with no reservation
bysort stateid: egen any_reserved_state = max(pra_reserved_sc_st)
gen sample = (any_reserved_state==1)

// Sample includes only villages that had information on reservation status in 2011
global samplecondition "sample==1 & !missing(pra_reserved_sc_st)"

// Income as additional individual-level control
gen linc = log(income)

// 




gsort hhbase2 year

	// Reserved in 2011 = Not reserved in 2005
replace pra_reserved_sc_st = 0 if F6.pra_reserved_sc_st == 1

	// Alternative a): All never reserved
replace pra_reserved_sc_st = 0 if F6.pra_reserved_sc_st == 0 

xtreg tr1  pra_reserved_sc_st  linc i.year if $samplecondition [pw=fwt2005] , fe vce(cluster idpsu)
	estadd ysumm
est  store did1a

xtreg tr3  pra_reserved_sc_st  linc i.year if $samplecondition [pw=fwt2005] , fe vce(cluster idpsu)
	estadd ysumm
est  store did2a

	// Alternative b): All reserved
replace pra_reserved_sc_st = 1 if F6.pra_reserved_sc_st == 0 
xtreg tr1  pra_reserved_sc_st  linc  i.year if $samplecondition [pw=fwt2005] , fe vce(cluster idpsu)
	estadd ysumm
est  store did1b

xtreg tr3  pra_reserved_sc_st  linc  i.year if $samplecondition [pw=fwt2005] , fe vce(cluster idpsu)
	estadd ysumm
est  store did2b




#delimit ;
estout 
did1a did1b did2a did2b
using "$output/tab_survey_2.tex", style(tex)  
varlabels(pra_reserved_sc_st "\textbf{Pradhan seat reserved}" 2011.year "Time trend"
 linc "Log(Total income)" _cons "Constant")
drop(2005.year)
cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(N  N_clust r2 ymean, fmt(%9.0fc %9.0fc %9.2f %9.2f ) labels("\midrule \addlinespace N" "Number of villages" "R$^2$ (within)" "Mean of DV"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none) ; 
#delimit cr










*********************************************************************************************************
** With robust standard errors

	// Alternative a): All never reserved
replace pra_reserved_sc_st = 0 if F6.pra_reserved_sc_st == 0 

xtreg tr1  pra_reserved_sc_st  linc i.year if $samplecondition [pw=fwt2005] , fe robust
	estadd ysumm
est  store did1a

xtreg tr3  pra_reserved_sc_st  linc i.year if $samplecondition [pw=fwt2005] , fe robust
	estadd ysumm
est  store did2a

	// Alternative b): All reserved
replace pra_reserved_sc_st = 1 if F6.pra_reserved_sc_st == 0 
xtreg tr1  pra_reserved_sc_st  linc  i.year if $samplecondition [pw=fwt2005] , fe robust
	estadd ysumm
est  store did1b

xtreg tr3  pra_reserved_sc_st  linc  i.year if $samplecondition [pw=fwt2005] , fe robust
	estadd ysumm
est  store did2b




#delimit ;
estout 
did1a did1b did2a did2b
using "$output/tab_survey_did_robust.tex", style(tex)  
varlabels(pra_reserved_sc_st "\textbf{Pradhan seat reserved}" 2011.year "Time trend"
 linc "Log(Total income)" _cons "Constant")
drop(2005.year)
cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(N  r2 ymean, fmt(%9.0fc  %9.2f ) labels("\midrule \addlinespace N" "R$^2$ (within)" "Mean of DV"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none) ; 
#delimit cr










/*
// Regression by state is possible
levelsof stateid, local(l)
foreach x of local l {
cap qui xtreg mean_tr3  pra_reserved_sc_st i.year if stateid==`x' [pw=fwt2005] , fe vce(cluster idpsu)
matrix b = e(b)
matrix c = e(V)
local did = b[1,1]
local t = b[1,1]/sqrt(c
di "`t'"

di "DiD for `x' is `did', tval is `t'"
}

 

 
 






****************************************************************************************************
// IHDS-1
****************************************************************************************************

// Household level
**************************************************
use "./Data/IHDS/IHDS_1/DS0002/22626-0002-Data.dta", clear
ren *, lower

gen hhid_s = string(hhid)+string(hhsplitid)
// replace hhid_s =  hhid_s + "0" if length(hhid_s)<3
replace hhid_s =  "0" + hhid_s if length(hhid_s)<3
gen id_merge = string(idpsu) + hhid_s + string(hhsplitid)
order id_merge, after(idhh)

save "./Data/_gen/survey/IHDS_1_hh.dta", replace






// Village information	
**************************************************
use "./Data/IHDS/IHDS_1/DS0007/22626-0007-Data.dta", clear
ren *, lower
gen year=2005

save "./Data/_gen/survey/IHDS_1_village.dta", replace

// Merge with household data
merge 1:m stateid distid psuid using "./Data/_gen/survey/IHDS_1_hh.dta"
drop _merge
gen year=2005

tostring idhh, replace

save "./Data/_gen/survey/IHDS_1_merged.dta", replace

append using "./Data/_gen/survey/IHDS_2_merged.dta", force


****************************************************************************************************
// Merged data
****************************************************************************************************
order year stateid distid psuid idpsu id_merge idhh hhid hhsplitid
gsort id_merge
destring id_merge, replace
xtset id_merge year




label define conflict 0 "not much" 1 "some" 2 "a lot"
foreach var of varlist tr1 tr3 {
replace `var'=. if `var'!=1 & `var'!=2 & `var'!=3
replace `var'=-`var'+3
label values `var' conflict
}

label define social 0 "together" 1 "individually", replace
foreach var of varlist tr2 {
replace `var'=. if `var'!=1 & `var'!=2
replace `var'=`var'-1
label values `var' social
}


// Clean survey data for control variables
/////////////////////////////////////////////////////////////////////////////////////

// Road info: consistent over surveys
replace vi3a = 0 if vi3==3 & year==2011
replace vi3a = 1 if vi3==2 & year==2011
replace vi3a = 2 if vi3==1 & year==2011
replace vi3a = . if vi3a==-1

// Rural: Same as in 2005
bysort idpsu: egen rural=mean(currentrural)

label define rural 1 "Rural" 2 "Urban 2001" 3 "Urban 2011" , replace
label values rural rural

gen rural_status = 0
replace rural_status = 1 if rural==1
replace rural_status = 1 if rural==2
replace rural_status = 1 if rural==3 & year==2011


// Schools
gen p_schools = (vsa2 + vsc3)
gen m_schools = (vsa3 + vsc3)


// Caste
gen forward = (id13==1 | id13==2)
gen backward = (id13==4 | id13==5 | id13==6)

gen caste = backward + 2*forward
label define caste 0 "SC" 1 "Other backward" 2 "Forward"
label values caste caste
 

/////////////////////////////////////////////////////////////////////////////////////
// Run all regressions with/without controls and with/without clustered standard errors
// Set control varaibles
/*
- Percent SC: vh1d
- Percent Forward: vh1b
- Percent Muslim: vh2b

- Number of primary schools: p_schools
- Number of middle schools: m_schools

- Village accessible by road: vi3a
- Rural status: rural_status: Not relevant because only villages
*/

global controls "vh1d p_schools vj3a  vi3a"


// Reduced Form OLS regressions
reg tr3 i.pra_reserved_sc i.id13 i.id11 i.state if year==2011 & vg17a!=3 & vg17a!=5  [pw=fwt] , cluster(idpsu)

est store reg_nc_nc
reg tr3  i.pra_reserved_sc_st#i1.caste i.id11 i.state  $controls if year==2011  & vg17a!=3 & vg17a!=5  [pw=fwt] , cluster(idpsu)


, cluster(idpsu)
est store reg_nc_c

reg tr3 pra_reserved_sc_st i.vg17c  $controls if year==2011 & vg17a!=3 & vg17a!=4 [pw=fwt]
est store reg_c_nc
reg tr3 pra_reserved_sc_st i.pra_reserved_sc_st##i.vj3a $controls  if year==2011 [pw=fwt], cluster(idpsu)
est store reg_c_c


ritest pra_reserved_sc_st _b[pra_reserved_sc_st], reps(1000) force cluster(idpsu) strata(distid) seed(546): reg tr3  pra_reserved_sc_st i.id13 i.id11 i.state  if year==2011  & vg17a!=3 & vg17a!=5  [pw=fwt] 
ritest pra_reserved_sc_st _b[pra_reserved_sc_st], reps(1000) force cluster(idpsu) strata(distid) seed(546): reg tr3  pra_reserved_sc_st i.id13 i.id11 i.state  $controls if year==2011  & vg17a!=3 & vg17a!=5  [pw=fwt] 




// 2SLS estimates
ivreg2 tr3   (pra_is_sc_st=pra_reserved_sc_st) if year==2011
est store iv_nc_nc
ivreg2 tr3   (pra_is_sc_st=pra_reserved_sc_st) if year==2011, cluster(idpsu)
est store iv_nc_c

ivreg2 tr3  $controls (pra_is_sc_st=pra_reserved_sc_st) if year==2011
est store iv_c_nc
ivreg2 tr3  $controls (pra_is_sc_st=pra_reserved_sc_st) if year==2011, cluster(idpsu)
est store iv_c_c




#delimit ;
estout 
reg_nc_nc reg_nc_c
reg_c_nc reg_c_c
iv_nc_nc iv_nc_c
iv_c_nc iv_c_c
using "$output/tab_survey_1.tex", style(tex)  
varlabels(pra_reserved_sc_st "\textbf{Pradhan seat reserved}" vh1d "Proportion SC population"
vh1b "Proportion forward castes" vh2b "Proportion Muslim" p_schools "Number of primary schools"
m_schools "Number of middle schools" vi3a "Village accessible by road")
drop(_cons)
cells(b(star fmt(%9.2f)) se(par)) 
stats(N  , fmt(%9.0fc) labels("N" ))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none) ; 
#delimit cr
























forvalues z=1(2)3 {
gen tr`z'_someconflict = (tr`z'==1 | tr`z'==2)
replace tr`z'_someconflict = . if missing(tr`z')
gen tr`z'_lotsconflict = (tr`z'==2)
replace tr`z'_lotsconflict = . if missing(tr`z')
gen tr`z'_lotscvnone = .
replace tr`z'_lotscvnone = 1 if (tr`z'==2)
replace tr`z'_lotscvnone = 0 if (tr`z'==0)
}



local controls_survey "vh1d vh1e vh1b vh2a vh2b vi3a currentrural"

ivreg2 tr3_somec   (pra_is_sc_st=pra_reserved_sc_st) if year==2011,  cluster(idpsu)
ivreg2 tr3_somec  vh1d vh1e vh1b vh2a vh2b vi3a currentrural (pra_is_sc_st=pra_reserved_sc_st) if year==2011,  cluster(idpsu)


logit tr3_somec pra_reserved_sc_st vh1d vh1e vh1b vh2a vh2b vi3a currentrural  if year==2011


ivprobit tr3_somecon vh1d vh1e vh1b vh2a vh2b vi3a   (pra_is_sc_st=pra_reserved_sc_st) if year==2011
ivprobit tr3_somecon vh1d vh1e vh1b vh2a vh2b vi3a   (pra_is_sc_st=pra_reserved_sc_st) if year==2011, vce(cluster idpsu)
ivprobit tr3_lotscon vh1d vh1e vh1b vh2a vh2b vi3a   (pra_is_sc_st=pra_reserved_sc_st) if year==2011
ivprobit tr3_lotscv vh1d vh1e vh1b vh2a vh2b vi3a   (pra_is_sc_st=pra_reserved_sc_st) if year==2011



logit tr3_somecon  pra_reserved_sc_st vh1d vh1e vh1b vh2a vh2b vi3a , vce(cluster idpsu)


ologit tr3  pra_reserved_sc_st vh1d vh1e vh1b vh2a vh2b vi3a , vce(cluster idpsu)
ologit tr1  pra_reserved_sc_st vh1d vh1e vh1b vh2a vh2b vi3a , vce(cluster idpsu)

reg tr3  pra_reserved_sc_st vh1d vh1e vh1b vh2a vh2b vi3a
reg tr3  mla_reserved_sc_st vh1d vh1e vh1b vh2a vh2b vi3a
reg tr3  mp_reserved_sc_st vh1d vh1e vh1b


ologit tr1  mla_reserved_sc_st vh1d vh1e vh1b

ivreg2 tr3  vh1d vh1e vh1b vh2a vh2b vi3a  (pra_is_sc_st=pra_reserved_sc_st)
ivreg2 tr3  vh1d vh1e vh1b vh2a vh2b vi3a (mla_is_sc_st=mla_reserved_sc_st)



ivreg2 tr1 vh1d vh1e vh1b vh2a vh2b vi3a  (pra_is_sc_st=pra_reserved_sc_st) if year==2011









