 
/*----------------------------------------------------*/
   /* [>   1.  Read in Fetzer data   <] */ 
/*----------------------------------------------------*/
 use "${data}/fetzer_2019_jeea/data files/ANALYSIS.DATA.dta"
keep year-nonlethalevents rainfed-ANNUAL_temp agriculturalgdp logagriwage lightsum treat
foreach var of varlist _all {
	local lbl : variable label `var' 
	local lbl = "`lbl'" + " (Fetzer)"
	label var `var' "`lbl'"
}

foreach var of varlist rainfed cultivated {
	cap replace `var' = "" if `var'=="NA"
	cap destring `var', replace
}

drop if mi(districtmatch)
gen district_fetzer = code + "_" + districtmatch
replace district_fetzer = lower(district_fetzer)

replace district_fetzer = "up_kanpur nagar" if district_fetzer=="up_kanpur"
replace district_fetzer = "or_balasore" if district_fetzer=="or_baleshwar"
replace district_fetzer = "or_subarnapur" if district_fetzer=="or_sonepur"
replace district_fetzer = "up_mahamaya nagar" if district_fetzer=="up_hathras"
replace district_fetzer = "ut_garhwal" if district_fetzer=="ut_pauri garhwal"

encode district_fetzer, gen(id_fetzer)
gen obs_in_fetzer = 1
save "$gen/fetzer_district.dta", replace

bysort district_fetzer : gen seq=_n
bysort district_fetzer : gen max=_N
keep if seq==max
drop if missing(districtmatch)

keep district_fetzer id_fetzer

save "$gen/fetzer_ids.dta", replace




 
/*----------------------------------------------------*/
   /* [>   2.  Merge to delimitation data   <] */ 
/*----------------------------------------------------*/
matchit id_fetzer district_fetzer using "${gen}/delimitations_district_ids.dta" , idusing(district_id) txtusing(district_crosswalk) override


// Drop matched districts from different states
drop if  substr(district_fetzer,1,2)!=substr(district_crosswalk,1,2)

replace similscore = round(similscore, 0.0001)
cap drop max_*

bysort district_fetzer: egen max_1 = max(similscore)
bysort district_crosswalk: egen max_2 = max(similscore) 

cap drop keep
gen keep = 0
replace keep = 1 if round(similscore,0.0001) == round(max_1,0.0001) & round(max_1,0.0001) == round(max_2,0.0001)
bysort district_fetzer: egen max_3 = max(keep)

drop if keep==0 // & max_3==1
// br if keep==0

keep district_fetzer district_crosswalk

save "$gen/matchit_fetzer.dta", replace


