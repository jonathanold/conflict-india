 
/*----------------------------------------------------*/
   /* [>   1.  Read in data from Bhavnani and generate district IDs   <] */ 
/*----------------------------------------------------*/

use "${data}/Bhavnani_Replication Code/APP2016-0030_data/dist08analysis.dta", clear

decode uniqdist_coded, gen(district)
split district, parse("/") 

statenames, sv(district1)

replace district2 = strltrim(district2)

gen district_bhavnani = code + "_" + district2

replace district_bhavnani = subinstr(district_bhavnani, "West", "Paschim", 1)
replace district_bhavnani = subinstr(district_bhavnani, "East", "Purvi", 1)
replace district_bhavnani = subinstr(district_bhavnani, "Burdwan", "Bardhaman", 1)
replace district_bhavnani = lower(district_bhavnani)

foreach var of varlist _all {
	local lbl : variable label `var' 
	local lbl = "`lbl'" + " (Bhavnani)"
	label var `var' "`lbl'"
}
gen obs_in_bhavnani = 1
save "${gen}/bhavnani_district.dta", replace

ren uniqdist_coded id_bhavnani
keep district_bhavnani  id_bhavnani
save "$gen/bhavnani_ids.dta", replace




/*----------------------------------------------------*/
   /* [>   2.  Match IDs with delimitation data   <] */ 
/*----------------------------------------------------*/

matchit id_bhavnani district_bhavnani using "${data}/_gen/delimitations_district_ids.dta" , idusing(district_id) txtusing(district_crosswalk) override

// Drop matched districts from different states
drop if  substr(district_bhavnani,1,2)!=substr(district_crosswalk,1,2)

replace similscore = round(similscore, 0.0001)
cap drop max_*

bysort district_bhavnani: egen max_1 = max(similscore)
bysort district_crosswalk: egen max_2 = max(similscore) 

cap drop keep
gen keep = 0
replace keep = 1 if round(similscore,0.0001) == round(max_1,0.0001) & round(max_1,0.0001) == round(max_2,0.0001)
bysort district_bhavnani: egen max_3 = max(keep)

drop if keep==0 // & max_3==1
// br if keep==0

keep district_bhavnani district_crosswalk

save "$gen/matchit_bhavnani.dta", replace



/*



 
/*----------------------------------------------------*/
   /* [>   2.  Merge Bhavnani and delimitation data with district matches   <] */ 
/*----------------------------------------------------*/
use "./Maps/matchit_bhav.dta", clear

merge 1:m id district_bhavnani using "./Data/_gen/delimitations_all_bhav.dta"
keep if _merge==3 
drop _merge
merge m:1 district_bhavnani1 using "./Data/_gen/Bhavnani_edit.dta"
keep if _merge==3
drop _merge



save "./Data/_gen/bhav_1.dta", replace





 
/*----------------------------------------------------*/
   /* [>   3.  Replicate regressions from Bhavnani   <] */ 
/*----------------------------------------------------*/
use "./Data/_gen/bhav_1.dta", replace

// Fpost = pop_perc_sc

*Tab 1
areg propconsscwinner Rpre Rpost 						scprop noofconspre noofconspost totnewarea, a(stateyear) cl(stateyear)
areg propconsscwinner Rpre Rpost Fpre Fpost 			scprop noofconspre noofconspost totnewarea, a(stateyear) cl(stateyear)
areg propconsscwinner Rpre Rpost Fpre pop_perc_sc 		scprop noofconspre noofconspost totnewarea, a(stateyear) cl(stateyear)



*Tab 2
areg propconsscwinner Rpre Rpost Fpre pop_perc_sc nscprop 		scprop noofconspre noofconspost totnewarea, a(stateyear) cl(stateyear)
areg propconsscwinner Rpre Rpost Fpre pop_perc_sc prnewarea 	scprop noofconspre noofconspost totnewarea, a(stateyear) cl(stateyear)

*Tab 3
foreach var in meannoofsccancons propmeannoofsccancons propconsscran totscvoteper {
	areg `var' Rpre Rpost Fpre pop_perc_sc scprop noofconspre noofconspost totnewarea, a(stateyear) cl(stateyear)
}











/*----------------------------------------------------*/
   /* [>   2.  ID-matchit Fetzer data with Bhavnani data   <] */ 
/*----------------------------------------------------*/

matchit id district_fetzer using "./Data/_gen/Bhavnani_edit.dta" , idusing(uniqdist_coded) txtusing(district_bhavnani) override

/* [> Drop observations from different states <] */  
drop if  substr(district_bhavnani,1,2)!=substr(district_fetzer,1,2)

replace similscore = round(similscore, 0.0001)
cap drop max_*

bysort district_fetzer: egen max_1 = max(similscore)
bysort district_bhavnani: egen max_2 = max(similscore) 

cap drop keep
gen keep = 0
replace keep = 1 if round(similscore,0.0001) == round(max_1,0.0001) & round(max_1,0.0001) == round(max_2,0.0001)
bysort district_bhavnani: egen max_3 = max(keep)
// drop if keep==0 

gen keep1 = 0
replace keep1 = 1 if round(similscore,0.0001) == round(max_1,0.0001)
drop if keep1==0

merge 1:1 district_fetzer using "$gen/fetzer_district.dta", keepusing(id district_fetzer )

ren district_bhavnani district_bhavnani
ren district_fetzer district_bhavnani

save "$gen/district_id_master.dta", replace

// matchit_bf

 
/*----------------------------------------------------*/
   /* [>   3.  Merge back with Fetzer data   <] */ 
/*----------------------------------------------------*/
merge 1:m district_fetzer using 

