// prep_ovde.do

use "$data/Data_OVDE/targets_of_violence.dta", clear

keep year state district deathevent antiattack attack civattack 
statenames, sv(state)
replace district = lower(district)
drop if district=="total"
replace code = lower(code)

gen district_ovde = code + "_" + district
encode district_ovde, gen(id_ovde)

gen obs_in_ovde = 1
save "$gen/ovde_district.dta", replace

bysort district_ovde : gen seq=_n
bysort district_ovde : gen max=_N
keep if seq==max

keep district_ovde id_ovde

save "$gen/ovde_ids.dta", replace




 
/*----------------------------------------------------*/
   /* [>   2.  Merge to delimitation data   <] */ 
/*----------------------------------------------------*/
matchit id_ovde district_ovde using "${gen}/delimitations_district_ids.dta" , idusing(district_id) txtusing(district_crosswalk) override


// Drop matched districts from different states
drop if  substr(district_ovde,1,2)!=substr(district_crosswalk,1,2)

replace similscore = round(similscore, 0.0001)
cap drop max_*

bysort district_ovde: egen max_1 = max(similscore)
bysort district_crosswalk: egen max_2 = max(similscore) 

cap drop keep
gen keep = 0
replace keep = 1 if round(similscore,0.0001) == round(max_1,0.0001) & round(max_1,0.0001) == round(max_2,0.0001)
bysort district_ovde: egen max_3 = max(keep)

drop if keep==0  & max_3==1
 br if keep==0

keep district_ovde district_crosswalk

save "$gen/matchit_ovde.dta", replace


