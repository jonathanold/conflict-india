 
/*------------------------- 
	To Do:
		 1. Clean up this file  
		 2. Re-make all regression tables
-------------------------*/ 

cap ssc install shp2dta


/*----------------------------------------------------*/
   /* [>   1.  Merge AC data with relevant district data.   <] */ 
/*----------------------------------------------------*/
use "${gen}/delimitations/all_states.dta", clear
drop if missing(district)
gen district_delimdata = state+"_"+lower(proper(district))


save "${gen}/ac_rounding_info.dta", replace
use "${gen}/ac_rounding_info.dta", replace
matchit n  district_delimdata using "${gen}/district_data_for_ac.dta" , idusing(id) txtusing(district_crosswalk) 

// Drop observations from different states
drop if  substr(district_delimdata,1,2)!=substr(district_crosswalk,1,2)

replace similscore = round(similscore, 0.0001)
cap drop max_*
bysort district_delimdata: egen max_1 = max(similscore)
bysort district_crosswalk: egen max_2 = max(similscore) 


cap drop keep
gen keep = 0
replace keep = 1 if round(similscore,0.0001) == round(max_1,0.0001) & round(max_1,0.0001) == round(max_2,0.0001)
bysort district_delimdata: egen max_3 = max(keep)

drop if keep==0 & max_3==1
// THIS IS WEIRD!!
collapse (firstnm) district_delimdata district_crosswalk, by(id)

merge 1:1 district_crosswalk using "${gen}/district_data_for_ac.dta"
drop _merge 

save "${gen}/ac_with_district_data.dta", replace
use "${gen}/ac_with_district_data.dta", replace




 
/*----------------------------------------------------*/
   /* [>   2.  Read in delimitation data, generate summary variables, generate samples   <] */ 
/*----------------------------------------------------*/
use "${gen}/delimitations/all_states.dta", clear

order name district state total sc perc_sc sc_reserved st_reserved 
gsort state district name 

label var name "Assembly constituency name"
label var district "District name"
label var state "State code"
label var total "Total population at assembly constituency level"
label var sc "SC population at assembly constituency level"
label var perc_sc "Share of SC population at assembly constituency level"
label var sc_reserved "AC reserved for SCs"
label var st_reserved "AC reserved for STs"

drop sc_seats n m is_district


drop if missing(district)
gen one = 1
label var one "One (for computations)"

bysort district state: egen dt_sc_reserved_total = sum(sc_reserved)
label var dt_sc_reserved_total "Reserved seats for SCs at district level"
bysort district state: egen dt_seats_total = sum(one)
label var dt_seats_total "Number of seats at district level"
gen dt_seats_perc_sc_reserved = dt_sc_reserved_total / dt_seats_total
label var dt_seats_perc_sc_reserved "Share of seats at district level reserved for SCs"

bysort district state: egen dt_pop_sc = sum(sc)
label var dt_pop_sc "Total SC population at district level"

bysort district state: egen dt_pop_total = sum(total)
label var dt_pop_total "Total population at district level"

gen dt_perc_sc_pop = dt_pop_sc/dt_pop_total
label var dt_perc_sc_pop "SC population share at district level"


bys state: egen st_pop_sc = sum(sc)
label var st_pop_sc "Scheduled caste population at state level"
cap drop st_tot
bys state: egen st_pop_total = sum(total)
label var st_pop_total "Total population at state level"
bys state: egen st_sc_reserved_total = sum(sc_reserved)
label var st_sc_reserved_total "Reserved seats for SCs at state level"


gsort state district name 


/* [> Generate variables for district-level variation <] */
gen dt_sc_seats_by_rule = dt_pop_sc/st_pop_sc*st_sc_reserved_total
label var dt_sc_seats_by_rule "Expected SC seats at district level (if rule followed)"
// gen dt_rounding_error = dt_sc_reserved_total-dt_sc_seats_by_rule
// label var dt_rounding_error "Difference between actual and expected SC seats at district level"








// Generate sample for constituency-level analysis

// Runner-ups: Those who are just below the cutoff
gsort district -perc_sc
// Sample 1: Just above/below cutoff
gen runner_up_sc_1 = (sc_reserved==0 & sc_reserved[_n-1]==1 & district==district[_n-1])
gen sc_sample_1 = runner_up_sc_1[_n+1] if district==district[_n+1]
replace sc_sample_1 = 0 if missing(sc_sample_1)
gen sample_1 = 0
replace sample_1 = 1 if sc_sample_1 == 1 | runner_up_sc_1 == 1


// Sample 2: Two up and down
gen sample_2 = 0
gen sc_sample_2 =  sc_sample_1
replace sc_sample_2 = 1 if sc_reserved==1 & sc_sample_1[_n+1]==1 & district==district[_n+1]
gen runner_up_sc_2 = runner_up_sc_1
replace runner_up_sc_2 = 1 if sc_reserved==0 & runner_up_sc_1[_n-1]==1 & district==district[_n-1]

replace sample_2 = 1 if sc_sample_2 == 1 | runner_up_sc_2 == 1


// Sample 3: 5% margin
gen sample_3 = 0
gen sc_sample_3 =  sc_sample_1
 forvalues i=1/5 {
 replace sc_sample_3 = 1 if sc_reserved==1 & sc_sample_1[_n+`i']==1 & district==district[_n+`i'] & perc_sc - perc_sc[_n+`i'] <= 5
 }
gen runner_up_sc_3 = runner_up_sc_1
 forvalues i=1/5 {
 replace runner_up_sc_3 = 1 if sc_reserved==0 & runner_up_sc_1[_n-`i']==1 & district==district[_n-`i'] & perc_sc - perc_sc[_n-`i'-1] >= -5
 }
replace sample_3 = 1 if sc_sample_3 == 1 | runner_up_sc_3 == 1



// Sample 4: 3% margin
gen sample_4 = 0
gen sc_sample_4 =  sc_sample_1
 forvalues i=1/5 {
 replace sc_sample_4 = 1 if sc_reserved==1 & sc_sample_1[_n+`i']==1 & district==district[_n+`i'] & perc_sc - perc_sc[_n+`i'] <= 3
 }
gen runner_up_sc_4 = runner_up_sc_1
 forvalues i=1/5 {
 replace runner_up_sc_4 = 1 if sc_reserved==0 & runner_up_sc_1[_n-`i']==1 & district==district[_n-`i'] & perc_sc - perc_sc[_n-`i'-1] >= -3
 }
replace sample_4 = 1 if sc_sample_4 == 1 | runner_up_sc_4 == 1




// Sample 5: 3% margin from bottom also
gen sample_5 = 0
gen sc_sample_5 =  sc_sample_1
 forvalues i=1/5 {
 replace sc_sample_5 = 1 if sc_reserved==1 & sc_sample_1[_n+`i']==1 & district==district[_n+`i'] & perc_sc - perc_sc[_n+`i'] <= 3
 }
gen runner_up_sc_5 = runner_up_sc_1
 forvalues i=1/5 {
 replace runner_up_sc_5 = 1 if sc_reserved==0 & runner_up_sc_1[_n-`i']==1 & district==district[_n-`i'] & perc_sc - perc_sc[_n-`i'-1] >= -3
 }
replace sample_5 = 1 if sc_sample_5 == 1 | runner_up_sc_5 == 1





cap drop min_sc_threshold
cap drop temp
bysort state: egen temp = min(perc_sc) if sc_reserved==1
bysort state: egen min_sc_threshold = min(temp)
cap drop temp

// Some more cleaning
ren name ac_name
ren district district_name
gen id = _n
replace state=upper(state)

// Required for matchit
replace ac_name = "Barauli" if ac_name=="Baroli"

duplicates tag ac_name state, gen(dupnames)
replace ac_name = ac_name + ", " + district_name if dupnames

gen matchitname = state + "_" + ac_name


save "${gen}/delimitations_all.dta", replace



 

 
/*----------------------------------------------------*/
   /* [>   3.  Import shapefile and merge   <] */ 
/*----------------------------------------------------*/

shp2dta using "./Maps/India_Maps_New/assembly-constituencies/India_AC.shp", ///
		database("./Maps/India_Maps_New/assembly-constituencies/India_AC_data.dbf") ///
		coordinates("./Maps/India_Maps_New/assembly-constituencies/India_AC.dta") gencentroids(midpoint) genid(id_ac) ///
		replace

use "./Maps/India_Maps_New/assembly-constituencies/India_AC_data.dbf", clear
ren *, lower
ren dist_name district_name
gen id_old = ac_name

replace st_name=proper(st_name)

statenames, sv(st_name)


gen reserved_sc = (substr(ac_name, -4,4)=="(SC)")
gen reserved_st = (substr(ac_name, -4,4)=="(ST)")
replace ac_name = (substr(ac_name, 1,length(ac_name)-4)) if substr(ac_name, -4,4)=="(SC)"
replace ac_name = (substr(ac_name, 1,length(ac_name)-4)) if substr(ac_name, -4,4)=="(ST)"
replace ac_name = proper(ac_name)
// Remove leading / trailing spaces
replace ac_name = strtrim(ac_name)
replace ac_name = stritrim(ac_name)

replace district_name = proper(district_name)
replace pc_name = proper(pc_name)

duplicates tag ac_name code, gen(dupnames)
replace ac_name = ac_name + ", " + district_name if dupnames


gen matchitname = code + "_" + ac_name

duplicates drop matchitname, force

save "./Maps/India_Maps_New/assembly-constituencies/India_AC_data.dta", replace
use "./Maps/India_Maps_New/assembly-constituencies/India_AC_data.dta", replace

// Merge Spatial Data with Delimitation data using matchit
matchit id_ac matchitname using "${gen}/delimitations_all.dta" , idusing(id) txtusing(matchitname)

// Drop observations from different states
drop if  substr(matchitname,1,2)!=substr(matchitname1,1,2)

/* [> Procedure to keep only appropriate matches <] */ 
replace similscore = round(similscore, 0.0001)
cap drop max_*
bysort matchitname: egen max_1 = max(similscore)
bysort matchitname1: egen max_2 = max(similscore) 


cap drop keep
gen keep = 0
replace keep = 1 if round(similscore,0.0001) == round(max_1,0.0001) & round(max_1,0.0001) == round(max_2,0.0001)
bysort matchitname1: egen max_3 = max(keep)

drop if keep==0 & max_3==1

// The last remaining ones can be done manually
replace keep = 1 if matchitname=="GJ_Mahesana" & matchitname1=="GJ_Mehsana"
replace keep = 1 if matchitname=="GJ_Dahegam" & matchitname1=="GJ_Dehgam"
replace keep = 1 if matchitname=="BR_Brahampur" & matchitname1=="BR_Barhampur"
replace keep = 1 if matchitname=="TR_Karamchhara" & matchitname1=="TR_Karmacherra"

keep if keep==1
drop if similscore <= 0.55

drop max_1 max_2 max_3 keep
cap drop t1 t2 t3

save "./Maps/matchit.dta", replace



 
/*----------------------------------------------------*/
   /* [>   4.  Merge with other data   <] */ 
/*----------------------------------------------------*/
use "./Maps/matchit.dta", clear

/************************************/
/*  4a: Merge back to delimitation data  */
/************************************/
ren matchitname1 ac_name
duplicates drop ac_name, force
gen state = substr(ac_name,1,2)
replace ac_name = substr(ac_name, 4, .)

			   
merge 1:1 ac_name state using "${gen}/delimitations_all.dta"
drop _merge

merge 1:1 matchitname using "./Maps/India_Maps_New/assembly-constituencies/India_AC_data.dta"
drop _merge
drop dupnames

do "${code}/prep/label_vars.do"

drop if missing(id_ac)

export excel using "./Maps/merged.xlsx", replace firstrow(variables)


/************************************/
/*  4b: Merge with SHRUG data  */
/************************************/
cap drop shrug_assembly_id
gen ac08_id = "2008" + "-" + string(st_code,"%02.0f") +  "-" + string(ac_no,"%03.0f")
duplicates drop ac08_id , force
merge 1:1 ac08_id using "./Data/SHRUG/shrug-v1.4.samosa-assembly-dta/con_shrug_2008.dta"
drop _merge




gen sc_tot_01 = pc01_pca_p_sc / 1000
gen sc_prop_01 = pc01_pca_p_sc / pc01_pca_tot_p * 100
label var sc_tot_01 "SC population in 1000"
label var sc_prop_01 "Percent SC population"
gen sc_tot_11 = pc11_pca_p_sc / 1000
gen sc_prop_11 = pc11_pca_p_sc / pc11_pca_tot_p * 100
label var sc_tot_11 "SC population in 1000"
label var sc_prop_11 "Percent SC population"

set graph off

foreach var of varlist sc_prop_01 pc01_pca_tot_p pc01_td_area pc01_vd_area pc01_vd_m_sch pc01_vd_p_sch pc01_pca_p_lit pc01_vd_tar_road {
local t : variable label `var'

if "`var'" == "sc_prop_01" {
qui sum `var'
gen `var'_norm = (`var'-r(mean))/r(sd)
local t = "Scheduled caste population"
}

if "`var'" == "pc01_pca_tot_p" {
gen `var'_norm = `var'/1000
local t = "Population in 1000"
}
else if "`var'"== "pc01_vd_m_sch" {
cap gen `var'_norm = `var' /  pc01_pca_tot_p *1000
local t = "Middle schools per 1000" 
}
else if "`var'"== "pc01_td_area" {
cap gen `var'_norm = log(`var'+1)
local t = "log of Town Area in sq. km" 
}
else if "`var'"== "pc01_vd_area" {
cap gen `var'_norm = log(`var'+1)
local t = "log of Village Area in hectars" 
}

else if "`var'"== "pc01_vd_p_sch" {
cap gen `var'_norm = `var' /  pc01_pca_tot_p *1000
local t = "Primary schools per 1000" 
}
else if "`var'"== "pc01_pca_p_lit" {
cap gen `var'_norm = `var' /  pc01_pca_tot_p * 100
local t = "Literacy rate" 
}
else if "`var'"== "pc01_vd_tar_road" {
cap gen `var'_norm = `var'*100
local t = "Percentage paved roads" 
}
else {
cap gen `var'_norm = `var'
local t : variable label `var'
}



label var `var'_norm "`t'"
}




foreach var of varlist sc_prop_11 pc11_pca_tot_p pc11_td_area pc11_vd_area pc11_vd_m_sch pc11_vd_p_sch pc11_pca_p_lit pc11_vd_tar_road {
local t : variable label `var'


if "`var'" == "pc11_pca_tot_p" {
gen `var'_norm = `var'/1000
local t = "Population in 1000"
}
else if "`var'"== "pc11_vd_m_sch" {
cap gen `var'_norm = `var' /  pc11_pca_tot_p *1000
local t = "Middle schools per 1000" 
}
else if "`var'"== "pc11_td_area" {
cap gen `var'_norm = log(`var'+1)
local t = "log of Town Area in sq. km" 
}
else if "`var'"== "pc11_vd_area" {
cap gen `var'_norm = log(`var'+1)
local t = "log of Village Area in hectars" 
}

else if "`var'"== "pc11_vd_p_sch" {
cap gen `var'_norm = `var' /  pc11_pca_tot_p *1000
local t = "Primary schools per 1000" 
}
else if "`var'"== "pc11_pca_p_lit" {
cap gen `var'_norm = `var' /  pc11_pca_tot_p * 100
local t = "Literacy rate" 
}
else if "`var'"== "pc11_vd_tar_road" {
cap gen `var'_norm = `var'*100
local t = "Percentage paved roads" 
}
else {
cap gen `var'_norm = `var'
local t : variable label `var'
}



label var `var'_norm "`t'"
}


gen district_code = string(st_code) + "-" +  string(dt_code)

/* [> Generate variables for potential "discrete RDD" analysis <] */ 
cap drop rank_rdd
bys district_name state: egen rank_rdd = rank(perc_sc), field
bys district_name state: egen min_sc_reservation = min(perc_sc) if sc_reserved==1
bys district_name state : egen min_sc = min(min_sc_reservation)
gen rank_to_subtract = rank_rdd if min_sc == perc_sc 
bys district_name state: egen rts = min(rank_to_subtract)

gen rank_rdd_2 = -(rank_rdd-rts)



save "${gen}/constituency/const_data.dta", replace

saveold "${gen}/constituency/const_data_r.dta", version(12) replace


	

/************************************/
/*  4c: ACLED data read-in  */
/************************************/
import delim using "./Data/ACLED/acled_india.csv", clear delim(";")

keep if geo_precision == 1 | geo_precision==2

// example
// In Chattisgarh, highest number of fatalities per incident...
tabstat fatalities, by(admin1) statistics(count sum mean)
 
 
 gen one = 1
 
geoinpoly latitude longitude ///
	using "./Maps/India_Maps_New/assembly-constituencies/India_AC.dta", ///
	unique
	
ren _ID id_ac
collapse (sum) one fatalities , by(id_ac)

ren one acled_totalevents
ren fatalities acled_fatalities

save "${gen}/constituency/acled_polygons", replace

use "${gen}/constituency/acled_polygons", clear

/* [> Merge back with constituency-level dataset <] */ 
merge 1:1 id_ac using "${gen}/constituency/const_data.dta"
drop _merge

ren district_name district_name_old
gen district_crosswalk = lower(state)+"_"+lower(proper(district_name_old))

/* [> Merge with district-level <] */ 
merge m:1 district_crosswalk using  "${gen}/ac_with_district_data.dta"
drop if _merge==2






 
/*----------------------------------------------------*/
   /* [>   5.  Data cleaning   <] */ 
/*----------------------------------------------------*/
replace acled_totalevents = 0 if missing(acled_totalevents)
replace acled_fatalities = 0 if missing(acled_fatalities)


drop if missing(ac_name)
compress

gen st = substr(matchitname, 1, 2)
order ac_name st district_crosswalk

drop similscore state district_name_old pc_name pc_id _merge 


gen levents = log(acled_totalevents+1)

reghdfe levents ///
	sc_reserved sc_prop_01_norm ///
	pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm ///
	if sample_2 == 1 ///
	, noabsorb  ///
	vce(cluster district_crosswalk) 
sum acled_totalevents if e(sample)






log(acled_totalevents+1) ~ sc_reserved + sc_prop_01_norm +  
    pc01_pca_tot_p_norm + 
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 





// Changes here (sample): JDO 25-10-2020
reg acled_totalevents sc_reserved  if sample_1==1, vce(cluster code)


reg acled_fatalities sc_reserved if sample_1==1, vce(cluster code)

cap drop prop_*

gen prop_sc = pc01_pca_p_sc/pc01_pca_tot_p*100		// Can also use 01...
gen prop_schools = pc01_vd_p_sch/pc01_pca_tot_p*100	// Can also use 01...


cap drop district_extra_seat
gen district_extra_seat = 0
replace district_extra_seat = 1 	if diff_post>=0.1 & !mi(diff_post)
replace district_extra_seat = -1 if diff_post<=-0.1


reg acled_totalevents district_extra_seat prop_sc
cap drop sample_from_rounding
gen sample_from_rounding = .
replace sample_from_rounding = 1 if sc_sample_1==1 & district_extra_seat==1
replace sample_from_rounding = 1 if runner_up_sc_1==1 & district_extra_seat==-1

cap drop nax
gen nax = 0
replace nax = 1 if code=="JH"
replace nax = 1 if code=="BR"
replace nax = 1 if code=="CT"
replace nax = 1 if code=="OR"

 replace nax = 1 if code=="AP"
 replace nax = 1 if code=="WB"

gen levents = log(acled_totalevents+1)
gen lfatalities = log(acled_fatalities+1)


poisson acled_totalevents sc_reserved##i.nax sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1
poisson acled_fatalities sc_reserved##i.nax sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1




reg levents sc_reserved##c.any_nax_event sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1, vce(cluster district_name)

 
/*----------------------------------------------------*/
   /* [>   0.  With Naxalite regions  <] */ 
/*----------------------------------------------------*/

reg levents sc_reserved##i.nax sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1
qui sum acled_totalevents if e(sample)==1
estadd scalar lmean = `r(mean)'
est sto eventsreg

reg lfatalities sc_reserved##i.nax sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1
qui sum acled_fatalities if e(sample)==1
estadd scalar lmean = `r(mean)'
est sto fatreg

poisson acled_totalevents sc_reserved##i.nax sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1
qui sum acled_totalevents if e(sample)==1
estadd scalar lmean = `r(mean)'
est sto eventspoi

poisson acled_fatalities sc_reserved##i.nax sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1
qui sum acled_fatalities  if e(sample)==1
estadd scalar lmean = `r(mean)'
est sto fatpoi

nbreg acled_totalevents sc_reserved##i.nax sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1
qui sum acled_totalevents if e(sample)==1
estadd scalar lmean = `r(mean)'
est sto eventsnb


nbreg acled_fatalities sc_reserved##i.nax sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1
qui sum acled_fatalities  if e(sample)==1
estadd scalar lmean = `r(mean)'
est sto fatnb



// ekidnap 
#delimit ;
estout 
eventsreg fatreg eventspoi fatpoi eventsnb fatnb 
using "${output}/tables/nax.tex" , style(tex) 
eqlabels(" " " ") 
wrap varwidth(45) 
varlabels(sc_prop_01_norm "Share SCs" 1.nax "Naxalite state" 1.sc_reserved "Reserved seat" 1.sc_reserved#1.nax " Reserved seat $\times$  Naxalite state")
keep(sc_prop_01_norm 1.sc_reserved 1.nax 1.sc_reserved#1.nax)
order(sc_prop_01_norm 1.sc_reserved 1.nax 1.sc_reserved#1.nax)
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N   , fmt(%9.3fc %9.0fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	















 
/*----------------------------------------------------*/
   /* [>   1.  With rounding sample   <] */ 
/*----------------------------------------------------*/

reg levents sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_from_rounding==1
qui sum acled_totalevents if sample_from_rounding==1
estadd scalar lmean = `r(mean)'
est sto eventsreg

reg lfatalities sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_from_rounding==1
qui sum acled_fatalities if sample_from_rounding==1
estadd scalar lmean = `r(mean)'
est sto fatreg

poisson acled_totalevents sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_from_rounding==1
qui sum acled_totalevents if sample_from_rounding==1
estadd scalar lmean = `r(mean)'
est sto eventspoi

poisson acled_fatalities sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm if sample_from_rounding==1
qui sum acled_fatalities  if sample_from_rounding==1
estadd scalar lmean = `r(mean)'
est sto fatpoi

nbreg acled_totalevents sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_from_rounding==1
qui sum acled_totalevents if sample_from_rounding==1
estadd scalar lmean = `r(mean)'
est sto eventsnb


nbreg acled_fatalities sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm if sample_from_rounding==1
qui sum acled_fatalities  if sample_from_rounding==1
estadd scalar lmean = `r(mean)'
est sto fatnb



// ekidnap 
#delimit ;
estout 
eventsreg fatreg eventspoi fatpoi eventsnb fatnb 
using "${output}/tables/sample_rounding.tex" , style(tex)  
eqlabels(" " " ")
wrap varwidth(45) 
varlabels(sc_prop_01_norm "Share SCs" sc_reserved " Seat reserved for  SCs" 
  _cons "\addlinespace Constant")
keep(sc_prop_01_norm sc_reserved)
order(sc_prop_01_norm sc_reserved)
	cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(lmean N   , fmt(%9.3fc %9.0fc) labels("\midrule \addlinespace Mean of DV" "\addlinespace Observations"))
starlevels(* 0.1 ** 0.05 *** 0.01)
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr	


zinb acled_totalevents sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1, inflate( sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm )  zip






nbreg acled_totalevents sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm  if sample_2==1, ///
		vce(robust)


nbreg acled_totalevents sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm if sample_2==1, ///
		vce(robust)




reghdfe acled_fatalities sc_reserved prop_schools  prop_sc pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm if sample_1==1, absorb(st_code) vce(cluster st_code)
reg acled_fatalities sc_reserved prop_schools  prop_sc if sample_1==1

gen acled_event_dummy = (acled_totalevents>=0.01 & !missing(acled_totalevents))
gen acled_fatality_dummy = (acled_fatalities>=0.01 & !missing(acled_fatalities))

reg acled_event_dummy i.sc_reserved##c.prop_sc prop_schools   if sample_1==1
reg acled_fatality_dummy sc_reserved prop_schools  prop_sc if sample_1==1

logit acled_event_dummy sc_reserved prop_schools  prop_sc if sample_1==1
logit acled_fatality_dummy sc_reserved prop_schools  prop_sc if sample_1==1



poisson acled_totalevents i.sc_reserved  prop_schools prop_sc if sample_1==1
margins sc_reserved,  atmeans
poisson acled_fatalities i.sc_reserved prop_schools  prop_sc if sample_1==1
margins sc_reserved,  atmeans



nbreg acled_totalevents i.sc_reserved prop_schools  prop_sc if sample_1==1
margins sc_reserved,  atmeans
nbreg acled_fatalities i.sc_reserved prop_schools  prop_sc if sample_1==1
margins sc_reserved,  atmeans


cap drop levents
gen levents = log(acled_totalevents+1)


reg levents sc_reserved sc_prop_01_norm pc01_pca_tot_p_norm pc01_vd_p_sch_norm pc01_vd_tar_road_norm if sample_1==1



/*


 use "./Data/SHRUG/assembly_elections_clean.dta", clear
 

 foreach var of varlist ac07_id ac08_id {
 forvalues i = 1/5 {
 replace `var' = `var'[_n+1] if tr_ac_id == tr_ac_id[_n+1] & pc01_state_name==pc01_state_name[_n+1] & `var'[_n+1]!="" & `var'==""
 replace `var' = `var'[_n-1] if tr_ac_id == tr_ac_id[_n-1] & pc01_state_name==pc01_state_name[_n-1] & `var'[_n-1]!="" & `var'==""
 replace `var' = `var'[_n+1] if tr_ac_id == tr_ac_id[_n+1] & pc01_state_name==pc01_state_name[_n+1] & `var'[_n+1]!="" & `var'==""
}
 } 
 
gen date = ym(year,month)
gen dist_2008 = ym(2008,4) - ym(year,month)
keep if dist_2008>0 & !missing(dist_2008)
 bysort ac08_id : egen closest_2008 = min(dist_2008)
 keep if dist_2008 == closest_2008
 
 
 





tw ///
(scatter acled_totalevents rank_rdd_2, mcolor(gs10) msize(tiny)) ///
(lpolyci acled_totalevents rank_rdd_2 if rank_rdd_2<0, bw(0.05) deg(2) n(100) fcolor(none)) ///
(lpolyci acled_totalevents rank_rdd_2 if rank_rdd_2>=0, bw(0.05) deg(2) n(100) fcolor(none)), xline(0)  legend(off)



rdplot acled_totalevents rank_rdd_2, nbins(20 20) binselect(qs) ///
graph_options(graphregion(color(white)) ///
 xtitle(Score) ytitle(Outcome))


rdrobust acled_totalevents rank_rdd_2,  covs(perc_sc) kernel(uniform) p(1)

net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace

rdrobust acled_totalevents rank_rdd_2
rdplot acled_totalevents rank_rdd_2
rdrandinf  acled_totalevents rank_rdd_2, seed(50) kernel(triangular)


 
 
