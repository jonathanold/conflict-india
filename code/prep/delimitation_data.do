use "./Data/_gen/delimitations/all_states.dta", clear


drop if missing(district)
gen one = 1
bysort district state: egen sc_reserved_total = sum(sc_reserved)
bysort district state: egen seats_total = sum(one)
gen seats_perc_reserved = sc_reserved_total / seats_total

bysort district state: egen sc_pop_total = sum(sc)
bysort district state: egen pop_total = sum(total)
gen pop_perc_sc = sc_pop_total / pop_total

gen perc_diff_reserved = seats_perc_reserved - pop_perc_sc

// Generate sample
// Runner-ups: Those who are just below the cutoff

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




save "./Data/_gen/delimitations_all.dta", replace
use  "./Data/_gen/delimitations_all.dta", clear



set graph off
twoway (histogram perc_sc if sc_reserved==1, start(0) width(3.333333) color(maroon%60)) ///        
       (histogram perc_sc if sc_reserved==0, start(0) width(3.333333) color(emerald%60)), ///   
       graphregion(color(white)) xsize(6.5) ysize(4.5) legend(order(1 "Reserved" 2 "Unreserved" )) ///
	   title("All constituencies", color(black))
graph export  "./Output/figures/hist_all_ac.pdf", replace
	   
	   
twoway (histogram perc_sc if sc_reserved==1, start(0) width(3.3333333) color(maroon%60)) ///        
       (histogram perc_sc if sc_reserved==0 & sc_reserved[_n-1]==1 & perc_sc>=min_sc_threshold , start(0) width(3.333333) color(emerald%60)), ///   
       graphregion(color(white)) xsize(6.5) ysize(4.5) legend(order(1 "Reserved" 2 "Runner-up" ))	  ///
	   title("Reserved vs. Runner-up", color(black))
graph export  "./Output/figures/hist_runnerup_ac.pdf", replace
set graph on
	   
	   
	   
	   
// Import and merge with Shapefile
	   
/// MAP DATA
// ssc install shp2dta

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
matchit id_ac matchitname using "./Data/_gen/delimitations_all.dta" , idusing(id) txtusing(matchitname)

// Drop observations from different states
drop if  substr(matchitname,1,2)!=substr(matchitname1,1,2)

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
use "./Maps/matchit.dta", clear

ren matchitname1 ac_name
duplicates drop ac_name, force
gen state = substr(ac_name,1,2)
replace ac_name = substr(ac_name, 4, .)

			   
merge 1:1 ac_name state using "./Data/_gen/delimitations_all.dta"
drop _merge

merge 1:1 matchitname using "./Maps/India_Maps_New/assembly-constituencies/India_AC_data.dta"
drop _merge
drop is_district
drop dupnames

do "./Syntax/prep/label_vars.do"

drop if missing(id_ac)

export excel using "./Maps/merged.xlsx", replace firstrow(variables)


// Merge with SHRUG Data
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

qui sum `var'_norm, d
local w = r(max)/25
// Primary schools
colorpalette mrc, select(1 3) nograph

twoway (histogram `var'_norm if sc_sample_1==1,  start(0)  width(`w')   color("`r(p1)'%70")) ///        
       (histogram `var'_norm if runner_up_sc_1==1, start(0) width(`w') color("`r(p2)'%70")), ///   
       graphregion(color(white)) xsize(6.5) ysize(4.5) legend(order(1 "Reserved" 2 "Runner-up" ))	  ///
	   title("Balance check: `t'") xti("`t'")
graph export  "./Output/figures/hist_`var'.pdf", replace
   

twoway (kdensity `var'_norm if sc_sample_1==1,  color("`r(p1)'%70") recast(area)) (kdensity `var'_norm if runner_up_sc_1==1, color("`r(p2)'%70") recast(area)) , ///
		legend(order(1 "Reserved" 2 "Runner-up" )) ///
		title("Balance check: `t'", color(black)) yti("Density")  xti("`t'") ///
		graphregion(color(white)) xsize(6.5) ysize(4.5)
graph export  "./Output/figures/kernel_`var'.pdf", replace

// cap drop `var'_norm
}
set graph on



// The followign for the balance table!
 ttest pc01_pca_tot_p_norm if sample_1==1 , by(sc_reserved)
 
 global depvarlist  pc01_pca_tot_p_norm pc01_td_area_norm pc01_vd_area_norm  pc01_vd_tar_road_norm pc01_vd_m_sch_norm pc01_vd_p_sch_norm pc01_pca_p_lit_norm

 
reg sc_reserved sc_prop_01_norm $depvarlist  if sample_2==1

test $depvarlist
local fval: display %9.3f r(F)
local fpval: display %9.3f r(p)
// local fpval: display %9.3f Ftail(e(df_m), e(df_r), e(F))
 
balancetable  sc_reserved sc_prop_01_norm $depvarlist if sample_2==1 using "./Output/tables/balancetable.tex", ///
	replace  ///
	varlabels ctitles("Runner-up" "Reserved" "Difference") ///
	pvalues  prefoot("\addlinespace  \midrule  Wald test statistic & & &  `fval' \\ p-value & & & `fpval' \\ \addlinespace \midrule ") ///
	format(%9.3f) booktabs
	
	
	
	
	
	
	
	
	
	
	
	

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



	
save "./Data/_gen/constituency/const_data.dta", replace
use "./Data/_gen/constituency/const_data.dta", replace


saveold "./Data/_gen/constituency/const_data_r.dta", version(12) replace


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

// ACLED data read-in
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

save "./Data/_gen/constituency/acled_polygons", replace
use "./Data/_gen/constituency/acled_polygons", clear

merge 1:1 id_ac using "./Data/_gen/constituency/const_data.dta"

replace acled_totalevents = 0 if missing(acled_totalevents)
replace acled_fatalities = 0 if missing(acled_fatalities)


// Changes here (sample): JDO 25-10-2020
reg acled_totalevents sc_reserved  if sample_1==1, vce(cluster code)


reg acled_fatalities sc_reserved if sample_1==1, vce(cluster code)

cap drop prop_*

gen prop_sc = pc01_pca_p_sc/pc01_pca_tot_p*100		// Can also use 01...
gen prop_schools = pc01_vd_p_sch/pc01_pca_tot_p*100	// Can also use 01...
reg acled_totalevents sc_reserved prop_schools  prop_sc if sample_1==1
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
 
 
 
 
 
