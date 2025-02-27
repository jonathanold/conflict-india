 
/*----------------------------------------------------*/
   /* [>   1.  Read in data   <] */ 
/*----------------------------------------------------*/
import delim "${data}/Trivedi/TCPD_AE_All_States_2023-5-29.csv", clear
save "${data}/Trivedi/trivedi_original.dta", replace

collapse (mean) delimid, by(state year)
ren state s1
statenames, sv(s1)

ren delimid state_treat
replace state_treat = state_treat-3

drop if code=="unknown"
encode code, gen(c)
gsort state year 
xtset c year 
tsfill

foreach v of varlist state state_treat code {
	replace `v'=`v'[_n-1] if c==c[_n-1] & mi(`v')
}

keep state code year state_treat
save "${gen}/state_treat.dta", replace 


use "${data}/Trivedi/trivedi_original.dta", replace
 gsort state constituency_name  year
 order state_name constituency_name constituency_no year 
 forv i = 1/20 {
replace district_name = district_name[_n-1] if mi(district_name) & !mi(district_name[_n-1])
}
forv i = 1/20 {
replace district_name = district_name[_n+1] if mi(district_name)
}



gen reserved_sc_t = 0
replace reserved_sc_t = 1 if constituency_type=="SC"
gen reserved_st_t = 0
replace reserved_st_t = 1 if constituency_type=="ST"


collapse (mean)  reserved_sc_t reserved_st_t , by(constituency_name constituency_no district_name state year delimid)

ren state s1
statenames, sv(s1)

gsort state constituency_name year

bys constituency_name state: egen maxy = max(year)
drop if maxy<=2000

gsort state constituency_name year

gen reserved_st_t_L1 = reserved_st_t[_n-1] if constituency_name==constituency_name[_n-1] & delimid==4 & delimid[_n-1]==3
gen reserved_sc_t_L1 = reserved_sc_t[_n-1] if constituency_name==constituency_name[_n-1] & delimid==4 & delimid[_n-1]==3

keep if !mi(reserved_sc_t_L1)

duplicates list state constituency_name
save "${gen}/trivedi_asemblies.dta", replace 

use "${gen}/trivedi_asemblies.dta", replace 


// Some more cleaning
ren constituency_name ac_name


gen id = _n
drop state  
ren code state
replace state=upper(state)

// Required for matchit
replace ac_name=proper(ac_name)
replace ac_name = strtrim(ac_name)
replace ac_name = stritrim(ac_name)

replace district_name = proper(district_name)
replace ac_name = "Barauli" if ac_name=="Baroli"


// Here
drop if reserved_sc_t!=0 & reserved_sc_t!=1
drop if reserved_st_t!=0 & reserved_st_t!=1 

bys ac_name state: egen yearX = max(year)
keep if year==yearX
drop yearX 

duplicates drop  ac_name state, force

replace ac_name = subinstr(ac_name, "Indore-III", "Indore-3", 1)
replace ac_name = subinstr(ac_name, "Indore-II", "Indore-2", 1)
replace ac_name = subinstr(ac_name, "Indore-IV", "Indore-4", 1)
replace ac_name = subinstr(ac_name, "Indore-I", "Indore-1", 1)
replace ac_name = subinstr(ac_name, "Indore-V", "Indore-5", 1)

replace ac_name = subinstr(ac_name, "Uluberia North", "Uluberia Uttar", 1)

gen matchitname = state + "_" + ac_name

save "${gen}/trivedi_assemblies_clean.dta", replace 

use "${gen}/trivedi_assemblies_clean.dta", replace 
collapse (firstnm) district_name reserved_sc_t reserved_st_t state , by(matchitname)

gen id_ac = _n
save "${gen}/trivedi_assemblies_for_matchit.dta", replace 
use "${gen}/trivedi_assemblies_for_matchit.dta", replace 

matchit id_ac matchitname using "${gen}/delimitations_all_dist.dta" , idusing(id) txtusing(matchitname)

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

save "${gen}/trivedi_matchedit.dta", replace 

use "${gen}/trivedi_matchedit.dta", replace 

merge 1:m matchitname using "${gen}/trivedi_assemblies_clean.dta"

keep if _merge==3 
drop _merge 

ren matchitname matchitname_trivdi
ren matchitname1 matchitname 
drop district_name

merge m:1 matchitname using  "${gen}/delimitations_all_dist.dta" // "./Data/_gen/constituency/const_data.dta"
drop _merge

gsort ac_name year 

bys district_name state : egen dt_sc_reserved_total_trivedi = total(reserved_sc_t)
bys district_name state : egen dt_st_reserved_total_trivedi = total(reserved_st_t)
bys district_name state : egen dt_sc_reserved_total_trivediL1 = total(reserved_sc_t_L1)
bys district_name state : egen dt_st_reserved_total_trivediL1 = total(reserved_st_t_L1)

gen trivedi = 1 
bys district_name state : egen dt_trivedi_seats = total(trivedi)


save "${gen}/trivedi_merged.dta", replace 

/*

use "${gen}/trivedi_merged.dta", replace 

ren ac_name name
replace state = lower(state)
ren district_name district

merge 1:1 name state district using "${gen}/delimitations/all_states.dta"

drop if _merge==1
drop _merge

save  "./Data/_gen/delimitations/all_states_trivedi.dta", replace
