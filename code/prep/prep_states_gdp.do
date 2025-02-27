// States GDP data

import excel using "./Data/State_GDP/60_69.xlsx", clear firstrow cellrange(A6)
drop if missing(B)

foreach var of varlist B-J {
local c: variable label `var'
local g = substr("`c'", 1,4)
local d gdp`g'
rename `var'  `d'
}
ren State* state
reshape long gdp, i(state) j(year)

save "./Data/_gen/state/state_gdp.dta", replace




import excel using "./Data/State_GDP/69_78.xlsx", clear firstrow cellrange(A6)
drop if missing(B)

foreach var of varlist B-J {
local c: variable label `var'
local g = substr("`c'", 1,4)
local d gdp`g'
rename `var'  `d'
}
ren State* state
reshape long gdp, i(state) j(year)

append using "./Data/_gen/state/state_gdp.dta"


save "./Data/_gen/state/state_gdp.dta", replace



import excel using "./Data/State_GDP/78_87.xlsx", clear firstrow cellrange(A6)
drop if missing(B)

foreach var of varlist B-J {
local c: variable label `var'
local g = substr("`c'", 1,4)
local d gdp`g'
rename `var'  `d'
}
ren State* state
reshape long gdp, i(state) j(year)

append using "./Data/_gen/state/state_gdp.dta"
replace gdp = subinstr(gdp,"-", "", 1)
destring gdp, replace

replace state="goa" if state=="Goa, Daman & Diu"
statenames, sv(state)
drop if state=="india"
save "./Data/_gen/state/state_gdp.dta", replace






forvalues z=1/3 {

import excel using "./Data/State_GDP/pt`z'.xlsx" ,clear
keep in 5/6

// get the names in the first row
unab allvars : _all
foreach v of loc allvars {
    
    // get the name in the first row
    loc name = `v'[1]
    if ("`name'" == "") {
        // if empty, replace with previous name
        loc name `prev'
    }
    loc prev `name' // save previous name
    
    // get additional name in second row
    // (this is for the merged cells)
    loc secondrow = `v'[2]
    if ("`secondrow'" != "") {
        
        // add the name in the secon row
        // if there is one
        loc name `name'_`secondrow'
    }
    
    // make it a valid Stata name
    // and build a namelist
    loc name = strtoname("`name'")
    loc names `names' `name'
}


// now get the complete data, passing the namelist to -import excel-
// (this is same as show earlier, but automated)
import excel `names' using "./Data/State_GDP/pt`z'.xlsx" ,cellrange(A7) clear
ren *, lower


forvalues z=1/10{
replace year = subinstr(year, " ", "" , 2)
replace year = subinstr(year, "#", "" , 1)
replace year = subinstr(year, "	", "" , 2)
replace year = subinstr(year, "    ", "", 1)
replace year = subinstr(year, "# ", "", 1)
}


gen byr = substr(year,8,6) if substr(year,1,4)=="Base"
replace byr = byr[_n-1] if missing(by)
drop if substr(year,1,3)=="Bas"
replace year = year + byr
drop byr
 cap drop if missing(assam)
 cap drop if missing(kerala)
 cap drop if missing(west_bengal)
 
save "./Data/State_GDP/pt`z'.dta", replace
local name ""
local names ""

}




use "./Data/State_GDP/pt1.dta", replace

merge 1:1 year using "./Data/State_GDP/pt2.dta"
drop _merge
merge 1:1 year using "./Data/State_GDP/pt3.dta"
drop _merge

ren (* *) (**)
foreach var of varlist andhra_pradesh-allindia {
	local x `x' `var'
}
di "`x'"

foreach var of varlist andhra_pradesh - allindia {
cap replace `var' = subinstr(`var', "-", "", 1)
cap destring `var', replace
}

gen year_ = substr(year, 1, 4)
replace year_ = year_+"a" if year_ == year_[_n-1] 
// gen baseyear = substr(year, -6, 4)
drop year
// gen year = year_+"-"+baseyear
order year_
replace year_ = "gdp"+year_


sxpose, clear force firstnames destring
gen state = "."
forvalues n=1/35 {
local z : word `n' of `x'
replace state = "`z'" if _n==`n'
}

gen deflator1993 = gdp1993a / gdp1993
gen deflator1999 = gdp1999a / gdp1999
gen deflator2004a = gdp2004a / gdp2004
gen deflator2011a = gdp2011a / gdp2011
gen deflator2012a = gdp2012a / gdp2012
gen deflator2013a = gdp2013a / gdp2013

forvalues i = 1980/1993 {
replace gdp`i' = gdp`i'*deflator1993 
}

forvalues i = 1980/1999 {
replace gdp`i' = gdp`i'*deflator1999
}
forvalues i = 1980/2004 {
replace gdp`i' = gdp`i'*deflator2004
}

drop gdp*a
drop deflator*
drop if state=="byr"





reshape long gdp , i(state) j(year)
ren gdp gdp2


drop if state=="allindia"
statenames, sv(state)

merge 1:1 code year using "./Data/_gen/state/state_gdp.dta"
drop _merge
gsort code year
// drop state


gen deflator = gdp2/gdp
bysort code: egen defl = mean(deflator)
replace gdp2 = gdp*defl if missing(gdp2)

keep year gdp2 code
ren gdp2 gdp





save "./Data/_gen/state/state_gdp.dta", replace



