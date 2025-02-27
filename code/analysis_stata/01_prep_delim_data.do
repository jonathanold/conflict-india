

// 1. Import text coded files from tabula

// import delim "./Data-PDF/_csv/tabula-mp_p3.csv", clear varnames(1)
// Tabula files: Text coded
local files : dir "./Data-PDF/_csv" files "*.csv"

foreach file in `files' {
import delim "./Data-PDF/_csv/`file'", clear varnames(1)
local state = substr("`file'", 8,2)
gen state = substr("`file'", 8,2)


foreach var of varlist _all {
cap tostring `var', replace force
}

// Clean and rename
cap ren (assemblyconstituency total scs ofscs scseats) (name total sc perc_sc sc_seats)
cap ren assemblyconstituency name
cap ren noassemblyconstituency name
cap ren scs sc
cap ren ofscs perc_sc
cap ren scseats sc_seats
cap drop no

replace name = subinstr(name, "!", "Th", 1)

forvalues i=1/2 {
// Remove leading / trailing spaces
replace name = strtrim(name)
replace name = stritrim(name)

// Remove leading numbers
forvalues i=1/5 {
replace name = trim(regexr(trim(name), "^[0-9]", ""))
}
// Remove leading special characters
forvalues i=1/5 {
replace name = trim(regexr(trim(name), "^[-]", ""))
replace name = trim(regexr(trim(name), "^[:]", ""))
replace name = trim(regexr(trim(name), "^[.]", ""))
}
}
/*
forvalues i=0/9 {
replace name = subinstr(name, "`i'", "", 9)
}
*/


gen is_district = 1 if !missing(sc_seats)
forvalues i=1/100 {
replace is_district = 0 if sc_seats=="`i'"
}
replace is_district = 0 if missing(sc_seats)

gen sc_reserved = (!missing(sc_seats) & is_district==0)
gen st_reserved = (substr(name, -3, 2)=="ST") 

replace name = subinstr(name, "(SC)", "", 1)
replace name = subinstr(name, "(SC )", "", 1)
replace name = subinstr(name, "(S.C)", "", 1)
replace name = subinstr(name, "(S.C.)", "", 1)
replace name = subinstr(name, "(ST)", "", 1)
replace name = proper(name)
replace name = subinstr(name, "District", "", 1)


// Remove leading / trailing spaces
replace name = strtrim(name)
replace name = stritrim(name)

// Sometimes numbers in name
replace name = name + "a" if substr(name, 1, 6) == "Indore"


gen total_temp = total
forvalues i=1/9 {
replace name = substr(name, 1, length(name)-1)   if substr(name,-1,1) == substr(total_temp, -1,1)
replace total_temp = substr(total_temp, 1, length(total_temp)-1)   if substr(total_temp,-1,1) == substr(total, -`i',1)
}
drop total_temp

replace name = substr(name, 1, length(name)-1) if substr(name, 1, 6) == "Indore"


// Remove leading / trailing spaces
replace name = strtrim(name)
replace name = stritrim(name)

gen district = name if is_district==1
replace district=district[_n-1] if missing(district) & !missing(district[_n-1]) 

// Drop districts (they remain after text encoding and are not useful for analysis)
drop if is_district==1

foreach var of varlist total sc perc_sc sc_seats {
destring `var', replace
}


save "./Data/_gen/delimitations/`state'.dta", replace
}

cap erase "./Data/_gen/delimitations/all_states.dta"

// Append for all states
use "./Data/_gen/delimitations/hr.dta", replace

local files : dir "./Data/_gen/delimitations" files "*.dta"
foreach file in `files' {
if substr("`file'", 1,2) != "hr" {
append using "./Data/_gen/delimitations/`file'"
di "Appended `file'"
}
else {
}
}

gen n = _n
cap drop m
encode district, gen(m)





save "./Data/_gen/delimitations/all_states.dta", replace
use "./Data/_gen/delimitations/all_states.dta", replace

