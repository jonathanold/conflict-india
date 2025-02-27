


import delim using "${gen}/dataset_satp_complete.csv", clear delim(";")

replace year = year[_n-1] if mi(year)
gen note = description
 gen castev = 0
 // Most events are peaceful protests
 
local keywords1 ""caste" "schedule" "brahmin" "untouch" "forward" "obc" "dalit" "reserv"  "jat" "jaat" " sc " "discrim"  "(sc)" "category" "
 
 foreach var of local keywords1 {
 cap gen `var' = (strpos(note, "`var'")!=0)
 if _rc==198 {
 gen vx = (strpos(note, "`var'")!=0)
 }
 replace castev = 1 if `var'==1
 
 }
 
tab castev


hist year, d
