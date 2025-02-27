
cd "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/coding_events/"


import excel "./humancoding/sample_events_Christine.xlsx", firstrow clear
keep description-note
foreach var of varlist relevant_event-islamist_terrorism {
	cap destring `var', replace force 
	ren `var' `var'_christine
}
ren note note_christine
drop if missing(id_satp)
collapse (mean) relevant_event-islamist_terrorism (firstnm) note , by(id_satp)
duplicates drop 
gen merge = "Christine"
tempfile christine 
save `christine'


foreach name in Jade Jonathan Umar Vaidehi Varun {
	import excel "./humancoding/sample_events_`name'.xlsx", firstrow clear
	keep description-note
	cap ren isl*terrorism islamist_terrorism
	cap gen islamist_terrorism = ., before(note)

	foreach var of varlist relevant_event-islamist_terrorism {
		cap destring `var', replace force 
		ren `var' `var'_`name'
	}
	ren note note_`name'
	drop if missing(id_satp)
	collapse (mean) relevant_event-islamist_terrorism (firstnm) note , by(id_satp)
	duplicates drop 
	di "`name'"
	merge 1:1 id_satp using `christine'
	replace merge = merge + "`name'" if _merge==3 | _merge==2
	drop _merge 
	save `christine', replace 
}




import excel "./humancoding/sample_events_Christine.xlsx", firstrow clear
gen name = "Christine"
tempfile c 
save `c'
foreach name in Jade Jonathan Umar Vaidehi Varun {
	import excel "./humancoding/sample_events_`name'.xlsx", firstrow clear
	gen name = "`name'"
	tempfile `name'x
	save ``name'x'
}
use `c' , replace 
foreach name in Jade Jonathan Umar Vaidehi Varun {
	append using ``name'x', force 
}
drop if missing(id_satp)

keep description-note name-islamic_terrorism

duplicates drop 
replace islamist_terrorism = islamic_terrorism if missing(islamist_terrorism)

replace number_casualties = "0" if number_casualties==""
destring number_casualties, replace force 

duplicates drop description-note , force 
gsort id_satp 

collapse (mean) relevant_event-islamist_terrorism (firstnm) note , by(id_satp)
foreach var of varlist relevant_event-islamist_terrorism {
	replace `var' = round(`var',1)
}satp
tempfile ttt
save `ttt', replace 

import delim "./data_sample_new.csv", clear bindquote(strict)  maxquotedrows(100000) encoding("UTF-8")

merge 1:1 id_satp using `ttt'
drop _merge note 

tempfile aaa 
save `aaa', replace 

import delim "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/coding_events/sample_events_coded.csv", clear bindquote(strict) varn(1) maxquotedrows(100000) encoding("UTF-8")

keep id_satp relvent_events_new
merge 1:1 id_satp using `aaa'

replace relevant_event = relvent_events_new if missing(relevant_event)
drop relvent_events_new 
drop _merge 



export delim "./sample_events_coded_new.csv", replace  quote

