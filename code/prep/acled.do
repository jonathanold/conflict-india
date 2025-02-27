

// Only caste data

import delimited using "${data}/ACLED/acled_india_30may.csv", clear delim(";")
export delimited using "${data}/ACLED/acled_india_csv.csv", replace delim(";")


keep if geo_precision == 1

tabstat fatalities, by(admin1) statistics(count sum mean)
 
  
 
 // Admin 2 gives districts!
 codebook latitude longitude
 // 7681 unique locations
 
 replace notes = lower(notes)
 
 gen castev = 0
 // Most events are peaceful protests
 


local keywords1 ""caste" "schedule" "brahmin" "untouch" "forward" "obc" "dalit" "reserv"  "jat" "jaat" " sc " "discrim"  "(sc)" "category" "
 
 foreach var of local keywords1 {
 cap gen `var' = (strpos(note, "`var'")!=0)|(strpos(actor1, "`var'")!=0)|(strpos(actor2, "`var'")!=0)|(strpos(assoc_actor_1, "`var'")!=0)|(strpos(assoc_actor_2, "`var'")!=0)
 
 replace castev = 1 if `var'==1
 
 }
 
tab castev

preserve 
keep if castev


export delimited using "${data}/ACLED/acled_india_castes.csv", replace delim(";")

restore
drop if castev

export delimited using "${data}/ACLED/acled_india_nocastes.csv", replace delim(";")








// Drop state-only actions (strategic dev, etc.)

import delimited using "${data}/ACLED/acled_india_30may.csv", clear delim(";")

keep if geo_precision == 1

drop if event_type=="Strategic developments" 
drop if sub_event_type=="Air/drone strike"
drop if sub_event_type=="Government regains territory"
drop if sub_event_type=="Shelling/artillery/missile attack"

// drop if sub_event_type=="Peaceful protest"
export delimited using "${data}/ACLED/acled_india_nostate.csv", replace delim(";")
