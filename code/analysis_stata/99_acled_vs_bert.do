// ACLED BART predictions

cd "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/coding_events/coding_bert/"

import delim using acled_predictions_riots.csv, clear
tempfile x
save `x'

local files : dir "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/coding_events/coding_bert/" files "*.csv"
foreach file in `files' {
	import delim using `file', clear
	merge 1:1 id_satp_new using `x'
	drop _merge
	save `x', replace 
	}

import delim using "../acled_plus_satp.csv", clear bindquote(strict)  maxquotedrows(2000)
keep id_satp_new description
drop if missing(id_satp_new)
merge 1:1 id_satp_new using `x'

gsort -strat_dev
gsort -battl
gsort -viol