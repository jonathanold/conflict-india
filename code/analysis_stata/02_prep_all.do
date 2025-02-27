// All dataset preparations in one

// Iyer 2012
do "$code/prep/prep_states_iyer.do"

// Fetzer state data
do "$code/prep/prep_states_fetzer.do"

// INSCR state data
do "$code/prep/prep_states_polity.do"

// ACLED data
do "$code/prep/acled.do"

// District data
do "$code/prep/prep_district_data.do"
