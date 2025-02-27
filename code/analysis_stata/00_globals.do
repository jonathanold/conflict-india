

// Globals

global code "$dir/Code"
global syntax "$dir/Code"

global output "$dir/Output"
global data "$dir/Data"
global gen "$dir/Data/_gen"


cap mkdir "${syntax}"
cap mkdir "${output}"
cap mkdir "${data}"
cap mkdir "${gen}"


cap mkdir "${gen}/state"
cap mkdir "${gen}/district"
cap mkdir "${gen}/constituency"
cap mkdir "${gen}/gp"
