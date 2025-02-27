/*
Master do-File for Extended Essay
Submitted by Jonathan Old
All you need to do is:
	- Change your system directory
	- Stata needs folder writing access
	- Required packages will be installed automatically at the beginning
	- This is run on Stata 15 for Mac. Some commands (e.g. transparency for graphs)
		will not work on earlier versions. Some system commands might be different for Windows.
	

	
ssc install egenmore
ssc install sxpose
ssc install geoinpoly
ssc install balancetable
ssc install matchit


*/
graph set window fontface "Palatino-Roman"
set scheme mine

global dir "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/"

cd "$dir"


// Set globals and make folders
do "$dir/Code/analysis_stata/00_globals.do"

	
// Initialize manual programs
do "./Code/analysis_stata/statecodes.do"

// Prepare data for delimitation file
do "./Code/analysis_stata/01_prep_delim_data.do"

// Prepare data files
do "./Code/analysis_stata/02_prep_all.do"


// Analysis: Constituency level
do "./Code/analysis_stata/03_analysis_constituency.do"

// Analysis: District level
do "./Code/analysis_stata/04_analysis_districts.do"


// Analysis: State-level
do "./Code/analysis_stata/70_state_level.do"
