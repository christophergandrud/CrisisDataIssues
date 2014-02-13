///////////////
// Update basic Keefer (2007)
// Christopher Gandrud
// 10 February 2014
// Using Stata 12
///////////////

cd "~/Dropbox/AMCProject/KeeferReplication/tables"

use "~/Dropbox/AMCProject/MissingLinkDataAnalysis/data/KeeferExtended.dta", clear

// Keefer Table 4, Model 2
gen LogKeefer = log(Keefer2007_Fiscal + 0.1)	
gen Logstab = log(stabnsLag3 + 0.1)	
	
regress Keefer2007_Fiscal Checks33 DiEiec33 stabnsLag3, vce(cluster country)
	regsave using "KeeferBasic.dta", detail(all) replace table(KeeferOriginal, order(regvars r2) format(%5.2f) paren(stderr) asterisk())
	
regress LogKeefer Checks33 DiEiec33 stabnsLag3, vce(cluster country)
	regsave using "KeeferLog.dta", detail(all) replace table(KeeferLog, order(regvars r2) format(%5.2f) paren(stderr) asterisk())
	
// Using Honohan original
regress Honohan2003_Fiscal Checks33 DiEiec33 stabnsLag3, vce(cluster country)
	regsave using "Honohan.dta", detail(all) replace table(Honohan, order(regvars r2) format(%5.2f) paren(stderr) asterisk())


// Using data updated through the 2007/09 crisis
regress LV2012_Fiscal Checks33 DiEiec33 stabnsLag3, vce(cluster country)
	regsave using "LVNoOn.dta", detail(all) replace table(LVNoOnFull, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

/// Ongoing variable indicates that the crisis is still ongoing
regress LV2012_Fiscal Checks33 DiEiec33 stabnsLag3 ongoingLV, vce(cluster country)
	regsave using "LVFullBasic.dta", detail(all) replace table(LVFull, order(regvars r2) format(%5.2f) paren(stderr) asterisk())
	
gen LogLV = log(LV2012_Fiscal + 0.1)	

//regress LogLV Checks33 DiEiec33 Logstab ongoingLV, vce(cluster country)
//	regsave using "LVFullLogged.dta", detail(all) replace table(LVFullLogged, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

/// Ongoing variable indicates that the crisis is still ongoing with logged DV
regress LogLV Checks33 DiEiec33 stabnsLag3 ongoingLV, vce(cluster country)
	regsave using "LVFullLogged.dta", detail(all) replace table(LVFullLogged, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

	
////////// Subset Data ///////////////
// Using updated LV data for crises before 2001
keep if year < 2001

regress LV2012_Fiscal Checks33 DiEiec33 stabnsLag3, vce(cluster country)
	regsave using "LVPre2001.dta", detail(all) replace table(LVPre2001, order(regvars r2) format(%5.2f) paren(stderr) asterisk())

// Using updated LV data for crises before 2001 only when Keefer also has data
keep if Keefer2007_Fiscal != .

regress LV2012_Fiscal Checks33 DiEiec33 stabnsLag3, vce(cluster country)
	regsave using "LVPre2001KeeferSample.dta", detail(all) replace table(LVPre2001Keef, order(regvars r2) format(%5.2f) paren(stderr) asterisk())
