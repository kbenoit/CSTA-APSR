** File-Name:       03_addCFdata.do
** Date:            2013-10-28
** Author:          Ken Benoit
** Email:           kbenoit@lse.ac.uk                                      
** Purpose:         Incorporate CF data into master dataset
** Data Used:       data_created/crowd_coding_all.dta
** Code Used:       coding_all_long_labels.do                 
** Packages Used:   
** Output File:     allcrowddata.dta
**                  
** Data Output:     
** Machine:         Ken Benoit's Macbook Air, Stata 12

** THIS WILL CONTAIN THE Stata code to load and transform and clean the crowd data
** into the same format (one obs per sentence) as the expert coding data

// set correct working directory (for Ken's system)
if c(username)=="kbenoit" {
  chdir "~/Dropbox/Papers/CMP_recoding/newanalysis/03 Make Updated Master Codes Dataset"
}
clear
set more off

global THISRUNDATESTRING "2014-03-14" // YYYY-MM-DD

// read in all .csv files in the CF downloads folder
local myfilelist : dir "../Data - CF Jobs/" files "*.csv"
foreach f of local myfilelist {
	clear
	insheet using "../Data - CF Jobs/`f'"
	gen file_name = subinstr("`f'", ".csv", "", 1)
	compress
	capture confirm file _tempcsvfile.dta
	if !_rc {
		append using _tempcsvfile.dta
	}
	save _tempcsvfile.dta, replace
}
erase _tempcsvfile.dta


rename _unit_id codingid
rename _worker_id coderid

// manifesto id
gen int manifestoid = floor(sentenceid / 10000000)
label values manifestoid manifesto

// sentenceid defined in data

// scale variable 
gen byte code = econ_scale
replace code = soc_scale if econ_scale>=.

gen byte title_shown = 0
gen byte core = (manifestoid <=6)

// timestamp
gen double coding_timestamp = clock(_created_at, "MDY hms")
format %tc coding_timestamp
drop _created_at

gen byte scale = policy_area - 1
label values scale scale

gen byte party = 1
replace party = 2 if manifestoid==3 | manifestoid==6 | manifestoid==109 | manifestoid==112 | manifestoid==115 | manifestoid==118  
replace party = 3 if manifestoid==2 | manifestoid==5 | manifestoid==108 | manifestoid==111 | manifestoid==114 | manifestoid==117
label values party party

gen int year = manifestoid
recode year (1/3 = 1987) (4/6=1997) (107/109=1992) (110/112=2001) (113/115=2005) (116/118=2010)

gen byte stage = 6
label values stage stage


gen byte er = .
gen byte context=1
gen sequence = 0
gen screener = 0
gen source = 2
label values source source

gen int country = .
replace country = 25  if _country=="GBR"
replace country = 100 if _country=="ARE"
replace country = 101 if _country=="ARG"
replace country = 102 if _country=="BEL"
replace country = 1   if _country=="BRA"
replace country = 3   if _country=="CAN"
replace country = 103 if _country=="COL"
replace country = 104 if _country=="HUN"
replace country = 105 if _country=="IND"
replace country = 106 if _country=="LVA"
replace country = 107 if _country=="MKD"
replace country = 108 if _country=="MYS"
replace country = 109 if _country=="OMN"
replace country = 110 if _country=="PAK"
replace country = 111 if _country=="PRT"
replace country = 112 if _country=="URY"
replace country = 27  if _country=="USA"
replace country = 113 if _country=="ZAF"
replace country = 15  if _country=="ITA"
replace country = 19  if _country=="NLD"
replace country = 21  if _country=="ROU"
replace country = 7   if _country=="CZE"
replace country = 11  if _country=="DEU"
replace country = 8   if _country=="DNK"
replace country = 29  if _country=="FIN"
replace country = 30  if _country=="HKG"
replace country = 13  if _country=="IRL"
replace country = 18  if _country=="MEX"
label values country country
rename _country country_text

gen byte gold = (_golden=="true")*2
label values gold gold

rename _region region
rename _city city
rename _missed missed
rename _trust trust

rename _channel channel
drop _* econ_scale* soc_scale* policy_area* pre_sentence post_sentence sentence_text orig* 
// drop text_unit_id

recode code (.z=.)
recode screener (.z=0)

replace stage = 7 if file_name=="f354285"


compress
drop codesneeded
save "../Data - Temp/addedCFdata.dta", replace


use "../Data - External/coding_all_long_earlyPhases.dta", clear
append using "../Data - Temp/addedCFdata.dta"
label define stage 6 CF_kb 7 CF12_kb, add
label define gold 2 gold504, add
do coding_all_long_labels.do

// JUST USE THE COUNTRY TEXT FROM CF, BUT ENCODE IT
drop country
encode country_text, gen(country)
compress


//
// CORRECT SOME MISTAKES IN THE EARLY PHASE FILE
//
// correct a mistake in the master data with respect to "er" variable
replace er = . if stage=="ER":stage
// correct some mistakes in the codes/scale assignment
replace code=. if scale=="None":scale  // 3 sentences with codes despite scale==None
drop if scale=="Economic":scale & code==. // 149 with Econ scale but no code
drop if scale=="Social":scale & code==.   // 105 with Econ scale but no code
// drop the "bad" early run of Job 99419
drop if file_name=="f99419"

label data "Consolidated long format coding from all stages, $THISRUNDATESTRING"
saveold "../Data - Created/coding_all_long_$THISRUNDATESTRING", replace
outsheet using "../Data - Created/coding_all_long_$THISRUNDATESTRING.csv", comma replace

// save the wide format dataset
preserve
collapse (mean) code gold (count) Ncoded=code, by(scale year party source sentenceid manifestoid)
label var code "Position code for this sentence" 
label var Ncoded "Number of codings for this sentence for this source"
egen float SD = sd(code), by(scale year party source)
label var SD "SD of codes for this manifesto for this source/scale"
egen int N = count(code), by(scale year party source)
label var N "Total codes for this manifesto for this source/scale"
gen float SE = SD/sqrt(N)
label var SE "SE of mean code for this manifesto for this source/scale"
drop SD
format Ncoded %6.0g
format code SE %6.2f
decode source, gen(source_text)
drop source
// deal with the "fractional" gold
rename gold _tempgold
gen gold = (_tempgold==1)
label values gold gold
drop _tempgold
recode gold (.=0)
// reshape to wide
// drop gold
reshape wide code N Ncoded SE gold, i(sentenceid scale) j(source_text) string
drop goldExperts goldSemiExperts
label data "Consolidated wide format from all stages, $THISRUNDATESTRING"
saveold "../Data - Created/coding_all_wide_$THISRUNDATESTRING.dta", replace
restore


//
// Produce a table of the core codings still needed to reach 20 per sentence
// 
egen Ncoded = count(scale), by(sentenceid source)
gen byte gold504 = (gold=="gold504":gold)
table source, c(min Ncoded max Ncoded mean Ncoded median Ncoded n Ncoded)
collapse (count) Ncoded=scale (max) gold504, by(sentenceid source)
egen gold = max(gold504), by(sentenceid)
drop gold504
reshape wide Ncoded, i(sentenceid) j(source)
rename Ncoded1 NcodedExperts
rename Ncoded2 NcodedCrowd
rename Ncoded3 NcodedSemiExp
recode Ncoded* (.=0)
drop if sentenceid<=10
gen byte codesneeded = 20 - NcodedCrowd
replace codesneeded = 0 if codesneeded<0
sort sentenceid

hist codesneeded if sentenceid<1000000000, bin(19) frequency scheme(s1mono) xtitle("Codes still needed (core 6)")
table codesneeded if sentenceid < 1000000000, replace
gen neededtotal = codesneeded * table1
rename codesneeded Needed
rename table1 Freq
rename neededtotal Total
list, sum(Total) noobs clean



