** File-Name:       manuscript_analysis.do
** Date:            2014-01-10
** Author:          Ken Benoit
** Email:           kbenoit@lse.ac.uk                                      
** Purpose:         Replicate figures and tables in the paper
** Data Used:       coding_all_long_2013-12-19.dta
**                  GBexpertscores.dta
**                  CMP_BLM2009_logitscaleSE.dta
** Packages Used:   
** Output File:     
** Data Output:     
** Machine:         Ken Benoit's Macbook Air

** Copyright (c) 2012, under the Simplified BSD License.  
** For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
** All rights reserved.


// set correct working directory (for Ken's system)
if c(username)=="kbenoit" {
  chdir "~/Dropbox/Papers/CMP_recoding/newanalysis/06 Produce Paper Tables and Figures"
}

global LONGDATA = "..//Data - Created/coding_all_long_2014-03-14.dta"

global latestdatawide "../Data - Created/coding_all_wide_2014-03-14.dta"


/******************************************************/
/** FIGURE: TRI-PLOT OF DOMAIN CODING                **/
/******************************************************/
use $LONGDATA if stage!=3 & er==0, clear
keep if manifestoid!=0 & manifestoid!=99 & stage=="ES":stage
tab scale, gen(scaleBoolean)
collapse (sum) none=scaleBoolean1 econ=scaleBoolean2 soc=scaleBoolean3, by(sentenceid party year)
gen noneProp = none / (none + econ + soc)
label var noneProp "Coded Neither"
gen econProp = econ / (none + econ + soc)
label var econProp "Coded Economic"
gen socProp  = soc  / (none + econ + soc)
label var socProp "Coded Social"
// triplot socProp econProp noneProp, separate(party) m(o o o) mcolor(red green blue)
triplot socProp econProp noneProp, y title(Domain Coding) ///
  m(o) msize(vtiny) jitter(5) name(triAll, replace) scheme(s1mono)
// graph export plots_created/scaleTriplotES.pdf, replace



/******************************************************/
/** FIGURE: EXPERT CONSENSUS                         **/
/******************************************************/
use $LONGDATA if manifestoid!=0 & manifestoid!=99 & stage=="ES"




/******************************************************/
/** TABLE: Estimates of British parties         **/
/******************************************************/
// only import ES (er==0)
use "$LONGDATA" if (year==1987 | year==1997) & ///
   stage!=3 & er==0 & scale>0, clear 
collapse (mean) code (count) Ncoded=code, by(scale year party er sentenceid) 
egen SD = sd(code), by(scale year party er)
egen N = count(code), by(scale year party er)
gen SE = SD/sqrt(N)
table scale year party, c(mean code mean SE N code) format(%6.2f)

//
// Table of summary means by manifesto - mean of sentence means
//
use "$LONGDATA" if manifestoid>0 & source!=3, clear
collapse (mean) code (count) Ncoded=code, by(scale year party source sentenceid) 
egen float SD = sd(code), by(scale year party source)
egen int N = count(code), by(scale year party source)
gen float SE = SD/sqrt(N)
drop SD
format Ncoded %6.0g
format code SE %6.2f
table source year party if scale=="Economic":scale, c(mean code mean SE N code sum Ncoded) 
table source year party if scale=="Social":scale, c(mean code mean SE N code sum Ncoded) 

/***********************************************************/
/** FIGURE: plot expert v. crowd differences by sentence  **/
/***********************************************************/
use "$latestdatawide", clear
graph twoway (lfitci codeExperts codeCrowd) (scatter codeExperts codeCrowd, m(oh) msize(tiny) jitter(1)) if scale!="None":scale, ///
   by(scale, legend(off) note("")) scheme(s1mono)  aspectratio(1) ///
   xline(0, lp(shortdash) lc(red)) yline(0, lp(shortdash) lc(red)) ///
   xtitle(Crowd Mean Code) ytitle(Expert Mean Code) name(expertsVcrowd, replace)



/*********************************************/
/** TABLE A1: Expert coding summaries and   **/
/**           total sentences per manifesto **/
/*********************************************/
use "$LONGDATA" if manifestoid!=0 & manifestoid!=99, clear
gen stage2 = stage
recode stage2 (2=0) (5/7=4)
label define stage2 0 ES 1 ER 3 SE 4 Crowd
label values stage2 stage2
table manifesto source

// drop if stage2>1
egen codes = count(coderid), by(sentenceid stage)
table manifesto stage, c(n coderid min codes mean codes max codes)
table manifesto source, c(n coderid min codes mean codes max codes)

table manifesto stage2, c(mean codes)


use "../Data - External/MPDataset_MPDS2013b.dta", clear


table manifestoid er, c(count coderid) row col
duplicates drop sentenceid, force
table manifestoid, c(count coderid) row col


/******************************************************/
/** TABLE A2: classification agreement frequencies   **/
/******************************************************/
use "data_created/expert_coding_all.dta" if stage!=3, clear
keep if stage=="ES":stage
quietly tab scale, generate(scale_)
collapse (sum) Economic=scale_2 Social=scale_3, by(sentenceid)
label var Economic "Economic policy"
label var Social "Social policy"
table Economic Social, row col


// don't load the semi-experts (stage==3) or the screener questions (manifestoid==0)
// or the Neither sentences (scale==0)
use data_created/coding_all_long.dta if stage!=3 & scale>0 & manifestoid!=0, clear

// manifesto sentences coded by coding stage
table manifestoid stage, row col miss

// get means of the manifestos
preserve
collapse (mean) code (count) Ncoded=code, by(scale year party source sentenceid) 
egen float SD = sd(code), by(scale year party source)
egen int N = count(code), by(scale year party source)
gen float SE = SD/sqrt(N)
drop SD
format Ncoded %6.0g
format code SE %6.2f
table source year party if scale=="Economic":scale, c(mean code mean SE N code sum Ncoded) 
table source year party if scale=="Social":scale, c(mean code mean SE N code sum Ncoded) 
restore

// plot expert v. crowd differences by sentence
use data_created/coding_econsocbySentence_wide.dta, clear
graph twoway (lfitci codeExperts codeCrowd) (scatter codeExperts codeCrowd, msize(vsmall)) if scale!="None":scale, ///
   by(scale, legend(off) note("")) scheme(s1mono) ///
   xtitle(Crowd Mean Code) ytitle(Expert Mean Code)

   

/******************************************************/
/** MISC CROWD-CODER DATA                            **/
/******************************************************/
use "$LONGDATA" if manifestoid!=0 & manifestoid!=99 & stage>=4, clear
// total unique coders
quietly tab coderid
return list
// from which and how many countries
tab country, miss
return list
// total codes per coder
collapse (count) totalcoded=codingid, by(coderid)
gen ltotalcoded = log10(totalcoded)


  
   
/******************************************************/
/** TABLE: IMMIGRATION POLICY                        **/
/******************************************************/

// CHECK CODER OVERLAP
clear
import delimited "../Data - CF Jobs/immigration/f354277_immigration1.csv"
gen job = "f354277"
save temp, replace
clear
import delimited "../Data - CF Jobs/immigration/f389381_immigration2.csv"
gen job = "f389381"
append using temp, force
drop if manifestoid=="screener"

tab job
quietly tab _worker_id if job=="f354277"
return list
quietly tab _worker_id if job=="f389381"
return list
quietly tab _worker_id
return list

preserve
gen immigration = (policy_area==4)
collapse (mean) immigration, by(sentenceid manifestoid job)
gen immigrationBoolean = (immigration>=.5)
table manifestoid immigrationBoolean job, row
restore

preserve
gen immigration = (policy_area==4)
collapse (mean) immigration, by(sentenceid manifestoid)
gen immigrationBoolean = (immigration>=.5)
table manifestoid immigrationBoolean, row
restore



collapse (count) ncoded=_unit_id, by(_worker_id job)
reshape wide ncoded, i(_worker_id) j(job) string
recode ncoded* (.=0)
gen overlap = ncodedf354277>0 & ncodedf389381 >0
tab overlap
tab ncodedf354277 overlap if ncodedf354277>0, row
tab ncodedf389381 overlap if ncodedf389381>0, row
// Results: 73% of the first round coders did not participate in the 2nd
//          71% of the second round coders did not participate in the 1st 
//          Total overlap was just 16.47%
table overlap, c(sum ncodedf354277 sum ncodedf389381) row
// and 63% of the second round codes came from new coders
//  di 15749/24964 = .63086845


chdir "~/Dropbox/Papers/CMP_recoding/Crowdflower/Immigration"

clear
// insheet using f354277.csv
import delimited "~/Dropbox/Papers/CMP_recoding/newanalysis/Data - CF Jobs/immigration/f354277_immigration1.csv"

recode policy_area (1=0) (4=1)
rename _golden gold

collapse (count) npolicy_area=policy_area (mean) meanpolicy_area=policy_area ///
			(mean) meanimmigr = immigr_scale (count) nimmigr=immigr_scale, by(sentenceid manifestoid gold)
		 
format %12.0f sentenceid 

tab meanpolicy_area npolicy_area if gold!="true", miss

gen immigration = (meanpolicy_area >=.5)

collapse (sum) totimmigrcodes=immigration ///
         (mean) meanimmigrcodes=meanpolicy_area ///
		 (mean) meanimmigrpos=meanimmigr ///
		 (count) totsentences=sentenceid, ///
		by(manifestoid)
list, clean noobs		

		
clear
//insheet using f354277.csv
insheet using f389381.csv

drop if manifestoid=="screener"
sort manifestoid

capture program drop meanimmg
program define meanimmg, rclass
	preserve 
	collapse immigr_scale, by(sentenceid manifestoid)
	collapse immcrowd = immigr_scale, by(manifestoid)
	// list
	return scalar immBNP = immcrowd[1]
	return scalar immCoal = immcrowd[2]
	return scalar immCon = immcrowd[3]
	return scalar immGR = immcrowd[4]
	return scalar immLD = immcrowd[5]
	return scalar immLab = immcrowd[6]
	return scalar immPC = immcrowd[7]
	return scalar immSNP = immcrowd[8]
	return scalar immUKIP = immcrowd[9]
	restore
end

meanimmg	
return list

bootstrap immBNP=(r(immBNP)) ///
    immCoal=(r(immCoal))  ///
    immCon=(r(immCon)) ///
	immGR=(r(immGR)) ///
	immLD=(r(immLD)) ///
	immLab=(r(immLab)) ///
	immPC=(r(immPC)) ///
	immSNP=(r(immSNP)) ///
	immUKIP=(r(immUKIP)): meanimmg, reps(500) saving(bscrowdscoresFeb.dta, replace)


/********************************************************/
/** TABLE 3: Inter-coder reliabilities Experts ES Core **/
/********************************************************/
use "$LONGDATA" if (year==1987 | year==1997) & ///
   stage<=1 & scale>0, clear 
// Generate a three point -1, 0, 1 "directional coding of each sentence
generate codethree = code
recode codethree (-2 = -1) (2 = 1)
tabulate code codethree
// drop "old duplicates" - DOESN'T ACTUALLY DROP ANYTHING
gsort -coding_timestamp
duplicates drop coderid sentenceid party year, force
drop codingid coding_timestamp 
// economic and social codes if that scale was assigned, otherwise missing
gen economic5 = code if scale=="Economic":scale
gen social5  = code if scale=="Social":scale
gen economic3 = codethree if scale=="Economic":scale
gen social3 = codethree if scale=="Social":scale
// oneway ANOVA model - but not really applicable since assumes ~ N(mu,sigma)
//                      and codes are ordinal-5/categorical-3
/**
loneway scale sentenceid if stage=="ES":stage
loneway economic sentenceid if stage=="ES":stage
loneway economic sentenceid if stage=="ER":stage
loneway social sentenceid if stage=="ES":stage
loneway social sentenceid if stage=="ER":stage
// estimate the variances of the aggregate (mean) scores 
table manifestoid scale er if scale>0, contents(mean code sd code n code) format(%6.2f)
table manifestoid scale er if scale>0, contents(sd code n code) format(%6.2f)
**/
// calculate Fleiss's kappa statistics, after reshaping to wide
decode coderid, gen(coder) // encode as text
drop coderid
// keep if er=="Sequential":er  // keep sequential only
reshape wide scale code codethree econ* soc*, i(sentenceid er) j(coder) string
kap scale* if stage=="ES":stage
kap scale* if stage=="ER":stage

kap economic5* if stage=="ES":stage
kap economic3* if stage=="ES":stage
kap social5* if stage=="ES":stage
kap social3* if stage=="ES":stage

kap economic5* if stage=="ER":stage
kap economic3* if stage=="ER":stage
kap social5* if stage=="ER":stage
kap social3* if stage=="ER":stage

kap economic5* 
kap economic3* 
kap social5*
kap social3*


kap code* if stage=="ES":stage
kap codethree* if stage=="ES":stage

//
// TABLE 3: Cronbach's alpha for Economic with item detail by coder
//
alpha economicKen economicMik economicSlava economicIulia economicPablo economicAlex if stage=="ES":stage, item

//
// TABLE A1d: Cronbach's alpha for Social with item detail by coder
//
alpha socialKen socialMik socialSlava socialIulia socialPablo socialAlex if stage=="ES":stage, item

	
	
	
