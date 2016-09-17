/***
 *** Preliminary analysis of the core coded sentence set
 *** This is from "Phase 1" where we aimed for 20 crowd codes per sentence
 ***
 ***
 ***
 ***/

// set correct working directory (for Ken's system)
if c(username)=="kbenoit" {
  chdir "~/Dropbox/Papers/CMP_recoding/newanalysis/04 Analyze Core Crowd Data"
}
// set for Mik's system
if c(username)=="ml127" {
  cd "C:\Users\ml127\Dropbox\Text\Crowdsourcing\newanalysis/04 Analyze Core Crowd Data"
}
// IF THIS DOES NOT WORK, THEN SET THE CURRENT DIRECTORY MANUALLY
// USING File -> Change Working Directory... to 
// newanalysis/04 Analyze Core Crowd Data/


// //////////////////////////////////////////////////////////////////
// CURRENT FILES -- UPDATE IF RUNS CHANGE
global latestdata "../Data - Created/coding_all_long_2014-03-14.dta"
global latestdatawide "../Data - Created/coding_all_wide_2014-03-14.dta"
// //////////////////////////////////////////////////////////////////


// ---------------------- 
// Manifesto |TOT. UNIQUE
// ID        |  SENTENCES
// ----------+-----------
//  Con 1987 |      1,015
//   LD 1987 |        878
//  Lab 1987 |        455
//  Con 1997 |      1,171
//   LD 1997 |        873
//  Lab 1997 |      1,052
// ----------------------

//
// Table of summary means by manifesto - mean of sentence means
//
use "$latestdata" if manifestoid>0 & manifestoid<=6 & source!=3, clear
collapse (mean) code (count) Ncoded=code, by(scale year party source sentenceid) 
egen float SD = sd(code), by(scale year party source)
egen int N = count(code), by(scale year party source)
gen float SE = SD/sqrt(N)
drop SD
format Ncoded %6.0g
format code SE %6.2f
table source year party if scale=="Economic":scale, c(mean code mean SE N code sum Ncoded) 
table source year party if scale=="Social":scale, c(mean code mean SE N code sum Ncoded) 


// plot expert v. crowd differences by sentence
use "$latestdatawide", clear
graph twoway (lfitci codeExperts codeCrowd) (scatter codeExperts codeCrowd, m(oh) msize(tiny) jitter(1)) if scale!="None":scale, ///
   by(scale, legend(off) note("")) scheme(s1mono) ///
   xtitle(Crowd Mean Code) ytitle(Expert Mean Code) name(expertsVcrowd, replace)


//
// compute and compare 'overall' manifesto scores by policy area - separate crowd results
//
use "$latestdata" if manifestoid>0 & manifestoid<=6, clear
preserve
gen source2 = stage
recode source2 (0/3 = 1) (4/5 = 2) (6 = 3) 
label define source2 1 Experts 2 DrewCF 3 KenCF
label values source2 source2
collapse (mean) code (count) Ncoded=code, by(scale party year manifestoid source2 sentenceid) 
table manifestoid source2 scale if scale>0, contents(sd code n code) format(%6.2f)
// save summary results for ES to a dataset
statsby, by(source2 manifestoid party year scale) clear: ci code
drop if scale=="None":scale
keep mean manifestoid source2 scale
decode source2, gen(source)
// decode scale, gen(scalet)
drop source2 // scale
reshape wide mean, i(manifestoid scale) j(source) string
graph matrix mean* if scale=="Economic":scale, msize(vsmall) mlabel(manifestoid) scheme(s2mono) name(twoCF, replace)
list, clean
restore

//
// compute and compare 'overall' manifesto scores by policy area - combined crowd results
//
use "$latestdata" if manifestoid>0 & manifestoid<=6, clear
drop if source==3
collapse (mean) code (count) Ncoded=code, by(scale party year manifestoid source sentenceid) 
// compare the SDs of the experts to the crowd
table manifestoid source scale if scale>0, contents(sd code n code) format(%6.2f)
// save summary results for ES to a dataset
statsby, by(source manifestoid scale) clear: ci code
drop if scale=="None":scale
format mean se %6.2f
format N %6.0f
table manifestoid source scale if scale>0, contents(mean mean mean se mean N)
keep mean manifestoid source scale
decode source, gen(source2)
// decode scale, gen(scalet)
drop source // scale
reshape wide mean, i(manifestoid scale) j(source2) string
graph twoway (scatter mean*, msize(vsmall) mlabel(manifestoid)) ///
             (lfit mean*, lcol(gray) lstyle(dash)) if scale=="Economic":scale, ///
    legend(off) scheme(s1mono) ///
    ytitle(Crowd Mean) title(Economic Scale Comparison) ///
    xscale(range(-1 1)) yscale(range(-1 1)) ylabel(-1(.5)1) xlabel(-1(.5)1) ///
    ysize(7) xsize(7) name(CFvExpertsEcon, replace)
graph twoway (scatter mean*, msize(vsmall) mlabel(manifestoid)) ///
             (lfit mean*, lcol(gray) lstyle(dash)) if scale=="Social":scale, ///
    legend(off) scheme(s1mono) ///
    ytitle(Crowd Mean) title(Social Scale Comparison) ///
    xscale(range(-1 1)) yscale(range(-1 1)) ylabel(-1(.5)1) xlabel(-1(.5)1) ///
    ysize(7) xsize(7) name(CFvExpertsSocial, replace)
list, clean
// correlations of the two scales
bysort scale: cor mean*


//
// scatterplot matrixes comparion proportions of scale categories
//
use "$latestdata" if manifestoid>0 & manifestoid<=6, clear
tab scale, gen(areadum)
tab code, gen(scaledum)
collapse (count) Ncodes=coderid (sum) areadum* scaledum*, by(stage sentenceid)

gen propnone = areadum1 / Ncodes
gen propecon = areadum2 / Ncodes
gen propsoc  = areadum3 / Ncodes

gen propleft   = (scaledum1 + scaledum2)/Ncodes
gen propcentre = (scaledum3)/Ncodes
gen propright  = (scaledum4 + scaledum5)/Ncodes

drop *dum* 

decode stage, gen(stagetxt)
drop stage Ncodes
reshape wide prop*, i(sentenceid) j(stagetxt) string

graph matrix propnone*, jitter(2) msize(tiny) name(none, replace)
graph matrix propecon*, jitter(2) msize(tiny) name(econ, replace)
graph matrix propsoc*, jitter(2) msize(tiny) name(soc, replace)


//
// RE logit analysis in Stata
//
// use just Labour 1997 for now
use "$latestdata" if manifestoid==6, clear
tab scale, gen(areadum) // generate policy area dummies
xtmelogit areadum1 || sentenceid:
predict sentenceRE, reffects
predict sentenceREse, reses
predict catprob
collapse (mean) sentenceRE sentenceREse catprob (count) Ncodes=coderid (sum) areadum*, by(sentenceid)
gen propnone = areadum1 / Ncodes
browse sentence* catprob propnone
scatter catprob propnone, msize(vsmall) m(Oh) scheme(s1mono) jitter(.5) ///
    xtitle(Empirical proportion of not econ or social) ytitle(RE logit predicted probability of not econ or social) ///
    yline(.5, lp(dash)) xline(.5, lp(dash)) name(RElogitXempirical, replace)


// How many sentences still need to be coded to reach the 20/sentence
use "$latestdata" if manifestoid>0 & manifestoid<=6, clear
drop if file_name=="f99419"
egen Ncoded = count(scale), by(sentenceid source)
gen byte gold504 = (gold=="gold504":gold)
table manifestoid source, c(min Ncoded max Ncoded median Ncoded n Ncoded)
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
hist codesneeded if sentenceid<1000000000, ///
    bin(19) frequency scheme(s1mono) xtitle("Codes still needed (core 6)") name(stillneeded, replace)
quietly table codesneeded if sentenceid < 1000000000, replace
gen neededtotal = codesneeded * table1
rename codesneeded Needed
rename table1 Freq
rename neededtotal Total
list, sum(Total) noobs clean

/**
//
// NOT WORKING YET
// calculate Fleiss's kappa statistics, after reshaping to wide
//
gen coder = string(coderid) // encode as text
drop coderid
gen sentence=string(sentenceid)
generate codethree = code
recode codethree (-2 = -1) (2 = 1)
gen economic = code if scale=="Economic":scale
gen social  = code if scale=="Social":scale
// keep if er=="Sequential":er  // keep sequential only
drop if source=="SemiExperts":source
reshape wide scale code codethree econ soc, i(coder source) j(sentenceid) string
**/


