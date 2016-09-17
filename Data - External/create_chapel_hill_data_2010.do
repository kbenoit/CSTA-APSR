
use "CHES_2010_expert_data_public.dta", clear
keep if cname=="uk"
collapse (mean) DimEconomicPosMean = spendvtax (sd) DimEconomicPosSD = spendvtax (count) DimEconomicPosN = spendvtax ///
         (mean) DimEconomicImpMean = spendvtax_salience (sd) DimEconomicImpSD = spendvtax_salience (count) DimEconomicImpN = spendvtax_salience ///
         (mean) DimSocialPosMean = sociallifestyle (sd) DimSocialPosSD = sociallifestyle (count) DimSocialPosN = sociallifestyle ///
         (mean) DimSocialImpMean = social_salience (sd) DimSocialImpSD = social_salience (count) DimSocialImpN = social_salience ///
         (mean) DimImmigrationPosMean = immigrate_policy (sd) DimImmigrationPosSD = immigrate_policy (count) DimImmigrationPosN = immigrate_policy ///
         (mean) DimImmigrationImpMean = immigra_salience (sd) DimImmigrationImpSD = immigra_salience (count) DimImmigrationImpN = immigra_salience ///
         (mean) DimAsylumPosMean = immigrant_asylum (sd) DimAsylumPosSD = immigrant_asylum (count) DimAsylumPosN = immigrant_asylum ///
         (mean) DimAsylumImpMean = immasylum_salience (sd) DimAsylumImpSD = immasylum_salience (count) DimAsylumImpN = immasylum_salience, ///
		 by(party_name)
reshape long DimEconomic DimSocial DimImmigration DimAsylum, i(party) j(measure) string
reshape long Dim, i(party_name measure) j(Dimension) string
reshape wide Dim, i(party_name Dimension) j(measure) string
rename Dimension xDimension
reshape long Dim, i(party_name xDimension) j(temp) string
gen scale = "Position"
replace scale = "Importance" if regexm(temp, "Imp")
replace temp="Mean" if temp=="ImpMean"
replace temp="Mean" if temp=="PosMean"
replace temp="SD" if temp=="ImpSD"
replace temp="SD" if temp=="PosSD"
replace temp="N" if temp=="ImpN"
replace temp="N" if temp=="PosN"

reshape wide Dim, i(party_name xDimension scale) j(temp) string
rename party_name party
rename xDimension dimension
rename DimMean Mean
rename DimN N
rename DimSD SD
gen SE = SD / sqrt(N)
saveold CHexpertsurveyscores.dta, replace

