* Davi Ferreira Veronese
* USP Number: 10277641
* University of São Paulo - Department of Political Science
* Quantitative Methods IV

*********************************************
* These are some analyses of time-series cross-sectional data I conducted in Stata.
*********************************************

******************************
* 1) Prepare the data
******************************

** Set directory
cd "C:\Users\daviv\Documents\Davi\Mestrado DCP\Métodos Quantitativos IV - Análises Temporais e Espaciais\Time-Series Cross-Section Project"

** Import data
use "C:\Users\daviv\Documents\Davi\Mestrado DCP\Métodos Quantitativos IV - Análises Temporais e Espaciais\Time-Series Cross-Section Project\Data\tscs_project_data - Davi Ferreira Veronese.dta", clear

** Set country and time variables
encode country, gen (country_n)
encode year, gen (year_n)
xtset country_n year_n
format year_n %ty


******************************
* 2) Data description
******************************

xtdescribe

** Create graphs
xtline GDPpercapita, i(country_n) t(year_n) xtitle("Year") ytitle("GDP per capita") xlab(0(10)55, format(%ty))

twoway (tsline GDPpercapita if country == "Austria", xtitle("Year") ytitle("GDP per capita")) ///
       (tsline GDPpercapita if country == "Belgium", xtitle("Year") ytitle("GDP per capita")) ///
       (tsline GDPpercapita if country == "Denmark", xtitle("Year") ytitle("GDP per capita")) ///
       (tsline GDPpercapita if country == "Finland", xtitle("Year") ytitle("GDP per capita")) ///
       (tsline GDPpercapita if country == "France", xtitle("Year") ytitle("GDP per capita")) ///
       (tsline GDPpercapita if country == "Germany", xtitle("Year") ytitle("GDP per capita")) ///
       (tsline GDPpercapita if country == "Norway", xtitle("Year") ytitle("GDP per capita")) ///
       (tsline GDPpercapita if country == "Sweden", xtitle("Year") ytitle("GDP per capita")) ///
       (tsline GDPpercapita if country == "United Kingdom", xtitle("Year") ytitle("GDP per capita")) ///
       (tsline GDPpercapita if country == "United States", xtitle("Year") ytitle("GDP per capita")), ///
       legend(order(1 "Austria" 2 "Belgium" 3 "Denmark" 4 "Finland" 5 "France" 6 "Germany" 7 "Norway" 8 "Sweden" 9 "United Kingdom" 10 "United States")) 
	   
** Calculate mean and variance for each series
tabstat GDPpercapita, by(country) statistics(mean sd var min max)

** Period 
// All the series start in 1970 and end in 2020 and are spaced annually without missing data

** Structural breaks

*** Zivot-Andrews' Test
// The null is the hypothesis of a unit root and no structural break

zandrews GDPpercapita if country == "Austria", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "Austria", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "Austria", break(both) lagmethod(TTest) // can't reject the null

zandrews GDPpercapita if country == "Belgium", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "Belgium", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "Belgium", break(both) lagmethod(TTest) // can't reject the null

zandrews GDPpercapita if country == "Denmark", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "Denmark", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "Denmark", break(both) lagmethod(TTest) // can't reject the null

zandrews GDPpercapita if country == "Finland", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "Finland", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "Finland", break(both) lagmethod(TTest) // can't reject the null

zandrews GDPpercapita if country == "France", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "France", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "France", break(both) lagmethod(TTest) // can't reject the null

zandrews GDPpercapita if country == "Germany", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "Germany", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "Germany", break(both) lagmethod(TTest) // can't reject the null

zandrews GDPpercapita if country == "Norway", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "Norway", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "Norway", break(both) lagmethod(TTest) // can't reject the null

zandrews GDPpercapita if country == "Sweden", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "Sweden", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "Sweden", break(both) lagmethod(TTest) // can't reject the null

zandrews GDPpercapita if country == "United Kingdom", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "United Kingdom", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "United Kingdom", break(both) lagmethod(TTest) // can't reject the null

zandrews GDPpercapita if country == "United States", break(both) lagmethod(AIC) // can't reject the null
zandrews GDPpercapita if country == "United States", break(both) lagmethod(BIC) // can't reject the null
zandrews GDPpercapita if country == "United States", break(both) lagmethod(TTest) // can't reject the null

** Trends

// First, let's use the dfgls test to determine optimal lags
dfgls GDPpercapita if country == "Austria" // SC indicates 1 lag, while MAIC indicates 2 lags
dfgls GDPpercapita if country == "Belgium" // SC indicates 1 lag, while MAIC indicates 2 lags
dfgls GDPpercapita if country == "Denmark" // SC indicates 1 lag, while MAIC indicates 2 lags
dfgls GDPpercapita if country == "Finland" // SC indicates 1 lag, while MAIC indicates 2 lags
dfgls GDPpercapita if country == "France" // SC indicates 1 lag, while MAIC indicates 2 lags
dfgls GDPpercapita if country == "Germany" // SC indicates 1 lag, while MAIC indicates 2 lags
dfgls GDPpercapita if country == "Norway" // SC indicates 1 lag, while MAIC indicates 2 lags
dfgls GDPpercapita if country == "Sweden" // SC indicates 1 lag, while MAIC indicates 2 lags
dfgls GDPpercapita if country == "United Kingdom" // SC indicates 3 lags, while MAIC indicates 5 lags
dfgls GDPpercapita if country == "United States" // SC indicates 1 lags, while MAIC indicates 2 lags

// Dickey fuller tests
dfuller GDPpercapita if country == "Austria", regress trend lags(1) // not trend-stationary
dfuller GDPpercapita if country == "Austria", regress trend lags(2) // not trend-stationary

dfuller GDPpercapita if country == "Belgium", regress trend lags(1) // not trend-stationary
dfuller GDPpercapita if country == "Belgium", regress trend lags(2) // not trend-stationary

dfuller GDPpercapita if country == "Denmark", regress trend lags(1) // not trend-stationary
dfuller GDPpercapita if country == "Denmark", regress trend lags(2) // not trend-stationary

dfuller GDPpercapita if country == "Finland", regress trend lags(1) // not trend-stationary
dfuller GDPpercapita if country == "Finland", regress trend lags(2) // not trend-stationary

dfuller GDPpercapita if country == "France", regress trend lags(1) // not trend-stationary
dfuller GDPpercapita if country == "France", regress trend lags(2) // not trend-stationary

dfuller GDPpercapita if country == "Germany", regress trend lags(1) // not trend-stationary
dfuller GDPpercapita if country == "Germany", regress trend lags(2) // not trend-stationary

dfuller GDPpercapita if country == "Norway", regress trend lags(1) // not trend-stationary
dfuller GDPpercapita if country == "Norway", regress trend lags(2) // not trend-stationary

dfuller GDPpercapita if country == "Sweden", regress trend lags(1) // not trend-stationary
dfuller GDPpercapita if country == "Sweden", regress trend lags(2) // not trend-stationary

dfuller GDPpercapita if country == "United Kingdom", regress trend lags(3) // not trend-stationary
dfuller GDPpercapita if country == "United Kingdom", regress trend lags(5) // not trend-stationary

dfuller GDPpercapita if country == "United States", regress trend lags(1) // not trend-stationary
dfuller GDPpercapita if country == "United States", regress trend lags(2) // not trend-stationary

******************************
* 3) Diagnostics
******************************

** 3.1. Cross-Sectional Dependence

xtcd2 GDPpercapita, noestimation // we reject the null that the errors are weakly cross-sectional dependent
// there is evidence of strong cross-sectional dependence

** 3.2. Stationarity

*** Variable in level
multipurt GDPpercapita, lags(8)
// most of the evidence that comes from CIPS test indicates that the series are non-stationary

*** Variable in first-difference
// Following Soderbom et al. (2015, p. 408)'s guidelines, test for the first-difference as well
gen first_dif_GDPpercapita = d.GDPpercapita

xtcd2 first_dif_GDPpercapita, noestimation // As expected, there is still strong cross-sectional dependence

multipurt first_dif_GDPpercapita, lags(8)
// Now we can clearly reject the null of a unit root

xtline first_dif_GDPpercapita // The graph is coherent with CIPS' results, which suggest stationarity of the series in first-difference

******************************
* 4) Conclusion
******************************

** 4.1) What do diagnostics suggest about a pooled researched design of this sample of spatial units?
// Models need to consider cross-sectional dependence since conventional pooled estimators will be inefficient and inconsistent. One alternative is the common correlated effects (CCE) estimator, which has good performance when there is cross-sectional dependence.
// In addition, there is evidence that the series are non-stationary. Therefore, models should use strategies of differenciation or error-correction models.

** 4.2) How much variation do we observe in the variable across time and across spatial units?

xtsum GDPpercapita // there is much more within-variation (across time) than between-variation (across spatial units)

** 4.3) What do you conclude about the amount of heterogeneity or homogeneity of your data?

gen L1_GDPpercapita = l.GDPpercapita
gen L2_GDPpercapita = l2.GDPpercapita
gen L3_GDPpercapita = l3.GDPpercapita

// Estimate the regression models for each country
regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "Austria"
eststo Austria

regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "Belgium"
eststo Belgium

regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "Denmark"
eststo Denmark

regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "Finland"
eststo Finland

regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "France"
eststo France

regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "Germany"
eststo Germany

regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "Norway"
eststo Norway

regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "Sweden"
eststo Sweden

regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "United Kingdom"
eststo UK

regress GDPpercapita l.GDPpercapita l2.GDPpercapita l3.GDPpercapita if country == "United States"
eststo US

esttab Austria Belgium Denmark Finland France Germany Norway Sweden UK US, se

// Plot the coefficients using coefplot
coefplot Austria Belgium Denmark Finland France Germany Norway Sweden UK US, drop(_cons) xline(0) ylabel(, angle(horizontal))
// There is no evidence of heterogeneity

// If there was heterogeneity, estimate using Mean Group Estimator (MGE)
xtmg GDPpercapita L1_GDPpercapita L2_GDPpercapita L3_GDPpercapita
