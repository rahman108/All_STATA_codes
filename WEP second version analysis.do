//Do file with the weight of the data and the actual health outcome variables

use "C:\Users\Saiem 70\Dropbox\Research stuff\Alok bohara research\Water quality data with weight\FinalMasterData_weight_Danda2016_May31_ver13.dta" 


// Working on all the independent variables*******
// The self reported health status
tab HealthRating
hist HealthRating
gen HealthRating2 = HealthRating   // 0 completely not satisfied, 10 = completelky satisfied
recode HealthRating2 ( 0 1 2 3 4 5 =1) (6 7 8 = 2) (9 10 = 3)   
label variable HealthRating2 "Health Rating Range, 0-10: Not Satisfied= 0; Satisfied = 10; Halway = 5"   
label define  HealthRating2l 1 "Poor Health (0-5)" 2  "Medium Health (6-8)" 3 "Good Health (9-10)"  
label value HealthRating2 HealthRating2l
tab HealthRating2
hist HealthRating2

gen healthtrt= HealthRating2 if TreatmentWater==1
tab healthtrt

gen healthtrt_bad= HealthRating2 if TreatmentWater==2 & 3
tab healthtrt_bad

gen noofsickdhr = NoSickChildDhr + NoSickAdultDhr
gen noofsickdhrActual= noofsickdhr if TreatmentWater==1
gen noofsickdhrActual_bad= noofsickdhr if TreatmentWater==2 & 3

gen NoSickAdultDhr_good = NoSickAdultDhr if TreatmentWater==1
gen NoSickAdultDhr_bad = NoSickAdultDhr if TreatmentWater==2 & 3

gen waterpercept=.
replace waterpercept=1 if WaterPerceptionOwn==1
replace waterpercept=0 if waterpercept==.
tab waterpercept

gen healthtrtwatNO = HealthRating2 if waterpercept==1
tab healthtrtwatNO

gen healthtrtwatYES = HealthRating2 if waterpercept==0
tab healthtrtwatYES


**************************
tab DistanceToPavedRoadMin
gen DistanceToPavedRoadMin_5Min = DistanceToPavedRoadMin <=5
gen DistanceToPavedRoadMin_10Min = DistanceToPavedRoadMin >5 & DistanceToPavedRoadMin <=10
gen DistanceToPavedRoadMin_30Min = DistanceToPavedRoadMin >10 & DistanceToPavedRoadMin <=30
gen DistanceToPavedRoadMin_More30Min = DistanceToPavedRoadMin >30

gen DistanceToRoadRange = DistanceToPavedRoadMin_5Min + 2*DistanceToPavedRoadMin_10Min + ///
	3*DistanceToPavedRoadMin_30Min + 4*DistanceToPavedRoadMin_More30Min
**********************
tab Gender
hist Gender
gen gender=.
replace gender= 1 if Gender==1
replace gender= 0 if Gender==2
tab gender
rename gender Male
************************
tab Caste
gen casteorder= Caste
recode casteorder (2 4 6 7 8= 1) (5=2) (3=3) (1=4) 
label variable casteorder "Order of the Caste system"  
label define casteorderl  1 "Everyting else" 2  "Third highest" 3 "Second Highest"  4 "Highest"  
label value casteorder  casteorderl
tab casteorder

gen CasteBrahman = .
replace CasteBrahman = 1 if Caste==1
replace CasteBrahman = 0 if CasteBrahman == .
tab CasteBrahman 

gen CasteTopTwo= .
replace CasteTopTwo= 1 if Caste== 1 & 3
replace CasteTopTwo= 0 if CasteTopTwo==.
tab CasteTopTwo
************************ 
tab AirPollutionEffect
gen strongairpollution =.
replace strongairpollution=1 if AirPollutionEffect==4| AirPollutionEffect==5
replace strongairpollution=0 if strongairpollution==.
tab strongairpollution
************************
tab NoisePollutionEffect
gen strongnoisepollution =.
replace strongnoisepollution=1 if NoisePollutionEffect ==4| NoisePollutionEffect ==5
replace strongnoisepollution =0 if strongnoisepollution ==.
tab strongnoisepollution
***********************



***********************
tab EducationLevel
hist EducationLevel
gen EducationLevel2= EducationLevel
recode EducationLevel2 (0 1 8 7 9 =1) (2 3 4 =2) (5 =3) (6 =4) 
label variable EducationLevel2 "Education level Range"
label define EducationLevel2l 1"Less than 10" 2 "Less than or equal 12" 3 "Bachelors complete" 4 "MS complete" 5 "Vocational Training"
label value EducationLevel2 EducationLevel2l
tab EducationLevel2
hist EducationLevel2
rename EducationLevel2 Education

graph pie, over(Education) plabel(_all name) title("education level")
**********************
tab IncomeQuintile
hist IncomeQuintile
rename IncomeQuintile AssetIndex

***********************
tab Age
corr Age AgeRespondent
gen age2= Age^2
hist Age
hist age2
rename age2 AgeSq
***********************
tab SourceWater
gen safetubewell= SourceWater==3 | SourceWater==4
tab safetubewell
rename safetubewell DeepTubewell
***********************
tab HandWash
gen HandWash2 = HandWash
recode HandWash2 (6 =1) (7 8 =0)
label variable HandWash2 "Level of Handwash,1- Every time ; 0 - most of the time"
label define HandWash2l 1" Every time" 0 "Most of the time" 
label value HandWash2 HandWash2l
rename HandWash2 DailyHandwash
***********************
tab WashUtensil
gen Dailywash=.
replace Dailywash=1 if WashUtensil==1
replace Dailywash=0 if Dailywash==.
tab Dailywash
rename Dailywash DailyUtensilsWashing
***********************
tab ToiletFacility
gen typeoftoilet= ToiletFacility==1
rename typeoftoilet FlushToilet
***********************
tab EcoliPresent
gen ecolipresent =.
replace ecolipresent=1 if EcoliPresent==1
replace ecolipresent=0 if EcoliPresent==2
rename ecolipresent Ecoli
*********************
gen Boil=1 if TypeTreatment==1
replace Boil=0 if Boil==.
tab Boil

rename DailyHandwash Handwashing
rename DailyUtensilsWashing UtensilsWashing 



***********************
twoway (scatter HealthRating2 Ecoli) (lfit HealthRating2 Ecoli), by (Location)
twoway (scatter HealthRating2 Ecoli) (lfitci HealthRating2 Ecoli), by ( TreatmentWater )
twoway (scatter noofsickdhr Ecoli) (lfitci noofsickdhr Ecoli), by ( TreatmentWater )


graph bar (mean) HealthRating2, over(Ecoli, relabel(1 "Absence of E.Coli=0" 2 "Presence of E.Coli=1"))  ytitle("Self-reported Health") title("Average Self-reported Health Status") blabel(bar) 
graph bar (mean) noofsickdhr, over(Ecoli, relabel(1 "Absence of E.Coli=0" 2 "Presence of E.Coli=1")) ytitle("No.of Diarrheal Incidennce") title("Average count of HH member affectd by Diarrhea") blabel(bar) 
graph combine g4 g5

graph bar (mean) DeepTubewell FlushToilet Handwashing  UtensilsWashing , over(Ecoli, relabel(1 "Absence of E.Coli=0" 2 "Presence of E.Coli=1"))  ytitle("Percentage of Households") title("The Relation Between E.Coli, Cleanliness Behavior, and Hygiene Infrastructure")blabel(bar)  

estpost tab HealthRating2 Ecoli, chi2
estpost tab  noofsickdhr Ecoli, chi2

esttab using chitest.tex, label replace  ///
   title(Table 1: Test table)

//Summary Statistics
estpost sum HealthRating2 NoSickChildDhr NoSickAdultDhr noofsickdhr healthtrt healthtrt_bad Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Age AgeSq Male Education AssetIndex if Ecoli==1 | Ecoli==0
esttab using summary_ecoli_only307.tex, label cells("mean sd min max") replace  ///
title(Table 1: Summary Statistics table)   
   
   
probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil Age AgeSq Male Education AssetIndex i.(Location)
outreg2 using Ecolimodel1_boil.doc, replace ctitle (Ecoli) keep (DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil Age AgeSq Male Education AssetIndex) addtext( Observations, Fixed effect) 
probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil Age AgeSq Male Education AssetIndex
outreg2 using Ecolimodel1_boil.doc, append ctitle (Ecoli) keep (DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil Age AgeSq Male Education AssetIndex) addtext( Observations, Fixed effect)  

   
probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil Age AgeSq Male Education AssetIndex i.(Location)
eststo margin: margins, dydx (*) post
est sto m1
probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil Age AgeSq Male Education AssetIndex
eststo margin: margins, dydx (*) post
est sto m2
esttab m1 m2 using effect.rtf



 
   
   
//Generating the residuals are first stage with fixed effect****************************************
probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil Age AgeSq Male Education AssetIndex  i.(Location)
predict ecolifeprob
predict probitfexb, xb
gen pdfprobitfe = normalden(probitfexb)
gen cdfprobitfe = normal(probitfexb)
gen IMR= pdfprobitfe/ cdfprobitfe
gen generresidualfe = Ecoli* IMR + (1- Ecoli) * ( pdfprobitfe/(1- cdfprobitfe))
gen generresidualfeSq = generresidualfe^2

*******************************************************************************************************

//Generating the residuals are first stage without fixed effect****************************************
probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil Age AgeSq Male Education AssetIndex
predict ecoliprob
predict probitxb, xb
gen pdfprobit = normalden(probitxb)
gen cdfprobit = normal(probitxb)
gen IMRfe= pdfprobit/ cdfprobit
gen generresidual = Ecoli* IMRfe + (1- Ecoli) * ( pdfprobit/(1- cdfprobit))
gen generresidualSq = generresidual^2 

********************************************************************************************************
//doing the same thing except excluding the age ,agesq amd male for the children************************  

probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil  Education AssetIndex i.(Location)
predict ecoliprobnoage
predict probitxbnoage, xb
gen pdfprobitnoage = normalden(probitxbnoage)
gen cdfprobitnoage = normal(probitxbnoage)
gen IMRfenoage= pdfprobitnoage/ cdfprobitnoage
gen generresidualnoage = Ecoli* IMRfenoage + (1- Ecoli) * ( pdfprobitnoage/(1- cdfprobitnoage))
gen generresidualSqnoage = generresidualnoage^2


********************************************************************************************************
//doing the same thing except excluding the age ,agesq amd male for the children with no fixed effect************************ 


probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil  Education AssetIndex
predict ecoliprobnoagenofe
predict probitxbnoagenofe, xb
gen pdfprobitnoagenofe = normalden(probitxbnoagenofe)
gen cdfprobitnoagenofe = normal(probitxbnoagenofe)
gen IMRfenoagenofe= pdfprobitnoagenofe/ cdfprobitnoagenofe
gen generresidualnoagenofe = Ecoli* IMRfenoagenofe + (1- Ecoli) * ( pdfprobitnoagenofe/(1- cdfprobitnoagenofe))
gen generresidualSqnoagenofe = generresidualnoagenofe^2



//*********************************************************************************************
//***********************Doing both the stage together*****************************************

 bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli 
 outreg2 using Ecolimodel2_boil.doc, replace ctitle (HealthRating2) keep (Ecoli) addtext(Observations) 
 
 bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location)
 outreg2 using Ecolimodel2_boil.doc, append ctitle (HealthRating2) keep (Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe) addtext(Observations) 
 
 bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Ecoli Boil Age AgeSq Male Education AssetIndex generresidual
 outreg2 using Ecolimodel2_boil.doc, append ctitle (HealthRating2) keep (Ecoli Boil Age AgeSq Male Education AssetIndex generresidual) addtext(Observations) 
  
 nbreg NoSickChildDhr Ecoli Boil Education AssetIndex generresidualnoage i.(Location), offset(TotChild) nolog
 outreg2 using Ecolimodel4_boil.doc, replace ctitle (NoSickChildDhr) keep (Ecoli Boil Education AssetIndex generresidualnoage) addtext(Observations) 
 
 nbreg NoSickChildDhr Ecoli Boil Education AssetIndex  generresidualnoagenofe  , offset(lnnoofhhmember) nolog
 outreg2 using Ecolimodel4_boil.doc, append ctitle (NoSickChildDhr) keep (Ecoli Boil Education AssetIndex  generresidualnoagenofe) addtext(Observations) 
 
 nbreg NoSickAdultDhr Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location), offset(TotAdult) nolog
 outreg2 using Ecolimodel4_boil.doc, append ctitle (NoSickAdultDhr) keep (Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe) addtext(Observations)
  
 nbreg NoSickAdultDhr Ecoli Boil Age AgeSq Male Education AssetIndex generresidual  , offset(lnnoofhhmember) nolog
 outreg2 using Ecolimodel4_boil.doc, append ctitle (NoSickAdultDhr) keep (Ecoli Boil Age AgeSq Male Education AssetIndex generresidual) addtext(Observations)
 
 nbreg noofsickdhr  Ecoli Boil Age AgeSq Male Education AssetIndex  generresidualfe  i.(Location), offset(noofhhmember) nolog
 outreg2 using Ecolimodel4_boil.doc, append ctitle (noofsickdhr) keep (Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe) addtext(Observations)
  
 nbreg noofsickdhr  Ecoli Boil Age AgeSq Male Education AssetIndex generresidual  , offset(lnnoofhhmember) nolog
 outreg2 using Ecolimodel4_boil.doc, append ctitle (noofsickdhr) keep (Ecoli Boil Age AgeSq Male Education AssetIndex generresidual) addtext(Observations)


 
//**********************************Marginal effect************************************************
bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location)
eststo margin: margins, dydx (*) predict(pr outcome(1)) post
est sto m3
bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location)
eststo margin: margins, dydx (*) predict(pr outcome(2)) post
est sto m4
bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location)
eststo margin: margins, dydx (*) predict(pr outcome(3)) post
est sto m5
nbreg NoSickChildDhr Ecoli Boil Education AssetIndex generresidualnoage i.(Location), offset(lnnoofhhmember) nolog
eststo margin: margins, dydx (*) 
est sto m6
nbreg NoSickAdultDhr Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location), offset(lnnoofhhmember) nolog
eststo margin: margins, dydx (*) 
est sto m7
nbreg noofsickdhr  Ecoli Boil Age AgeSq Male Education AssetIndex  generresidualfe  i.(Location), offset(lnnoofhhmember) nolog
eststo margin: margins, dydx (*) 
est sto m8
esttab m3 m4 m5 m6 m7 m8 using effect_final.rtf


rename  Self HealthRating2 


bootstrap, reps(100) seed(1):quietly oprobit Self  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location)
margins, at (Ecoli=0) at (Ecoli=1) predict(pr outcome(1)) post 
marginsplot, recast(line) recastci(rarea) xscale(range(0.1 3.1))
bootstrap, reps(100) seed(1):quietly oprobit Self  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location)
margins, at (Ecoli=0) at (Ecoli=1) predict(pr outcome(2)) post 
marginsplot, recast(line) recastci(rarea) xscale(range(0.1 3.1))
bootstrap, reps(100) seed(1):quietly oprobit Self  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location)
margins, at (Ecoli=0) at (Ecoli=1) predict(pr outcome(3)) post 
marginsplot, recast(line) recastci(rarea) xscale(range(0.1 3.1)) 





//**********************************All the results only for siddharthanagar************************************************

probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil Age AgeSq Male Education AssetIndex if Location==1
predict ecolifeprobsid
predict probitfexbsid, xb
gen pdfprobitfesid = normalden(probitfexbsid)
gen cdfprobitfesid = normal(probitfexbsid)
gen IMRsid= pdfprobitfesid/ cdfprobitfesid
gen generresidualfesid = Ecoli* IMRsid + (1- Ecoli) * ( pdfprobitfesid/(1- cdfprobitfesid))
gen generresidualfeSqsid = generresidualfesid^2



probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Boil  Education AssetIndex if Location==1
predict ecoliprobnoagesid
predict probitxbnoagesid, xb
gen pdfprobitnoagesid = normalden(probitxbnoagesid)
gen cdfprobitnoagesid = normal(probitxbnoagesid)
gen IMRfenoagesid= pdfprobitnoagesid/ cdfprobitnoagesid
gen generresidualnoagesid = Ecoli* IMRfenoagesid + (1- Ecoli) * ( pdfprobitnoagesid/(1- cdfprobitnoagesid))
gen generresidualSqnoagesid = generresidualnoagesid^2



 
bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfesid if Location==1
outreg2 using Ecolimodel2_boilsid.doc, replace ctitle (HealthRating2) keep (Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfesid) addtext(Observations) 
 
nbreg NoSickChildDhr Ecoli Boil Education AssetIndex generresidualnoagesid if Location==1, offset(lnnoofhhmember) nolog
outreg2 using Ecolimodel2_boilsid.doc, append ctitle (NoSickChildDhr) keep (Ecoli Boil Education AssetIndex generresidualnoagesid) addtext(Observations) 
 
nbreg NoSickAdultDhr Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfesid if Location==1, offset(lnnoofhhmember) nolog
outreg2 using Ecolimodel2_boilsid.doc, append ctitle (NoSickAdultDhr) keep (Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfesid) addtext(Observations)
  
nbreg noofsickdhr  Ecoli Boil Age AgeSq Male Education AssetIndex  generresidualfesid if Location==1, offset(lnnoofhhmember) nolog
outreg2 using Ecolimodel2_boilsid.doc, append ctitle (noofsickdhr) keep (Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfesid) addtext(Observations)




bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfesid if Location==1
eststo margin: margins, dydx (*) predict(pr outcome(1)) post
est sto m1
 
bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfesid if Location==1
eststo margin: margins, dydx (*) predict(pr outcome(2)) post
est sto m2

bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfesid if Location==1
eststo margin: margins, dydx (*) predict(pr outcome(3)) post
est sto m3

nbreg NoSickChildDhr Ecoli Boil Education AssetIndex generresidualnoagesid if Location==1, offset(lnnoofhhmember) nolog
eststo margin: margins, dydx (*) 
est sto m4
 
nbreg NoSickAdultDhr Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfesid if Location==1, offset(lnnoofhhmember) nolog
eststo margin: margins, dydx (*) 
est sto m5

nbreg noofsickdhr  Ecoli Boil Age AgeSq Male Education AssetIndex  generresidualfesid if Location==1, offset(lnnoofhhmember) nolog
eststo margin: margins, dydx (*) 
est sto m6

esttab m1 m2 m3 m4 m5 m6 using effect_final_sid.rtf
 
 //**********************************Without controlling for endogeneity************************************************
  

bootstrap, reps(100) seed(1):quietly oprobit HealthRating2  Ecoli Boil Age AgeSq Male Education AssetIndex  
outreg2 using Ecolimodel2_boilsid_noendo.doc, replace ctitle (HealthRating2) keep (Ecoli Boil Age AgeSq Male Education AssetIndex ) addtext(Observations) 
 
nbreg NoSickChildDhr Ecoli Boil Education AssetIndex , offset(lnnoofhhmember) nolog
outreg2 using Ecolimodel2_boilsid_noendo.doc, append ctitle (NoSickChildDhr) keep (Ecoli Boil Education AssetIndex ) addtext(Observations) 
 
nbreg NoSickAdultDhr Ecoli Boil Age AgeSq Male Education AssetIndex , offset(lnnoofhhmember) nolog
outreg2 using Ecolimodel2_boilsid_noendo.doc, append ctitle (NoSickAdultDhr) keep (Ecoli Boil Age AgeSq Male Education AssetIndex  ) addtext(Observations)
  
nbreg noofsickdhr  Ecoli Boil Age AgeSq Male Education AssetIndex  , offset(lnnoofhhmember) nolog
outreg2 using Ecolimodel2_boilsid_noendo.doc, append ctitle (noofsickdhr) keep (Ecoli Boil Age AgeSq Male Education AssetIndex  ) addtext(Observations) 





//**********************************Graph Coombine command************************************************
bootstrap, reps(100) seed(1):quietly oprobit Self  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location)
margins, at (Ecoli=0) at (Ecoli=1) predict(pr outcome(1)) post 
marginsplot, recast(line) recastci(rarea) xscale(range(0.1 3.1)) name (g10)
bootstrap, reps(100) seed(1):quietly oprobit Self  Ecoli Boil Age AgeSq Male Education AssetIndex generresidualfe i.(Location)
margins, at (Ecoli=0) at (Ecoli=1) predict(pr outcome(2)) post 
marginsplot, recast(line) recastci(rarea) xscale(range(0.1 3.1)) name (g11)
graph combine g10 g11 














   
probit Ecoli DeepTubewell FlushToilet DailyHandwash  DailyUtensilsWashing  Age AgeSq Male Education AssetIndex TreatmentWater i.(Location)
predict ecolifeprob
predict probitfexb, xb
gen pdfprobitfe = normalden(probitfexb)
gen cdfprobitfe = normal(probitfexb)
gen IMR= pdfprobitfe/ cdfprobitfe
gen generresidualfe = Ecoli* IMR + (1- Ecoli) * ( pdfprobitfe/(1- cdfprobitfe))
gen generresidualfeSq = generresidualfe^2


oprobit HealthRating2  Ecoli AssetIndex Age AgeSq Male Education  generresidualfe TreatmentWater noofhhmember i.(Location)
nbreg NoSickChildDhr Ecoli AssetIndex  Education  generresidualnoage  TreatmentWater noofhhmember i.(Location)
nbreg NoSickAdultDhr Ecoli AssetIndex Age AgeSq Male Education  generresidualfe  TreatmentWater i.(Location)
nbreg noofsickdhr  Ecoli AssetIndex Age AgeSq Male Education   generresidualfe  TreatmentWater i.(Location)

oprobit HealthRating2  Ecoli AssetIndex Age AgeSq Male Education  generresidualfe TreatmentWater noofhhmember if Location==1
nbreg NoSickChildDhr Ecoli AssetIndex  Education  generresidualnoage  TreatmentWater noofhhmember if Location==1
nbreg NoSickAdultDhr Ecoli AssetIndex Age AgeSq Male Education  generresidualfe  TreatmentWater if Location==1
nbreg noofsickdhr  Ecoli AssetIndex Age AgeSq Male Education   generresidualfe  TreatmentWater if Location==1

  
   
   