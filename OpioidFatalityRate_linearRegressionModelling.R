#### Script to identify a linear regression-based model of opioid fatalities 
#### at the county-level for the state of Texas

install.packages('corrplot', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('AER', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(corrplot)
library(AER)
library(MASS)
library(MuMIn)

#### Step 1. Import dataset

setwd("//DSHS4SVCHSFS01.dshs.txnet.state.tx.us/CHS Share/CHS-GIS/Projects/Opioids/GIS_Project/data/FinalDataset")

fullVars <- read.csv("ExplanatoryVariables_CountyLevel.csv")
View(fullVars)
#### Step 2. Reduce to just the variables of interest

### this has a lot of extraneous variables, so let's cut it down to just a handful that we're interested in
### list out those variables
subVars <- c("opd19_21", "E_TOTPOP", "GiniCoefficient", "EP_POV150", "IncPerC", "EP_NOHSDP", "FemaleHH", "EP_UNEMP", "EP_HISP", "EP_NHW", "EP_AFAM", "EP_UNINSUR",
             "EP_DISABL", "EP_MOBILE", "EP_NOINT", "EP_MINRTY", "EP_GROUPQ", "EP_HBURD", "EP_SNGPNT", "EP_MUNIT", "EP_NOVEH", "EP_AGE65", "EP_AGE17",
             "McaidPct", "McarePct", "PctRiskOccup")
### drop everything but the ones we just listed
subData <- fullVars[subVars]

#### Step 3. Plot correlation matrix visualization
correlation <- cor(subData)
### This summarizes how correlated each variable is with all of its fellow variables
corrplot(correlation, method="circle")

#### Step 4. We want to know what kind of model to use based on the distribution of
#### the response variable (counts of deaths)

plot(density(fullVars$opd19_21))
plot(hist(fullVars$opd19_21))
mean(fullVars$opd19_21)
### average counts is 24.6

var(fullVars$opd19_21)
### variance of counts is 15339.9


#### Step 5. Create a full generalized linear regression model
### We're going to use the full variables dataset so we have access to the county names (non-numerical variable)

fullModel <- glm(formula = opd19_21 ~ E_TOTPOP + GiniCoefficient + EP_POV150 + IncPerC + EP_NOHSDP + FemaleHH + EP_UNEMP + EP_HISP + EP_NHW + + EP_AFAM + EP_UNINSUR +
    EP_DISABL + EP_MOBILE + EP_NOINT + EP_MINRTY + EP_GROUPQ + EP_HBURD + EP_SNGPNT + EP_MUNIT + EP_NOVEH + EP_AGE65 + EP_AGE17 + McaidPct + McarePct + PctRiskOccup, data = fullVars, family = "poisson")

summary(fullModel)

### Step 6. Test for over-dispersion indicates variance is greater than mean
dispersiontest((fullModel))

#### Based on the test for over-dispersion, using a negative binomial model
nbModel <- glm.nb(formula = opd19_21 ~ + GiniCoefficient + 
                    EP_POV150 + IncPerC + EP_NOHSDP + EP_CROWD + FemaleHH + 
                    EP_UNEMP + EP_HISP + EP_NHW + + EP_AFAM + EP_UNINSUR + 
                    EP_DISABL + EP_MOBILE + EP_NOINT + EP_MINRTY + EP_GROUPQ + 
                    EP_HBURD + EP_SNGPNT + EP_MUNIT + EP_NOVEH + EP_AGE65 + EP_AGE17 + 
                    McaidPct + McarePct + PctRiskOccup + synEDpct + opEDrt19_21 + offset(log(E_TOTPOP)), data = fullVars)

summary(nbModel)

sapply(fullVars, class)
### Step 7. Run a univariate models for each of the variables being considered (keeping the population estimate as an offset)

Gini <- glm.nb(formula = opd19_21 ~ GiniCoefficient + offset(log(E_TOTPOP)), data = fullVars)
summary(Gini)
confint(Gini, 'GiniCoefficient', level=0.90)

pov <- glm.nb(formula = opd19_21 ~ EP_POV150 + offset(log(E_TOTPOP)), data = fullVars)
summary(pov)
confint(pov, 'EP_POV150', level=0.90)

income <- glm.nb(formula = opd19_21 ~ IncPerC + offset(log(E_TOTPOP)), data = fullVars)
summary(income)
confint(income, 'IncPerC', level=0.90)

noHS <- glm.nb(formula = opd19_21 ~ EP_NOHSDP + offset(log(E_TOTPOP)), data = fullVars)
summary(noHS)
confint(noHS, 'EP_NOHSDP', level=0.90)

crowd <- glm.nb(formula = opd19_21 ~ EP_CROWD + offset(log(E_TOTPOP)), data = fullVars)
summary(crowd)
confint(crowd, 'EP_CROWD', level=0.90)

femHH <- glm.nb(formula = opd19_21 ~ FemaleHH + offset(log(E_TOTPOP)), data = fullVars)
summary(femHH)
confint(femHH, 'FemaleHH', level=0.90)


unemp <- glm.nb(formula = opd19_21 ~ EP_UNEMP + offset(log(E_TOTPOP)), data = fullVars)
summary(unemp)
confint(unemp, 'EP_UNEMP', level=0.90)

hisp <- glm.nb(formula = opd19_21 ~ EP_HISP + offset(log(E_TOTPOP)), data = fullVars)
summary(hisp)
confint(hisp, 'EP_HISP', level=0.90)


white <- glm.nb(formula = opd19_21 ~ EP_NHW + offset(log(E_TOTPOP)), data = fullVars)
summary(white)
confint(white, 'EP_NHW', level=0.90)


afam <- glm.nb(formula = opd19_21 ~ EP_AFAM + offset(log(E_TOTPOP)), data = fullVars)
summary(afam)
confint(afam, 'EP_AFAM', level=0.90)


uninsured <- glm.nb(formula = opd19_21 ~ EP_UNINSUR + offset(log(E_TOTPOP)), data = fullVars)
summary(uninsured)
confint(uninsured, 'EP_UNINSUR', level=0.90)

disability <- glm.nb(formula = opd19_21 ~ EP_DISABL + offset(log(E_TOTPOP)), data = fullVars)
summary(disability)
confint(disability, 'EP_DISABL', level=0.90)

mobile <- glm.nb(formula = opd19_21 ~ EP_MOBILE + offset(log(E_TOTPOP)), data = fullVars)
summary(mobile)
confint(mobile, 'EP_MOBILE', level=0.90)


noint <- glm.nb(formula = opd19_21 ~ EP_NOINT + offset(log(E_TOTPOP)), data = fullVars)
summary(noint)
confint(noint, 'EP_NOINT', level=0.90)


groupq <- glm.nb(formula = opd19_21 ~ EP_GROUPQ + offset(log(E_TOTPOP)), data = fullVars)
summary(groupq)
confint(groupq, 'EP_GROUPQ', level=0.90)

housingBurd <- glm.nb(formula = opd19_21 ~ EP_HBURD + offset(log(E_TOTPOP)), data = fullVars)
summary(housingBurd)
confint(housingBurd, 'EP_HBURD', level=0.90)


singleparent <- glm.nb(formula = opd19_21 ~ EP_SNGPNT + offset(log(E_TOTPOP)), data = fullVars)
summary(singleparent)
confint(singleparent, 'EP_SNGPNT', level=0.90)

multiUnit <- glm.nb(formula = opd19_21 ~ EP_MUNIT + offset(log(E_TOTPOP)), data = fullVars)
summary(multiUnit)
confint(multiUnit, 'EP_MUNIT', level=0.90)


noVehicle <- glm.nb(formula = opd19_21 ~ EP_NOVEH + offset(log(E_TOTPOP)), data = fullVars)
summary(noVehicle)
confint(noVehicle, 'EP_NOVEH', level=0.90)

age65 <- glm.nb(formula = opd19_21 ~ EP_AGE65 + offset(log(E_TOTPOP)), data = fullVars)
summary(age65)
confint(age65, 'EP_AGE65', level=0.90)


age18 <- glm.nb(formula = opd19_21 ~ EP_AGE17 + offset(log(E_TOTPOP)), data = fullVars)
summary(age18)
confint(age18, 'EP_AGE17', level=0.90)

mcaid <- glm.nb(formula = opd19_21 ~ McaidPct + offset(log(E_TOTPOP)), data = fullVars)
summary(mcaid)
confint(mcaid, 'McaidPct', level=0.90)

mcare <- glm.nb(formula = opd19_21 ~ McarePct + offset(log(E_TOTPOP)), data = fullVars)
summary(mcare)

risky <- glm.nb(formula = opd19_21 ~ PctRiskOccup + offset(log(E_TOTPOP)), data = fullVars)
summary(risky)

ED_rate <- glm.nb(formula = opd19_21 ~ opEDrt19_21 + offset(log(E_TOTPOP)), data = fullVars)
summary(ED_rate)
confint(ED_rate, 'opEDrt19_21', level=0.90)


### Step 8. Finally, use a model search (dredge) function to find the best combination of variables
### based on AIC
nbModel <- glm.nb(formula = opd19_21 ~ + GiniCoefficient + 
                    EP_POV150 + IncPerC + EP_NOHSDP + EP_CROWD + FemaleHH + 
                    EP_UNEMP + EP_HISP + EP_NHW + + EP_AFAM + EP_UNINSUR + 
                    EP_DISABL + EP_MOBILE + EP_NOINT + EP_MINRTY + EP_GROUPQ + 
                    EP_HBURD + EP_SNGPNT + EP_MUNIT + EP_NOVEH + EP_AGE65 + EP_AGE17 + 
                    McaidPct + McarePct + PctRiskOccup + offset(log(E_TOTPOP)), data = fullVars, na.action = "na.fail")


modelSearch <- dredge(nbModel, beta = "none", rank = "AICc", fixed=offset(log(E_TOTPOP)))
topModel <- modelSearch[1]

