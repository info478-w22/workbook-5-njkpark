# Workbook 6: analyze NHANES data

# Set up
library(survey)
library(Hmisc)

demo <- sasxport.get('DEMO_I.XPT')
alc <- sasxport.get('ALQ_I.XPT')

#joining datasets together by the identifying characteristic 

nhanes <- merge(x = demo, y = alc, by = 'seqn', all = TRUE)

#sample weight variable, take sum and interpret the result 
wt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)
#samples weights are relative to the total US population. This represents the overall US population. 

## ANALYSIS

#calculate survey weighted percentage
# in alq151, we want 2 to be 0, and we want to ignore or disregard 7 and 9

#change 2's to 0
nhanes$alq151[nhanes$alq151 == 2] <- 0
#ignore 7 and 9
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA

#create survey design
nhanes_survey <- svydesign(
  id = ~sdmvpsu, 
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)

#svymean
nhanes_mean <- svymean(~alq151, nhanes_survey, na.rm=FALSE)

mean_by_gender <- svyby(~alq151, ~riagendr, nhanes_survey, svymean)
