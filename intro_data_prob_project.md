# Exploring the BRFSS data
Dale Richardson  
`r format(Sys.Date(), "%B %d, %Y")`  

## Setup

### Overview

This document is an Exploratory Data Analysis (EDA) of the 2013 [Behavioral Risk Factor Surveillance System (BRFSS)](https://cdc.gov/brfss) data that has been provided to students of the Coursera course, [Introduction to Probability and Data](https://www.coursera.org/learn/probability-intro/home/info), taught by Duke University. 

The primary objective of the BRFSS is to, "collect uniform, state-specific data on preventive health practices and risk behaviors that are linked to chronic diseases, injuries, and preventable infectious diseases that affect the adult population." 

For more information on the BRFSS, please see the [Centers for Disease Control and Prevention overview](https://www.cdc.gov/brfss/annual_data/2013/pdf/Overview_2013.pdf). 

In this document, I will briefly comment on the data itself (how it was collected and if causality can be inferred), suggest three research questions based on the variables measured in the dataset and lastly perform the exploratory analyses to address the research questions. 

We begin by loading necessary R packages and the BRFSS data itself. 

### Load packages


```r
library(ggplot2)
library(dplyr)
```

### Load data


```r
load("brfss2013.RData")
```



* * *

## Part 1: Data

As noted above, the data analyzed here were obtained from the 2013 Behavioral Risk Factor Surveillance System (BRFSS) survey, administered by the Centers for Disease Control's Population Health Surveillance Branch. The data are based on a population of non-institutionalized adults residing in all 50 states of the USA and its territories, who are at least 18 years old and who have a landline or cellular telephone. Results based on these data are **NOT** generalizable beyond the aforementioned population from which the samples were obtained, e.g. not to a population under 18 years of age, nor to those living outside of the USA and its territories. 

These data were collected by using a disproportionate stratified sampling design in 51 projects, whereas only simple random sampling was employed in Guam and Puerto Rico. Disproportionate stratified sampling was used in order to ensure that enough members of underrepresented groups would be included in the samples. For more details, please see page 6 of the [BRFSS overview](https://www.cdc.gov/brfss/annual_data/2013/pdf/Overview_2013.pdf).

These data comprise an in-depth observational study, without random assignment of subjects into experimental/control groups. Therefore, it is **NOT** possible to infer causality based on these data. 

With any survey, it is important to consider sources of potential bias. One potential bias that should be considered is non-response. For example, perhaps very busy people are underrepresented, or people who work nights and sleep during the day, thus would be absent during the times of the phone calls. 

Additionally, there may be an inherent bias between individuals/households who primarily use cell phones instead of landlines, i.e. perhaps people who only use cell phones are younger and more mobile whereas those who maintain landlines spend more time at home. Another potential source of bias could arise from the demeanor of the interviewer conducting the survey over the telephone. If the interviewer's performance was sub-standard or somehow offputting, this could affect the responsiveness of the participant. 


* * *

## Part 2: Research questions

**Research quesion 1:**

The first question I have is the following: 

Of the 50 states in the U.S.A, which have the highest proportion of college graduates? Or, in other words, is the distribution of college graduates uniform across the USA? The variables of interest here are, `education level` and `state of residence`. The question could be considered interesting from the standpoint of subsetting the U.S population by education level and then looking at other indicators of health -- moving towards to the hypothesis that having a higher education results in a healthier populace.

**Research quesion 2:**

The second question I have come up with is the following:

How does marital status and employment status correlate with the incidence of having been told that one has major depression? In other words, are retired people more happy than employed people, and does this happiness change depending on if one is married or divorced? The variables of interest here are `employment status`, `marital status` and `depression status`.


**Research quesion 3:**

The final question is the following:

Do people who exercise regularly have better general health, and how does education level relate to the tendency to exercise regularly? In other words, do people with college degrees exercise more than those who do not and consequently, have better overall health? 

The variables of interest here are `general health`, `exercise level` and `education level`. Once again, the underlying hypothesis is that having a higher education moves one towards increased physical activity, which improves overall health. 

* * *

## Part 3: Exploratory data analysis

Here you will find the relevant `R` code and descriptions on how I have processed the `brfss2013` data set to answer my research questions. 

NOTE: Insert code chunks as needed by clicking on the "Insert a new code chunk" 
button (green button with orange arrow) above. Make sure that your code is visible
in the project you submit. Delete this note when before you submit your work.

**Research quesion 1:**

Of the 50 states in the U.S.A, which have the highest proportion of college graduates? To answer this question, I will filter the `brfss2013` data by the variable `educa` and then create a choropleth plot of the relative frequencies of college graduate by state. 



```r
## First subset the dataframe to obtain only the state column and the educa column
collegeGradsbyState <- brfss2013 %>% select(X_state, educa)

## group by state and education level, get total number of respondents per state
## and calculate relative frequency of each education level
collegeGradsbyState <- collegeGradsbyState %>% group_by(X_state, educa) %>%
        summarise(total = n()) %>% 
        mutate(freq = total / sum(total))

## filter rows to get frequencies of college graduates only, select only state and freq,
## rename to region and value, transform to lowercase and remove Guam + Puerto Rico for choroplethr plotting
forPlotting <- collegeGradsbyState %>% 
        filter(educa == "College 4 years or more (College graduate)") %>% 
        mutate(region = X_state, value = freq) %>% 
        ungroup() %>% 
        select(region, value) %>% 
        mutate_each(funs(tolower), matches("region")) %>%
        filter(region != "guam") %>%
        filter(region != "puerto rico")

## Show the top ten states with highest relative frequency of College graduates
library(knitr)
top10 <- head(
        arrange(forPlotting, desc(value)),10)

kable(top10, caption = "Top 10 States with highest relative frequency of College Graduates",
      align = c("c", "r"))
```



Table: Top 10 States with highest relative frequency of College Graduates

        region               value
----------------------  ----------
 district of columbia    0.6240114
     connecticut         0.4373541
    massachusetts        0.4328843
       colorado          0.4292622
       maryland          0.4272539
       new york          0.4195345
      minnesota          0.4116457
       vermont           0.4114518
      washington         0.4075435
      california         0.3983330

```r
## Show the bottom 10 states
bot10 <-head(
        arrange(forPlotting, value),10)

kable(bot10, caption = "Top 10 States with lowest relative frequency of College Graduates",
      align = c("c", "r"))        
```



Table: Top 10 States with lowest relative frequency of College Graduates

    region            value
---------------  ----------
 west virginia    0.2446177
   arkansas       0.2634776
   kentucky       0.2638595
    alabama       0.2650269
   tennessee      0.2720550
  mississippi     0.2739836
   missouri       0.2829446
    indiana       0.2854517
   oklahoma       0.2908782
   nebraska       0.2920240

```r
## load choroplethr and choroplethrMaps
library(choroplethr)
library(choroplethrMaps)

state_choropleth(forPlotting,
                 title      = "2013 Estimates of College Graduates (4 years or more)",
                 legend     = "Relative frequency by State",
                 num_colors = 4)
```

![](intro_data_prob_project_files/figure-html/question-1-1.png)<!-- -->

```r
## detach packages to avoid conflicts with dplyr
detach("package:choroplethr", unload = TRUE)
detach("package:choroplethrMaps", unload = TRUE)
detach("package:acs", unload = TRUE)
detach("package:plyr", unload = TRUE)
```

The distribution of college graduates (4 years or more) is **not uniform** across the U.S.A. Interestingly, the *highest proportion of graduates reside in the District of Columbia*, while the *lowest proportion of degree holders reside in West Virginia*. Perhaps this high number of college degree holders in the District of Columbia can be attributed to the high concentration of politics and government-related careers in DC. Generally, the Northeast and West Coast of the United States are home to a higher proportion of 4-year college graduates, along with states like Colorado and Minnesota. The states with the lowest percentages of college graduates are the southern states, along with some mid-western states such as Nebraska and Oklahoma.  

**Research quesion 2:**

How does marital status and employment status correlate with the incidence of having been told that one has major depression? In other words, are retired people more happy than employed people, and does this happiness change depending on if one is married or divorced? 

To answer this question, I will use the variables `addepev2`, `employ1` and `marital`. `addepev2`corresponds to whether or not the participant had been told he or she has major depressive disorder. I will filter the brfss2013 dataframe to pull out only those cases where people are either `Employed for wages` or have been/are `Divorced`, `Married` or `Widowed`.



```r
## subset the original dataframe to grab only the variables of interest
depressedorNot <- brfss2013 %>% select(addepev2, employ1, marital) %>%
        filter(employ1 == "Employed for wages" | employ1 == "Retired") %>%
        filter(marital == "Divorced" | marital == "Married" | marital == "Widowed") %>%
        filter(!is.na(addepev2))

## group by each variable
depressedorNotGrouped <- depressedorNot %>% group_by(addepev2, employ1, marital)

## create summary of counts and relative frequencies for each class
forPlotting <- depressedorNotGrouped %>%
        summarise(n = n()) %>%
        mutate(freq = n / sum(n))

## Show the table
kable(forPlotting, caption = "Table of counts and relative frequencies of Major Depression based on Marital and Employment status", align = c("c", "r")) 
```



Table: Table of counts and relative frequencies of Major Depression based on Marital and Employment status

 addepev2               employ1   marital          n     freq    
----------  -------------------  ----------  -------  -----------
   Yes       Employed for wages   Married      15031   0.6588787 
   Yes       Employed for wages   Divorced      6348   0.2782624 
   Yes       Employed for wages   Widowed       1434   0.0628589 
   Yes                  Retired   Married       8959   0.4491853 
   Yes                  Retired   Divorced      4297   0.2154425 
   Yes                  Retired   Widowed       6689   0.3353723 
    No       Employed for wages   Married     100415   0.7799344 
    No       Employed for wages   Divorced     21905   0.1701386 
    No       Employed for wages   Widowed       6428   0.0499270 
    No                  Retired   Married      56818   0.5297617 
    No                  Retired   Divorced     14725   0.1372935 
    No                  Retired   Widowed      35709   0.3329448 

```r
## plot relative frequencies with a stacked bar plot
ggplot(forPlotting,aes(x = addepev2, y = freq,fill = marital)) + 
    geom_bar(position = "fill",stat = "identity") + facet_wrap(~employ1) +
        ggtitle("Occurrence of Major Depression based on Marital and Employment status") +
        xlab("Major Depression") +
        ylab("Relative frequency") +
        theme_bw()
```

![](intro_data_prob_project_files/figure-html/question-2-1.png)<!-- -->

The above plot suggests a few interesting trends. For one, those who currently employed for wages have a higher proportion of people who are married, irrespective of depressive status than those who are retired. This could be explained in part by the higher proportion of widowers in the Retired group. It is quite interesting to note no difference in the proportion of major depression in the retired widowers. One would assume that the incidence of depression would be higher in those cases where one has lost their spouse, but we do not see this reflected here. Unsurprisingly, the fraction of those people who have been told they have or have had major depression is higher in the divorced groups, regardless of employment status. Lastly, and also unsurprisingly, married individuals are less likely to have been told they have or have had major depression, irrespective of employment status.


**Research quesion 3:**

Do people who exercise regularly have better general health, and how does education level relate to the tendency to exercise regularly? In other words, do people with college degrees exercise more than those who do not and consequently, have better overall health?

To answer this question, I will once again subset the `brfss2013` dataframe and pull out only the relevant variables "needed" to address this question. From the codebook, I will use the following variables: 

- `genhlth`: general health status
- `exerany2`: exercise In Past 30 Days
- `educa`: education Level


```r
## subset the original dataframe to grab only the variables of interest, remove all NAs
genHealth <- brfss2013 %>% select(genhlth, exerany2, educa) %>%
        na.omit()

## create new variable to denote college degree or not
genHealth <- genHealth %>% 
        mutate(degree = ifelse(educa == "College 4 years or more (College graduate)", "Yes", "No")) %>% 
        select(genhlth, exerany2, degree) 


## group by each variable
genHealthGrouped <- genHealth %>% group_by(genhlth, exerany2, degree)

## create summary of counts and relative frequencies for each class
forPlotting <- genHealthGrouped %>%
        summarise(n = n()) %>%
        mutate(freq = n / sum(n))

## Show the table
kable(forPlotting, caption = "Table of counts and relative frequencies of General Health status based on Exercise in past 30 days and and Education level", align = c("c", "r")) 
```



Table: Table of counts and relative frequencies of General Health status based on Exercise in past 30 days and and Education level

  genhlth     exerany2   degree        n     freq    
-----------  ---------  --------  ------  -----------
 Excellent         Yes     No      30933   0.4567240 
 Excellent         Yes    Yes      36795   0.5432760 
 Excellent          No     No       7789   0.6830659 
 Excellent          No    Yes       3614   0.3169341 
 Very good         Yes     No      64419   0.5351838 
 Very good         Yes    Yes      55949   0.4648162 
 Very good          No     No      20358   0.7070960 
 Very good          No    Yes       8433   0.2929040 
   Good            Yes     No      65011   0.6713169 
   Good            Yes    Yes      31830   0.3286831 
   Good             No     No      33179   0.7837621 
   Good             No    Yes       9154   0.2162379 
   Fair            Yes     No      27515   0.7916619 
   Fair            Yes    Yes       7241   0.2083381 
   Fair             No     No      22738   0.8464746 
   Fair             No    Yes       4124   0.1535254 
   Poor            Yes     No       9118   0.8360535 
   Poor            Yes    Yes       1788   0.1639465 
   Poor             No     No      12967   0.8760895 
   Poor             No    Yes       1834   0.1239105 

```r
## plot relative frequencies with a stacked bar plot
ggplot(forPlotting,aes(x = genhlth, y = freq,fill = degree)) + 
    geom_bar(position = "fill",stat = "identity") + facet_grid(~exerany2) +
        ggtitle("General health status based on exercise in past 30 days and education level\n
                Exercised in past 30 days?") +
        xlab("General health status") +
        ylab("Relative frequency") +
        guides(fill = guide_legend(reverse=TRUE)) +
        scale_fill_discrete(name="College degree") +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](intro_data_prob_project_files/figure-html/question-3-1.png)<!-- -->

Here we can see an interesting trend, those who have exercised in the past 30 days and have excellent health consist of a higher proportion of college degree holders than those who have not exercised in the past 30 days but also consider themselves to be of excellent health. Does this mean that college degree holders are more likely to exercise and thus be of better health status?

Furthermore, of the group who did not exercise in the past 30 days (right panel of figure), there appear to be generally lower proportions of college degree holders. Considering the general health status of "Poor", there seems to be a disproportionate amount of non-degree holders in this category, irrespective of exercise in the past 30 days. Does it make sense that those who have not obtained a college degree consider themselves to be of poorer health? 

Unfortunately, diving deeper into this data will have to wait for another day. 