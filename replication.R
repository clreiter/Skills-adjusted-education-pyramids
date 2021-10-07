#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# Inequality in Quality: Population Heterogeneity in Literacy Skills around the World
# Author: Claudia Reiter
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# REPLICATION OF RESULTS
#-------------------------------------------------------------------------------------

# clear workspace
rm(list = ls(all.names = TRUE)) 

# set working directory
setwd("C:/Users/creiter/Dropbox/Claudia/PIAAC/Demographic Research/Replication")


# load packages
library(intsvy)
library(tidyverse)
library(wcde)
library(readstata13)
library(BIFIEsurvey)
library(reshape2)
library(ggpattern)
library(extrafont)

loadfonts()

# load PIAAC data
austria <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgautp1.csv")
belgium <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgbelp1.csv")
canada <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgcanp1.csv")
chile <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgchlp1.csv")
czechia <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgczep1.csv")
germany <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgdeup1.csv")
denmark <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgdnkp1.csv")
ecuador <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgecup1.csv")
spain <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgespp1.csv")
estonia <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgestp1.csv")
finland <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgfinp1.csv")
france <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgfrap1.csv")
uk <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prggbrp1.csv")
greece <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prggrcp1.csv")
hungary <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prghunp1.csv")
ireland <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgirlp1.csv")
israel <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgisrp1.csv")
italy <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgitap1.csv")
japan <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgjpnp1.csv")
kazakhstan <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgkazp1.csv")
south_korea <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgkorp1.csv")
lithuania <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgltup1.csv")
mexico <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgmexp1.csv")
netherlands <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgnldp1.csv")
norway <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgnorp1.csv")
new_zealand <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgnzlp1.csv")
peru <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgperp1.csv")
poland <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgpolp1.csv")
russia <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgrusp1.csv")
singapore <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgsgpp1.csv")
slovakia <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgsvkp1.csv")
slovenia <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgsvnp1.csv")
sweden <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgswep1.csv")
turkey <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgturp1.csv")
usa <- read.csv("https://webfs.oecd.org/piaac/puf-data/CSV/prgusap1_2012.csv")

piaac <- rbind (austria, belgium, canada, chile, czechia, germany, denmark, 
                ecuador, spain, estonia, finland, france, uk, greece, 
                hungary, ireland, israel, italy, japan, kazakhstan, south_korea, 
                lithuania, mexico, netherlands, norway, new_zealand, peru, poland, 
                russia, singapore, slovakia, slovenia, sweden, turkey, usa)

#---------------------------------------------------------------------------------------------------------------------------------------------
#Note: Australian PIAAC data is not publicly available, hence excluded from the replication code.
#      Replicated results for OECD mean PIAAC literacy scores are therefore slighly different from the numbers presented in the paper (Table 1)
#---------------------------------------------------------------------------------------------------------------------------------------------

# STEP 1: CALCULATE POPULATION-WEIGHTED OECD MEAN LITERACY SCORE BY AGE, SEX, EDUCATION

# define education categories
piaac_educ <- piaac %>%
  mutate(educ = ifelse (EDCAT7 == 1, 1, 0),                                                                    #educ 1 = Primary or less
         educ = ifelse (EDCAT7 == 2, 2, educ),                                                                 #educ 2 = Lower secondary
         educ = ifelse (EDCAT7 == 3, 3, educ),                                                                 #educ 3 = Upper secondary
         educ = ifelse (EDCAT7 == 4 | EDCAT7 == 5 | EDCAT7 == 6 | EDCAT7 == 7 | EDCAT7 == 8, 4, educ)) %>%     #educ 4 = Post secondary
  filter (educ > 0, GENDER_R==1 | GENDER_R==2)


# calculate mean by country
mean_piaac_educ <- piaac_educ %>%
  piaac.mean.pv(pvlabel = "LIT", by=c("AGEG5LFS", "GENDER_R", "CNTRYID", "educ")) %>%
  rename(age = AGEG5LFS, sex = GENDER_R, iso = CNTRYID, mean_country = Mean, n = Freq) %>%
  mutate(age = as.numeric(as.factor(age)),
         age = (age*5) + 10)

# add missing values (country-age-sex-education categories with too small sample sizes)
missing_values <- read.csv("Input/PIAAC-missing-values_age_sex_educ.csv")

mean_piaac_educ <- mean_piaac_educ %>%
  rbind(missing_values)%>%
  mutate(iso = as.numeric(as.character(iso)),
         sex = as.numeric(as.factor(sex)),
         educ = as.numeric(as.factor(educ)))


# fill missing values with data of subsequent education groups (same country-age-sex-group) for education categories 1, 2, 3
# and fill with data from lower education groups for education category 4

mean_piaac_educ <- mean_piaac_educ [with(mean_piaac_educ, order(iso, age, sex, educ)),]

mean_piaac_educ <- mean_piaac_educ %>%
  group_by(iso, age, sex) %>%
  mutate(mean_country = ifelse(is.na(mean_country),
                               imputeTS::na_locf(mean_country, option = "nocb", na_remaining="keep"),
                               mean_country)) %>%
  mutate(mean_country = ifelse(is.na(mean_country),
                               imputeTS::na_locf(mean_country_new,  na_remaining="keep"),
                               mean_country))

# filter OECD countries
mean_piaac_oecd <- mean_piaac_educ %>%
  filter(iso!=196,       #remove Cyprus
         iso!=643,       #remove Russia 
         iso!=702,       #remove Singapore
         iso!=604,       #remove Peru
         iso!=398,       #remove Kazakhstan
         iso!=218)%>%    #remove Ecuador
  select(iso, age, sex, educ, mean_country)


# load population data from WIC Human Capital Data Explorer 
pop <- get_wcde(indicator =  "epop") %>%
  filter(scenario==2, year == 2015, sex!="Both", age!="All", age!="100+", age!="0--4", age!="5--9") %>%
  mutate (educ = ifelse (education == "No Education" | education == "Incomplete Primary" | education == "Primary", 1, 0),
          educ = ifelse (education == "Lower Secondary", 2, educ),
          educ = ifelse (education == "Upper Secondary", 3, educ),
          educ = ifelse (education == "Post Secondary", 4, educ)) %>%
  rename(iso = country_code, country=name) %>%
  mutate(age=as.numeric(as.character(str_sub(age, 1, 2))),
         sex=ifelse(sex=="Male", 1, 2)) %>%
  filter (educ > 0, age>10, age<65, iso<900) %>%
  group_by(iso, country, age, sex, educ) %>%
  summarise(epop=sum(epop), .groups="keep") %>%
  select(iso, country, age, sex, educ, epop) 
  

#calculate population-weighted OECD Mean (--> Table 1)
mean_piaac_oecd <- mean_piaac_oecd %>%
  merge(pop) %>%
  group_by(age, sex, educ) %>%
  mutate(share=epop/sum(epop)) %>%
  filter(share!="NaN") %>%
  mutate(weighted_mean = mean_country * share) %>%
  group_by(age, sex, educ) %>%
  summarise(oecd_mean=sum(weighted_mean), .groups="keep")

#create table
table1 <- spread(mean_piaac_oecd, educ, oecd_mean) %>%
  rename ("Primary or less" = "1", "Lower secondary" = "2", "Upper secondary" = "3", "Post secondary" = "4") %>%
  mutate(sex = ifelse(sex==1, "Men", "Women")) %>%
  arrange(sex)

dir.create("Output")
write.csv(table1, "Output/table1.csv", row.names=FALSE)

#---------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------

# STEP 2: ESTIMATE POPULATION DISTRIBUTION BY AGE, SEX AND SKILLS-ADJUSTED EDUCATIONAL ATTAINMENT FOR 45 COUNTRIES


#---------------------------------------------------------------------------------------------------------------------------------------------
# a) PIAAC countries

piaac_loop<-plyr::rename(piaac_educ, c("AGEG5LFS"="age", "GENDER_R"="sex", "CNTRYID"="iso"))
mean_piaac_oecd_loop <- mean_piaac_oecd %>%
  mutate(age = (age/5)-2)


# loop through all age-sex-education groups to calculate share below and above OECD average
for (i in 1:10)
{for (j in 1:2)
{for (k in 1:4)
{loop<-subset(piaac_loop, age==i)
loop<-subset(loop, sex==j)
loop<-subset(loop, educ==k)
oecd_average<-subset(mean_piaac_oecd_loop, age==i)
oecd_average<-subset(oecd_average,sex==j)
oecd_average<-subset(oecd_average,educ==k)
loop<-piaac.ben.pv(pvlabel="LIT", by=c("iso", "age", "sex", "educ"), data=loop, cutoff=c(0,oecd_average[1,4]))
write.csv(loop, file = paste("Output/loop_",i,j,k, ".csv"), row.names=FALSE)
}}}


# combine results
dir<-("Output/")
filePaths <- list.files(dir, "\\.csv$", full.names = TRUE)
piaac_results <- do.call(rbind, lapply(filePaths, read.csv))
do.call(rbind, lapply(filePaths, file.remove))

piaac_results <- piaac_results %>%
  filter(Benchmarks!="Below/equal to 0")
  
piaac_results <- piaac_results %>%
  mutate(Benchmarks = gsub("\\d+", "OECD average", Benchmarks),
         Benchmarks = gsub("greater than OECD average to less/equal than OECD average.OECD average", "less/equal than OECD average", Benchmarks),
         Benchmarks = gsub("Above OECD average.OECD average", "above OECD average", Benchmarks)) %>%
  filter(Benchmarks!="Above NA", Benchmarks!="greater than OECD average to less/equal than NA")

     
piaac_results_wide <- tidyr::spread(piaac_results, key = Benchmarks , value = Percentage) %>%
  rename("above_OECD_avg" = "above OECD average", "below_OECD_avg" = "less/equal than OECD average") %>%
  mutate(above_OECD_avg=ifelse(is.na(above_OECD_avg), (100 - below_OECD_avg), above_OECD_avg ),
         age = (age+2)*5) %>%
  filter(below_OECD_avg!="NA")  %>%
  select(-Std..err.)
 
# add missing values (country-age-sex-education categories with too small sample sizes) and assume a 50-50 split
missing_shares_piaac<-read.csv("Input/PIAAC-missing-shares_age_sex_educ.csv")
piaac_results_wide <- piaac_results_wide %>%
  rbind(missing_shares_piaac) %>%
  mutate (above_OECD_avg=ifelse(is.na(above_OECD_avg), 50, above_OECD_avg),
          below_OECD_avg=ifelse(is.na(below_OECD_avg), 50, below_OECD_avg))


#---------------------------------------------------------------------------------------------------------------------------------------------
# b) STEP countries
#    (Data can be downloaded from the World Bank Microdata Library: https://microdata.worldbank.org/index.php/catalog/step)

armenia <- read.dta13("/Users/creiter/Dropbox/Claudia/PIAAC/STEP/Data/stata/STEP Armenia_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(iso = 51) 

bolivia <- read.dta13("/Users/creiter/Dropbox/Claudia/PIAAC/STEP/Data/stata/STEP Bolivia_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(iso = 68)

colombia <- read.dta13("/Users/creiter/Dropbox/Claudia/PIAAC/STEP/Data/stata/STEP Colombia_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(iso = 170)

georgia <- read.dta13("/Users/creiter/Dropbox/Claudia/PIAAC/STEP/Data/stata/STEP Georgia_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(iso = 268)

ghana <- read.dta13("/Users/creiter/Dropbox/Claudia/PIAAC/STEP/Data/stata/STEP Ghana_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(iso = 288)

kenya <- read.dta13("/Users/creiter/Dropbox/Claudia/PIAAC/STEP/Data/stata/STEP Kenya_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(iso = 404)

ukraine <- read.dta13("/Users/creiter/Dropbox/Claudia/PIAAC/STEP/Data/stata/STEP Ukraine_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(iso = 804)

vietnam <- read.dta13("/Users/creiter/Dropbox/Claudia/PIAAC/STEP/Data/stata/STEP Vietnam_working.dta", convert.factors=FALSE)  %>%
  select(PVLIT1, PVLIT2, PVLIT3, PVLIT4, PVLIT5, PVLIT6, PVLIT7, PVLIT8, PVLIT9, PVLIT10, age, m1a_q09, W_FinSPwt, gender) %>%
  mutate(iso = 704)

#calculate share above and below OECD average by age, sex, education for STEP countries
step <- rbind(armenia, bolivia, colombia, georgia, ghana, kenya, ukraine, vietnam) %>%
   mutate(sex = ifelse(gender ==0, 1, 0),
          sex = ifelse(gender ==1, 2, sex),
          age5 = cut(age, breaks=c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65), right = FALSE),
          age=as.numeric(as.character(str_sub(age5, 2, 3))),
          educ = ifelse (m1a_q09==1 | m1a_q09==2, 1, 0),
          educ = ifelse (m1a_q09==3, 2, educ),
          educ=  ifelse (m1a_q09==4, 3, educ),
          educ = ifelse (m1a_q09==5 | m1a_q09==6 | m1a_q09==7 | m1a_q09==8 | m1a_q09==9, 4, educ))  %>%
  filter(educ>0) %>%
  merge(mean_piaac_oecd) %>%
  mutate(PVABOVE1 = ifelse(PVLIT1 > oecd_mean, 1, 0),
         PVABOVE2 = ifelse(PVLIT2 > oecd_mean, 1, 0),
         PVABOVE3 = ifelse(PVLIT3 > oecd_mean, 1, 0),
         PVABOVE4 = ifelse(PVLIT4 > oecd_mean, 1, 0),
         PVABOVE5 = ifelse(PVLIT5 > oecd_mean, 1, 0),
         PVABOVE6 = ifelse(PVLIT6 > oecd_mean, 1, 0),
         PVABOVE7 = ifelse(PVLIT7 > oecd_mean, 1, 0),
         PVABOVE8 = ifelse(PVLIT8 > oecd_mean, 1, 0),
         PVABOVE9 = ifelse(PVLIT9 > oecd_mean, 1, 0),
         PVABOVE10 = ifelse(PVLIT10 > oecd_mean, 1, 0))


bifie_step <- BIFIE.data(step, wgt=c("W_FinSPwt"), wgtrep=NULL, pv_vars=c("PVABOVE"))

step_mean <- BIFIE.univar(bifie_step, vars=c("PVABOVE"), group=c("iso", "age", "sex", "educ"))

above_OECD_avg <- step_mean$stat$M
iso <- step_mean$stat$groupval1
age <- step_mean$stat$groupval2
sex <- step_mean$stat$groupval3
educ <- step_mean$stat$groupval4

step_results_wide <- data.frame(iso, age, sex, educ, above_OECD_avg) %>%
  mutate(above_OECD_avg = 100*above_OECD_avg,
         below_OECD_avg = 100-above_OECD_avg)


# add missing values (country-age-sex-education categories with too small sample sizes) and assume a 50-50 split
missing_shares_step<-read.csv("Input/STEP-missing-shares_age_sex_educ.csv")
step_results_wide <- step_results_wide %>%
  rbind(missing_shares_step) %>%
  mutate (above_OECD_avg=ifelse(is.na(above_OECD_avg), 50, above_OECD_avg),
          below_OECD_avg=ifelse(is.na(below_OECD_avg), 50, below_OECD_avg))
step_results_wide <- step_results_wide[!(step_results_wide$age==15 & step_results_wide$educ==4), ]

#---------------------------------------------------------------------------------------------------------------------------------------------
# RESULTS FOR ALL COUNTRIES


# add population data by age, sex, educational attainment
results <- rbind(piaac_results_wide,
                 step_results_wide) %>%  merge(pop) %>%
  mutate(pop_below_OECD_avg = (epop * below_OECD_avg) /100,
         pop_above_OECD_avg = (epop * above_OECD_avg) / 100)

results<-melt(results, id.vars=c("iso", "country", "age", "sex", "educ", "above_OECD_avg", "below_OECD_avg","epop"),measure.vars=c("pop_below_OECD_avg", "pop_above_OECD_avg"))

results <- results %>%
  mutate(seduc = educ,
         seduc = ifelse(variable=="pop_below_OECD_avg" & educ==1, "Low-skill primary or less", seduc),
         seduc = ifelse(variable=="pop_above_OECD_avg" & educ==1, "High-skill primary or less", seduc),
         seduc = ifelse(variable=="pop_above_OECD_avg" & educ==2, "High-skill lower secondary", seduc),
         seduc = ifelse(variable=="pop_below_OECD_avg" & educ==2, "Low-skill lower secondary", seduc),
         seduc = ifelse(variable=="pop_above_OECD_avg" & educ==3, "High-skill upper secondary", seduc),
         seduc = ifelse(variable=="pop_below_OECD_avg" & educ==3, "Low-skill upper secondary", seduc),
         seduc = ifelse(variable=="pop_above_OECD_avg" & educ==4, "High-skill post-secondary", seduc),
         seduc = ifelse(variable=="pop_below_OECD_avg" & educ==4, "Low-skill post-secondary", seduc),
         age = ifelse(age==15, "15-19", age),
         age = ifelse(age==20, "20-24", age),
         age = ifelse(age==25, "25-29", age),
         age = ifelse(age==30, "30-34", age),
         age = ifelse(age==35, "35-39", age),
         age = ifelse(age==40, "40-44", age),
         age = ifelse(age==45, "45-49", age),
         age = ifelse(age==50, "50-54", age),
         age = ifelse(age==55, "55-59", age),
         age = ifelse(age==60, "60-64", age)) %>%
  rename(sepop = value)
         
country_list <- results %>%
  pull(iso) %>%
  unique
       

# add population below 15 and above 65
pop_below15_above65 <- get_wcde(indicator =  "pop") %>%
  filter(scenario==2, year == 2015, sex!="Both", age!="All", age=="0--4"| age=="5--9" | age=="10--14" | age=="65--69" | age=="70--74" | age=="75--79" | age=="80--84" | age=="85--99" | age=="90--94"| age=="95--99" | age=="100+") %>%
  rename(epop = pop, iso = country_code, country=name) %>%
  mutate(age = gsub("--", "-", age),
         variable = "N",
         sepop = epop,
         seduc = ifelse (age=="0-4" | age=="5-9" | age=="10-14", "Below 15", "Above 65"),
         above_OECD_avg = 1,
         below_OECD_avg = 0,
         sex=ifelse(sex=="Male", 1, 2),
         educ = 0) %>%
  select(-scenario, -year) %>%
  filter(iso  %in% country_list)



results <- results %>%
  rbind(pop_below15_above65) %>%
  mutate(age = factor(age, level=c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")),
         seduc = factor(seduc, level=c("Above 65", "High-skill post-secondary", "Low-skill post-secondary","High-skill upper secondary","Low-skill upper secondary","High-skill lower secondary","Low-skill lower secondary","High-skill primary or less","Low-skill primary or less","Below 15")))


#---------------------------------------------------------------------------------------------------------------------------------------------

# FINAL RESULTS: POPULATION BY 5-YEAR AGE GROUP, SEX, AND SKILLS-ADJUSTED EDUCATIONAL ATTAINMENT FOR 44 COUNTRIES
write.csv(results, "Output/results.csv", row.names=FALSE)


# plot skills-adjusted education pyramids

for(i in 1:45){
  ggplot(results, aes(x=age, y=sepop, pattern=seduc, fill=seduc, colour=seduc)) + 
    geom_bar_pattern(data = results %>% filter(sex==2), stat = "identity", size=0.3, pattern_density=0.0005, pattern_spacing=0.01, pattern_colour="white", pattern_fill="white", pattern_angle=45) + 
    geom_bar_pattern(data = results %>% filter(sex == 1), stat = "identity", mapping = aes(y =-sepop), size=0.3, pattern_density=0.0005, pattern_spacing=0.01, pattern_colour="white", pattern_fill="white", pattern_angle=135) + 
    coord_flip() +
    scale_fill_manual(name="Skills-adjusted \neducational attainment", values=c("#969696","#2166ac", "#2166ac", "#92c5de", "#92c5de", "#f4a582", "#f4a582", "#b2182b", "#b2182b", "#cccccc")) + 
    scale_color_manual(values=c("#969696","#2166ac", "#2166ac", "#92c5de", "#92c5de", "#f4a582", "#f4a582", "#b2182b", "#b2182b", "#cccccc")) + 
    theme_bw() + 
    facet_wrap_paginate (~ country, ncol=1, nrow=1, page=i, scales="free") + 
    scale_y_continuous(labels=abs) + 
    labs(fill="Skills-adjusted educational attainment", y="Population (000's)", x="Age Group")+
    theme(legend.position ="none") + 
    theme(strip.text=element_text(size=11.5),
          text=element_text(family="Times New Roman"),
          axis.title=element_text(face="bold"),
          strip.background = element_rect(fill="white"))+
    geom_hline(yintercept = 0)+
    scale_pattern_manual(values=c("none", "none", "stripe", "none", "stripe", "none","stripe", "none","stripe", "none")) 
  
  ggsave(paste0("Output/country_", i, ".jpg"), width=4.2, height=3.4)
}


