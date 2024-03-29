#Calculate HIGH school rankings
library(tidyverse)
library(janitor)
library(here)
library(broom)
library(modelr)
library(robustbase)
library(lmtest)
library(sandwich)
library(Hmisc)
library(ggrepel)

#LOAD & FILTER data -------------------
campus_master = read_csv(here::here("data-clean/campus_master.csv"))

high.grade.high <- c("09", "10", "11", "12")

high <- campus_master %>%
  filter(high.grade %in% high.grade.high) %>%
  select(-c(r.all.cnt:m.masters.pct)) %>%
  rowwise() %>%
  mutate(
    eng2.meets.cnt.2 = ifelse(is.na(eng2.meets.cnt.2),eng1.meets.cnt.2, eng2.meets.cnt.2), #if no eng2, replace with eng1; makes sure w.avg isn't NA
    eng2.all.cnt.2 = ifelse(is.na(eng2.all.cnt.2),eng1.all.cnt.2, eng2.all.cnt.2), #if no eng2, replace with eng1; makes sure w.avg isn't NA
    e.avg = ((eng1.meets.cnt.2 + eng2.meets.cnt.2)/(eng1.all.cnt.2 + eng2.all.cnt.2))*100, #generate weighted meets percentage
    m.avg = ((alg1.meets.cnt.2)/(alg1.all.cnt.2))*100,
    w.avg = mean(c(e.avg, m.avg), na.rm = T)
  ) %>%
  mutate(
    satact.part.rate = ifelse(is.na(satact.part.rate), 0,
                                   ifelse(satact.part.rate > 100, 100, satact.part.rate)),
         sat.avg = ifelse(is.na(sat.avg), 0, sat.avg),
         act.avg = ifelse(is.na(act.avg), 0, act.avg),
         ap.ib.part.rate = ifelse(is.na(ap.ib.part.rate), 0,
                                  ifelse(ap.ib.part.rate > 100, 100, ap.ib.part.rate)),
         ap.ib.above.criterion.rate = ifelse(is.na(ap.ib.above.criterion.rate), 0, ap.ib.above.criterion.rate),
         ap.ib.sci.pct = ifelse(is.na(ap.ib.sci.pct), 0, ap.ib.sci.pct),
         ap.ib.sci.crit.pct = ifelse(is.na(ap.ib.sci.crit.pct), 0, ap.ib.sci.crit.pct),
         ap.ib.math.pct = ifelse(is.na(ap.ib.math.pct), 0, ap.ib.math.pct),
         ap.ib.math.crit.pct = ifelse(is.na(ap.ib.math.crit.pct), 0, ap.ib.math.crit.pct),
         county = str_to_upper(county.nam)
  ) %>%
  ungroup() 

tabyl(high, low.grade, high.grade)

feeder_check <- high %>%
  filter((low.grade == "09" & high.grade == "09") | (low.grade == "09" & high.grade == "10") |
           (low.grade == "09" & high.grade == "11") | (low.grade == "08" & high.grade == "09") |
           (low.grade == "08" & high.grade == "10") | (low.grade == "08" & high.grade == "11"))


#Calculate rankings ------------------
## Student acheivement ---------
#generate student achievment grade based on meets percent

high <- high %>%
  mutate(
    stud.ach.ntile = w.avg, #percent_rank(w.avg)*100,
    stud.ach.grade = case_when(
      stud.ach.ntile>=80 ~ "A",
      stud.ach.ntile<80 & stud.ach.ntile>=60 ~ "B",
      stud.ach.ntile<60 & stud.ach.ntile>=40 ~ "C",
      stud.ach.ntile<40 & stud.ach.ntile>=20 ~ "D",
      stud.ach.ntile<20 & stud.ach.ntile>=0 ~ "F"
    ),
    #Old method
    # stud.ach.grade = ifelse(w.avg >= 60, "A",
    #                         ifelse(w.avg >= 45 & w.avg < 60, "B",
    #                                ifelse(w.avg >= 35 & w.avg < 45, "C",
    #                                       ifelse(w.avg >= 25 & w.avg < 35, "D",
    #                                              ifelse(w.avg < 25, "F", NA))))),
    stud.ach.grade2 = NA
  ) #to check using Claire's old code at end of script

#t = dplyr::select(high1, CAMPUS, w.avg, stud.ach.ntile)
#check grades

tabyl(high, stud.ach.grade)


## Student Growth --------------

high <- high %>%
  mutate(
    growth.avg = percent_rank(growth.avg)*100,
    growth.grade = case_when(
      growth.avg>=80 ~ "A",
      growth.avg<80 & growth.avg>=60 ~ "B",
      growth.avg<60 & growth.avg>=40 ~ "C",
      growth.avg<40 & growth.avg>=20 ~ "D",
      growth.avg<20 & growth.avg>=0 ~ "F"
    ),

    #OLD METHOD
    # growth.grade = ifelse(growth.avg >= 55, "A",
    #                       ifelse(growth.avg >= 52 & growth.avg < 55, "B",
    #                              ifelse(growth.avg >= 50 & growth.avg < 52, "C",
    #                                     ifelse(growth.avg >= 48 & growth.avg < 50, "D",
    #                                            ifelse(growth.avg < 48, "F", NA))))),
    growth.grade2 = NA)

tabyl(high, growth.grade)




##Campus Performance ----------------

#create loess regression

camp.performance.high <- loess(w.avg ~ ecodis.pct, high)

summary(camp.performance.high)
plot(camp.performance.high)

#add residuals and predicted performance to original dataframe and generate performance grade

high <- high %>%
  add_residuals(camp.performance.high, "residuals") %>%
  add_predictions(camp.performance.high, "pred.perf") %>%
  mutate(camp.perf.score = (w.avg + residuals))

#create campus performance grade

high <- high %>%
  mutate(
    camp.perf.score = percent_rank(camp.perf.score)*100,
    camp.perf.grade = case_when(
      camp.perf.score>=80 ~ "A",
      camp.perf.score<80 & camp.perf.score>=60 ~ "B",
      camp.perf.score<60 & camp.perf.score>=40 ~ "C",
      camp.perf.score<40 & camp.perf.score>=20 ~ "D",
      camp.perf.score<20 & camp.perf.score>=0 ~ "F"
    ),
    #OLD METHOD
    # camp.perf.grade = ifelse(camp.perf.score >= 60, "A",
    #                          ifelse(camp.perf.score >= 45 & camp.perf.score < 60, "B",
    #                                 ifelse(camp.perf.score >= 35 & camp.perf.score < 45, "C",
    #                                        ifelse(camp.perf.score >= 25 & camp.perf.score < 35, "D",
    #                                               ifelse(camp.perf.score < 25, "F", NA))))),
    camp.perf.grade2 = NA)

tabyl(high, camp.perf.grade)



## College Readiness ----------------

#generate C@R grduation rate

high <- high %>%
  mutate(
    #I think it makes more sense to do these flags first
    g.rate.flag.6 = ifelse((cohort.six.yr.2 <= 2.5 & grads.six.yr.2 <= 2.5) | is.na(cohort.six.yr.2), 1, 0),
    g.rate.flag.5 = ifelse((cohort.five.yr.2 <= 2.5 & grads.five.yr.2 <= 2.5)  | is.na(cohort.five.yr.2), 1, 0),
    g.rate.flag.4 = ifelse((cohort.four.yr.2 <= 2.5 & grads.four.yr.2 <= 2.5) | is.na(cohort.four.yr.2), 1, 0),
    
    grad.rate.6yr = ifelse(g.rate.flag.6==0, grads.six.yr.2/(cohort.six.yr.2 - died.six.yr.2), NA),
    grad.rate.5yr = ifelse(g.rate.flag.5==0, grads.five.yr.2/(cohort.five.yr.2 - died.five.yr.2), NA),
    grad.rate.4yr = ifelse(g.rate.flag.4==0, grads.four.yr.2/(cohort.four.yr.2 - died.four.yr.2), NA),
    
    grad.rate.6yr = ifelse(is.na(grad.rate.6yr) | is.infinite(grad.rate.6yr), -99, grad.rate.6yr),
    grad.rate.5yr = ifelse(is.na(grad.rate.5yr) | is.infinite(grad.rate.5yr), -99, grad.rate.5yr),
    grad.rate.4yr = ifelse(is.na(grad.rate.4yr) | is.infinite(grad.rate.4yr), -99, grad.rate.4yr),
    
    #Added some new exceptions
    grad.rate.car = case_when(
      grad.rate.6yr > grad.rate.5yr & grad.rate.6yr > grad.rate.4yr & grad.rate.6yr!=-99 ~ grad.rate.6yr,
      grad.rate.5yr > grad.rate.6yr & grad.rate.5yr > grad.rate.4yr & grad.rate.5yr!=-99 ~ grad.rate.5yr,
      grad.rate.4yr > grad.rate.6yr & grad.rate.4yr > grad.rate.5yr & grad.rate.4yr!=-99  ~ grad.rate.4yr,
      grad.rate.6yr == grad.rate.5yr & grad.rate.6yr > grad.rate.4yr & grad.rate.6yr!=-99 ~ grad.rate.6yr,
      grad.rate.6yr == grad.rate.4yr & grad.rate.6yr > grad.rate.5yr & grad.rate.6yr!=-99  ~ grad.rate.6yr,
      grad.rate.5yr == grad.rate.4yr & grad.rate.5yr > grad.rate.6yr & grad.rate.5yr!=-99  ~ grad.rate.5yr,
      grad.rate.6yr == grad.rate.5yr & grad.rate.6yr == grad.rate.4yr & grad.rate.6yr!=-99 ~ grad.rate.6yr
    ),
    grad.rate.car = ifelse(grad.rate.car > 1, 1, grad.rate.car),
    #g.rate.flag.all = ifelse(g.rate.flag.4 == 1 | g.rate.flag.5 == 1 | g.rate.flag.6 == 1, 1, 0)
    #I don't get the point of this flag
  ) %>%
  filter(grad.rate.car > 0 & !is.na(grad.rate.car)) 


#convert SAT/ACT scores to percentile (0-100 scale)

high <- high %>%
  mutate(sat.perc = ntile(sat.avg, 100),
         act.perc = ntile(act.avg, 100)
  )

#calculate college readiness score

high <- high %>%
  mutate(
    coll.ready.score = ((grad.rate.car*100)*.6) + (satact.part.rate*.1) + (ap.ib.part.rate*.1) + (ap.ib.above.criterion.rate*.1) +
      (sat.perc*.05) + (act.perc*.05)
  ) %>%
  # select(CAMPUS, CNAME, district, coll.ready.score, grad.rate.car, satact.part.rate, ap.ib.part.rate, ap.ib.above.criterion.rate,
  #        sat.perc, act.perc, sat.avg, act.avg)
  filter(!is.na(coll.ready.score)) 

#create college readiness grade

high <- high %>%
  mutate(
    coll.ready.score = percent_rank(coll.ready.score)*100,
    coll.ready.grade = case_when(
      coll.ready.score>=80 ~ "A",
      coll.ready.score<80 & coll.ready.score>=60 ~ "B",
      coll.ready.score<60 & coll.ready.score>=40 ~ "C",
      coll.ready.score<40 & coll.ready.score>=20 ~ "D",
      coll.ready.score<20 & coll.ready.score>=0 ~ "F"
    ),
    # coll.ready.grade = ifelse(coll.ready.score >= 79, "A",
    #                           ifelse(coll.ready.score >= 72 & coll.ready.score < 79, "B",
    #                                  ifelse(coll.ready.score >= 68 & coll.ready.score < 72, "C",
    #                                         ifelse(coll.ready.score >= 65 & coll.ready.score < 68, "D",
    #                                                ifelse(coll.ready.score < 65, "F", NA))))),
    coll.ready.grade2 = NA)

tabyl(high, coll.ready.grade)


##OVERALL GRADE####

high <- high %>%
  mutate(
    #camp.perf.score_ntile = ntile(camp.perf.score, 100),
    #growth.avg_ntile = ntile(growth.avg, 100),
    #coll.ready.score_ntile = ntile(coll.ready.score, 100),
    
    overall.score = (w.avg + camp.perf.score + growth.avg + coll.ready.score)/4,
    #overall.score_ntile = (w.avg + camp.perf.score_ntile + growth.avg_ntile + coll.ready.score_ntile)/4,
    
    overall.grade = case_when(
      overall.score>=80 ~ "A",
      overall.score<80 & overall.score>=60 ~ "B",
      overall.score<60 & overall.score>=40 ~ "C",
      overall.score<40 & overall.score>=20 ~ "D",
      overall.score<20 & overall.score>=0 ~ "F"
    )
    # overall.grade = ifelse(overall.score >= 70, "A",
    #                        ifelse(overall.score >= 55 & overall.score < 70, "B",
    #                               ifelse(overall.score >= 45 & overall.score < 55, "C",
    #                                      ifelse(overall.score >= 35 & overall.score < 45, "D",
    #                                             ifelse(overall.score < 35, "F", NA)))))
    ) %>%
  arrange(desc(overall.score)) %>%
  mutate(Rank = row_number()) %>%
  select(c(Rank, CAMPUS, CNAME, district, overall.score, overall.grade, all.cnt, ecodis.pct, stud.ach.grade, 
           camp.perf.grade, growth.grade, coll.ready.grade, ecodis.pct, charter, magnet, echs, everything())) %>%
  filter(!is.na(overall.grade))

tabyl(high, overall.grade)

#determine +/- cutoffs for each grade

# high_tertiles <- high %>%
#   group_by(overall.grade) %>%
#   summarise(tertile = list(enframe(quantile(overall.score, probs = c(.33, .67))))) %>%
#   unnest(tertile) %>% 
#   arrange(overall.grade, desc(value))
#assign +/- based on tertile scores

high <- high %>%
  mutate(
    overall.grade2 = case_when(
      overall.score>=94 ~ "A+", 
      overall.score<94 & overall.score>=86 ~ "A", 
      overall.score<86 & overall.score>=80 ~ "A-", 
      overall.score<80 & overall.score>=74 ~ "B+",
      overall.score<74 & overall.score>=66 ~ "B",
      overall.score<66 & overall.score>=60 ~ "B-",
      overall.score<60 & overall.score>=54 ~ "C+",
      overall.score<54 & overall.score>=46 ~ "C",
      overall.score<46 & overall.score>=40 ~ "C-",
      overall.score<40 & overall.score>=34 ~ "D+",
      overall.score<34 & overall.score>=26 ~ "D",
      overall.score<26 & overall.score>=20 ~ "D-",
      overall.score<20 ~ "F"
    )
    #OLD METHOD
    # overall.grade2 = ifelse(overall.score >= 81.3, "A+",
    #                         ifelse(overall.score >= 70 & overall.score <= 74.7, "A-",
    #                                ifelse(overall.score >= 63.7 & overall.score < 70, "B+",
    #                                       ifelse(overall.score >= 55 & overall.score <= 59.1, "B-",
    #                                              ifelse(overall.score >= 51.9 & overall.score < 55, "C+",
    #                                                     ifelse(overall.score >= 45 & overall.score <= 48.5, "C-",
    #                                                            ifelse(overall.score >= 42.1 & overall.score < 45, "D+",
    #                                                                   ifelse(overall.score >= 35 & overall.score <= 39.3, "D-", overall.grade))))))))
    # 
    ) %>%
  arrange(desc(overall.score)) %>%
  filter(CAMPUS != 101828001) %>% #remove Houston Gateway schools
  mutate(Rank = row_number()) %>%
  select(c(Rank, CAMPUS, CNAME, district, overall.score, overall.grade, overall.grade2, all.cnt, ecodis.pct, stud.ach.grade, 
           camp.perf.grade, growth.grade, everything()))

tabyl(high, overall.grade, overall.grade2)



##Gold Ribbon flags ------------------
houston <- c("BRAZORIA", "CHAMBERS", "FORT BEND", "GALVESTON", "HARRIS", "LIBERTY", "MONTGOMERY", "WALLER")
northtx <- c("COLLIN", "DALLAS", "DENTON", "ELLIS", "HUNT", "JOHNSON", "KAUFMAN", "ROCKWALL", "TARRANT")
centraltx <- c("BASTROP", "BLANCO", "BURNET", "CALDWELL", "HAYS", "TRAVIS", "WILLIAMSON")
sanantonio <- c("ATASCOSA", "BANDERA", "BEXAR", "COMAL", "GUADALUPE", "MEDINA")
rgv <- c("CAMERON", "HIDALGO", "STARR", "WILLACY")
westtx = c("EL PASO", 'HUDSPETH')

high <- high %>%
  mutate(
    goldribbon = ifelse(((ecodis.pct >= 75 & charter != 1 & magnet != 1)) & (overall.grade == "A" | overall.grade == "B"), 1, 0),
    greligible = ifelse((ecodis.pct >= 75 & charter != 1 & magnet != 1), 1, 0),
    grcharter = ifelse(((ecodis.pct >= 75 & charter == 1 & magnet != 1) & (overall.grade == "A" | overall.grade == "B")), 1, 0),
    grchartereligible = ifelse((ecodis.pct >= 75 & charter == 1 & magnet != 1), 1, 0),
    qualitycharter = ifelse((charter == 1 & (overall.grade == "A" | overall.grade == "B")), 1, 0),
    region_new = case_when(
      county %in% houston ~ "Houston",
      county %in% centraltx ~ "Central TX",
      county %in% northtx ~ "North TX",
      county %in% sanantonio ~ "San Antonio",
      county %in% westtx ~ "West Texas",
      county %in% rgv ~ "Rio Grande Valley",
      
    )
  )

tabyl(high, magnet)
tabyl(high, goldribbon)
tabyl(high, greligible)
tabyl(high, grcharter)
tabyl(high, grchartereligible)
tabyl(high, qualitycharter)
tabyl(high, charter)
tabyl(high, region_new)


#Final for distribution ------------------------------
high_final <- high %>%
  #select(c(Rank:county, low.grade, high.grade, region, growth.avg, w.avg, camp.perf.score:region_new, mobility.pct)) %>%
  select(-c(overall.grade, camp.perf.grade2, grad.rate.6yr:act.perc, coll.ready.grade2)) %>%
  mutate(charter = ifelse(charter == 1, "Yes", "No"),
         goldribbon = ifelse(goldribbon == 1, "Yes", "No"),
         greligible = ifelse(greligible == 1, "Yes", "No"),
         grcharter = ifelse(grcharter == 1, "Yes", "No"),
         grchartereligible = ifelse(grchartereligible == 1, "Yes", "No"),
         qualitycharter = ifelse(qualitycharter == 1, "Yes", "No"),
         magnet = ifelse(magnet == 1, "Yes", "No"),
         echs = ifelse(echs == 1, "Yes", "No"),
         overall.score = round(overall.score, 1),
         ecodis.pct = round(ecodis.pct, 1),
         growth.avg = round(growth.avg, 1),
         w.avg = round(w.avg, 1),
         camp.perf.score = round(camp.perf.score, 1),
         coll.ready.score = round(coll.ready.score, 1)
  ) %>%
  dplyr::select(
    Rank, CAMPUS, CNAME, district, overall.score, overall.grade2,
    all.cnt, 
    #eng1.all.cnt.2, eng2.all.cnt.2, alg1.all.cnt.2,
    #cohort.six.yr.2, cohort.five.yr.2, cohort.four.yr.2,
    ecodis.pct, stud.ach.grade, camp.perf.grade, growth.grade, coll.ready.grade,
    charter, county, low.grade, high.grade, region,
    growth.avg, w.avg, camp.perf.score, coll.ready.score, 
    goldribbon, greligible, grcharter,
    grchartereligible, qualitycharter, region_new, mobility.pct
  ) %>% 
  arrange(desc(overall.score)) %>% 
  dplyr::select(Rank, CAMPUS, CNAME, district, overall.score, everything()) %>% 
  reshape::rename(c(CAMPUS = "Campus ID Number",
                    CNAME = "Campus Name",
                    district = "District Name",
                    overall.score = "Total Score",
                    overall.grade2 = "Overall Grade",
                    all.cnt = "Total Enrollment",
                    eng1.all.cnt.2 = "# Eng 1 test-takers",
                    eng2.all.cnt.2 = "# Eng 2 test-takers",
                    alg1.all.cnt.2 = "# Alg 1 test-takers",
                    cohort.six.yr.2 = "# in 6-year grad cohort",
                    cohort.five.yr.2 = "# in 5-year grad cohort",
                    cohort.four.yr.2 = "# in 4-year grad cohort",
                    ecodis.pct = "Percent Economically Disadvantaged",
                    stud.ach.grade = "Student Achievement Grade",
                    camp.perf.grade = "Campus Performance Grade",
                    coll.ready.grade = "College Readiness Grade",
                    growth.grade = "Growth Grade",
                    charter = "Charter School?",
                    county = "County Name",
                    low.grade = "Low Grade",
                    high.grade = "High Grade",
                    region = "ESC Region Number",
                    growth.avg = "Growth Score",
                    w.avg = "Student Achievement Score",
                    camp.perf.score = "Campus Performance Score",
                    coll.ready.score = "College Readiness Score",
                    goldribbon = "Gold Ribbon School?",
                    greligible = "Gold Ribbon Eligible School?",
                    grcharter = "High-Performing, High-Poverty Charter School?",
                    grchartereligible = "High-Performing, High-Poverty Charter Eligible?",
                    qualitycharter = "Any High-Performing Charter?",
                    region_new = "Local Region",
                    mobility.pct = "Student Mobility Quartile")) 


write_csv(high_final, file=here::here("output", "high_2022_all_state.csv"))

# Regional tables --------------
regions = sort(unique(high_final$`Local Region`))

lapply(1:length(regions), function(x) {
  print(regions[x])
  df = filter(high_final, `Local Region`==regions[x]) %>% 
    mutate(`Region Rank` = row_number()) %>% 
    dplyr::select("Region Rank", "State Rank" = Rank, everything())
  
  region_export_name = str_replace_all(str_to_lower(regions[x]), " ", "")
  
  write_csv(df, file=here::here("output", paste0("high_2022_all_", region_export_name, '.csv')))
})

# Gold Ribbon tables ----------------------
gr_all = filter(high_final, `Gold Ribbon School?`=="Yes")
write_csv(gr_all, file=here::here("output", "high_2022_goldribbon_state.csv"))

lapply(1:length(regions), function(x) {
  print(regions[x])
  df = filter(high_final, `Local Region`==regions[x] & `Gold Ribbon School?`=="Yes") %>% 
    mutate(`Gold Ribbon Region Rank` = row_number()) %>% 
    dplyr::select("Gold Ribbon Region Rank", "State Rank" = Rank, everything())
  
  region_export_name = str_replace_all(str_to_lower(regions[x]), " ", "")
  
  write_csv(df, file=here::here("output", paste0("high_2022_goldribbon_", region_export_name, '.csv')))
})
