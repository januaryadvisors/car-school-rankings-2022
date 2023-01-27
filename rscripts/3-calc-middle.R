#Calculate MIDDLE school rankings
#library(plyr)
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

missing_6_8 = campus_master %>% 
  filter(
    r.all.cnt_6==0 & r.all.cnt_7==0 & r.all.cnt_8 == 0 &
      m.all.cnt_6==0 & m.all.cnt_7==0 & m.all.cnt_8 == 0
  )

middle.grade.low <- c("EE", "EE KG", "EE 01", "EE 02", "EE 09", "PK", "PK 02", "PK 09",
                      "KG", "01", "02", "03", "04", "05", "06", "07", "08")
middle.grade.high <- c("06", "07", "08", "09", "10", "11", "12")

middle <- campus_master %>%
  rowwise() %>%
  filter(low.grade %in% middle.grade.low) %>%
  filter(high.grade %in% middle.grade.high) %>%
  filter(!((str_detect(low.grade, "EE") & high.grade == "06") | 
             (str_detect(low.grade, "PK") & high.grade == "06") |
             (low.grade == "KG" & high.grade == "06") | (low.grade == "01" & high.grade == "06") |
             (str_detect(low.grade, "02") & high.grade == "06") | (low.grade == "03" & high.grade == "06") |
             (low.grade == "04" & high.grade == "06") | (low.grade == "05" & high.grade == "06"))) %>%
  select(-c(alg1.all.cnt:eng2.masters.pct, eng1.all.cnt.2:dup)) %>%
  mutate(
    soc.pct = 100-white.pct,
    w.avg = mean(c(m.meets.pct, r.meets.pct), na.rm = T),#generate weighted meets percentage
    w.avg_soc = mean(c(m.meets.pct_soc, r.meets.pct_soc), na.rm = T),
    w.avg_ecod = mean(c(m.meets.pct_ecod, r.meets.pct_ecod), na.rm = T),
    county = str_to_upper(county.nam)) %>% 
  ungroup() %>% 
  #Drop additional campuses (n=68) where no one took reading/math test in 6-8 grade 
  filter(!(CAMPUS %in% missing_6_8$CAMPUS))



#check grade ranges

#Calculate rankings ------------------
## Student acheivement ---------
#generate student achievment grade based on meets percent

middle <- middle %>%
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
    stud.ach.grade2 = NA) #to check using Claire's old code at end of script

#check grades

tabyl(middle, stud.ach.grade)


## Student Growth --------------

middle <- middle %>%
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

tabyl(middle, growth.grade)


##Campus Performance ----------------

#create loess regression

camp.performance.middle <- loess(w.avg ~ ecodis.pct, middle)

summary(camp.performance.middle)
plot(camp.performance.middle)

#add residuals and predicted performance to original dataframe and generate performance grade

middle <- middle %>%
  add_residuals(camp.performance.middle, "residuals") %>%
  add_predictions(camp.performance.middle, "pred.perf") %>%
  mutate(camp.perf.score = (w.avg + residuals))

#create campus performance grade

middle <- middle %>%
  mutate(
    camp.perf.score = percent_rank(camp.perf.score)*100,
    camp.perf.grade = case_when(
      camp.perf.score>=80 ~ "A",
      camp.perf.score<80 & camp.perf.score>=60 ~ "B",
      camp.perf.score<60 & camp.perf.score>=40 ~ "C",
      camp.perf.score<40 & camp.perf.score>=20 ~ "D",
      camp.perf.score<20 & camp.perf.score>=0 ~ "F"
    ),
    # camp.perf.grade = ifelse(camp.perf.score >= 60, "A",
    #                          ifelse(camp.perf.score >= 45 & camp.perf.score < 60, "B",
    #                                 ifelse(camp.perf.score >= 35 & camp.perf.score < 45, "C",
    #                                        ifelse(camp.perf.score >= 25 & camp.perf.score < 35, "D",
    #                                               ifelse(camp.perf.score < 25, "F", NA))))),
    camp.perf.grade2 = NA)

tabyl(middle, camp.perf.grade)


##OVERALL GRADE ----------------

middle <- middle %>%
  mutate(
    #camp.perf.score_ntile = ntile(camp.perf.score, 100),
    #growth.avg_ntile = ntile(growth.avg, 100),
    #overall.score_ntile = (w.avg + camp.perf.score_ntile + growth.avg_ntile)/3,
    
    overall.score = (w.avg + camp.perf.score + growth.avg)/3,
    overall.grade = case_when(
      overall.score>=80 ~ "A",
      overall.score<80 & overall.score>=60 ~ "B",
      overall.score<60 & overall.score>=40 ~ "C",
      overall.score<40 & overall.score>=20 ~ "D",
      overall.score<20 & overall.score>=0 ~ "F"
    )
    # overall.grade = ifelse(overall.score >= 60, "A",
    #                        ifelse(overall.score >= 45 & overall.score < 60, "B",
    #                               ifelse(overall.score >= 35 & overall.score < 45, "C",
    #                                      ifelse(overall.score >= 25 & overall.score < 35, "D",
    #                                             ifelse(overall.score < 25, "F", NA)))))
    ) %>%
  arrange(desc(overall.score)) %>%
  mutate(Rank = row_number()) %>%
  select(c(Rank, CAMPUS, CNAME, district, overall.score, overall.grade, all.cnt, ecodis.pct, stud.ach.grade, 
           camp.perf.grade, growth.grade, everything())) %>%
  filter(!is.na(overall.grade))

tabyl(middle, overall.grade)

#determine +/- cutoffs for each grade

# middle_tertiles <- middle %>%
#   group_by(overall.grade) %>%
#   summarise(tertile = list(enframe(quantile(overall.score, probs = c(.33, .67))))) %>%
#   unnest(tertile)
# #assign +/- based on tertile scores

middle <- middle %>%
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
  #   overall.grade2 = ifelse(overall.score >= 71.8, "A+",
  #                           ifelse(overall.score >= 60 & overall.score <= 64.5, "A-",
  #                                  ifelse(overall.score >= 53.8 & overall.score < 60, "B+",
  #                                         ifelse(overall.score >= 45 & overall.score <= 48.8, "B-",
  #                                                ifelse(overall.score >= 41.8 & overall.score < 45, "C+",
  #                                                       ifelse(overall.score >= 35 & overall.score <= 38.5, "C-",
  #                                                              ifelse(overall.score >= 32.4 & overall.score < 35, "D+",
  #                                                                     ifelse(overall.score >= 25 & overall.score <= 29, "D-", overall.grade))))))))
  ) %>%
  arrange(desc(overall.score)) %>%
  filter(CAMPUS != 101828001 & CAMPUS != 101828002 & CAMPUS != 101828101) %>% #remove Houston Gateway campuses
  mutate(Rank = row_number(),
         CNAME = ifelse(CAMPUS == 101806101, "BROWNSVILLE RAUL YZAGUIRRE STEM SCHOLARS PREP",
                        ifelse(CAMPUS == 101806042, "HOUSTON STEM AND EARLY COLLEGE MIDDLE", CNAME)) #rename Raul Yzaguirre campuses
  ) %>%
  select(c(Rank, CAMPUS, CNAME, district, overall.score, overall.grade, overall.grade2, all.cnt, ecodis.pct, stud.ach.grade, 
           camp.perf.grade, growth.grade, everything()))

tabyl(middle, overall.grade, overall.grade2)



##Gold Ribbon flags ------------------
houston <- c("BRAZORIA", "CHAMBERS", "FORT BEND", "GALVESTON", "HARRIS", "LIBERTY", "MONTGOMERY", "WALLER")
northtx <- c("COLLIN", "DALLAS", "DENTON", "ELLIS", "HUNT", "JOHNSON", "KAUFMAN", "ROCKWALL", "TARRANT")
centraltx <- c("BASTROP", "BLANCO", "BURNET", "CALDWELL", "HAYS", "TRAVIS", "WILLIAMSON")
sanantonio <- c("ATASCOSA", "BANDERA", "BEXAR", "COMAL", "GUADALUPE", "MEDINA")
rgv <- c("CAMERON", "HIDALGO", "STARR", "WILLACY")
westtx = c("EL PASO", 'HUDSPETH')

middle <- middle %>%
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

tabyl(middle, magnet, goldribbon)
tabyl(middle, goldribbon)
tabyl(middle, greligible)
tabyl(middle, grcharter)
tabyl(middle, grchartereligible)
tabyl(middle, qualitycharter)
tabyl(middle, charter)
tabyl(middle, region_new)


#Final for distribution ------------------------------
middle_final <- middle %>%
  #select(c(Rank:county, low.grade, high.grade, region, growth.avg, w.avg, camp.perf.score:region_new, mobility.pct)) %>%
  select(-c(overall.grade, camp.perf.grade2)) %>%
  mutate(charter = ifelse(charter == 1, "Yes", "No"),
         goldribbon = ifelse(goldribbon == 1, "Yes", "No"),
         greligible = ifelse(greligible == 1, "Yes", "No"),
         grcharter = ifelse(grcharter == 1, "Yes", "No"),
         grchartereligible = ifelse(grchartereligible == 1, "Yes", "No"),
         qualitycharter = ifelse(qualitycharter == 1, "Yes", "No"),
         overall.score = round(overall.score, 1),
         ecodis.pct = round(ecodis.pct, 1),
         growth.avg = round(growth.avg, 1),
         w.avg = round(w.avg, 1),
         camp.perf.score = round(camp.perf.score, 1)
  ) %>%
  dplyr::select(
    Rank, CAMPUS, CNAME, district, overall.score, overall.grade2,
    all.cnt, 
    #r.all.cnt_3, r.all.cnt_4, r.all.cnt_5, r.all.cnt_6, r.all.cnt_7, r.all.cnt_8,
    #m.all.cnt_3, m.all.cnt_4, m.all.cnt_5, m.all.cnt_6, m.all.cnt_7, m.all.cnt_8,
    ecodis.pct, stud.ach.grade, camp.perf.grade, growth.grade,
    charter, county, low.grade, high.grade, region,
    growth.avg, w.avg, camp.perf.score, 
    goldribbon, greligible, grcharter,
    grchartereligible, qualitycharter, region_new, mobility.pct
  ) %>% 
  dplyr::select(Rank, CAMPUS, CNAME, district, overall.score, everything()) %>% 
  reshape::rename(c(CAMPUS = "Campus ID Number",
                    CNAME = "Campus Name",
                    district = "District Name",
                    overall.score = "Total Score",
                    overall.grade2 = "Overall Grade",
                    all.cnt = "Total Enrollment",
                    ecodis.pct = "Percent Economically Disadvantaged",
                    stud.ach.grade = "Student Achievement Grade",
                    camp.perf.grade = "Campus Performance Grade",
                    growth.grade = "Growth Grade",
                    charter = "Charter School?",
                    county = "County Name",
                    low.grade = "Low Grade",
                    high.grade = "High Grade",
                    region = "ESC Region Number",
                    growth.avg = "Growth Score",
                    w.avg = "Student Achievement Score",
                    camp.perf.score = "Campus Performance Score",
                    goldribbon = "Gold Ribbon School?",
                    greligible = "Gold Ribbon Eligible School?",
                    grcharter = "High-Performing, High-Poverty Charter School?",
                    grchartereligible = "High-Performing, High-Poverty Charter Eligible?",
                    qualitycharter = "Any High-Performing Charter?",
                    region_new = "Local Region",
                    mobility.pct = "Student Mobility Quartile"))



write_csv(middle_final, file=here::here("output", "middle_2022_all_state.csv"))

# Regional tables --------------
regions = sort(unique(middle_final$`Local Region`))

lapply(1:length(regions), function(x) {
  print(regions[x])
  df = filter(middle_final, `Local Region`==regions[x]) %>% 
    mutate(`Region Rank` = row_number()) %>% 
    dplyr::select("Region Rank", "State Rank" = Rank, everything())
  
  region_export_name = str_replace_all(str_to_lower(regions[x]), " ", "")
  
  write_csv(df, file=here::here("output", paste0("middle_2022_all_", region_export_name, '.csv')))
})

# Gold Ribbon tables ----------------------
gr_all = filter(middle_final, `Gold Ribbon School?`=="Yes")
write_csv(gr_all, file=here::here("output", "middle_2022_goldribbon_state.csv"))

lapply(1:length(regions), function(x) {
  print(regions[x])
  df = filter(middle_final, `Local Region`==regions[x] & `Gold Ribbon School?`=="Yes") %>% 
    mutate(`Region Rank` = row_number()) %>% 
    dplyr::select("Region Rank", "State Rank" = Rank, everything())
  
  region_export_name = str_replace_all(str_to_lower(regions[x]), " ", "")
  
  write_csv(df, file=here::here("output", paste0("middle_2022_goldribbon_", region_export_name, '.csv')))
})
