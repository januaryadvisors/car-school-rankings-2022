#Calculate elementary school rankings
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


#filter by elementary low grade levels

elem.grade <- c("EE", "EE KG", "EE 01", "EE 02", "EE 09", "PK", "PK 02", "PK 09",
                "KG", "01", "02", "03", "04", "05")

missing_3_5 = campus_master %>% 
  filter(
    r.all.cnt_3==0 & r.all.cnt_4==0 & r.all.cnt_5 == 0 &
      m.all.cnt_3==0 & m.all.cnt_4==0 & m.all.cnt_5 == 0
  )

elementary <- campus_master %>%
  filter(low.grade %in% elem.grade) %>%
  filter(!((low.grade == "05" & high.grade == "07") | (low.grade == "05" & high.grade == "08") |
             (low.grade == "05" & high.grade == "09") | (low.grade == "05" & high.grade == "10") |
             (low.grade == "05" & high.grade == "11") | (low.grade == "05" & high.grade == "12"))) %>%
  select(-c(alg1.all.cnt:eng2.masters.pct, eng1.all.cnt.2:dup)) %>%
  mutate(
    w.avg = rowMeans(select(.,c(m.meets.pct, r.meets.pct)), na.rm = T),
    w.avg = ifelse(is.nan(w.avg), NA, w.avg),
    county = str_to_upper(county.nam)
  ) %>% 
  ungroup() %>% 
  filter(!(CAMPUS %in% missing_3_5$CAMPUS))
  
  

#check grade crosstabs to verify elementary schools have been selected
tabyl(elementary, low.grade, high.grade)
summary(elementary$w.avg)

#Calculate rankings ------------------
## Student acheivement ---------
#generate student achievment grade based on meets percent

elementary <- elementary %>%
  mutate(
    stud.ach.grade = ifelse(w.avg >= 65, "A",
                            ifelse(w.avg >= 50 & w.avg < 65, "B",
                                   ifelse(w.avg >= 40 & w.avg < 50, "C",
                                          ifelse(w.avg >= 30 & w.avg < 40, "D",
                                                 ifelse(w.avg < 30, "F", NA))))),
    stud.ach.grade2 = NA
  ) 

tabyl(elementary, stud.ach.grade)


## Student Growth --------------
elementary <- elementary %>%
  mutate(
    growth.grade = ifelse(growth.avg >= 55, "A",
                          ifelse(growth.avg >= 52 & growth.avg < 55, "B",
                                 ifelse(growth.avg >= 50 & growth.avg < 52, "C",
                                        ifelse(growth.avg >= 48 & growth.avg < 50, "D",
                                               ifelse(growth.avg < 48, "F", NA))))))

tabyl(elementary, growth.grade)



##Campus Performance ----------------

#create loess regression

camp.performance.elem <- loess(w.avg ~ ecodis.pct, elementary)

summary(camp.performance.elem)
plot(camp.performance.elem)

#add residuals and predicted performance to original dataframe and generate performance grade

elementary <- elementary %>%
  add_residuals(camp.performance.elem, "residuals") %>%
  add_predictions(camp.performance.elem, "pred.perf") %>%
  mutate(camp.perf.score = (w.avg + residuals))

#create campus performance grade

elementary <- elementary %>%
  mutate(
    camp.perf.grade = ifelse(camp.perf.score >= 65, "A",
                             ifelse(camp.perf.score >= 50 & camp.perf.score < 65, "B",
                                    ifelse(camp.perf.score >= 40 & camp.perf.score < 50, "C",
                                           ifelse(camp.perf.score >= 30 & camp.perf.score < 40, "D",
                                                  ifelse(camp.perf.score < 30, "F", NA))))))

tabyl(elementary, camp.perf.grade)




##OVERALL GRADE ----------------

elementary <- elementary %>%
  mutate(
    overall.score = (w.avg + camp.perf.score + growth.avg)/3,
    overall.grade = ifelse(overall.score >= 65, "A",
                           ifelse(overall.score >= 50 & overall.score < 65, "B",
                                  ifelse(overall.score >= 40 & overall.score < 50, "C",
                                         ifelse(overall.score >= 30 & overall.score < 40, "D",
                                                ifelse(overall.score < 30, "F", NA)))))) %>% 
  arrange(desc(overall.score)) %>%
  ungroup() %>% 
  mutate(Rank = dplyr::row_number()) %>%
  select(c(Rank, CAMPUS, CNAME, district, overall.score, overall.grade, all.cnt, ecodis.pct, stud.ach.grade, 
           camp.perf.grade, growth.grade, everything())) %>%
  filter(!is.na(overall.grade))

tabyl(elementary, overall.grade)

#determine +/- cutoffs for each grade

elementary_tertiles <- elementary %>%
  group_by(overall.grade) %>%
  summarise(tertile = list(enframe(quantile(overall.score, probs = c(.33, .67))))) %>%
  unnest(tertile)

#assign +/- based on tertile scores

elementary <- elementary %>%
  mutate(
    overall.grade2 = ifelse(overall.score >= 73.3, "A+",
                            ifelse(overall.score >= 65 & overall.score <= 68.3, "A-",
                                   ifelse(overall.score >= 58.6 & overall.score < 65, "B+",
                                          ifelse(overall.score >= 50 & overall.score <= 54, "B-",
                                                 ifelse(overall.score >= 46.5 & overall.score < 50, "C+",
                                                        ifelse(overall.score >= 40 & overall.score <= 43.1, "C-",
                                                               ifelse(overall.score >= 37.2 & overall.score < 40, "D+",
                                                                      ifelse(overall.score >= 30 & overall.score <= 34, "D-", overall.grade))))))))
  ) %>%
  arrange(desc(overall.score)) %>%
  filter(CAMPUS != 101828001 & CAMPUS != 101828002 & CAMPUS != 101828101) %>% #remove Houston Gateway Academy schools
  mutate(Rank = row_number(),
         CNAME = ifelse(CAMPUS == 101806101, "BROWNSVILLE RAUL YZAGUIRRE STEM SCHOLARS PREP",
                        ifelse(CAMPUS == 101806102, "ELITE STEM PRIMARY ACADEMY", CNAME)) #rename Raul Yzaguirre campuses 
  ) %>%
  select(c(Rank, CAMPUS, CNAME, district, overall.score, overall.grade, overall.grade2, all.cnt, ecodis.pct, stud.ach.grade, 
           camp.perf.grade, growth.grade, everything()))

tabyl(elementary, overall.grade, overall.grade2)


##Gold Ribbon flags ------------------
houston <- c("BRAZORIA", "CHAMBERS", "FORT BEND", "GALVESTON", "HARRIS", "LIBERTY", "MONTGOMERY", "WALLER")
northtx <- c("COLLIN", "DALLAS", "DENTON", "ELLIS", "HUNT", "JOHNSON", "KAUFMAN", "ROCKWALL", "TARRANT")
centraltx <- c("BASTROP", "BLANCO", "BURNET", "CALDWELL", "HAYS", "TRAVIS", "WILLIAMSON")
sanantonio <- c("ATASCOSA", "BANDERA", "BEXAR", "COMAL", "GUADALUPE", "MEDINA")
rgv <- c("CAMERON", "HIDALGO", "STARR", "WILLACY")
westtx = c("EL PASO", 'HUDSPETH')

elementary = elementary %>% 
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

tabyl(elementary, goldribbon)
tabyl(elementary, greligible)
tabyl(elementary, grcharter)
tabyl(elementary, grchartereligible)
tabyl(elementary, qualitycharter)
tabyl(elementary, charter)
tabyl(elementary, region_new)


#Final for distribution ------------------------------
elementary_final <- elementary %>%
  select(-c(overall.grade, -race.pct.check)) %>%
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
    all.cnt, r.all.cnt_3, r.all.cnt_4, r.all.cnt_5, r.all.cnt_6, r.all.cnt_7, r.all.cnt_8,
    m.all.cnt_3, m.all.cnt_4, m.all.cnt_5, m.all.cnt_6, m.all.cnt_7, m.all.cnt_8,
    ecodis.pct, stud.ach.grade, camp.perf.grade, growth.grade,
    charter, county, low.grade, high.grade, region,
    growth.avg, w.avg, camp.perf.score, goldribbon, greligible, grcharter,
    grchartereligible, qualitycharter, region_new, mobility.pct
  ) %>% 
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

write_csv(elementary_final, file=here::here("output", "elem_2022_all_state.csv"))

# Regional tables --------------
regions = sort(unique(elementary_final$`Local Region`))

lapply(1:length(regions), function(x) {
  print(regions[x])
  df = filter(elementary_final, `Local Region`==regions[x]) %>% 
    mutate(`Region Rank` = row_number()) %>% 
    dplyr::select("Region Rank", "State Rank" = Rank, everything())
  
  region_export_name = str_replace_all(str_to_lower(regions[x]), " ", "")
  
  write_csv(df, file=here::here("output", paste0("elem_2022_all_", region_export_name, '.csv')))
})

# Gold Ribbon tables ----------------------
gr_all = filter(elementary_final, `Gold Ribbon School?`=="Yes")
write_csv(gr_all, file=here::here("output", "elem_2022_goldribbon_state.csv"))

lapply(1:length(regions), function(x) {
  print(regions[x])
  df = filter(elementary_final, `Local Region`==regions[x] & `Gold Ribbon School?`=="Yes") %>% 
    mutate(`Region Rank` = row_number()) %>% 
    dplyr::select("Region Rank", "State Rank" = Rank, everything())
  
  region_export_name = str_replace_all(str_to_lower(regions[x]), " ", "")
  
  write_csv(df, file=here::here("output", paste0("elem_2022_goldribbon_", region_export_name, '.csv')))
})
