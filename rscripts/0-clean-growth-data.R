#Clean growth data
library(tidyverse)
library(here)
library(janitor)
library(data.table)

#Load data --------------------------
## 2021 --------
g3_21 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2021/pir54995_STAAR3_MAY21_DATA.CSV"))
g4_21 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2021/pir54995_STAAR4_MAY21_DATA.CSV"))
g5_21 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2021/pir54995_G5_STAAR_APR21_DATA.CSV"))
g6_21 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2021/pir54995_STAAR6_MAY21_DATA.CSV"))
g7_21 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2021/pir54995_STAAR7_MAY21_DATA.CSV"))
g8_21 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2021/pir54995_G8_STAAR_APR21_DATA.CSV"))
a1_21 = read_csv(here::here("data-raw/TEA Student Assessment Data/EOC/2021/pir54995_staareocA1_spr21_DATA.CSV")) %>%
  reshape::rename(c(A1_RAW = "M_RAW",
                    A1_SCODE = "M_SCODE",
                    A1_TESTVER = "M_TESTVER")) %>%
  mutate(M_TESTLANG = "E",
         R_TESTLANG = NA)

e1_21 = read_csv(here::here("data-raw/TEA Student Assessment Data/EOC/2021/pir54995_staareocE1_spr21_DATA.CSV")) %>%
  reshape::rename(c(E1_RAW = "R_RAW",
                    E1_SCODE = "R_SCODE",
                    E1_TESTVER = "R_TESTVER")) %>%
  mutate(M_TESTLANG = NA,
         R_TESTLANG = "E")

e2_21 = read_csv(here::here("data-raw/TEA Student Assessment Data/EOC/2021/pir54995_staareocE2_spr21_DATA.CSV")) %>%
  reshape::rename(c(E2_RAW = "R_RAW",
                    E2_SCODE = "R_SCODE",
                    E2_TESTVER = "R_TESTVER")) %>%
  mutate(M_TESTLANG = NA,
         R_TESTLANG = "E")

data2021 = rbindlist(list(g3_21, g4_21, g5_21, g6_21, g7_21, g8_21, a1_21, e1_21, e2_21), fill=T)
rm(g3_21, g4_21, g5_21, g6_21, g7_21, g8_21, a1_21, e1_21, e2_21)

## 2022 --------
g3_22 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2022/pir54995_STAAR3_MAY22_DATA.CSV"))
g4_22 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2022/pir54995_STAAR4_MAY22_DATA.CSV"))
g5_22 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2022/pir54995_STAAR5_MAY22_DATA.CSV"))
g6_22 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2022/pir54995_STAAR6_MAY22_DATA.CSV"))
g7_22 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2022/pir54995_STAAR7_MAY22_DATA.CSV"))
g8_22 = read_csv(here::here("data-raw/TEA Student Assessment Data/G38/2022/pir54995_STAAR8_MAY22_DATA.CSV"))
a1_22 = read_csv(here::here("data-raw/TEA Student Assessment Data/EOC/2022/pir54995_staareocA1_spr22_DATA.CSV")) %>%
  reshape::rename(c(A1_RAW = "M_RAW",
                    A1_SCODE = "M_SCODE",
                    A1_TESTVER = "M_TESTVER")) %>%
  mutate(M_TESTLANG = "E",
         R_TESTLANG = NA)

e1_22 = read_csv(here::here("data-raw/TEA Student Assessment Data/EOC/2022/pir54995_staareocE1_spr22_DATA.CSV")) %>%
  reshape::rename(c(E1_RAW = "R_RAW",
                    E1_SCODE = "R_SCODE",
                    E1_TESTVER = "R_TESTVER")) %>%
  mutate(M_TESTLANG = NA,
         R_TESTLANG = "E")

e2_22 = read_csv(here::here("data-raw/TEA Student Assessment Data/EOC/2022/pir54995_staareocE2_spr22_DATA.CSV")) %>%
  reshape::rename(c(E2_RAW = "R_RAW",
                    E2_SCODE = "R_SCODE",
                    E2_TESTVER = "R_TESTVER")) %>%
  mutate(M_TESTLANG = NA,
         R_TESTLANG = "E")

data2022 = rbindlist(list(g3_22, g4_22, g5_22, g6_22, g7_22, g8_22, a1_22, e1_22, e2_22), fill=T)
rm(g3_22, g4_22, g5_22, g6_22, g7_22, g8_22, a1_22, e1_22, e2_22)



#Clean data -----------------------------
combined = rbindlist(list(data2021, data2022), fill=T) %>%
  mutate(STUDENTI = as.character(STUDENTI), CAMPUS = as.character(CAMPUS), 
         PEIMSCDC = as.character(PEIMSCDC), M_SCODE = as.character(M_SCODE), 
         R_SCODE = as.character(R_SCODE), STUIDFLAG = as.character(STUIDFLAG), 
         M_TESTVER = as.character(M_TESTVER), R_TESTVER = as.character(R_TESTVER), 
         M_TESTLANG = as.character(M_TESTLANG), R_TESTLANG = as.character(R_TESTLANG),
         GRADE = as.character(GRADE), YEAR = as.character(YEAR), COURSE = as.character(COURSE),
         CNAME = as.character(CNAME),
         
         M_TESTLANG = ifelse(is.na(M_TESTLANG), "E", M_TESTLANG),
         R_TESTLANG = ifelse(is.na(R_TESTLANG), "E", R_TESTLANG),
         M_RAW = ifelse(M_RAW == "NaN", NA, M_RAW),
         R_RAW = ifelse(R_RAW == "NaN", NA, R_RAW)
         ) %>%
  dplyr::rename(read = R_RAW, math = M_RAW) %>%
  mutate(GRADE = as.numeric(GRADE), #must be converted to characters then numeric to avoid issues with factors
         YEAR = as.numeric(YEAR),
         CAMPUS = as.numeric(CAMPUS))


combined_math <- combined %>%
  filter(STUIDFLAG == "Y" & M_SCODE == "S" & !is.na(math))

combined_read <- combined %>%
  filter(STUIDFLAG == "Y", R_SCODE == "S", !is.na(read))

rm(combined, data2021, data2022)
#save(combined_math, combined_read, file=here::here("data-clean/clean-growth-data.RData"))



#Analysis ------------------------------
##Remove duplicates, keep highest score ---------
growthmath <- combined_math %>%
  arrange(STUDENTI, YEAR, desc(math)) %>% ##sort by student and year, and take only highest scores
  group_by(STUDENTI, YEAR) %>%
  mutate(dup = row_number()) %>%
  filter(dup == 1) %>%
  ungroup()

growthread <- combined_read %>%
  arrange(STUDENTI, YEAR, desc(read)) %>% ##sort by student and year, and take only highest scores
  group_by(STUDENTI, YEAR) %>%
  mutate(dup = row_number()) %>%
  filter(dup == 1) %>%
  ungroup()

##generate lag variables on score group components####
growthmath <- growthmath %>%
  arrange(STUDENTI, YEAR) %>%
  group_by(STUDENTI) %>%
  mutate(lmath = lag(math, order_by = YEAR),
         lmgrade = lag(GRADE, order_by = YEAR),
         lmtestver = lag(M_TESTVER, order_by = YEAR),
         lmtestlang = lag(M_TESTLANG, order_by = YEAR))

growthread <- growthread %>%
  arrange(STUDENTI, YEAR) %>%
  group_by(STUDENTI) %>%
  mutate(lread = lag(read, order_by = YEAR),
         lrgrade = lag(GRADE, order_by = YEAR),
         lrtestver = lag(R_TESTVER, order_by = YEAR),
         lrtestlang = lag(R_TESTLANG, order_by = YEAR))


## drop students who skipped a grade or were retained and the first record for every student --------

growthmath <- growthmath %>%
  mutate(lmgrade_1 = lmgrade + 1) %>%  
  filter(GRADE == lmgrade_1) %>%
  select(-c(lmgrade_1))

growthread <- growthread %>%
  mutate(lrgrade_1 = lrgrade + 1) %>% 
  filter(GRADE == lrgrade_1) %>%
  select(-c(lrgrade_1))

save(growthmath, growthread, file = here::here("data-clean/growth-by-student.RData"))

##create Z scores and normal curve equivalents####

#zscores by student

growthmathz <- growthmath %>%
  group_by(lmath, GRADE, lmgrade, M_TESTLANG, lmtestlang, M_TESTVER, lmtestver, YEAR) %>% 
  mutate(z_math = (math - mean(math, na.rm = T)) / sd(math, na.rm = T))


growthreadz <- growthread %>%
  group_by(lread, GRADE, lrgrade, R_TESTLANG, lrtestlang, R_TESTVER, lrtestver, YEAR) %>% 
  mutate(z_read = (read - mean(read, na.rm = T)) / sd(read, na.rm = T))

#t = filter(growthmathz, CAMPUS=="227901105")

#aggregate to campus and drop schools with too few observations

growthmath_camp <- growthmathz %>%
  group_by(CAMPUS, CNAME) %>%
  summarise(mean_zmath = mean(z_math), 
            obs = n()) %>%
  filter(obs > 10) %>% #Changed from 24 to 10
  mutate(ncemath = ((mean_zmath * 21.06) + 50)) #normal curve equivalent scores

growthread_camp <- growthreadz %>%
  group_by(CAMPUS, CNAME) %>%
  summarise(mean_zread = mean(z_read),
            obs = n()) %>%
  filter(obs > 10) %>% #Changed from 24 to 10
  mutate(nceread = ((mean_zread * 21.06) + 50)) #normal curve equivalent scores

##combine growth datasets####

growth_all <- full_join(growthmath_camp, growthread_camp, by = "CAMPUS") %>%
  select(c(CAMPUS, CNAME.x, obs.x, ncemath, CNAME.y, obs.y, nceread))

saveRDS(growth_all, file=here::here("data-clean/growth-by-campus.rds"))


