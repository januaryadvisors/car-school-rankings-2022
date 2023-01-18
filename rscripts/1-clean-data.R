library(tidyverse)
library(here)
library(janitor)
library(naniar)
library(readxl)
library(sf)
sf_use_s2(F)

# STAAR DATA ------------------------------------------

## Load STAAR aggregate data ---------------------- 
### downloaded from https://tea.texas.gov/student.assessment/staar/aggregate/
### check raw files first to make sure nothing has changed since last year
staar_agg_files = list.files(here::here("data-raw/STAAR aggregate"))

staar_eng3_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22e3.dat")) #Revised New
staar_sp3_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22s3.dat")) #Revised New
staar_eng4_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22e4.dat")) #Revised New
staar_sp4_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22s4.dat")) #Revised New
staar_eng5_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22e5.dat")) #Revised New
staar_sp5_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22s5.dat")) #Revised New
staar_6_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22e6.dat")) #Revised New
staar_7_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22e7.dat")) #Revised New
staar_8_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22e8.dat")) #Revised New
staar_a1_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22ea1.dat")) #Revised New
staar_e1_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22ee1.dat")) #%>%  #Revised New
staar_e2_raw <- read_csv(here::here("data-raw", "STAAR aggregate", "cfy22ee2.dat")) #%>%  #Revised New

## Clean and combine -------------------

clean_staar_agg = function(.data, .grade, .lang) {
  .data %>% 
    select(c(CAMPUS, r_all_d, r_all_unsatgl_nm, r_all_approgl_nm, r_all_meetsgl_nm, r_all_mastrgl_nm, m_all_d, m_all_unsatgl_nm,
             m_all_approgl_nm, m_all_meetsgl_nm, m_all_mastrgl_nm,
             r_ecoy_d, r_ecoy_unsatgl_nm, r_ecoy_approgl_nm, r_ecoy_meetsgl_nm, r_ecoy_mastrgl_nm,
             m_ecoy_d, m_ecoy_unsatgl_nm, m_ecoy_approgl_nm, m_ecoy_meetsgl_nm, m_ecoy_mastrgl_nm,
             r_ethh_d, r_ethh_unsatgl_nm, r_ethh_approgl_nm, r_ethh_meetsgl_nm, r_ethh_mastrgl_nm,
             m_ethh_d, m_ethh_unsatgl_nm, m_ethh_approgl_nm, m_ethh_meetsgl_nm, m_ethh_mastrgl_nm,
             r_ethi_d, r_ethi_unsatgl_nm, r_ethi_approgl_nm, r_ethi_meetsgl_nm, r_ethi_mastrgl_nm,
             m_ethi_d, m_ethi_unsatgl_nm, m_ethi_approgl_nm, m_ethi_meetsgl_nm, m_ethi_mastrgl_nm,
             r_etha_d, r_etha_unsatgl_nm, r_etha_approgl_nm, r_etha_meetsgl_nm, r_etha_mastrgl_nm,
             m_etha_d, m_etha_unsatgl_nm, m_etha_approgl_nm, m_etha_meetsgl_nm, m_etha_mastrgl_nm,
             r_ethb_d, r_ethb_unsatgl_nm, r_ethb_approgl_nm, r_ethb_meetsgl_nm, r_ethb_mastrgl_nm,
             m_ethb_d, m_ethb_unsatgl_nm, m_ethb_approgl_nm, m_ethb_meetsgl_nm, m_ethb_mastrgl_nm,
             r_eth2_d, r_eth2_unsatgl_nm, r_eth2_approgl_nm, r_eth2_meetsgl_nm, r_eth2_mastrgl_nm,
             m_eth2_d, m_eth2_unsatgl_nm, m_eth2_approgl_nm, m_eth2_meetsgl_nm, m_eth2_mastrgl_nm,
             r_ethp_d, r_ethp_unsatgl_nm, r_ethp_approgl_nm, r_ethp_meetsgl_nm, r_ethp_mastrgl_nm,
             m_ethp_d, m_ethp_unsatgl_nm, m_ethp_approgl_nm, m_ethp_meetsgl_nm, m_ethp_mastrgl_nm
             )) %>%
    reshape::rename(c(r_all_d = "r.all.cnt", r_all_unsatgl_nm = "r.notmeet.cnt", r_all_approgl_nm = "r.approaches.cnt",
                      r_all_meetsgl_nm = "r.meets.cnt", r_all_mastrgl_nm = "r.masters.cnt", m_all_d = "m.all.cnt",
                      m_all_unsatgl_nm = "m.notmeet.cnt", m_all_approgl_nm = "m.approaches.cnt", m_all_meetsgl_nm = "m.meets.cnt",
                      m_all_mastrgl_nm = "m.masters.cnt",
                      r_ecoy_d = "r.ecoy.all.cnt", r_ecoy_unsatgl_nm = "r.ecoy.notmeet.cnt", r_ecoy_approgl_nm = "r.ecoy.approaches.cnt",
                      r_ecoy_meetsgl_nm = "r.ecoy.meets.cnt", r_ecoy_mastrgl_nm = "r.ecoy.masters.cnt", m_ecoy_d = "m.ecoy.all.cnt",
                      m_ecoy_unsatgl_nm = "m.ecoy.notmeet.cnt", m_ecoy_approgl_nm = "m.ecoy.approaches.cnt", m_ecoy_meetsgl_nm = "m.ecoy.meets.cnt",
                      m_ecoy_mastrgl_nm = "m.ecoy.masters.cnt",
                      r_ethh_d = "r.ethh.all.cnt", r_ethh_unsatgl_nm = "r.ethh.notmeet.cnt", r_ethh_approgl_nm = "r.ethh.approaches.cnt",
                      r_ethh_meetsgl_nm = "r.ethh.meets.cnt", r_ethh_mastrgl_nm = "r.ethh.masters.cnt", m_ethh_d = "m.ethh.all.cnt",
                      m_ethh_unsatgl_nm = "m.ethh.notmeet.cnt", m_ethh_approgl_nm = "m.ethh.approaches.cnt", m_ethh_meetsgl_nm = "m.ethh.meets.cnt",
                      m_ethh_mastrgl_nm = "m.ethh.masters.cnt",
                      r_ethi_d = "r.ethi.all.cnt", r_ethi_unsatgl_nm = "r.ethi.notmeet.cnt", r_ethi_approgl_nm = "r.ethi.approaches.cnt",
                      r_ethi_meetsgl_nm = "r.ethi.meets.cnt", r_ethi_mastrgl_nm = "r.ethi.masters.cnt", m_ethi_d = "m.ethi.all.cnt",
                      m_ethi_unsatgl_nm = "m.ethi.notmeet.cnt", m_ethi_approgl_nm = "m.ethi.approaches.cnt", m_ethi_meetsgl_nm = "m.ethi.meets.cnt",
                      m_ethi_mastrgl_nm = "m.ethi.masters.cnt",
                      r_etha_d = "r.etha.all.cnt", r_etha_unsatgl_nm = "r.etha.notmeet.cnt", r_etha_approgl_nm = "r.etha.approaches.cnt",
                      r_etha_meetsgl_nm = "r.etha.meets.cnt", r_etha_mastrgl_nm = "r.etha.masters.cnt", m_etha_d = "m.etha.all.cnt",
                      m_etha_unsatgl_nm = "m.etha.notmeet.cnt", m_etha_approgl_nm = "m.etha.approaches.cnt", m_etha_meetsgl_nm = "m.etha.meets.cnt",
                      m_etha_mastrgl_nm = "m.etha.masters.cnt",
                      r_ethb_d = "r.ethb.all.cnt", r_ethb_unsatgl_nm = "r.ethb.notmeet.cnt", r_ethb_approgl_nm = "r.ethb.approaches.cnt",
                      r_ethb_meetsgl_nm = "r.ethb.meets.cnt", r_ethb_mastrgl_nm = "r.ethb.masters.cnt", m_ethb_d = "m.ethb.all.cnt",
                      m_ethb_unsatgl_nm = "m.ethb.notmeet.cnt", m_ethb_approgl_nm = "m.ethb.approaches.cnt", m_ethb_meetsgl_nm = "m.ethb.meets.cnt",
                      m_ethb_mastrgl_nm = "m.ethb.masters.cnt",
                      r_eth2_d = "r.eth2.all.cnt", r_eth2_unsatgl_nm = "r.eth2.notmeet.cnt", r_eth2_approgl_nm = "r.eth2.approaches.cnt",
                      r_eth2_meetsgl_nm = "r.eth2.meets.cnt", r_eth2_mastrgl_nm = "r.eth2.masters.cnt", m_eth2_d = "m.eth2.all.cnt",
                      m_eth2_unsatgl_nm = "m.eth2.notmeet.cnt", m_eth2_approgl_nm = "m.eth2.approaches.cnt", m_eth2_meetsgl_nm = "m.eth2.meets.cnt",
                      m_eth2_mastrgl_nm = "m.eth2.masters.cnt",
                      r_ethp_d = "r.ethp.all.cnt", r_ethp_unsatgl_nm = "r.ethp.notmeet.cnt", r_ethp_approgl_nm = "r.ethp.approaches.cnt",
                      r_ethp_meetsgl_nm = "r.ethp.meets.cnt", r_ethp_mastrgl_nm = "r.ethp.masters.cnt", m_ethp_d = "m.ethp.all.cnt",
                      m_ethp_unsatgl_nm = "m.ethp.notmeet.cnt", m_ethp_approgl_nm = "m.ethp.approaches.cnt", m_ethp_meetsgl_nm = "m.ethp.meets.cnt",
                      m_ethp_mastrgl_nm = "m.ethp.masters.cnt"
                      )) %>% 
    dplyr::select(CAMPUS, r.all.cnt:m.ethp.masters.cnt) %>% 
    pivot_longer(cols = c(r.all.cnt:m.ethp.masters.cnt), names_to = "variable", values_to = "value") %>% 
    mutate(grade = .grade, lang = .lang)
}

staar_eng3 = clean_staar_agg(staar_eng3_raw, "3", "e")
staar_sp3 = clean_staar_agg(staar_sp3_raw, "3", "s")
staar_eng4 = clean_staar_agg(staar_eng4_raw, "4", "e")
staar_sp4 = clean_staar_agg(staar_sp4_raw, "4", "s")
staar_eng5 = clean_staar_agg(staar_eng5_raw, "5", "e")
staar_sp5 = clean_staar_agg(staar_sp5_raw, "5", "s")
staar_6 = clean_staar_agg(staar_6_raw, "6", "e")
staar_7 = clean_staar_agg(staar_7_raw, "7", "e")
staar_8 = clean_staar_agg(staar_8_raw, "8", "e")

staar_a1 = staar_a1_raw %>% 
  dplyr::rename(CAMPUS = campus) %>%  
  select(c(CAMPUS, a1_all_d, a1_all_unsatgl_nm, a1_all_approgl_nm, a1_all_meetsgl_nm, a1_all_mastrgl_nm,
           a1_ecoy_d, a1_ecoy_unsatgl_nm, a1_ecoy_approgl_nm, a1_ecoy_meetsgl_nm, a1_ecoy_mastrgl_nm,
           a1_ethh_d, a1_ethh_unsatgl_nm, a1_ethh_approgl_nm, a1_ethh_meetsgl_nm, a1_ethh_mastrgl_nm,
           a1_ethi_d, a1_ethi_unsatgl_nm, a1_ethi_approgl_nm, a1_ethi_meetsgl_nm, a1_ethi_mastrgl_nm,
           a1_etha_d, a1_etha_unsatgl_nm, a1_etha_approgl_nm, a1_etha_meetsgl_nm, a1_etha_mastrgl_nm,
           a1_ethb_d, a1_ethb_unsatgl_nm, a1_ethb_approgl_nm, a1_ethb_meetsgl_nm, a1_ethb_mastrgl_nm,
           a1_ethp_d, a1_ethp_unsatgl_nm, a1_ethp_approgl_nm, a1_ethp_meetsgl_nm, a1_ethp_mastrgl_nm,
           a1_eth2_d, a1_eth2_unsatgl_nm, a1_eth2_approgl_nm, a1_eth2_meetsgl_nm, a1_eth2_mastrgl_nm
           )) %>%
  reshape::rename(c(a1_all_d = "alg1.all.cnt", a1_all_unsatgl_nm = "alg1.notmeet.cnt", 
                    a1_all_approgl_nm = "alg1.approaches.cnt",a1_all_meetsgl_nm = "alg1.meets.cnt", 
                    a1_all_mastrgl_nm = "alg1.masters.cnt",
                    a1_ecoy_d = "alg1.ecoy.all.cnt", a1_ecoy_unsatgl_nm = "alg1.ecoy.notmeet.cnt", 
                    a1_ecoy_approgl_nm = "alg1.ecoy.approaches.cnt", a1_ecoy_meetsgl_nm = "alg1.ecoy.meets.cnt", 
                    a1_ecoy_mastrgl_nm = "alg1.ecoy.masters.cnt",
                    a1_ethh_d = "alg1.ethh.all.cnt", a1_ethh_unsatgl_nm = "alg1.ethh.notmeet.cnt", 
                    a1_ethh_approgl_nm = "alg1.ethh.approaches.cnt", a1_ethh_meetsgl_nm = "alg1.ethh.meets.cnt", 
                    a1_ethh_mastrgl_nm = "alg1.ethh.masters.cnt",
                    a1_ethi_d = "alg1.ethi.all.cnt", a1_ethi_unsatgl_nm = "alg1.ethi.notmeet.cnt", 
                    a1_ethi_approgl_nm = "alg1.ethi.approaches.cnt", a1_ethi_meetsgl_nm = "alg1.ethi.meets.cnt", 
                    a1_ethi_mastrgl_nm = "alg1.ethi.masters.cnt",
                    a1_etha_d = "alg1.etha.all.cnt", a1_etha_unsatgl_nm = "alg1.etha.notmeet.cnt", 
                    a1_etha_approgl_nm = "alg1.etha.approaches.cnt", a1_etha_meetsgl_nm = "alg1.etha.meets.cnt", 
                    a1_etha_mastrgl_nm = "alg1.etha.masters.cnt",
                    a1_ethb_d = "alg1.ethb.all.cnt", a1_ethb_unsatgl_nm = "alg1.ethb.notmeet.cnt", 
                    a1_ethb_approgl_nm = "alg1.ethb.approaches.cnt", a1_ethb_meetsgl_nm = "alg1.ethb.meets.cnt", 
                    a1_ethb_mastrgl_nm = "alg1.ethb.masters.cnt",
                    a1_ethp_d = "alg1.ethp.all.cnt", a1_ethp_unsatgl_nm = "alg1.ethp.notmeet.cnt", 
                    a1_ethp_approgl_nm = "alg1.ethp.approaches.cnt", a1_ethp_meetsgl_nm = "alg1.ethp.meets.cnt", 
                    a1_ethp_mastrgl_nm = "alg1.ethp.masters.cnt",
                    a1_eth2_d = "alg1.eth2.all.cnt", a1_eth2_unsatgl_nm = "alg1.eth2.notmeet.cnt", 
                    a1_eth2_approgl_nm = "alg1.eth2.approaches.cnt", a1_eth2_meetsgl_nm = "alg1.eth2.meets.cnt", 
                    a1_eth2_mastrgl_nm = "alg1.eth2.masters.cnt"
                    )) %>% 
  pivot_longer(cols = c(alg1.all.cnt:alg1.eth2.masters.cnt), names_to = "variable", values_to = "value") %>% 
  mutate(grade = "", lang = "e")

staar_e1 <- staar_e1_raw %>%
  dplyr::rename(CAMPUS = campus) %>%  
  select(c(CAMPUS, e1_all_d, e1_all_unsatgl_nm, e1_all_approgl_nm, e1_all_meetsgl_nm, e1_all_mastrgl_nm,
           e1_ecoy_d, e1_ecoy_unsatgl_nm, e1_ecoy_approgl_nm, e1_ecoy_meetsgl_nm, e1_ecoy_mastrgl_nm,
           e1_ethh_d, e1_ethh_unsatgl_nm, e1_ethh_approgl_nm, e1_ethh_meetsgl_nm, e1_ethh_mastrgl_nm,
           e1_ethi_d, e1_ethi_unsatgl_nm, e1_ethi_approgl_nm, e1_ethi_meetsgl_nm, e1_ethi_mastrgl_nm,
           e1_etha_d, e1_etha_unsatgl_nm, e1_etha_approgl_nm, e1_etha_meetsgl_nm, e1_etha_mastrgl_nm,
           e1_ethb_d, e1_ethb_unsatgl_nm, e1_ethb_approgl_nm, e1_ethb_meetsgl_nm, e1_ethb_mastrgl_nm,
           e1_ethp_d, e1_ethp_unsatgl_nm, e1_ethp_approgl_nm, e1_ethp_meetsgl_nm, e1_ethp_mastrgl_nm,
           e1_eth2_d, e1_eth2_unsatgl_nm, e1_eth2_approgl_nm, e1_eth2_meetsgl_nm, e1_eth2_mastrgl_nm
  )) %>%
  reshape::rename(c(e1_all_d = "eng1.all.cnt", e1_all_unsatgl_nm = "eng1.notmeet.cnt", 
                    e1_all_approgl_nm = "eng1.approaches.cnt",e1_all_meetsgl_nm = "eng1.meets.cnt", 
                    e1_all_mastrgl_nm = "eng1.masters.cnt",
                    e1_ecoy_d = "eng1.ecoy.all.cnt", e1_ecoy_unsatgl_nm = "eng1.ecoy.notmeet.cnt", 
                    e1_ecoy_approgl_nm = "eng1.ecoy.approaches.cnt", e1_ecoy_meetsgl_nm = "eng1.ecoy.meets.cnt", 
                    e1_ecoy_mastrgl_nm = "eng1.ecoy.masters.cnt",
                    e1_ethh_d = "eng1.ethh.all.cnt", e1_ethh_unsatgl_nm = "eng1.ethh.notmeet.cnt", 
                    e1_ethh_approgl_nm = "eng1.ethh.approaches.cnt", e1_ethh_meetsgl_nm = "eng1.ethh.meets.cnt", 
                    e1_ethh_mastrgl_nm = "eng1.ethh.masters.cnt",
                    e1_ethi_d = "eng1.ethi.all.cnt", e1_ethi_unsatgl_nm = "eng1.ethi.notmeet.cnt", 
                    e1_ethi_approgl_nm = "eng1.ethi.approaches.cnt", e1_ethi_meetsgl_nm = "eng1.ethi.meets.cnt", 
                    e1_ethi_mastrgl_nm = "eng1.ethi.masters.cnt",
                    e1_etha_d = "eng1.etha.all.cnt", e1_etha_unsatgl_nm = "eng1.etha.notmeet.cnt", 
                    e1_etha_approgl_nm = "eng1.etha.approaches.cnt", e1_etha_meetsgl_nm = "eng1.etha.meets.cnt", 
                    e1_etha_mastrgl_nm = "eng1.etha.masters.cnt",
                    e1_ethb_d = "eng1.ethb.all.cnt", e1_ethb_unsatgl_nm = "eng1.ethb.notmeet.cnt", 
                    e1_ethb_approgl_nm = "eng1.ethb.approaches.cnt", e1_ethb_meetsgl_nm = "eng1.ethb.meets.cnt", 
                    e1_ethb_mastrgl_nm = "eng1.ethb.masters.cnt",
                    e1_ethp_d = "eng1.ethp.all.cnt", e1_ethp_unsatgl_nm = "eng1.ethp.notmeet.cnt", 
                    e1_ethp_approgl_nm = "eng1.ethp.approaches.cnt", e1_ethp_meetsgl_nm = "eng1.ethp.meets.cnt", 
                    e1_ethp_mastrgl_nm = "eng1.ethp.masters.cnt",
                    e1_eth2_d = "eng1.eth2.all.cnt", e1_eth2_unsatgl_nm = "eng1.eth2.notmeet.cnt", 
                    e1_eth2_approgl_nm = "eng1.eth2.approaches.cnt", e1_eth2_meetsgl_nm = "eng1.eth2.meets.cnt", 
                    e1_eth2_mastrgl_nm = "eng1.eth2.masters.cnt"
  )) %>% 
  pivot_longer(cols = c(eng1.all.cnt:eng1.eth2.masters.cnt), names_to = "variable", values_to = "value") %>% 
  mutate(grade = "", lang = "e")

staar_e2 <- staar_e2_raw %>%
  dplyr::rename(CAMPUS = campus) %>%  
  select(c(CAMPUS, e2_all_d, e2_all_unsatgl_nm, e2_all_approgl_nm, e2_all_meetsgl_nm, e2_all_mastrgl_nm,
           e2_ecoy_d, e2_ecoy_unsatgl_nm, e2_ecoy_approgl_nm, e2_ecoy_meetsgl_nm, e2_ecoy_mastrgl_nm,
           e2_ethh_d, e2_ethh_unsatgl_nm, e2_ethh_approgl_nm, e2_ethh_meetsgl_nm, e2_ethh_mastrgl_nm,
           e2_ethi_d, e2_ethi_unsatgl_nm, e2_ethi_approgl_nm, e2_ethi_meetsgl_nm, e2_ethi_mastrgl_nm,
           e2_etha_d, e2_etha_unsatgl_nm, e2_etha_approgl_nm, e2_etha_meetsgl_nm, e2_etha_mastrgl_nm,
           e2_ethb_d, e2_ethb_unsatgl_nm, e2_ethb_approgl_nm, e2_ethb_meetsgl_nm, e2_ethb_mastrgl_nm,
           e2_ethp_d, e2_ethp_unsatgl_nm, e2_ethp_approgl_nm, e2_ethp_meetsgl_nm, e2_ethp_mastrgl_nm,
           e2_eth2_d, e2_eth2_unsatgl_nm, e2_eth2_approgl_nm, e2_eth2_meetsgl_nm, e2_eth2_mastrgl_nm
  )) %>%
  reshape::rename(c(e2_all_d = "eng2.all.cnt", e2_all_unsatgl_nm = "eng2.notmeet.cnt", 
                    e2_all_approgl_nm = "eng2.approaches.cnt",e2_all_meetsgl_nm = "eng2.meets.cnt", 
                    e2_all_mastrgl_nm = "eng2.masters.cnt",
                    e2_ecoy_d = "eng2.ecoy.all.cnt", e2_ecoy_unsatgl_nm = "eng2.ecoy.notmeet.cnt", 
                    e2_ecoy_approgl_nm = "eng2.ecoy.approaches.cnt", e2_ecoy_meetsgl_nm = "eng2.ecoy.meets.cnt", 
                    e2_ecoy_mastrgl_nm = "eng2.ecoy.masters.cnt",
                    e2_ethh_d = "eng2.ethh.all.cnt", e2_ethh_unsatgl_nm = "eng2.ethh.notmeet.cnt", 
                    e2_ethh_approgl_nm = "eng2.ethh.approaches.cnt", e2_ethh_meetsgl_nm = "eng2.ethh.meets.cnt", 
                    e2_ethh_mastrgl_nm = "eng2.ethh.masters.cnt",
                    e2_ethi_d = "eng2.ethi.all.cnt", e2_ethi_unsatgl_nm = "eng2.ethi.notmeet.cnt", 
                    e2_ethi_approgl_nm = "eng2.ethi.approaches.cnt", e2_ethi_meetsgl_nm = "eng2.ethi.meets.cnt", 
                    e2_ethi_mastrgl_nm = "eng2.ethi.masters.cnt",
                    e2_etha_d = "eng2.etha.all.cnt", e2_etha_unsatgl_nm = "eng2.etha.notmeet.cnt", 
                    e2_etha_approgl_nm = "eng2.etha.approaches.cnt", e2_etha_meetsgl_nm = "eng2.etha.meets.cnt", 
                    e2_etha_mastrgl_nm = "eng2.etha.masters.cnt",
                    e2_ethb_d = "eng2.ethb.all.cnt", e2_ethb_unsatgl_nm = "eng2.ethb.notmeet.cnt", 
                    e2_ethb_approgl_nm = "eng2.ethb.approaches.cnt", e2_ethb_meetsgl_nm = "eng2.ethb.meets.cnt", 
                    e2_ethb_mastrgl_nm = "eng2.ethb.masters.cnt",
                    e2_ethp_d = "eng2.ethp.all.cnt", e2_ethp_unsatgl_nm = "eng2.ethp.notmeet.cnt", 
                    e2_ethp_approgl_nm = "eng2.ethp.approaches.cnt", e2_ethp_meetsgl_nm = "eng2.ethp.meets.cnt", 
                    e2_ethp_mastrgl_nm = "eng2.ethp.masters.cnt",
                    e2_eth2_d = "eng2.eth2.all.cnt", e2_eth2_unsatgl_nm = "eng2.eth2.notmeet.cnt", 
                    e2_eth2_approgl_nm = "eng2.eth2.approaches.cnt", e2_eth2_meetsgl_nm = "eng2.eth2.meets.cnt", 
                    e2_eth2_mastrgl_nm = "eng2.eth2.masters.cnt"
  )) %>% 
  pivot_longer(cols = c(eng2.all.cnt:eng2.eth2.masters.cnt), names_to = "variable", values_to = "value") %>% 
  mutate(grade = "", lang = "e")


#Combine all and aggregate up to campus level; calculate percentages
staar_counts_by_grade = rbind(staar_eng3, staar_sp3, staar_eng4, staar_sp4, staar_eng5, staar_sp5,
                              staar_6, staar_7, staar_8, staar_a1, staar_e1, staar_e2) %>% 
  filter(variable=="r.all.cnt" | variable =="m.all.cnt") %>% 
  group_by(CAMPUS, variable, grade) %>% 
  summarise(value = sum(value, na.rm=T)) %>% 
  ungroup() %>% 
  unite(col = "var_grade", c("variable", "grade"), sep="_") %>% 
  spread(var_grade, value) %>% 
  mutate(across(c(m.all.cnt_3:r.all.cnt_8), ~replace_na(., 0)))



staar_all = rbind(staar_eng3, staar_sp3, staar_eng4, staar_sp4, staar_eng5, staar_sp5,
                  staar_6, staar_7, staar_8, staar_a1, staar_e1, staar_e2) %>% 
  group_by(CAMPUS, variable) %>% 
  summarise(value = sum(value, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(
    group = case_when(
      str_detect(variable, "eth") ~ "soc",
      str_detect(variable, "ecoy") ~ "ecod",
      TRUE ~ "all"
    ),
    variable = str_remove(variable, "ecoy\\.|ethh\\.|ethi\\.|etha\\.|ethb\\.|ethp\\.|eth2\\.")
  ) %>%
  group_by(CAMPUS, group, variable) %>% 
  summarise(value = sum(value, na.rm=T)) %>% 
  mutate(
    variable = case_when(
      group=="soc" ~ paste0(variable, "_soc"),
      group=="ecod" ~ paste0(variable, "_ecod"),
      TRUE ~ variable
    )
  ) %>% 
  ungroup() %>% 
  dplyr::select(-group) %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(
    r.notmeet.pct = (r.notmeet.cnt/r.all.cnt)*100,
    m.notmeet.pct = (m.notmeet.cnt/m.all.cnt)*100,
    r.approaches.pct = (r.approaches.cnt/r.all.cnt)*100,
    m.approaches.pct = (m.approaches.cnt/m.all.cnt)*100,
    r.meets.pct = (r.meets.cnt/r.all.cnt)*100,
    m.meets.pct = (m.meets.cnt/m.all.cnt)*100,
    r.masters.pct = (r.masters.cnt/r.all.cnt)*100,
    m.masters.pct = (m.masters.cnt/m.all.cnt)*100,
    eng1.notmeet.pct = (eng1.notmeet.cnt/eng1.all.cnt)*100,
    eng1.approaches.pct = (eng1.approaches.cnt/eng1.all.cnt)*100,
    eng1.meets.pct = (eng1.meets.cnt/eng1.all.cnt)*100,
    eng1.masters.pct = (eng1.masters.cnt/eng1.all.cnt)*100,
    eng2.notmeet.pct = (eng2.notmeet.cnt/eng2.all.cnt)*100,
    eng2.approaches.pct = (eng2.approaches.cnt/eng2.all.cnt)*100,
    eng2.meets.pct = (eng2.meets.cnt/eng2.all.cnt)*100,
    eng2.masters.pct = (eng2.masters.cnt/eng2.all.cnt)*100,
    alg1.notmeet.pct = (alg1.notmeet.cnt/alg1.all.cnt)*100,
    alg1.approaches.pct = (alg1.approaches.cnt/alg1.all.cnt)*100,
    alg1.meets.pct = (alg1.meets.cnt/alg1.all.cnt)*100,
    alg1.masters.pct = (alg1.masters.cnt/alg1.all.cnt)*100,
    
    r.meets.pct_soc = (r.meets.cnt_soc/r.all.cnt_soc)*100,
    m.meets.pct_soc = (m.meets.cnt_soc/m.all.cnt_soc)*100,
    alg1.meets.pct_soc = (alg1.meets.cnt_soc/alg1.all.cnt_soc)*100,
    eng1.meets.pct_soc = (eng1.meets.cnt_soc/eng1.all.cnt_soc)*100,
    eng2.meets.pct_soc = (eng2.meets.cnt_soc/eng2.all.cnt_soc)*100,
    
    r.meets.pct_ecod = (r.meets.cnt_ecod/r.all.cnt_ecod)*100,
    m.meets.pct_ecod = (m.meets.cnt_ecod/m.all.cnt_ecod)*100,
    alg1.meets.pct_ecod = (alg1.meets.cnt_ecod/alg1.all.cnt_ecod)*100,
    eng1.meets.pct_ecod = (eng1.meets.cnt_ecod/eng1.all.cnt_ecod)*100,
    eng2.meets.pct_ecod = (eng2.meets.cnt_ecod/eng2.all.cnt_ecod)*100,
    
    
  ) %>% 
  ungroup() %>%
  select(c("CAMPUS","r.all.cnt","r.notmeet.cnt","r.approaches.cnt","r.meets.cnt","r.masters.cnt","m.all.cnt","m.notmeet.cnt",
           "m.approaches.cnt","m.meets.cnt","m.masters.cnt","r.notmeet.pct","r.approaches.pct","r.meets.pct","r.masters.pct",
           "m.notmeet.pct","m.approaches.pct","m.meets.pct","m.masters.pct", "eng1.notmeet.cnt", alg1.all.cnt:eng2.masters.pct,
           r.meets.pct_soc:eng2.meets.pct_ecod,
           alg1.meets.cnt_soc,alg1.all.cnt_soc, eng1.meets.cnt_soc, eng1.all.cnt_soc,
           eng2.meets.cnt_soc, eng2.all.cnt_soc,
           alg1.meets.cnt_ecod,alg1.all.cnt_ecod, eng1.meets.cnt_ecod, eng1.all.cnt_ecod,
           eng2.meets.cnt_ecod, eng2.all.cnt_ecod,
           )) %>%
  naniar::replace_with_na(replace = list(r.meets.pct = c(NaN), m.meets.pct = c(NaN))) %>% 
  left_join(., staar_counts_by_grade)



# COLLEGE READINESS ----------------------

## SAT/ACT from PIR --------------

sat_act <- read_csv(here::here("data-raw", "SAT_ACT_2021.csv")) %>% #Revised NEW
  reshape::rename(c(Campus_Number = "CAMPUS", SAT_ACT_Participation_Rate = "satact.part.rate",
                    Average_SAT_Total_Score = "sat.avg", Average_ACT_Composite_Score = "act.avg")) %>%
  select(c(CAMPUS, satact.part.rate, sat.avg, act.avg)) %>%
  mutate(
    CAMPUS = gsub("=00", "", CAMPUS),
    CAMPUS = gsub("=0", "", CAMPUS),
    CAMPUS = gsub("=", "", CAMPUS),
    CAMPUS = gsub("\"", "", CAMPUS),
    CAMPUS = as.integer(CAMPUS),
    CAMPUS = as.character(CAMPUS),
    sat.avg = gsub("=", "", sat.avg),
    sat.avg = gsub("\"", "", sat.avg),
    act.avg = gsub("=", "", act.avg),
    act.avg = gsub("\"", "", act.avg),
    satact.part.rate = gsub("=", "", satact.part.rate),
    satact.part.rate = gsub("\"", "", satact.part.rate),
    sat.avg = as.numeric(sat.avg),
    act.avg = as.numeric(act.avg),
    satact.part.rate = as.numeric(satact.part.rate)
  )

## AP/IB from PIR -----------

ap_ib <- read_csv(here::here("data-raw", "AP_IB_2021.csv")) %>% # Revised New
  reshape::rename(c(Campus_Number = "CAMPUS", Examinees_Above_Crit_Rate = "ap.ib.above.criterion.rate",
                    AP_IB_Participation_Rate = "ap.ib.part.rate", PctTookSciExam = "ap.ib.sci.pct",
                    PctSciCrit = "ap.ib.sci.crit.pct", PctTookMathExam = "ap.ib.math.pct",
                    PctMathCrit = "ap.ib.math.crit.pct")) %>%
  select(c(CAMPUS, ap.ib.part.rate, ap.ib.above.criterion.rate, ap.ib.sci.pct, ap.ib.sci.crit.pct, 
           ap.ib.math.pct, ap.ib.math.crit.pct)) %>%
  mutate(
    CAMPUS = gsub('="00', "", CAMPUS),
    CAMPUS = gsub('="0', "", CAMPUS),
    CAMPUS = gsub('="', "", CAMPUS),
    CAMPUS = gsub('"', "", CAMPUS),
    ap.ib.part.rate = gsub('="', "", ap.ib.part.rate),
    ap.ib.part.rate = gsub('"', "", ap.ib.part.rate),
    ap.ib.above.criterion.rate = gsub('="', "", ap.ib.above.criterion.rate),
    ap.ib.above.criterion.rate = gsub('"', "", ap.ib.above.criterion.rate),
    ap.ib.sci.pct = gsub('="', "", ap.ib.sci.pct),
    ap.ib.sci.pct = gsub('"', "", ap.ib.sci.pct),
    ap.ib.sci.crit.pct = gsub('="', "", ap.ib.sci.crit.pct),
    ap.ib.sci.crit.pct = gsub('"', "", ap.ib.sci.crit.pct),
    ap.ib.math.pct = gsub('="', "", ap.ib.math.pct),
    ap.ib.math.pct = gsub('"', "", ap.ib.math.pct),
    ap.ib.math.crit.pct = gsub('="', "", ap.ib.math.crit.pct),
    ap.ib.math.crit.pct = gsub('"', "", ap.ib.math.crit.pct),
    ap.ib.part.rate = as.numeric(ap.ib.part.rate),
    ap.ib.above.criterion.rate = as.numeric(ap.ib.above.criterion.rate),
    ap.ib.sci.pct = as.numeric(ap.ib.sci.pct),
    ap.ib.sci.crit.pct = as.numeric(ap.ib.sci.crit.pct),
    ap.ib.math.pct = as.numeric(ap.ib.math.pct),
    ap.ib.math.crit.pct = as.numeric(ap.ib.math.crit.pct)
  )


## GRADUATION rates ---------------------------

clean_grad_data = function(.sheet, .cohort_col_name, .grads_col_name, .data_type) {
  read_excel(here::here("data-raw", "C423001_final.xlsx"), sheet=.sheet, skip=2) %>% 
    clean_names() %>% 
    dplyr::select(Campus = campus, cohort = .cohort_col_name, 
                  grads = .grads_col_name,
                  Died=died) %>% 
    filter(cohort!="") %>% 
    mutate(died = ifelse(Died == "-1", 2.5, as.numeric(Died)),
           cohort = ifelse(cohort == "-1", 2.5, as.numeric(cohort)),
           grads = ifelse(grads == "-1", 2.5, as.numeric(grads)),
           Campus = as.integer(Campus),
           Campus = as.character(Campus)
    ) %>%
    dplyr::select(-Died) %>% 
    reshape::rename(c(Campus = "CAMPUS")) %>% 
    mutate(data_type = .data_type)
}

four_yr_grads = clean_grad_data(1, "began_grade_9_at_this_campus_in_2017_18", 
                                "graduated_from_any_texas_public_school_within_four_years", "four.yr")

five_yr_grads = clean_grad_data(2, "began_grade_9_at_this_campus_in_2016_17",  
                                "graduated_from_any_texas_public_school_within_five_years","five.yr")

six_yr_grads = clean_grad_data(3, "began_grade_9_at_this_campus_in_2015_16", 
                               "graduated_from_any_texas_public_school_within_six_years", "six.yr")

grad_rates = rbind(four_yr_grads, five_yr_grads, six_yr_grads) %>% 
  pivot_longer(cols = c(cohort, grads, died), names_to = "variable", values_to = "value") %>% 
  mutate(variable = paste0(variable, ".", data_type)) %>% 
  dplyr::select(-data_type) %>% 
  pivot_wider(names_from = variable, values_from = value)

college_readiness = full_join(sat_act, ap_ib, by = "CAMPUS") %>% 
  full_join(., grad_rates, by = "CAMPUS") %>% 
  mutate(CAMPUS = str_pad(CAMPUS, 9, "left", "0"))



#DIRECTORY INFO & CAMPUS INFO -----------------------------------------

## Campus info -----------
#Shapefile of campus from TEA opean data website: https://schoolsdata2-93b5c-tea-texas.opendata.arcgis.com/datasets/7d8ba6b96ee14a6d882f6f35f7d27828/explore?location=30.947411%2C-100.146733%2C6.62
campus_sf = read_sf("https://services2.arcgis.com/5MVN2jsqIrNZD4tP/arcgis/rest/services/campus21to22/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
  st_drop_geometry() %>% 
  clean_names() #%>% 

campus = campus_sf %>% 
  dplyr::select(
    CAMPUS = school_num, CNAME = school_nam,
    dist.number = district_n,  district = district_1,
    county.num = county_num, county = county_nam, region = esc_region,
    grades = grade_rang,
    charter_type = charter_ty, 
    campus_address = match_addr, campus_lon = x, campus_lat = y,
    instructio
  ) %>% 
  mutate(charter = ifelse(charter_type != " ", 1, 0)) %>% 
  separate(grades, c("low.grade", "high.grade"), sep = "([\\-])") %>%
  select(-c(county, county.num))

kipp = campus %>% filter(str_detect(district, "KIPP")) %>% 
  st_as_sf(coords = c("campus_lon", "campus_lat"), crs=st_crs(4326))

county = tigris::counties(48) %>% st_transform(st_crs(4326))

campus_county = campus %>% 
  filter(!is.na(campus_lon)) %>% 
  st_as_sf(coords = c("campus_lon", "campus_lat"), crs=st_crs(4326)) %>% 
  st_join(., county) %>% 
  dplyr::select(CAMPUS, county.num = GEOID, county.nam=NAME) %>% 
  st_drop_geometry()
  


## PEG status----------
peg = read_excel(here::here("data-raw/Children At Risk PIR_PR Response Part 2.xlsx")) %>% 
  mutate(peg = 1) %>% 
  dplyr::select(CAMPUS = CDCN, peg)

## Magnet, ECHS ----------
magnet_echs = read_csv(here::here("data-raw/PRU_7128.csv")) %>% 
  dplyr::select(CAMPUS, MAGNET_STATUS, ECHS_IND) %>% 
  mutate(
    echs = ifelse(ECHS_IND == "YES", 1, 0),
    magnet = ifelse(MAGNET_STATUS == "YES", 1, 0)
  ) %>% 
  dplyr::select(CAMPUS, echs, magnet)

## TAPR demographics -----
# Always from previous year; the PEIMS report has a lot of missing data, don't use
# Source: https://rptsvr1.tea.texas.gov/perfreport/tapr/2021/download/DownloadData.html

campus_demographics = read_csv(here::here("data-raw/CAMPPROF.csv")) %>% 
  mutate(#CAMPUS = gsub("'00", "", CAMPUS),
         #CAMPUS = gsub("'0", "", CAMPUS),
         CAMPUS = gsub("'", "", CAMPUS)) %>%
  reshape::rename(c(CPETALLC = "all.cnt", CPETWHIC = "white.cnt",
                    CPETBLAC = "black.cnt", CPETHISC = "hisp.cnt",
                    CPETASIC = "asian.cnt", CPETINDC = "amer.ind.cnt",
                    CPETPCIC = "pac.isl.cnt", CPETTWOC = "tworace.cnt", 
                    CPETRSKC = "atrisk.cnt", CPETECOC = "ecodis.cnt", 
                    CPETBILC = "bil.esl.cnt", CPETGEEC = "earlyed.cnt", 
                    CPETGPKC = "prek.cnt", CPETGKNC = "kg.cnt",
                    CPETGIFC = "gt.cnt", CPETLEPC = "lep.cnt",
                    CPETSPEC = "sped.cnt", CPEMALLC = "mobility.cnt",
                    CPEMALLP = "mobility.pct")) %>%
  select(c(CAMPUS, all.cnt, white.cnt, black.cnt, hisp.cnt, asian.cnt, amer.ind.cnt, pac.isl.cnt, tworace.cnt, atrisk.cnt,
           ecodis.cnt, bil.esl.cnt, earlyed.cnt, prek.cnt, kg.cnt, gt.cnt, lep.cnt, sped.cnt, mobility.cnt, mobility.pct)) %>%
  mutate(
    white.pct = (white.cnt/all.cnt)*100,
    black.pct = (black.cnt/all.cnt)*100,
    hisp.pct = (hisp.cnt/all.cnt)*100,
    asian.pct = (asian.cnt/all.cnt)*100,
    amer.ind.pct = (amer.ind.cnt/all.cnt)*100,
    pac.isl.pct = (pac.isl.cnt/all.cnt)*100,
    tworace.pct = (tworace.cnt/all.cnt)*100,
    atrisk.pct = (atrisk.cnt/all.cnt)*100,
    ecodis.pct = (ecodis.cnt/all.cnt)*100,
    bil.esl.pct = (bil.esl.cnt/all.cnt)*100,
    race.pct.check = round((white.pct + black.pct + hisp.pct + asian.pct + amer.ind.pct + pac.isl.pct + tworace.pct), 1)
  )

campus_info = left_join(campus, campus_demographics) %>% 
  left_join(., campus_county) %>% 
  left_join(., peg) %>% 
  left_join(., magnet_echs)




#GROWTH DATA --------------------------

#read in growth scores and replace NA scores
growth <- readRDS(here::here("data-clean/growth-by-campus.rds")) %>% 
  select(c(CAMPUS, ncemath, nceread)) %>%
  reshape::rename(c(ncemath = "ncemath", nceread = "nceread")) %>%
  mutate(ncemath = ifelse(is.na(ncemath), nceread, ncemath),
         nceread = ifelse(is.na(nceread), ncemath, nceread),
         growth.avg = (nceread + ncemath)/2, #calculate average growth
         CAMPUS = str_pad(as.character(CAMPUS), 9, "left", "0")) #for merging


# Combine CAMPUS_ALL ----------
campus_all <- full_join(campus_info, staar_all, by = "CAMPUS") %>%
  left_join(., college_readiness, by = "CAMPUS") %>% 
  left_join(., growth, by="CAMPUS") %>% 
  mutate(CAMPUS = as.numeric(CAMPUS))


# FEEDER SCHOOLS -------------------------
## Relying on 2019 work; have not verified 
aldinehs<-c(101902001, 101902081)
davishs<-c(101902012, 101902082)
macarthurhs<-c(101902003, 101902083)
eisenhowerhs<-c(101902004, 101902084)
nimitzhs<-c(101902005, 101902085)
valleyviewhs<-c(108916001, 108916041)
joshuahs<-c(126905001, 126905003)
leehs<-c(165901002, 165901042)
midlandhs<-c(165901003, 165901044)
aledohs<-c(184907001, 184907009)
trinityhs<-c(220916002, 220916042, 220916045, 220916041) 
bellhs<-c(220916001, 220916043, 220916044, 220916041)
carrollseniorhs<-c(220919001, 220919003)
harlingenhs<-c(31903001, 31903007) #31903007 feeds into 2 HS
harlingensouth<-c(31903002, 31903007)
sanbenitohs<-c(31912001, 31912007)
allenhs<-c(43901001, 43901002)
planoeast<-c(43910006, 43910003, 43910011)
planowest<-c(43910010, 43910007, 43910009)
planosenior<-c(43910001, 43910004, 43910005)
newbraunfelshs<-c(46901001, 46901002)


campus_all = campus_all %>%
  mutate(
    CAMPUS.new = case_when(
      CAMPUS %in% aldinehs ~ "101902001",
      CAMPUS %in% davishs ~ "101902012",
      CAMPUS %in% macarthurhs ~ "101902003",
      CAMPUS %in% eisenhowerhs ~ "101902004",
      CAMPUS %in% nimitzhs ~ "101902005",
      CAMPUS %in% valleyviewhs ~ "108916001",
      CAMPUS %in% joshuahs ~ "126905001",
      CAMPUS %in% leehs ~ "165901002",
      CAMPUS %in% midlandhs ~ "165901003",
      CAMPUS %in% aledohs ~ "184907001",
      CAMPUS %in% trinityhs ~ "220916002",
      CAMPUS %in% bellhs ~ "220916001",
      CAMPUS %in% carrollseniorhs ~ "220919001",
      CAMPUS %in% harlingenhs ~ "31903001",
      CAMPUS %in% harlingensouth ~ "31903002",
      CAMPUS %in% sanbenitohs ~ "31912001",
      CAMPUS %in% allenhs ~ "43901001",
      CAMPUS %in% planoeast ~ "43910006",
      CAMPUS %in% planowest ~ "43910010",
      CAMPUS %in% planosenior ~ "43910001",
      CAMPUS %in% newbraunfelshs ~ "46901001",
      TRUE ~ as.character(CAMPUS)
    )
  )
           
feeders <- c("101902001", "101902012", "101902003", "101902004","101902005", 
             "108916001", "126905001", "165901002", "165901003", "184907001", 
             "220916002", "220916001", "220919001", "31903001", "31903002", 
             "31912001", "43901001", "43910006", "43910010", "43910001", 
             "46901001")          

campus_all_check <- campus_all %>% #generate mini data frame to check calculations 
  group_by(CAMPUS.new) %>%
  filter(CAMPUS.new %in% feeders) %>%
  mutate(eng1.all.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(eng1.all.cnt, na.rm = T), eng1.all.cnt) , 
         eng1.meets.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(eng1.meets.cnt, na.rm = T), eng1.meets.cnt),
         eng2.all.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(eng2.all.cnt, na.rm = T), eng2.all.cnt),
         eng2.meets.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(eng2.meets.cnt, na.rm = T), eng2.meets.cnt),
         alg1.all.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(alg1.all.cnt, na.rm = T), alg1.all.cnt),
         alg1.meets.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(alg1.meets.cnt, na.rm = T), alg1.meets.cnt),
         eng1.meets.pct.2 = (eng1.meets.cnt.2/eng1.all.cnt.2)*100,
         eng2.meets.pct.2 = (eng2.meets.cnt.2/eng2.all.cnt.2)*100,
         alg1.meets.pct.2 = (alg1.meets.cnt.2/alg1.all.cnt.2)*100,
         #soc
         eng1.all.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(eng1.all.cnt_soc, na.rm = T), eng1.all.cnt_soc) , 
         eng1.meets.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(eng1.meets.cnt_soc, na.rm = T), eng1.meets.cnt_soc),
         eng2.all.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(eng2.all.cnt_soc, na.rm = T), eng2.all.cnt_soc),
         eng2.meets.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(eng2.meets.cnt_soc, na.rm = T), eng2.meets.cnt_soc),
         alg1.all.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(alg1.all.cnt_soc, na.rm = T), alg1.all.cnt_soc),
         alg1.meets.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(alg1.meets.cnt_soc, na.rm = T), alg1.meets.cnt_soc),
         eng1.meets.pct.2_soc = (eng1.meets.cnt.2_soc/eng1.all.cnt.2_soc)*100,
         eng2.meets.pct.2_soc = (eng2.meets.cnt.2_soc/eng2.all.cnt.2_soc)*100,
         alg1.meets.pct.2_soc = (alg1.meets.cnt.2_soc/alg1.all.cnt.2_soc)*100,
         #ecod
         eng1.all.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(eng1.all.cnt_ecod, na.rm = T), eng1.all.cnt_ecod) , 
         eng1.meets.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(eng1.meets.cnt_ecod, na.rm = T), eng1.meets.cnt_ecod),
         eng2.all.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(eng2.all.cnt_ecod, na.rm = T), eng2.all.cnt_ecod),
         eng2.meets.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(eng2.meets.cnt_ecod, na.rm = T), eng2.meets.cnt_ecod),
         alg1.all.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(alg1.all.cnt_ecod, na.rm = T), alg1.all.cnt_ecod),
         alg1.meets.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(alg1.meets.cnt_ecod, na.rm = T), alg1.meets.cnt_ecod),
         eng1.meets.pct.2_ecod = (eng1.meets.cnt.2_ecod/eng1.all.cnt.2_ecod)*100,
         eng2.meets.pct.2_ecod = (eng2.meets.cnt.2_ecod/eng2.all.cnt.2_ecod)*100,
         alg1.meets.pct.2_ecod = (alg1.meets.cnt.2_ecod/alg1.all.cnt.2_ecod)*100,
  ) %>%
  arrange(CAMPUS.new) %>% 
  dplyr::select(CAMPUS, CAMPUS.new, CNAME, eng1.all.cnt.2:alg1.meets.pct.2_ecod)


#CAMPUS ALL FINAL -----------------
campus_all_final <- campus_all %>% #if all calculations check out, apply to full data frame    #Section Revised New
  group_by(CAMPUS.new) %>%
  mutate(eng1.all.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(eng1.all.cnt, na.rm = T), eng1.all.cnt) , 
         eng1.meets.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(eng1.meets.cnt, na.rm = T), eng1.meets.cnt),
         eng2.all.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(eng2.all.cnt, na.rm = T), eng2.all.cnt),
         eng2.meets.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(eng2.meets.cnt, na.rm = T), eng2.meets.cnt),
         alg1.all.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(alg1.all.cnt, na.rm = T), alg1.all.cnt),
         alg1.meets.cnt.2 = ifelse(CAMPUS.new %in% feeders, sum(alg1.meets.cnt, na.rm = T), alg1.meets.cnt),
         eng1.all.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(eng1.all.cnt_soc, na.rm = T), eng1.all.cnt_soc) , 
         eng1.meets.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(eng1.meets.cnt_soc, na.rm = T), eng1.meets.cnt_soc),
         eng2.all.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(eng2.all.cnt_soc, na.rm = T), eng2.all.cnt_soc),
         eng2.meets.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(eng2.meets.cnt_soc, na.rm = T), eng2.meets.cnt_soc),
         alg1.all.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(alg1.all.cnt_soc, na.rm = T), alg1.all.cnt_soc),
         alg1.meets.cnt.2_soc = ifelse(CAMPUS.new %in% feeders, sum(alg1.meets.cnt_soc, na.rm = T), alg1.meets.cnt_soc),
         eng1.all.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(eng1.all.cnt_ecod, na.rm = T), eng1.all.cnt_ecod) , 
         eng1.meets.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(eng1.meets.cnt_ecod, na.rm = T), eng1.meets.cnt_ecod),
         eng2.all.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(eng2.all.cnt_ecod, na.rm = T), eng2.all.cnt_ecod),
         eng2.meets.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(eng2.meets.cnt_ecod, na.rm = T), eng2.meets.cnt_ecod),
         alg1.all.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(alg1.all.cnt_ecod, na.rm = T), alg1.all.cnt_ecod),
         alg1.meets.cnt.2_ecod = ifelse(CAMPUS.new %in% feeders, sum(alg1.meets.cnt_ecod, na.rm = T), alg1.meets.cnt_ecod),
         cohort.four.yr.2 = ifelse(CAMPUS.new %in% feeders, sum(cohort.four.yr, na.rm =T), cohort.four.yr),
         grads.four.yr.2 = ifelse(CAMPUS.new %in% feeders, sum(grads.four.yr, na.rm = T), grads.four.yr),
         died.four.yr.2 = ifelse(CAMPUS.new %in% feeders, sum(died.four.yr, na.rm = T), died.four.yr),
         cohort.five.yr.2 = ifelse(CAMPUS.new %in% feeders, sum(cohort.five.yr, na.rm = T), cohort.five.yr),
         grads.five.yr.2 = ifelse(CAMPUS.new %in% feeders, sum(grads.five.yr, na.rm = T), grads.five.yr),
         died.five.yr.2 = ifelse(CAMPUS.new %in% feeders, sum(died.five.yr, na.rm = T), died.five.yr),
         cohort.six.yr.2 = ifelse(CAMPUS.new %in% feeders, sum(cohort.six.yr, na.rm = T), cohort.six.yr),
         grads.six.yr.2 = ifelse(CAMPUS.new %in% feeders, sum(grads.six.yr, na.rm = T), grads.six.yr),
         died.six.yr.2 = ifelse(CAMPUS.new %in% feeders, sum(died.six.yr, na.rm = T), died.six.yr)
  ) %>%
  group_by(CAMPUS.new) %>%
  mutate(dup = row_number()) %>%
  filter(dup == 1) %>%
  ungroup()


#write.csv(campus_all, here("R Projects", "Rankings Fall 2019", "Tables", "mastercampus_fortsg.csv"))

#create rankings analysis file

campus_master <- campus_all_final %>%
  #drop feeder campuses
  filter(CAMPUS != "101902081" & CAMPUS != "101902082" & CAMPUS != "101902083" & CAMPUS != "101902084" & CAMPUS != "101902085" &
           CAMPUS != "108916041" & CAMPUS != "126905003" & CAMPUS != "165901042" & CAMPUS != "165901044" & CAMPUS != "184907009" &
           CAMPUS != "220916042" & CAMPUS != "220916045" & CAMPUS != "220916041" & CAMPUS != "220916043" & CAMPUS != "220916044" &
           CAMPUS != "220919003" & CAMPUS != "226903041" & CAMPUS != "31903007" & CAMPUS != "31906002" & CAMPUS != "31912007" & 
           CAMPUS != "43901002" & CAMPUS != "43910003" & CAMPUS != "43910011" & CAMPUS != "43910007" & CAMPUS != "43910009" &
           CAMPUS != "43910004" & CAMPUS != "43910005" & CAMPUS != "46901002" & CAMPUS != "57904002") %>%
  #drop schools with fewer than 90 students, or no name, or missing growth
  filter(all.cnt >= 90 | is.na(CNAME) | is.na(growth.avg)) %>%
  select(-c(CAMPUS.new)) %>%
  filter(instructio == "REGULAR INSTRUCTIONAL") #drop aea schools

write_csv(campus_master, here::here("data-clean/campus_master.csv"))
#write file to use for rankings analysis
#write.csv(campus_master, here("R Projects", "Rankings Fall 2019", "Tables", "rankmaster_2019.csv"))

