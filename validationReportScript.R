#### Libraries ----
library(tidyverse)
library(officer)
library(flextable)
library(ggplot2)
library(lubridate)
library(readr)
library(DBI)
library(odbc)
#### Data warehouse ----
conn = dbConnect(odbc::odbc(), "", UID = "", PWD = "", encoding = "latin1")

tdi108 = dbGetQuery(conn, "select * from V_TDI_108") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi109 = dbGetQuery(conn, "select * from V_TDI_109") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi110 = dbGetQuery(conn, "select * from V_TDI_110") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi111 = dbGetQuery(conn, "select * from V_TDI_111") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi112 = dbGetQuery(conn, "select * from V_TDI_112") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi113 = dbGetQuery(conn, "select * from V_TDI_113") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi114 = dbGetQuery(conn, "select * from V_TDI_114") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi115 = dbGetQuery(conn, "select * from V_TDI_115") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi116 = dbGetQuery(conn, "select * from V_TDI_116") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi117 = dbGetQuery(conn, "select * from V_TDI_117") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi118 = dbGetQuery(conn, "select * from V_TDI_118") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi119 = dbGetQuery(conn, "select * from V_TDI_119") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi120 = dbGetQuery(conn, "select * from V_TDI_120") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi121 = dbGetQuery(conn, "select * from V_TDI_121") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi122 = dbGetQuery(conn, "select * from V_TDI_122") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi123 = dbGetQuery(conn, "select * from V_TDI_123") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi124 = dbGetQuery(conn, "select * from V_TDI_124") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi125 = dbGetQuery(conn, "select * from V_TDI_125") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi126 = dbGetQuery(conn, "select * from V_TDI_126") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi127 = dbGetQuery(conn, "select * from V_TDI_127") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi128 = dbGetQuery(conn, "select * from V_TDI_128") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdi129 = dbGetQuery(conn, "select * from V_TDI_129") %>% rename_all(tolower) %>% filter(dsc_status %in% c('Validation In Progress','Submitted'))
tdiTrends = tdi109 = dbGetQuery(conn, "select * from V_TDI_109") %>% rename_all(tolower)
countries = dbGetQuery(conn, "select distinct dsc_country, report_id from dwtap.mv_st34 where dsc_status in ('Validation In Progress','Submitted')") %>% rename_all(tolower)
#### GrandTotals ####
totals108 <- tdi108 %>% filter(type_treatment == 100, gender == 100, !treat_status == 99) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q108 = sum(answer_numeric)) %>% ungroup()
totals109 <- tdi109 %>% filter(type_treatment == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q109 = sum(answer_numeric)) %>% ungroup()
totals110 <- tdi110 %>% filter(gender == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q110 = sum(answer_numeric)) %>% ungroup()
totals112 <- tdi112 %>% filter(age_range == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q112 = sum(answer_numeric)) %>% ungroup()
totals113 <- tdi113 %>% filter(referral == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q113 = sum(answer_numeric)) %>% ungroup()
totals114 <- tdi114 %>% filter(with_whom == 100, gender == 100,  drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q114 = sum(answer_numeric)) %>% ungroup()
totals116 <- tdi116 %>% filter(living_where == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q116 = sum(answer_numeric)) %>% ungroup()
totals117 <- tdi117 %>% filter(education == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q117 = sum(answer_numeric)) %>% ungroup()
totals118 <- tdi118 %>% filter(labour_status == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q118 = sum(answer_numeric)) %>% ungroup()
totals119 <- tdi119 %>% filter(route_admin == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q119 = sum(answer_numeric))   %>% ungroup()
totals120 <- tdi120 %>% filter(frequency == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q120 = sum(answer_numeric)) %>% ungroup()
totals121 <- tdi121 %>% filter(age_range == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q121 = sum(answer_numeric)) %>% ungroup()
totals122 <- tdi122 %>% filter(injection == 100, drug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q122 = sum(answer_numeric)) %>% ungroup()
totals124 <- tdi124 %>% filter(!treat_status == 99, polydrug == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q124 = sum(answer_numeric)) %>% ungroup()
totals1250 <-tdi125 %>% filter(type_matrix == 1250, drug_2 == 100, drug_3 == 100) %>% mutate(treat_status = 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q1250 = sum(answer_numeric)) %>% ungroup()
totals1251 <-tdi125 %>% filter(type_matrix == 1251, drug == 100, drug_2 == 100) %>% mutate(treat_status = 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q1251 = sum(answer_numeric)) %>% ungroup()
totals126 <- tdi126 %>% filter(injection == 100, tested == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q126 = sum(answer_numeric)) %>% ungroup()
totals127 <- tdi127 %>% filter(injection == 100, tested == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q127 = sum(answer_numeric)) %>% ungroup()
totals128 <- tdi128 %>% filter(injection == 100, needles == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q128 = sum(answer_numeric)) %>% ungroup()
totals129 <- tdi129 %>% filter(type_matrix == 1290, drug == 100, ost_status == 100) %>% group_by(dsc_country, report_id, treat_status) %>% summarise(q129 = sum(answer_numeric)) %>% ungroup()

grandTotals <- totals108 %>%
  full_join(totals109) %>%
  full_join(totals110) %>%
  full_join(totals112) %>%
  full_join(totals113) %>%
  full_join(totals114) %>%
  full_join(totals116) %>%
  full_join(totals117) %>%
  full_join(totals118) %>%
  full_join(totals119) %>%
  full_join(totals120) %>%
  full_join(totals121) %>%
  full_join(totals122) %>%
  full_join(totals124) %>%
  full_join(totals1251) %>%
  full_join(totals126) %>%
  full_join(totals127) %>%
  full_join(totals128) %>%
  full_join(totals129)
grandTotals <- grandTotals %>%
  mutate(valid = ifelse(apply(grandTotals[,4:22], MARGIN=1, function(x) n_distinct(x, na.rm = T)) == 1, 0, 1),
         treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previous clients', 'All clients'))) %>%
  gather(-dsc_country, -report_id, -treat_status, -valid, key = question, value = nClients)


#### Primary drug ####
byDrug109 <- tdi109 %>% filter(type_treatment == 100) %>% group_by(dsc_country, report_id, year_of_treatment, treat_status, drug) %>% summarise(q109 = sum(answer_numeric)) %>% ungroup()
byDrug110 <- tdi110 %>% filter(gender == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q110 = sum(answer_numeric)) %>% ungroup()
byDrug112 <- tdi112 %>% filter(age_range == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q112 = sum(answer_numeric)) %>% ungroup()
byDrug113 <- tdi113 %>% filter(referral == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q113 = sum(answer_numeric)) %>% ungroup()
byDrug114 <- tdi114 %>% filter(gender == 100, with_whom == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q114 = sum(answer_numeric)) %>% ungroup()
byDrug116 <- tdi116 %>% filter(living_where == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q116 = sum(answer_numeric)) %>% ungroup()
byDrug117 <- tdi117 %>% filter(education == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q117 = sum(answer_numeric)) %>% ungroup()
byDrug118 <- tdi118 %>% filter(labour_status == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q118 = sum(answer_numeric)) %>% ungroup()
byDrug119 <- tdi119 %>% filter(route_admin == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q119 = sum(answer_numeric)) %>% ungroup()
byDrug120 <- tdi120 %>% filter(frequency == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q120 = sum(answer_numeric)) %>% ungroup()
byDrug121 <- tdi121 %>% filter(age_range == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q121 = sum(answer_numeric)) %>% ungroup()
byDrug129 <- tdi129 %>% filter(type_matrix == 1290, ost_status == 100) %>% group_by(dsc_country, report_id,  year_of_treatment, treat_status, drug) %>% summarise(q129 = sum(answer_numeric)) %>% ungroup()

byDrug <- byDrug109 %>%
  full_join(byDrug110) %>%
  full_join(byDrug112) %>%
  full_join(byDrug119) %>%
  full_join(byDrug120) %>%
  full_join(byDrug121) %>%
  full_join(byDrug129) %>%
  filter(!drug %in% c(10,20,30,40,50,60,70,90,99,100))
byDrug <- byDrug %>%
  mutate(valid = ifelse(apply(byDrug[,6:12], MARGIN=1, function(x) n_distinct(x, na.rm = T)) == 1, 0, 1),
         treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previous clients', 'All clients')))

byDrugGeneric <- byDrug109 %>%
  full_join(byDrug110) %>%
  full_join(byDrug112) %>%
  full_join(byDrug113) %>%
  full_join(byDrug114) %>%
  full_join(byDrug116) %>%
  full_join(byDrug117) %>%
  full_join(byDrug118) %>%
  full_join(byDrug119) %>%
  full_join(byDrug120) %>%
  full_join(byDrug121) %>%
  full_join(byDrug129) %>%
  filter(drug %in% c(10,20,30,40,50,60,70,90,99,100))
byDrugGeneric <- byDrugGeneric %>%
  mutate(valid = ifelse(apply(byDrugGeneric[,6:17], MARGIN=1, function(x) n_distinct(x, na.rm = T)) == 1, 0, 1),
         treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previous clients', 'All clients')))

#### Setting #####
s08Total <- tdi108 %>% filter(gender == 100, !treat_status == 99) %>% select(dsc_country, report_id, question, type_treatment, treat_status, answer_numeric)
s09Total <- tdi109 %>% filter(drug == 100) %>%  select(dsc_country, report_id, question, type_treatment, treat_status, answer_numeric)
bySetting <- s08Total %>% rbind(s09Total) %>%  spread(key = question, value = answer_numeric) %>%  arrange(treat_status, type_treatment) 
bySetting <- bySetting %>% mutate(valid = ifelse(apply(bySetting[ ,5:6], MARGIN=1, function(x) n_distinct(x, na.rm = T)) == 1, 0, 1),
                                  treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previous clients', 'All clients')),
                                  type_treatment = ifelse(type_treatment == 1, 'Outpatient',
                                                   ifelse(type_treatment == 2, 'Inpatient',
                                                   ifelse(type_treatment == 3, 'Low-threshold',
                                                   ifelse(type_treatment == 4, 'GPs',
                                                   ifelse(type_treatment == 5, 'Prison',
                                                   ifelse(type_treatment == 6, 'Other',
                                                   ifelse(type_treatment == 99, 'Missing', 'Total')))))))) %>%
  rename(q108 = `108`, q109 = `109`)
#### Gender ####
s08Gender <- tdi108 %>% filter(type_treatment == 100, !treat_status == 99, gender %in% c(1,2)) %>% select(report_id, question, treat_status, gender, answer_numeric)
s10Gender <- tdi110 %>% filter(drug == 100, gender %in% c(1,2)) %>% select(report_id, treat_status, gender, answer_numeric) %>% mutate(question = "110")
s14Gender <- tdi114 %>% filter(gender %in% c(1,2), drug == 100, with_whom == 100) %>% select(report_id, treat_status, gender, answer_numeric) %>% mutate(question = "114")

byGender <- s08Gender %>% rbind(s10Gender) %>% rbind(s14Gender) %>% spread(key = question, value = answer_numeric) 
byGender <- byGender %>% mutate(valid = ifelse(apply(byGender[ ,4:6], MARGIN=1, function(x) n_distinct(x, na.rm = T)) == 1, 0, 1),
                                gender = ifelse(gender == 1, 'Males', 'Females'),
                                treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previous clients', 'All clients')))
#### Age ####
s11Age <- tdi111 %>% filter(gender == 100, means %in% c(3, 6)) %>% select(report_id, means, treat_status, drug, answer_numeric) %>% mutate(question = "111")
s12Age <- tdi112 %>% filter(!age_range %in% c(99,100)) %>% mutate(means = 3) %>% 
  group_by(report_id, means, treat_status, drug) %>% summarise(answer_numeric = sum(answer_numeric)) %>% ungroup() %>% mutate(question = "112")
s21Age <- tdi121 %>% filter(!age_range %in% c(99,100)) %>% mutate(means = 6) %>% 
  group_by(report_id, means, treat_status, drug) %>% summarise(answer_numeric = sum(answer_numeric)) %>% ungroup() %>% mutate(question = "121")

byAgeTreatment <- s11Age %>% filter(means == 3) %>% rbind(s12Age) %>% spread(key = question, value = answer_numeric)
byAgeTreatment <- byAgeTreatment %>% mutate(diff = ifelse(apply(byAgeTreatment[ , 5:ncol(byAgeTreatment)], MARGIN=1, function(x) n_distinct(x, na.rm = T)) == 1, 0, 1),
                                            treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previous clients', 'All clients'))) 

byAgeFirstUse <- s11Age %>% filter(means == 6) %>% rbind(s21Age) %>% spread(key = question, value = answer_numeric)
byAgeFirstUse <- byAgeFirstUse %>% mutate(diff = ifelse(apply(byAgeFirstUse[ , 5:ncol(byAgeFirstUse)], MARGIN=1, function(x) n_distinct(x, na.rm = T)) == 1, 0, 1),
                                          treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previous clients', 'All clients'))) 
#### Injecting ####
injecting119 <- tdi119 %>% filter(drug == 100, route_admin == 1) %>% mutate (question = 'q119', injection = 23) %>% group_by(report_id, question, treat_status, injection) %>% summarise(nClients = sum(answer_numeric))
injecting122 <- tdi122 %>% filter(drug == 100) %>% mutate(question = 'q122') %>% group_by(report_id, question, treat_status, injection) %>% summarise(nClients = sum(answer_numeric))
injecting123 <- tdi123 %>% filter(age_range == 100, yrs_injection == 100) %>% mutate (question = 'q123', injection = 20) %>% group_by(report_id, question, treat_status, injection) %>% summarise(nClients = sum(answer_numeric))
injecting126 <- tdi126 %>% filter(tested == 100) %>% mutate(question = 'q126') %>% group_by(report_id, question, treat_status, injection) %>% summarise(nClients = sum(answer_numeric))
injecting127 <- tdi127 %>% filter(tested == 100) %>% mutate(question = 'q127') %>% group_by(report_id, question, treat_status, injection) %>% summarise(nClients = sum(answer_numeric))
injecting128 <- tdi128 %>% filter(needles == 100) %>% mutate(question = 'q128') %>% group_by(report_id, question, treat_status, injection) %>% summarise(nClients = sum(answer_numeric))

byInjection <- injecting119 %>%
  rbind(injecting122) %>%
  rbind(injecting123) %>%
  rbind(injecting126) %>%
  rbind(injecting127) %>%
  rbind(injecting128) %>%
  ungroup() %>%
  mutate(treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previously treated', 'All treatments'))) %>%
  spread(key = question, value = nClients)

byInjection <- byInjection %>% 
  mutate(diff = ifelse(apply(byInjection[ , 4:ncol(byInjection)], MARGIN=1, function(x) n_distinct(x, na.rm = T)) == 1, 0, 1))
#### Living with children ####
children110 <- tdi110 %>% filter(treat_status == 100, drug == 100, !gender == 99) %>% mutate(question = 'q110') %>% group_by(report_id, question, gender) %>% summarise(nClients = sum(answer_numeric))
children115 <- tdi115 %>% filter(drug == 100, with_children == 100) %>% mutate(question = 'q115') %>% group_by(report_id, question, gender) %>% summarise(nClients = sum(answer_numeric))

byWithChildren <- children110 %>% rbind(children115) %>%
  ungroup() %>%
  mutate(gender = ifelse(gender == 1, 'Males', ifelse(gender == 2, 'Females', 'All treatments'))) %>%
  spread(key = question, value = nClients) %>%
  gather(-report_id,-gender, key = question, value = nClients) %>%
  mutate(nClients = replace_na(nClients, 0)) %>%
  spread(key = question, value = nClients) %>%
  mutate(diff = ifelse(q110 <= q115, 1,0))

#### OST ####
ostYears <- tdi129 %>%
  filter(drug == 100, ost_status %in% c(21,22,-99), yrs_ost %in% c(-99,100)) %>%
  group_by(report_id, type_matrix) %>% summarise(nClients = sum(answer_numeric)) %>%
  spread(key = type_matrix, value = nClients) %>% ungroup()

ostYears <- ostYears %>%
  mutate(diff = ifelse(apply(ostYears[ , 2:ncol(ostYears)], MARGIN=1, function(x) n_distinct(x, na.rm = T)) == 1, 0, 1))
#### Smoked cocaine ####
smokedCocaine <- tdi119 %>%
  filter(drug == 21, route_admin == 2, gender == 100) %>%
  select(report_id, treat_status, answer_numeric) %>%
  mutate(treat_status = ifelse(treat_status == 1, 'New', ifelse(treat_status == 2, 'Previous', 'All'))) %>%
  rename(smoked = answer_numeric) %>% spread(key = treat_status, value = smoked) 
smokedCocaine <- smokedCocaine %>%
  mutate(valid = ifelse(rowSums(smokedCocaine[,2:4], na.rm = T) == 0, 0, 1))

#### Trends ####
trends <- tdiTrends %>%
  semi_join(tdiTrends %>% group_by(dsc_country, year_of_treatment) %>% summarise(yeara = sum(year_of_treatment)) %>% select(-yeara) %>% 
              mutate(rank = dense_rank(desc(year_of_treatment))) %>% filter(rank < 6)) %>%
  filter(dsc_country%in%countries$dsc_country, !type_treatment == 100, drug %in% c(10,20,30,40,50,60,70,90,99)) %>%
  group_by(dsc_country,year_of_treatment, treat_status, drug) %>% summarise(nClients = sum(answer_numeric)) %>% ungroup() %>%
  mutate(drug = ifelse(drug == 10, 'Opioids',
                ifelse(drug == 20, 'Cocaine',
                ifelse(drug == 30, 'Stimulants',
                ifelse(drug == 40, 'Hypnotics',
                ifelse(drug == 50, 'Hallucinogens',
                ifelse(drug == 60, 'Inhalants',
                ifelse(drug == 70, 'Cannabis',
                ifelse(drug == 90, 'Other', 'Missing')))))))),
         treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previously treated', 'All clients'))) %>%
  full_join(countries)

trendsAllClients <- tdiTrends %>% 
  filter(!type_treatment == 100, drug %in% c(10,20,30,40,50,60,70,90,99), !treat_status == 99) %>%
  mutate(treat_status = ifelse(treat_status == 1, 'New clients', ifelse(treat_status == 2, 'Previously treated', 'All clients'))) %>%
  group_by(dsc_country, treat_status, year_of_treatment) %>%
  summarise(nClients = sum(answer_numeric)) %>%
  full_join(countries, by = c('dsc_country' = 'dsc_country')) %>% 
  mutate(year_of_treatment = ymd(paste0(year_of_treatment, '-01-01')))

#### Loop ####
country <- countries # %>% filter(DSC_COUNTRY == 'France')
for (i in country$report_id) {
  # Filtering data ####
  cntry <- country %>% filter(report_id == i)
  v1 <- grandTotals %>% filter(report_id == i) 
  v2 <- byDrugGeneric %>% filter(report_id == i)
  v3 <- byDrug %>% filter(report_id == i)
  v4 <- bySetting %>% filter(report_id == i)
  v5 <- byGender %>% filter(report_id == i)
  v6 <- byAgeTreatment %>% filter(report_id == i)
  v7 <- byAgeFirstUse %>% filter(report_id == i)
  v8 <- byInjection %>% filter(report_id == i)
  v9 <- byWithChildren %>% filter(report_id == i)
  v10 <- ostYears %>% filter(report_id == i)
  v11 <- smokedCocaine %>% filter(report_id == i)
  v12 <- trends %>% filter(report_id == i) 
  v13 <- trendsAllClients %>% filter(report_id == i)

  # Styling ####
  docTitle <- fp_text(font.size = 18, bold = TRUE, font.family = "Calibri")
  docSection <- fp_text(font.size = 16, bold = TRUE, font.family = "Calibri", color = "gray28")
  docWarning <- fp_text(font.size = 10, bold = F, font.family = "Calibri", color = 'red')
  docError <- fp_text(font.size = 10, font.family = "Calibri", color = 'white', shading.color ="red")
  docOK <- fp_text(font.size = 10, font.family = "Calibri", color = 'white', shading.color ="blue")
  docNoData <- fp_text(font.size = 10, font.family = "Calibri", color = 'white', shading.color ="green4")
  docFootnote <- fp_text(font.size = 9, font.family = "Calibri")
  docText <- fp_text(font.size = 10, font.family = "Calibri")
  docTopic <- fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
  # 1st Page ####
  my_doc <- read_docx()
  body_add_img(my_doc, "documents/EMCDDA_MARQUE_EN_rgb.jpg", height = 0.99, width = 3.39)
  body_add_par(my_doc,"", style = "Normal")
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext("TDI Fonte Validation Report", prop = docTitle)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext(paste0(cntry$DSC_COUNTRY), prop = docSection)))
  body_add_fpar(my_doc, fpar(ftext(paste0(i), prop = docSection)))
  body_add_fpar(my_doc, fpar(ftext(paste0(Sys.time()), prop = docFootnote)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_break(my_doc, pos = "after")
  # Grand Totals ####
  body_add_fpar(my_doc, fpar(ftext('Grand Totals by Treatment Status', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('Validation(s) performed:', prop = docWarning)))
  body_add_fpar(my_doc, fpar(ftext('a) checks that the total number of clients reported in each section of the template matches. This is broken by treatment status.', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  
  if(sum(v1$valid) == 0) {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} else 
  {body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    v1 <- v1 %>% filter(valid == 1)
    body_add_par(my_doc,"", style = "Normal")
    for (j in unique(v1$treat_status)) {
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      body_add_flextable(my_doc, 
                         flextable(v1 %>% filter(treat_status == j) %>% 
                                     select(question, nClients)) %>% 
                           theme_vanilla() %>% 
                           autofit() %>% 
                           fontsize(size = 8) %>% 
                           height_all(height = .23) 
                         #%>%
                           #compose(col_key = 'nClients', pattern = "{{nClients}}", formatters = list (nClients ~ sprintf('%.00f',nClients)))
                         , align = "left")
      body_add_par(my_doc,"", style = "Normal")}}
  body_add_par(my_doc,"", style = "Normal")
  body_add_par(my_doc,"", style = "Normal")
  # Primary Drug ####
  body_add_fpar(my_doc, fpar(ftext('Primary drug by Treatment Status', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('Validation(s) performed:', prop = docWarning)))
  body_add_fpar(my_doc, fpar(ftext('a) checks that the number of clients reported in each section of the template matches by GENERIC DRUG. This is broken by treatment status.', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('b) checks that the number of clients reported in each section of the template matches by DRUG. This is broken by treatment status.', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  # Generic drug
  body_add_fpar(my_doc, fpar(ftext('a) Generic Drug', prop = docTopic)))
  body_add_par(my_doc,"", style = "Normal")
  if(sum(v2$valid) == 0) {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} else 
  {body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    v2 <- v2 %>% filter(valid == 1)
    body_add_par(my_doc,"", style = "Normal")
    for (j in unique(v2$treat_status)) {
      int_keys <- c("drug","q109","q110","q112","q113","q114","q116","q117","q118","q119","q120","q121", "q129")
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      body_add_flextable(my_doc, flextable(v2 %>% filter(treat_status == j) %>% select(-dsc_country, -report_id, -year_of_treatment, -treat_status, -valid)) %>% 
                           theme_vanilla() %>% autofit() %>% fontsize(size = 8) %>% height_all(height = .20) %>% width(width = 0.5) %>%
                           colformat_int(big.mark = ""), align = "left")
      body_add_par(my_doc,"", style = "Normal")}}
  body_add_par(my_doc,"", style = "Normal")
  # Drug
  body_add_fpar(my_doc, fpar(ftext('b) Drug', prop = docTopic)))
  body_add_par(my_doc,"", style = "Normal")
  if(sum(v3$valid) == 0) {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} else
  {body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    v3 <- v3 %>% filter(valid == 1)
    body_add_par(my_doc,"", style = "Normal")
    for (j in unique(v3$treat_status)) {
      int_keys <- c("drug","q109","q110","q112","q119","q120","q121","q129")
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      body_add_flextable(my_doc, flextable(v3 %>% filter(treat_status == j) %>% select(-dsc_country, -report_id, -year_of_treatment, -treat_status, -valid)) %>% 
                           theme_vanilla() %>% autofit() %>% fontsize(size = 8) %>% height_all(height = .20) %>% width(width = 0.5) %>%
                           colformat_int(big.mark = ""), align = "left")
      body_add_par(my_doc,"", style = "Normal")}}
  body_add_par(my_doc,"", style = "Normal")
  body_add_par(my_doc,"", style = "Normal")
  # Setting ####
  body_add_fpar(my_doc, fpar(ftext('Setting by Treatment Status', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('Validation(s) performed:', prop = docWarning)))
  body_add_fpar(my_doc, fpar(ftext('a) checks that the total number of client by setting matches in each section of the template.', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  if(sum(v4$valid) == 0) {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} else {
    body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    v4 <- v4 %>% filter(valid == 1)
    body_add_par(my_doc,"", style = "Normal")
    for (j in unique(v4$treat_status)) {
      int_keys <- c("q108","q109")
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      body_add_flextable(my_doc, flextable(v4 %>% filter(treat_status == j) %>% select(-dsc_country, -report_id, -treat_status, -valid)) %>% 
                           theme_vanilla() %>% autofit() %>% fontsize(size = 8) %>% height_all(height = .20) %>%
                           colformat_int(big.mark = ""), align = "left")
      body_add_par(my_doc,"", style = "Normal")}}
  body_add_par(my_doc,"", style = "Normal")
  # Gender ####
  body_add_fpar(my_doc, fpar(ftext('Gender by Treatment Status', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('Validation(s) performed:', prop = docWarning)))
  body_add_fpar(my_doc, fpar(ftext('a) checks that the number and males and females matches in each section of the template.', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  if(sum(v5$valid) == 0) {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} else {
    body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    v5 <- v5 %>% filter(valid == 1)
    body_add_par(my_doc,"", style = "Normal")
    for (j in unique(v5$treat_status)) {
      int_keys <- c("108","110","114")
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      body_add_flextable(my_doc, flextable(v5 %>% filter(treat_status == j) %>% select(-report_id, -treat_status, -valid)) %>% 
                           theme_vanilla() %>% autofit() %>% fontsize(size = 8) %>% height_all(height = .20) %>%
                           colformat_int(big.mark = ""), align = "left")
      body_add_par(my_doc,"", style = "Normal")}}
  body_add_par(my_doc,"", style = "Normal")
  # Age ####
  body_add_fpar(my_doc, fpar(ftext('Age by Treatment Status', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('Validation(s) performed:', prop = docWarning)))
  body_add_fpar(my_doc, fpar(ftext('a) checks that the number of clients reported in the mean ages section matches with age at entering treatment section.', prop = docText)))
  body_add_fpar(my_doc, fpar(ftext('b) checks that the number of clients reported in the mean ages section matches with age at first use section.', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  # Age at entering treatment 
  body_add_fpar(my_doc, fpar(ftext('a) Age at entering treatment', prop = docTopic)))
  body_add_par(my_doc,"", style = "Normal")
  if(sum(v6$diff) == 0) {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} else {
    body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    v6 <- v6 %>% filter(diff == 1)
    body_add_par(my_doc,"", style = "Normal")
    for (j in unique(v6$treat_status)) {
      int_keys <- c("drug","111","112")
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      body_add_flextable(my_doc, flextable(v6 %>% filter(treat_status == j) %>% select(-report_id, -means, -treat_status, -diff)) %>% 
                           theme_vanilla() %>% autofit() %>% fontsize(size = 8) %>% height_all(height = .20) %>%
                           colformat_int(big.mark = ""), align = "left")
      body_add_par(my_doc,"", style = "Normal")}}
  # Age at first use 
  body_add_fpar(my_doc, fpar(ftext('b) Age at first use', prop = docTopic)))
  body_add_par(my_doc,"", style = "Normal")
  if(dim(v7)[1] == 0) {body_add_fpar(my_doc, fpar(ftext('No data reported', prop = docNoData)))} else if (sum(v7$diff) == 0) 
  {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} else {
    body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    v7 <- v7 %>% filter(diff == 1)
    body_add_par(my_doc,"", style = "Normal")
    for (j in unique(v7$treat_status)) {
      int_keys <- c("drug","111","121")
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      body_add_flextable(my_doc, flextable(v7 %>% filter(treat_status == j) %>% select(-report_id, -means, -treat_status, -diff)) %>% 
                           theme_vanilla() %>% autofit() %>% fontsize(size = 8) %>% height_all(height = .20) %>%
                           colformat_int(big.mark = ""), align = "left")
      body_add_par(my_doc,"", style = "Normal")}}
  # Injecting ####
  body_add_fpar(my_doc, fpar(ftext('Injecting behaviour by treatment status', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('Validation(s) performed:', prop = docWarning)))
  body_add_fpar(my_doc, fpar(ftext('a) checks that the number of clients reported by injection behaviour matches across sections', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  if(dim(v8)[1] == 0) {body_add_fpar(my_doc, fpar(ftext('No data reported', prop = docNoData)))} else if (sum(v8$diff) == 0)
  {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} else {
    body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    v8 <- v8 %>% filter(diff == 1)
    body_add_par(my_doc,"", style = "Normal")
    for (j in unique(v8$treat_status)) {
      int_keys <- c("injection","q119","q122","q123","q126","q127","q128")
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      body_add_flextable(my_doc, flextable(v8 %>% filter(treat_status == j) %>% select(-report_id, -treat_status, -diff)) %>% 
                           theme_vanilla() %>% autofit() %>% fontsize(size = 8) %>% height_all(height = .20) %>%
                           colformat_int(big.mark = ""), align = "left")
      body_add_par(my_doc,"", style = "Normal")}}
  
  # With Children ####
  body_add_fpar(my_doc, fpar(ftext('Living with children by Gender', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('Validation(s) performed:', prop = docWarning)))
  body_add_fpar(my_doc, fpar(ftext('a) Only clients that have children should be reported under section Living with children. Checks that the number of clients is lower than in section Gender', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  if(dim(v9)[1] == 0) {body_add_fpar(my_doc, fpar(ftext('No data reported', prop = docNoData)))} else if (sum(v9$diff) == 0)
  {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} else {
    body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    v9 <- v9 %>% filter(diff == 1)
    body_add_par(my_doc,"", style = "Normal")
    for (j in unique(v9$gender)) {
      int_keys <- c("q110","q115")
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      body_add_flextable(my_doc, flextable(v9 %>% filter(gender == j) %>% select(-report_id, -gender, -diff)) %>% 
                           theme_vanilla() %>% autofit() %>% fontsize(size = 8) %>% height_all(height = .20) %>%
                           colformat_int(big.mark = ""), align = "left")
      body_add_par(my_doc,"", style = "Normal")}}
  body_add_par(my_doc,"", style = "Normal")
  
  # OST Years ####
  body_add_fpar(my_doc, fpar(ftext('Years since first OST', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('Validation(s) performed:', prop = docWarning)))
  body_add_fpar(my_doc, fpar(ftext('a) checks that the total number of clients for question Years since first OST matches with the sum of Ever been in OST + Currently in OST in question Primary drug by OST status', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  if(dim(v10)[1] == 0) 
    {body_add_fpar(my_doc, fpar(ftext('No data reported', prop = docNoData)))} 
  else if (sum(v10$diff) == 0)
    {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} 
  else {body_add_fpar(my_doc, fpar(ftext('The number of clients in Years since 1st OST does not matches with Ever been in OST + Currently in OST in question Primary drug by OST status', prop = docError)))}
  body_add_par(my_doc,"", style = "Normal")
  body_add_par(my_doc,"", style = "Normal")
  
  # Smoked cocaine ####
  body_add_fpar(my_doc, fpar(ftext('Smoked cocaine', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  body_add_fpar(my_doc, fpar(ftext('Validation(s) performed:', prop = docWarning)))
  body_add_fpar(my_doc, fpar(ftext('a) checks if Powder cocaine clients were reported under category "smoke / inhale". Default value should be 0.', prop = docText)))
  body_add_par(my_doc,"", style = "Normal")
  if(dim(v11)[1] == 0) 
  {body_add_fpar(my_doc, fpar(ftext('No data reported', prop = docNoData)))} 
  else if (v11$valid == 0)
  {body_add_fpar(my_doc, fpar(ftext('No errors found', prop = docOK)))} 
  else {body_add_fpar(my_doc, fpar(ftext('The report has errors', prop = docError)))
    body_add_par(my_doc,"", style = "Normal")
    int_keys <- c("All","New","Previous")
    body_add_flextable(my_doc, flextable(v11 %>% select(-report_id,-valid)) %>% theme_vanilla() %>% autofit() %>% 
                         fontsize(size = 8) %>% height_all(height = .20) %>% colformat_int(big.mark = ""), align = "left")}
  body_add_par(my_doc,"", style = "Normal")
  body_add_par(my_doc,"", style = "Normal")
  # Trends all clients ####
  body_add_fpar(my_doc, fpar(ftext('Trends in the number of clients', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  if(dim(v13)[1] == 0) {
    body_add_fpar(my_doc, fpar(ftext('No data reported', prop = docNoData)))
  }
  else {
    tGraph <- ggplot(v13 %>% filter(year_of_treatment > '2012-01-01'), aes(x=year_of_treatment, y=nClients, color = treat_status)) +
      geom_line(stat = 'identity') + geom_point() + 
      facet_wrap(~treat_status) + 
      theme(axis.title = element_blank(),
            legend.position = "none",
            axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5)) +
      scale_x_date(date_labels = '%Y', date_breaks = '1 year')
    body_add_gg(my_doc, value = tGraph, style = "centered", width = 6, height = 4)
  }
  body_add_par(my_doc,"", style = "Normal")
  body_add_par(my_doc,"", style = "Normal")
  # Trends ####
  body_add_fpar(my_doc, fpar(ftext('Trends by primary drug', prop = docSection)))
  body_add_par(my_doc,"", style = "Normal")
  if(dim(v12)[1] == 0) {body_add_fpar(my_doc, fpar(ftext('No data reported', prop = docNoData)))}
  else {
    for(j in unique(v12$treat_status))
      {
      v12_1 <- v12 %>% filter(treat_status == j)
      body_add_fpar(my_doc, fpar(ftext(paste0(j), prop = docTopic)))
      if(dim(v12_1)[1] == 0) {body_add_fpar(my_doc, fpar(ftext('No data reported', prop = docNoData)))}
      else{
        for(k in unique(v12_1$drug)) {
          v12_2 <- v12_1 %>% filter(drug == k)
          graph <- ggplot(v12_2, aes(x=year_of_treatment, y=nClients)) +
            geom_line(stat = 'identity') + geom_point() + theme(axis.title = element_blank())
          body_add_fpar(my_doc, fpar(ftext(paste0(k), prop = docTopic)))
          body_add_gg(my_doc, value = graph, width = 3, height = 2)
        }
      }
    }
  }
  # Saving the document ####
  print(my_doc, target = paste0("./outputs/",i,".docx"))
  print(i)
}
