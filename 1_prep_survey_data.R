# read in datasets
setwd("~/bwSyncShare/GIP offene fragen/")

para_data <- haven::read_dta("data/GIP_paradata_W59_V1.dta")
coded_data <- haven::read_dta("data/GIP_W59_A01_open_coded.dta")
open_text <- haven::read_dta("data/GIP_W59_A01_open_clean.dta")
survey_wide <- haven::read_dta("data/GIP_W59_V1.dta")

table(haven::as_factor(para_data$compl))

library(tidyverse)
summary(para_data$interviewLength)

survey_wide <- survey_wide %>% 
  select(-c(hhid_g, pid)) %>% 
  select(-c(RM59001:CF59106_h)) %>% 
  select(-c(BL59023:QE59008_TXT_na)) %>% 
  rename(
    hypertension = AP59001_a,
    allergy = AP59001_b,
    backpain = AP59001_c,
    sleepdisorder = AP59001_d,
    joint = AP59001_e,
    depression = AP59001_f,
    migraine = AP59001_g,
    heart = AP59001_h,
    bronchitis = AP59001_i,
    diabetes = AP59001_j,
    osteoporose = AP59001_k,
    liver = AP59001_l,
    asthma = AP59001_m,
    stroke = AP59001_n,
    cancer = AP59001_o,
    nodisease = AP59001_p
    
  )

survey_wide$gender <- haven::as_factor(survey_wide$gender_20)
survey_wide$year_birth <- haven::as_factor(survey_wide$year_of_birth_cat_20)
survey_wide$year_birth[survey_wide$year_birth=="-90. item nonresponse"] <- NA
survey_wide$year_birth[survey_wide$year_birth=="-80. Wert nicht plausibel"] <- NA
survey_wide$year_birth2 <- as.numeric(survey_wide$year_birth)-5
survey_wide$year_birth2 <- as.numeric(survey_wide$year_birth2)
survey_wide$educ_school = haven::as_factor(survey_wide$educ_school_21)
survey_wide$educ_job = haven::as_factor(survey_wide$educ_job_21)
survey_wide$marital_status = haven::as_factor(survey_wide$marital_status_21)
survey_wide$hh_members = haven::as_factor(survey_wide$number_hh_members_21)
survey_wide$occupation = haven::as_factor(survey_wide$occupation_21)
survey_wide$hh_members = haven::as_factor(survey_wide$number_hh_members_21)
survey_wide$german = haven::as_factor(survey_wide$german_citizenship_21)
survey_wide$internetuse = haven::as_factor(survey_wide$internet_usage_20)
survey_wide$state = haven::as_factor(survey_wide$state_21)
survey_wide$sample = haven::as_factor(survey_wide$sample)
survey_wide$onlinestatus = haven::as_factor(survey_wide$online_status_wave)
survey_wide$exp_order = haven::as_factor(survey_wide$expAP59001)
survey_wide$block_rand = haven::as_factor(survey_wide$rndAP59002)
survey_wide$AP59002 = haven::as_factor(survey_wide$AP59002)
survey_wide$AP59004 = haven::as_factor(survey_wide$AP59004)
survey_wide$AP59006 = haven::as_factor(survey_wide$AP59006)
survey_wide$expAP59002 = haven::as_factor(survey_wide$expAP59002)
survey_wide$expAP59004 = haven::as_factor(survey_wide$expAP59004)
survey_wide$expAP59006 = haven::as_factor(survey_wide$expAP59006)
survey_wide$effort = haven::as_factor(survey_wide$AP59008)
survey_wide$AP59008 <- NULL

survey_wide$educ <- ifelse(survey_wide$educ_school=="1. Noch Schüler/-in" |survey_wide$educ_school=="2. Schule beendet ohne Abschluss" |
                             survey_wide$educ_school=="3. Volks-/Hauptschulabschluss bzw. Polytechnische Oberschule mit Abschluss 8. oder 9. Klasse", "low", NA)
survey_wide$educ <- ifelse(survey_wide$educ_school=="4. Mittlere Reife, Realschulabschluss bzw. Polytechnische Oberschule mit Abschluss 10. Klasse" |
                             survey_wide$educ_school=="7. Anderen Schulabschluss: Bitte tragen Sie Ihren Schulabschluss ein: __________________", "medium", survey_wide$educ)
survey_wide$educ <- ifelse(survey_wide$educ_school=="5. Fachhochschulreife (Abschluss einer Fachoberschule etc.)" |
                             survey_wide$educ_school=="6. Abitur bzw. Erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)" , "high", survey_wide$educ)

survey_wide$training <- ifelse(survey_wide$educ_job=="1. Noch in beruflicher Ausbildung (Berufsvorbereitungsjahr, Auszubildende/-r, Praktikant/-in, Student/-in)" |
                                 survey_wide$educ_job=="2. Schüler/-in und besuche eine berufsorientierte Aufbau-, Fachschule o. ä." |
                                 survey_wide$educ_job=="3. Keinen beruflichen Abschluss und bin nicht in beruflicher Ausbildung",
                               "No degree (yet)", NA)
survey_wide$training <- ifelse(survey_wide$educ_job=="4. Beruflich-betriebliche Berufsausbildung (Lehre) abgeschlossen" |
                                 survey_wide$educ_job=="5. Beruflich-schulische Ausbildung (Berufsfachschule, Handelsschule, Vorbereitungsdienst für den mittleren Dienst in der öffentlichen Verwaltung) abgeschlossen"|
                                 survey_wide$educ_job=="6. Ausbildung an einer Fachschule der DDR abgeschlossen" |
                                 survey_wide$educ_job=="7. Ausbildung an einer Fach-, Meister-, Technikerschule, Berufs- oder Fachakademie abgeschlossen" |
                                 survey_wide$educ_job=="12. Ein anderer beruflicher Abschluss, und zwar (bitte eintragen): _____________________",
                               "Vocational training", survey_wide$training)
survey_wide$training <- ifelse(survey_wide$educ_job=="8. Bachelor an (Fach-)Hochschule abgeschlossen" |
                                 survey_wide$educ_job=="9. Fachhochschulabschluss (z. B. Diplom, Master)"|
                                 survey_wide$educ_job=="10. Universitätsabschluss (z. B. Diplom, Magister, Staatsexamen, Master)" |
                                 survey_wide$educ_job=="11. Promotion",
                               "University/College degree", survey_wide$training)



survey_wide <- survey_wide %>% 
  rename(don.blood = AP59009_a,
         don.sen = AP59009_b,
         don.med = AP59009_c,
         don.other = AP59009_d,
         don.nothing = AP59009_e)

# merge NLP coded open q responses
survey_wide <- merge(survey_wide, coded_data, by="id_g")

#remove old soc dem vars
survey_wide = survey_wide %>% 
  select(-c(gender_20, year_of_birth_cat_20, educ_school_21, educ_job_21,
            marital_status_21, number_hh_members_21, german_citizenship_21,
            internet_usage_20, state_21, online_status_wave, occupation_21))
survey_wide$nonsenser[survey_wide$nonsenser==-90] <- NA
rm(coded_data)

# whether the disease questions were asked before the vignette experiment or after
survey_wide$disease_q_first <- ifelse(survey_wide$exp_order=="1. Gruppe 1", T, NA)
survey_wide$disease_q_first <- ifelse(survey_wide$exp_order=="2. Gruppe 2", F, survey_wide$disease_q_first)
table(survey_wide$disease_q_first)
survey_wide$exp_order <- NULL
survey_wide$expAP59001 <- NULL

# The order in which the three vignettes per respondent were asked
survey_wide$block_order = ifelse(survey_wide$block_rand=="AP59006,AP59004,AP59002", "Bio,Med,Sen", NA)
survey_wide$block_order = ifelse(survey_wide$block_rand=="AP59006,AP59002,AP59004", "Bio,Sen,Med", survey_wide$block_order)
survey_wide$block_order = ifelse(survey_wide$block_rand=="AP59004,AP59006,AP59002", "Med,Bio,Sen", survey_wide$block_order)
survey_wide$block_order = ifelse(survey_wide$block_rand=="AP59004,AP59002,AP59006", "Med,Sen,Bio", survey_wide$block_order)
survey_wide$block_order = ifelse(survey_wide$block_rand=="AP59002,AP59004,AP59006", "Sen,Med,Bio", survey_wide$block_order)
survey_wide$block_order = ifelse(survey_wide$block_rand=="AP59002,AP59006,AP59004", "Sen,Bio,Med", survey_wide$block_order)

table(survey_wide$block_order)
survey_wide$block_rand <- NULL
survey_wide$rndAP59002 <- NULL



# willingness ratings
survey_wide <- survey_wide %>% 
  rename(willingness_sen = AP59002,
         willingness_med = AP59004,
         willingness_bio = AP59006)

# experimental variation regarding the recipient 
survey_wide$recipient <- ifelse(survey_wide$expAP59002=="5. Gruppe 5"|survey_wide$expAP59002=="6. Gruppe 6", "uni", NA)
survey_wide$recipient <- ifelse(survey_wide$expAP59002=="3. Gruppe 3"|survey_wide$expAP59002=="4. Gruppe 4", "priv", survey_wide$recipient)
survey_wide$recipient <- ifelse(survey_wide$expAP59002=="1. Gruppe 1"|survey_wide$expAP59002=="2. Gruppe 2", "agency", survey_wide$recipient)

# experimental variation regarding the purpose 
survey_wide$purpose <- ifelse(survey_wide$expAP59002=="4. Gruppe 4"|survey_wide$expAP59002=="6. Gruppe 6"|survey_wide$expAP59002=="2. Gruppe 2", "pers", NA)
survey_wide$purpose <- ifelse(survey_wide$expAP59002=="3. Gruppe 3"|survey_wide$expAP59002=="5. Gruppe 5"|survey_wide$expAP59002=="1. Gruppe 1", "pub", survey_wide$purpose)

# remove the old vars
survey_wide$expAP59002 <- NULL
survey_wide$expAP59004 <- NULL
survey_wide$expAP59006 <- NULL

# Robustness checks: speeders. So far, there are many NAs in the interviewLength variable
survey_wide <- merge(survey_wide, para_data[ , c("id_g", "interviewLength")], by = "id_g")
median(survey_wide$interviewLength, na.rm =T)
survey_wide$speeder <- ifelse(survey_wide$interviewLength < 0.6*median(survey_wide$interviewLength, na.rm = T),
                            1,
                            0)
table(survey_wide$speeder)

# Robustness check 2: self-reported attentiveness --- if R reported second to last or last cat on 7 pt scale
table(survey_wide$effort)
survey_wide$manip_fail <- ifelse(survey_wide$effort=="6"|survey_wide$effort=="7. Überhaupt nicht genau",
                              T,
                              NA)
survey_wide$manip_fail <- ifelse(survey_wide$effort=="1. Sehr genau"|survey_wide$effort=="2"|survey_wide$effort=="3"|survey_wide$effort=="4"|survey_wide$effort=="5",
                                 F,
                                 survey_wide$manip_fail)
table(survey_wide$manip_fail, survey_wide$effort)

## set negative values to missing: they usually indicate item nonresponse
## for numerics
fix_missing <- function(x, na.value) {
  x[x == na.value] <- NA
  x
}
lapply(survey_wide, class)
survey_wide[4:19] <- lapply(survey_wide[4:19], fix_missing, -90) 
survey_wide[23:27] <- lapply(survey_wide[23:27], fix_missing, -90) 

survey_wide[20:22] <- lapply(survey_wide[20:22], fix_missing, "-90. item nonresponse") 
survey_wide[28:29] <- lapply(survey_wide[28:29], fix_missing, "-90. item nonresponse") 
survey_wide[31:40] <- lapply(survey_wide[31:40], fix_missing, "-90. item nonresponse") 

survey_wide[20:22] <- lapply(survey_wide[20:22], fix_missing, "-99. Weiß nicht") 
survey_wide[28:29] <- lapply(survey_wide[28:29], fix_missing, "-99. Weiß nicht") 
survey_wide[31:40] <- lapply(survey_wide[31:40], fix_missing, "-99. Weiß nicht") 

survey_wide[20:22] <- lapply(survey_wide[20:22], fix_missing, "-80. Wert nicht plausibel") 
survey_wide[28:29] <- lapply(survey_wide[28:29], fix_missing, "-80. Wert nicht plausibel") 
survey_wide[31:40] <- lapply(survey_wide[31:40], fix_missing, "-80. Wert nicht plausibel") 

# drop unused factor levels
survey_wide[20:22] <- lapply(survey_wide[20:22], factor) 
survey_wide[28:29] <- lapply(survey_wide[28:29], factor) 
survey_wide[31:40] <- lapply(survey_wide[31:40], factor) 

survey_wide$sample <- factor(survey_wide$sample)

# build sum score of medical history
survey_wide22 <- survey_wide %>% 
  select(hypertension:cancer) %>% 
  mutate(sum_med_hist = rowSums(., na.rm = T))
survey_wide$sum_med_hist = survey_wide22$sum_med_hist
rm(survey_wide22)
survey_wide$sum_med_hist <- ifelse(is.na(survey_wide$asthma), NA, survey_wide$sum_med_hist)


# build sum score of donated data
survey_wide22 <- survey_wide %>% 
  select(don.blood:don.other) %>% 
  mutate(sum_don_hist = rowSums(., na.rm = T))
survey_wide$sum_don_hist = survey_wide22$sum_don_hist
rm(survey_wide22)
survey_wide$sum_don_hist <- ifelse(is.na(survey_wide$don.blood), NA, survey_wide$sum_don_hist)



## reshape to long format
surv_long <- reshape(survey_wide,
                direction = "long",
                varying = c("willingness_sen", "willingness_med", "willingness_bio"),
                timevar = "dtype",
                times = c("Sen", "Med", "Bio"),
                v.names = "willingness",
                idvar = "id_g")

surv_long$open_q_dtype <- str_sub(surv_long$block_order, 1, 1)
surv_long$open_q_dtype <- ifelse(surv_long$open_q_dtype=="B", "Bio", surv_long$open_q_dtype)
surv_long$open_q_dtype <- ifelse(surv_long$open_q_dtype=="M", "Med", surv_long$open_q_dtype)
surv_long$open_q_dtype <- ifelse(surv_long$open_q_dtype=="S", "Sen", surv_long$open_q_dtype)


survey_wide <- survey_wide %>% 
  arrange(predicted) %>% 
  mutate(open_ended = as.character(factor(predicted, levels=1:8, 
                                     labels= c("Data protection and privacy", 
                                               "(Lack of) trust", 
                                               "Missing information", 
                                               "Purpose of collected data", 
                                               "Recipient", 
                                               "Other", 
                                               "Unclear", 
                                               "Additional conditions"))))



table(survey_wide$open_ended, survey_wide$predicted)

surv_long$q_order = ifelse(surv_long$dtype==str_sub(surv_long$block_order, 1,3), "first", NA)
surv_long$q_order = ifelse(surv_long$dtype==str_sub(surv_long$block_order, 5,7), "second", surv_long$q_order)
surv_long$q_order = ifelse(surv_long$dtype==str_sub(surv_long$block_order, 9,11), "third", surv_long$q_order)

table(surv_long$q_order)

saveRDS(surv_long, "data/surv_long.rds")
saveRDS(survey_wide, "data/surv_wide.rds")
