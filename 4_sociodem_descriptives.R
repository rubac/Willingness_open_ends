
library(haven)
library(tidyverse)

library(sjlabelled)

library(labelled)

library(rio)

library(openxlsx)

library("readxl")

library("viridis")

survey_wide <- haven::read_dta("data/GIP_W59_V1.dta")

survey_wide$gender_20 <- haven::as_factor(survey_wide$gender_20)
survey_wide$year_of_birth_cat_20 <- haven::as_factor(survey_wide$year_of_birth_cat_20)
survey_wide$year_of_birth_cat_20[survey_wide$year_of_birth_cat_20=="-90. item nonresponse"] <- NA
survey_wide$year_of_birth_cat_20[survey_wide$year_of_birth_cat_20=="-80. Wert nicht plausibel"] <- NA

survey_wide$educ_school_21 = haven::as_factor(survey_wide$educ_school_21)
survey_wide$educ_job_21 = haven::as_factor(survey_wide$educ_job_21)
survey_wide$marital_status = haven::as_factor(survey_wide$marital_status_21)
survey_wide$hh_members = haven::as_factor(survey_wide$number_hh_members_21)
survey_wide$occupation = haven::as_factor(survey_wide$occupation_21)
survey_wide$hh_members = haven::as_factor(survey_wide$number_hh_members_21)
survey_wide$german_citizenship_21 = haven::as_factor(survey_wide$german_citizenship_21)
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

survey_wide$educ <- ifelse(survey_wide$educ_school_21=="1. Noch Schüler/-in" |survey_wide$educ_school_21=="2. Schule beendet ohne Abschluss" |
                             survey_wide$educ_school_21=="3. Volks-/Hauptschulabschluss bzw. Polytechnische Oberschule mit Abschluss 8. oder 9. Klasse", "low", NA)
survey_wide$educ <- ifelse(survey_wide$educ_school_21=="4. Mittlere Reife, Realschulabschluss bzw. Polytechnische Oberschule mit Abschluss 10. Klasse" |
                             survey_wide$educ_school_21=="7. Anderen Schulabschluss: Bitte tragen Sie Ihren Schulabschluss ein: __________________", "medium", survey_wide$educ)
survey_wide$educ <- ifelse(survey_wide$educ_school_21=="5. Fachhochschulreife (Abschluss einer Fachoberschule etc.)" |
                             survey_wide$educ_school_21=="6. Abitur bzw. Erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)" , "high", survey_wide$educ)

survey_wide$training <- ifelse(survey_wide$educ_job_21=="1. Noch in beruflicher Ausbildung (Berufsvorbereitungsjahr, Auszubildende/-r, Praktikant/-in, Student/-in)" |
                                 survey_wide$educ_job_21=="2. Schüler/-in und besuche eine berufsorientierte Aufbau-, Fachschule o. ä." |
                                 survey_wide$educ_job_21=="3. Keinen beruflichen Abschluss und bin nicht in beruflicher Ausbildung",
                               "No degree (yet)", NA)
survey_wide$training <- ifelse(survey_wide$educ_job_21=="4. Beruflich-betriebliche Berufsausbildung (Lehre) abgeschlossen" |
                                 survey_wide$educ_job_21=="5. Beruflich-schulische Ausbildung (Berufsfachschule, Handelsschule, Vorbereitungsdienst für den mittleren Dienst in der öffentlichen Verwaltung) abgeschlossen"|
                                 survey_wide$educ_job_21=="6. Ausbildung an einer Fachschule der DDR abgeschlossen" |
                                 survey_wide$educ_job_21=="7. Ausbildung an einer Fach-, Meister-, Technikerschule, Berufs- oder Fachakademie abgeschlossen" |
                                 survey_wide$educ_job_21=="12. Ein anderer beruflicher Abschluss, und zwar (bitte eintragen): _____________________",
                               "Vocational training", survey_wide$training)
survey_wide$training <- ifelse(survey_wide$educ_job_21=="8. Bachelor an (Fach-)Hochschule abgeschlossen" |
                                 survey_wide$educ_job_21=="9. Fachhochschulabschluss (z. B. Diplom, Master)"|
                                 survey_wide$educ_job_21=="10. Universitätsabschluss (z. B. Diplom, Magister, Staatsexamen, Master)" |
                                 survey_wide$educ_job_21=="11. Promotion",
                               "University/College degree", survey_wide$training)


gip54_quant <- survey_wide
# gip54_quant <- read.csv("GIP_W54_V1.csv")

# # Recode NAs, 1s and 0s
# gip54_quant[gip54_quant == -80] <- NA
# gip54_quant[gip54_quant == -90] <- NA
# gip54_quant[gip54_quant == "-80. Wert nicht plausibel"] <- NA
# gip54_quant[gip54_quant == "-90. item nonresponse"] <- NA
# gip54_quant[gip54_quant == "0. item not checked"] <- as.integer(0)
# gip54_quant[gip54_quant == "1. item checked"] <- as.integer(1)
# 
# gip54_quant[gip54_quant == "-99. WeiÃŸ nicht"] <- NA
# gip54_quant[gip54_quant == "2. Nein"] <- 0
# gip54_quant[gip54_quant == "1. Ja"] <- 1


# Renaming / Adding mutated variables
gip54_quant <- gip54_quant %>%
  transmute(
    birthyear = year_of_birth_cat_20,
    gender = gender_20,
    citizenship = german_citizenship_21,
    school_education = educ,
    job_education = training
  )


# gip54_quant <- gip54_quant %>%
#   mutate(school_education2 = case_when(school_education == "1. Noch Schüler/-in" ~ "1. Noch Schüler/-in",
#                                        school_education == "2. Schule beendet ohne Abschluss" ~ "2. Schule beendet ohne Abschluss",
#                                        school_education == "3. Volks-/Hauptschulabschluss bzw. Polytechnische Oberschule mit Abschluss 8. oder 9. Klasse" ~ "3. Volks-/Hauptschulabschluss bzw. Polytechnische Oberschule mit Abschluss 8. oder 9. Klasse",
#                                        school_education == "4. Mittlere Reife, Realschulabschluss bzw. Polytechnische Oberschule mit Abschluss 10. Klasse" ~ "4. Mittlere Reife, Realschulabschluss bzw. Polytechnische Oberschule mit Abschluss 10. Klasse",
#                                        school_education == "5. Fachhochschulreife (Abschluss einer Fachoberschule etc.)" | school_education == "6. Abitur bzw. Erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)" ~ "5. Abitur/Allgemeine oder fachgebundene Hochschulreife/Fachhochschulreife",
#                                        school_education == "7. Anderen Schulabschluss: Bitte tragen Sie Ihren Schulabschluss ein: __________________" ~ "6. Anderen Schulabschluss: Bitte tragen Sie Ihren Schulabschluss ein: __________________"))


gip54_quant <- gip54_quant %>%
  mutate(gender_eng = case_when(gender == "1. männlich" ~ "Male",
                                gender == "2. weiblich" ~ "Female"),
         citizenship_eng = case_when(citizenship == "1. Ja, nur die deutsche Staatsangehörig" ~ "Only German citizenship",
                                     citizenship == "2. Ja, die deutsche Staatsangehörigkeit" ~ "German and at least one other citizenship",
                                     citizenship == "3. Nein, habe eine andere Staatsangehörigkeit" ~ "Other citizenship"),
         age = case_when(birthyear == "1. 1935-1939" | birthyear == "2. 1940-1944" | birthyear == "3. 1945-1949" | birthyear == "4. 1950-1954" | birthyear == "5. 1955-1959" ~ "62 years and older",
                         birthyear == "6. 1960-1964" | birthyear == "7. 1965-1969" | birthyear == "8. 1970-1974" ~ "47 - 61 years",
                         birthyear == "9. 1975-1979" | birthyear == "10. 1980-1984" | birthyear == "11. 1985-1989" | birthyear == "12. 1990-1994" ~ "27 - 46 years",
                         birthyear == "13. 1995-1999" | birthyear == "14. 2000 und später" ~ "26 years and younger"),
         isced2011 = factor(case_when(((school_education == "1. Noch Schüler/-in" |
                                          school_education == "2. Schule beendet ohne Abschluss" |
                                          school_education == "3. Volks-/Hauptschulabschluss bzw. Polytechnische Oberschule mit Abschluss 8. oder 9. Klasse" |
                                          school_education == "4. Mittlere Reife, Realschulabschluss bzw. Polytechnische Oberschule mit Abschluss 10. Klasse" |
                                          school_education == "6. Anderen Schulabschluss: Bitte tragen Sie Ihren Schulabschluss ein: __________________") &
                                         (gip54_quant$job_education == "1. Noch in beruflicher Ausbildung (Berufsvorbereitungsjahr, Auszubildende/-r, Praktikant/-in, Student/-in)" |
                                            gip54_quant$job_education == "2. Schüler/-in und besuche eine berufsorientierte Aufbau-, Fachschule o. ä." |
                                            gip54_quant$job_education == "3. Keinen beruflichen Abschluss und bin nicht in beruflicher Ausbildung" |
                                            gip54_quant$job_education == "12. Ein anderer beruflicher Abschluss, und zwar (bitte eintragen): _____________________")) ~ "Low",
                                      ((school_education == "5. Abitur/Allgemeine oder fachgebundene Hochschulreife/Fachhochschulreife" &
                                          job_education != "6. Ausbildung an einer Fachschule der DDR abgeschlossen" &
                                          job_education != "7. Ausbildung an einer Fach-, Meister-, Technikerschule, Berufs- oder Fachakademie abgeschlossen" &
                                          job_education != "8. Bachelor an (Fach-)Hochschule abgeschlossen" &
                                          job_education != "9. Fachhochschulabschluss (z. B. Diplom, Master)" &
                                          job_education != "10. Universitätsabschluss (z. B. Diplom, Magister, Staatsexamen, Master)" &
                                          job_education != "11. Promotion") |
                                         job_education == "4. Beruflich-betriebliche Berufsausbildung (Lehre) abgeschlossen" |
                                         job_education == "5. Beruflich-schulische Ausbildung (Berufsfachschule, Handelsschule, Vorbereitungsdienst für den mittleren Dienst in der öffentlichen Verwaltung) abgeschlossen") ~ "Medium",
                                      (job_education == "6. Ausbildung an einer Fachschule der DDR abgeschlossen" |
                                         job_education == "7. Ausbildung an einer Fach-, Meister-, Technikerschule, Berufs- oder Fachakademie abgeschlossen" |
                                         job_education == "8. Bachelor an (Fach-)Hochschule abgeschlossen" |
                                         job_education == "9. Fachhochschulabschluss (z. B. Diplom, Master)" |
                                         job_education == "10. Universitätsabschluss (z. B. Diplom, Magister, Staatsexamen, Master)" |
                                         job_education == "11. Promotion") ~ "High"),
                            levels = c("Low", "Medium", "High")))


# table(gip54_quant$isced2011, useNA = "always")
table(gip54_quant$birthyear, useNA = "always")
table(gip54_quant$gender_eng, useNA = "always")

table(gip54_quant$age)
