library(haven)
## orig GIP Data
open_ends_orig <- read_dta("data/GIP_W59_A01_open_clean.dta")

## Orig GIP Data with our predictions (predictions where added in python script
## after loading the original GIP data from above)
open_ends_pred <- read.csv("data/coded_all_responses.csv")
open_ends_pred$X <- NULL

## Make sure that text in orig GIP data is always the same as in 
## our predicted data
open_ends_pred$orig_text <- open_ends_orig$openanswer
everything_OK <- ifelse(open_ends_pred$openanswer == open_ends_pred$orig_text, T, F)
table(everything_OK)
## Yay!

table(open_ends_pred$predicted)


open_ends_pred$open_ended_pred_char <- ifelse(open_ends_pred$predicted=="1", "(Lack of) trust", NA)
open_ends_pred$open_ended_pred_char <- ifelse(open_ends_pred$predicted=="7", "Additional conditions", open_ends_pred$open_ended_pred_char)
open_ends_pred$open_ended_pred_char <- ifelse(open_ends_pred$predicted=="2", "Missing information", open_ends_pred$open_ended_pred_char)
open_ends_pred$open_ended_pred_char <- ifelse(open_ends_pred$predicted=="0", "Data protection and privacy", open_ends_pred$open_ended_pred_char)
open_ends_pred$open_ended_pred_char <- ifelse(open_ends_pred$predicted=="3", "Purpose of collected data", open_ends_pred$open_ended_pred_char)
open_ends_pred$open_ended_pred_char <- ifelse(open_ends_pred$predicted=="4", "Recipient", open_ends_pred$open_ended_pred_char)
open_ends_pred$open_ended_pred_char <- ifelse(open_ends_pred$predicted=="5", "Other", open_ends_pred$open_ended_pred_char)
open_ends_pred$open_ended_pred_char <- ifelse(open_ends_pred$predicted=="6", "Unclear", open_ends_pred$open_ended_pred_char)
table(open_ends_pred$open_ended_pred_char)


write_dta(open_ends_pred, "data/dateiname.dta")


xx <- read_dta("data/dateiname.dta")
