library(tidyverse)
library(psych)
library(lme4)
library(lmerTest)
library(plm)
library(pglm)
library(ordinal)
library(stargazer)
library(officer)
library(flextable)
library(multilevelTools)
library(xtable)
library(ggplot2)
library(viridis)
# Load data
setwd("~/bwSyncShare/GIP offene fragen/")

surv_long <- readRDS("data/surv_long.rds")
surv_wide <- readRDS("data/surv_wide.rds")

## Means of outcome for each vignette

surv_long$y <- as.numeric(surv_long$willingness)

tab <- surv_long %>%
  group_by(dtype, purpose, recipient) %>%
  summarise(Mean = mean(y, na.rm = TRUE),
            Median = median(y, na.rm = TRUE),
            SD = sd(y, na.rm = TRUE),
            ci = list(mean_cl_normal(y) %>%
                        rename(Mean2 = y, Lower = ymin, Upper = ymax)),
            obs = n()) %>%
  unnest(cols = c(ci)) %>%
  select(-Mean2) %>% 
  filter(!is.na(purpose))


tabflex <- flextable(tab) %>% colformat_double(digits = 2)

word_exp <- read_docx()
word_exp <- body_add_flextable(word_exp, tabflex)
print(word_exp, "tables/desc_vignettes.docx")

summary(tab$Mean)
print(xtable(tab, type = "latex"), file = "tables/means.tex")

### Outcome continuous - random effects

c_icc <- iccMixed("y", "id_g", surv_long)

# Empty Model
m_0 <- lmer(y ~ 1 + (1|id_g), 
                  data = surv_long)

# Main effects model (H1 to H3)
m_1 <- lmer(y ~ 1 + dtype + recipient + purpose + q_order + (1|id_g),
                  data = surv_long)

# Main effects plus interaction of recipient and purpose (H4)
m_2 <- lmer(y ~ 1 + dtype + recipient*purpose + q_order + (1|id_g), 
                  data = surv_long)

# Main effects plus sum_med_history main effect (H5)
m_3 <- lmer(y ~ 1 + dtype + recipient + q_order + sum_med_hist*purpose + (1|id_g), 
                  data = surv_long)

######## No difference compared to models that do not differentiate between dtypes.
surv_long_med <- surv_long %>%
  filter(dtype=="Med")

surv_long_bio <- surv_long %>%
  filter(dtype=="Bio")

surv_long_sen <- surv_long %>%
  filter(dtype=="Sen")

# lm() because there is only one vignette per R in this case
m_3m <- lm(y ~ 1 + recipient + q_order + sum_med_hist*purpose,
                  data = surv_long_med)

m_3b <- lm(y ~ 1 + recipient + q_order + sum_med_hist*purpose,
            data = surv_long_bio)

m_3s <- lm(y ~ 1 + recipient + q_order + sum_med_hist*purpose,
            data = surv_long_sen)
summary(m_3m)
summary(m_3b)
summary(m_3s)

# ## Include donation history variables (H6.2) 
m_4 <- lmer(y ~ 1 + dtype + recipient + purpose + q_order + sum_don_hist + (1|id_g), 
                    data = surv_long)

# ## Include donation history variables and run separate models (H6.1) -- med

m_4s <- lm(y ~ 1 + recipient + purpose + q_order + don.sen, 
                    data = surv_long_sen)

# ## Include donation history variables and run separate models (H6.1) -- bio 
m_4b <- lm(y ~ 1 + recipient + purpose + q_order + don.blood, 
                    data = surv_long_bio)

# ## Include donation history variables and run separate models (H6.1) -- sen 
m_4m <- lm(y ~ 1 + recipient + purpose + q_order + don.med, 
                    data = surv_long_med)

### include soc dem
m_5 <- lmer(y ~ 1 + dtype + recipient + purpose + q_order + gender + german + 
              training + year_birth2 + (1|id_g),
                  data = surv_long)

### include soc dem
m_6 <- lmer(y ~ 1 + dtype + recipient*purpose + q_order + sum_med_hist*purpose +sum_don_hist+ gender + german + 
              training + year_birth2 + (1|id_g),
            data = surv_long)

summary(m_6)

saveRDS(m_0, file = "models/m_0.rds")
saveRDS(m_1, file = "models/m_1.rds")
saveRDS(m_2, file = "models/m_2.rds")
saveRDS(m_3, file = "models/m_3.rds")
saveRDS(m_3m, file = "models/m_3m.rds")
saveRDS(m_3b, file = "models/m_3b.rds")
saveRDS(m_3s, file = "models/m_3s.rds")
saveRDS(m_4, file = "models/m_4.rds")
saveRDS(m_4b, file = "models/m_4b.rds")
saveRDS(m_4m, file = "models/m_4m.rds")
saveRDS(m_4s, file = "models/m_4s.rds")
saveRDS(m_5, file = "models/m_5.rds")
saveRDS(m_6, file = "models/m_6.rds")

class(m_0) <- "lmerMod"
class(m_1) <- "lmerMod"
class(m_2)  <- "lmerMod"
class(m_3)  <- "lmerMod"
class(m_4)  <- "lmerMod"
class(m_5)  <- "lmerMod"
class(m_6)  <- "lmerMod"


stargazer(m_0, m_1, m_2, m_3, m_4, m_5,
          report = ('vcsp'),
          type = "latex",
          title = "Linear multilevel regression models",
          column.labels = c("Empty model", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          dep.var.labels = "Willingness rating",
          initial.zero = F,
          digits = 3,
          notes.align = "l",
          label = "tab:reg_res_1",
          digits.extra = 0,
          intercept.bottom = T,
          covariate.labels = c("Medical records", "Sensor data",
                               "Private company",
                               "University",
                               "Public benefit",
                               "Medical history*Public benefit",
                               "Second question", "Third question",
                               "Private company*Public benefit",
                               "University*Public benefit",
                               "Medical history",
                               "Donation history",
                               "Female",
                               "German citizenship",
                               "Other citizenship",
                               "University/College degree",
                               "Vocational training",
                               "Year of birth",
                               "Intercept"))

stargazer(m_6,
          report = ('vcsp'),
          type = "latex",
          title = "Linear multilevel regression models",
          column.labels = c("Model 6"),
          dep.var.labels = "Willingness rating",
          initial.zero = F,
          digits = 3,
          notes.align = "l",
          label = "tab:reg_res_1",
          digits.extra = 0,
          intercept.bottom = T,
          covariate.labels = c("Medical records", "Sensor data",
                               "Private company",
                               "University",
                               "Public benefit",
                               "Medical history*Public benefit",
                               "Second question", "Third question",
                               "Private company*Public benefit",
                               "University*Public benefit",
                               "Medical history",
                               "Donation history",
                               "Female",
                               "German citizenship",
                               "Other citizenship",
                               "University/College degree",
                               "Vocational training",
                               "Year of birth",
                               "Intercept"))

stargazer(m_3m, m_3b, m_3s, m_4b, m_4m, m_4s,
          report = ('vcsp'),
          type = "latex",
          title = "Linear regression models",
          column.labels = c("Model 3, Medical records", "Model 3, Biomarker",  "Model 3, Sensor data", "Model 4 Biomarker", "Model 4 Medical records", "Model 4 Sensor data"),
          dep.var.labels = "Willingness rating",
          initial.zero = F,
          digits = 3,
          notes.align = "l",
          label = "tab:reg_res_2",
          digits.extra = 0,
          intercept.bottom = T,
          covariate.labels = c("Private company",
                               "University",
                               "Second question", "Third question",
                               "Medical history",
                               "Donated blood",                               
                               "Donated medical records",
                               "Donated sensor data",
                               "Public benefit",
                               "Medical history*Public benefit",
                               "Intercept")
          
)


basic_regression_int.df <- rbind(rownames_to_column(as.data.frame(coef(summary((m_1))))), # Modell 1
                                 rownames_to_column(as.data.frame(coef(summary((m_2))))), # Modell 2
                                 rownames_to_column(as.data.frame(coef(summary((m_3))))), # Modell 2
                                 rownames_to_column(as.data.frame(coef(summary((m_4))))), # Modell 2
                                 rownames_to_column(as.data.frame(coef(summary((m_5)))))) # Modell 4
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="dtypeMed", "Medical records",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="dtypeSen", "Sensor data",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="gender2. weiblich", "Female",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="recipientpriv", "Private company",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="recipientuni", "University",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="purposepub", "Public benefit",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="q_ordersecond", "Second question",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="q_orderthird", "Third question",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="recipientpriv:purposepub", "Private company*Public benefit",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="recipientuni:purposepub", "University*Public benefit",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="sum_med_hist:purposepub", "Medical history*Public benefit",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="sum_don_hist", "Donation history",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="sum_med_hist", "Medical history",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="german2. Ja, die deutsche Staatsangehörigkeit", "German and second citizenship",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="german3. Nein, habe eine andere Staatsangehörigkeit", "Other citizenship",basic_regression_int.df$rowname)

names(basic_regression_int.df)[1] <- "name"
names(basic_regression_int.df)[3] <- "SE"
basic_regression_int.df <- basic_regression_int.df[-c(1,9,19,29,38),] # Hier streiche ich nur die Interzepte raus
basic_regression_int.df$model <- c(rep("M1", 7), # Extra Spalte, um die Modelle voneinander später mit ggplot unterscheiden zu können. Wiederholungen entsprechen der Anzahl der Koeffizienten im Modell
                                   rep("M2", 9),
                                   rep("M3", 9),
                                   rep("M4", 8),
                                   rep("M5", 13))
basic_regression_int.df <- basic_regression_int.df %>% 
  filter(name != "Female") %>% 
  filter(name != "German and second citizenship") %>% 
  filter(name != "year_birth2") %>% 
  filter(name != "Other citizenship") %>% 
  filter(name != "trainingUniversity/College degree") %>% 
  filter(name != "trainingVocational training")


table(basic_regression_int.df$name)

basic_regression_int.df$name <- str_wrap(basic_regression_int.df$name, width = 10)
basic_regression_int.df$name <- factor(basic_regression_int.df$name,      # Reordering group factor levels
                                       levels = c("Medical\nrecords", "Sensor\ndata", "Private\ncompany", "University",
                                                  "Public\nbenefit", "Second\nquestion", "Third\nquestion", "Private\ncompany*Public\nbenefit",
                                                  "University*Public\nbenefit", "Medical\nhistory", "Medical\nhistory*Public\nbenefit",
                                                  "Donation\nhistory"))

table(basic_regression_int.df$name)
basic_regression_int.df$model <- factor(basic_regression_int.df$model, levels = c("M1", "M2", "M3", "M4", "M5"))


fig.basic_int <- ggplot(basic_regression_int.df,
                        aes(y = Estimate, x = model)) +
  geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)),
                lwd=0.5, width=0.4, color = "grey66") +
  geom_point(aes(color = model), size = 3) +
  geom_text(aes(label = round(Estimate,2)),
            hjust = 0.5,
            vjust = -0.85,
            size = 2.3
  ) +
  scale_color_viridis(discrete = TRUE, name = "Model:") + #dafür musst du library(viridis) laden bzw. installieren. Ist nur für die Farben
  ylim(-.5,.4) +
  geom_hline(yintercept = 0) +
  facet_wrap( ~ name, nrow = 5, strip.position = "left") +
  xlab("") +
  ylab("Coefficients") +
  ggtitle("Outcome: Willingness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y =  element_blank(),
        axis.text.y =  element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        strip.text.y = element_text(size = 8)) +
  coord_flip()
  
ggsave("plots/regressions_one.eps", fig.basic_int, device = cairo_ps, width = 10, height = 10)

### plot for the lm() models
basic_regression_int.df <- rbind(rownames_to_column(as.data.frame(coef(summary((m_3m))))), # Modell 1
                                 rownames_to_column(as.data.frame(coef(summary((m_3b))))), # Modell 2
                                 rownames_to_column(as.data.frame(coef(summary((m_3s))))), # Modell 2
                                 rownames_to_column(as.data.frame(coef(summary((m_4m))))), # Modell 2
                                 rownames_to_column(as.data.frame(coef(summary((m_4b))))), # Modell 2
                                 rownames_to_column(as.data.frame(coef(summary((m_4s)))))) # Modell 4
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="dtypeMed", "Medical records",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="dtypeSen", "Sensor data",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="gender2. weiblich", "Female",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="recipientpriv", "Private company",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="recipientuni", "University",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="purposepub", "Public benefit",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="q_ordersecond", "Second question",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="q_orderthird", "Third question",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="recipientpriv:purposepub", "Private company*Public benefit",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="recipientuni:purposepub", "University*Public benefit",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="sum_med_hist:purposepub", "Medical history*Public benefit",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="sum_don_hist", "Donation history",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="sum_med_hist", "Medical history",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="don.blood", "Donated blood",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="don.med", "Donated medical records",basic_regression_int.df$rowname)
basic_regression_int.df$rowname <- ifelse(basic_regression_int.df$rowname=="don.sen", "Donated sensor data",basic_regression_int.df$rowname)

names(basic_regression_int.df)[1] <- "name"
names(basic_regression_int.df)[3] <- "SE"
basic_regression_int.df <- basic_regression_int.df[-c(1,9,17,25,32,39),] # Hier streiche ich nur die Interzepte raus
basic_regression_int.df$model <- c(rep("M3 Medical records", 7), # Extra Spalte, um die Modelle voneinander später mit ggplot unterscheiden zu können. Wiederholungen entsprechen der Anzahl der Koeffizienten im Modell
                                   rep("M3 Biomarkers", 7),
                                   rep("M3 Sensor data", 7),
                                   rep("M4 Medical records", 6),
                                   rep("M4 Biomarkers", 6),
                                   rep("M4 Sensor data", 6))
table(basic_regression_int.df$name)

basic_regression_int.df$name <- str_wrap(basic_regression_int.df$name, width = 10)
basic_regression_int.df$name <- factor(basic_regression_int.df$name,      # Reordering group factor levels
                                       levels = c("Private\ncompany", "University",
                                                  "Public\nbenefit", "Second\nquestion", "Third\nquestion", 
                                                  "Medical\nhistory", "Medical\nhistory*Public\nbenefit",
                                                  "Donated\nblood", "Donated\nmedical\nrecords", "Donated\nsensor\ndata"))

table(basic_regression_int.df$name)
basic_regression_int.df$model <- factor(basic_regression_int.df$model,
                                        levels = c("M3 Medical records", "M3 Biomarkers", "M3 Sensor data", "M4 Medical records", "M4 Biomarkers", "M4 Sensor data"))


fig.basic_int <- ggplot(basic_regression_int.df,
                        aes(y = Estimate, x = model)) +
  geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)),
                lwd=0.5, width=0.4, color = "grey66") +
  geom_point(aes(color = model), size = 3) +
  geom_text(aes(label = round(Estimate,2)),
            hjust = 0.5,
            vjust = -0.85,
            size = 2.3
  ) +
  scale_color_viridis(discrete = TRUE, name = "Model:") + #dafür musst du library(viridis) laden bzw. installieren. Ist nur für die Farben
  ylim(-.5,1.3) +
  geom_hline(yintercept = 0) +
  facet_wrap( ~ name, nrow = 5, strip.position = "left") +
  xlab("") +
  ylab("Coefficients") +
  ggtitle("Outcome: Willingness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y =  element_blank(),
        axis.text.y =  element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        strip.text.y = element_text(size = 8)) +
  coord_flip()

ggsave("plots/regressions_two.eps", fig.basic_int, device = cairo_ps, width = 10, height = 10)


##### Plots for open-ended responses
table(surv_wide$open_ended)
surv_wide$open_ended <- as.character(surv_wide$open_ended)
surv_wide$open_ended_num <- ifelse(surv_wide$open_ended=="(Lack of) trust", 1, NA)
surv_wide$open_ended_num <- ifelse(surv_wide$open_ended=="Additional conditions", 2, surv_wide$open_ended_num)
surv_wide$open_ended_num <- ifelse(surv_wide$open_ended=="Missing information", 3, surv_wide$open_ended_num)
surv_wide$open_ended_num <- ifelse(surv_wide$open_ended=="Data protection and privacy", 4, surv_wide$open_ended_num)
surv_wide$open_ended_num <- ifelse(surv_wide$open_ended=="Purpose of collected data", 5, surv_wide$open_ended_num)
surv_wide$open_ended_num <- ifelse(surv_wide$open_ended=="Recipient", 6, surv_wide$open_ended_num)
surv_wide$open_ended_num <- ifelse(surv_wide$open_ended=="Other", 7, surv_wide$open_ended_num)
surv_wide$open_ended_num <- ifelse(surv_wide$open_ended=="Unclear", 8, surv_wide$open_ended_num)
table(surv_wide$open_ended_num)

surv_wide$open_ended_new <- as.factor(surv_wide$open_ended_num)
table(surv_wide$open_ended_new)

levels(surv_wide$open_ended_new) <-  c("(Lack of) trust", "Additional\nconditions","Missing\ninformation",
                                       "Data protection\nand privacy", "Purpose of\ncollected data", "Recipient", "Other", "Unclear")
table(surv_wide$open_ended_new)
table(surv_wide$open_ended, surv_wide$open_ended_new)

# 
wrp.open <- str_wrap(c("(Lack of) trust", "Additional conditions","Missing information","Data protection and privacy","Purpose of collected data","Recipient", "Other", "Unclear"), width = 15)
wrp.purp <- str_wrap(c("Personal recommendations", "Public benefit"), width = 10)
wrp.rec <- str_wrap(c("Public health agency", "Private company", "University"), width = 10)
wrp.dtype <- str_wrap(c("Biomarker", "Medical records", "Sensor data"), width = 10)

plot_by_dtype <- surv_wide %>% 
  mutate(dtype.first = str_sub(block_order, 1 , 3),
         dtype.first = factor(dtype.first, labels = c("Biomarker", "Medical records", "Sensor data"))) %>%
  group_by(dtype.first, open_ended_new) %>% 
  count(open_ended_new) %>% 
  filter(!is.na(open_ended_new) & !is.na(dtype.first)) %>% 
  ggplot(., aes(x = open_ended_new, fill = dtype.first, colour = dtype.first)) + 
  geom_bar(aes(y = n, color = dtype.first), position = "stack", alpha = 0.4, stat = "identity") + 
  guides(fill = guide_legend(title = "Data type"),
         colour = guide_legend(title = "Data type")) +
  xlab("Predicted category of open-ended survey question") +
  ylab("Frequency") + 
  scale_x_discrete(limits = rev(levels(surv_wide$open_ended_new))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1, labels = wrp.dtype) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1, labels = wrp.dtype) +
  theme(axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14), # Adjust the y-axis font size
        axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
        legend.text = element_text(size = 14),       # Adjust the legend font size
        axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
        legend.title = element_text(size = 16)) +
  coord_flip()
ggsave("plots/stacked_open_q_by_dtype.eps", plot_by_dtype, device = cairo_ps, width = 10, height = 6)

plot_by_purp <- surv_wide %>%
  mutate(purpose = factor(purpose, labels = c("Personal recommendations", "Public benefit"))) %>%
  group_by(purpose, open_ended_new) %>% 
  count(open_ended_new) %>% 
  filter(!is.na(open_ended_new) & !is.na(purpose)) %>% 
  ggplot(., aes(x = open_ended_new, fill = purpose, colour = purpose)) + 
  geom_bar(aes(y = n, color = purpose), position = "stack", alpha = 0.4, stat = "identity") + 
  guides(fill = guide_legend(title = "Purpose"),
         colour = guide_legend(title = "Purpose")) +
  xlab("Predicted category of open-ended survey question") +
  ylab("Frequency") + 
  scale_x_discrete(limits = rev(levels(surv_wide$open_ended_new))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1, labels = wrp.purp) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1, labels = wrp.purp) +
  theme(axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14), # Adjust the y-axis font size
        axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
        legend.text = element_text(size = 14),       # Adjust the legend font size
        axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
        legend.title = element_text(size = 16)) +
  coord_flip()
ggsave("plots/stacked_open_q_by_purp.eps", plot_by_purp, device = cairo_ps, width = 10, height = 6)


plot_by_rec <- surv_wide %>% 
  group_by(recipient, open_ended_new) %>% 
  count(open_ended_new) %>% 
  filter(!is.na(open_ended_new) & !is.na(recipient)) %>% 
  ggplot(., aes(x = open_ended_new, fill = recipient, colour = recipient)) + 
  geom_bar(aes(y = n, color = recipient), position = "stack", alpha = 0.4, stat = "identity") + 
  guides(fill = guide_legend(title = "Recipient"),
         colour = guide_legend(title = "Recipient")) +
  xlab("Predicted category of open-ended survey question") +
  ylab("Frequency") + 
  scale_x_discrete(limits = rev(levels(surv_wide$open_ended_new))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1, labels = wrp.rec) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1, labels = wrp.rec) +
  theme(axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14), # Adjust the y-axis font size
        axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
        legend.text = element_text(size = 14),       # Adjust the legend font size
        axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
        legend.title = element_text(size = 16)) +
  coord_flip()
# plot_by_rec <- surv_wide %>% 
#   group_by(recipient, open_ended_new) %>% 
#   count(open_ended_new) %>% 
#   filter(!is.na(open_ended_new) & !is.na(recipient)) %>% 
#   ggplot(., aes(x = open_ended_new, fill = recipient, colour = recipient)) + 
#   geom_bar(aes(y = n, color = recipient), position = position_dodge(width = 1), alpha = 0.4, stat = "identity") + 
#   guides(fill = guide_legend(title = "Data type"),
#          colour = guide_legend(title = "Data type")) +
#   xlab("Predicted category of open-ended survey question") +
#   ylab("Frequency") + 
#   scale_x_continuous(breaks=seq(1,8,by=1), labels = wrp.open) +
#   scale_fill_viridis(discrete = TRUE, option = "D", direction = -1, labels = wrp.rec) +
#   scale_color_viridis(discrete = TRUE, option = "D", direction = -1, labels = wrp.rec) +
#   coord_flip()


ggsave("plots/stacked_open_q_by_rec.eps", plot_by_rec, device = cairo_ps, width = 10, height = 6)

surv_merge <- surv_wide %>% 
  select(id_g,open_ended_new)
surv_long <- merge(surv_long, surv_merge, by="id_g")

surv_long_plot <- surv_long %>%
  filter(q_order=="first") %>% 
  mutate(high_will = (willingness == "4. Eher wahrscheinlich" |willingness == "5. Sehr wahrscheinlich"),
         mean_will = (willingness == "3. Weder wahrscheinlich noch unwahrscheinlich"),
         low_will = (willingness == "2. Eher unwahrscheinlich" |willingness == "1. Sehr unwahrscheinlich"))

surv_long_plot$will_cat <- ifelse(surv_long_plot$high_will==T, "high", NA)
surv_long_plot$will_cat <- ifelse(surv_long_plot$mean_will==T, "neutral", surv_long_plot$will_cat)  
surv_long_plot$will_cat <- ifelse(surv_long_plot$low_will==T, "low", surv_long_plot$will_cat)  
table(surv_long_plot$will_cat)

surv_long_plot$will_cat <- factor(surv_long_plot$will_cat, levels=c('high', 'neutral', 'low'))
  
plot_by_will <- surv_long_plot %>%
  filter(!is.na(y)) %>% 
  group_by(will_cat, open_ended_new) %>% 
  count(open_ended_new) %>% 
  ggplot(., aes(x = open_ended_new, fill = will_cat, colour = will_cat)) + 
  geom_bar(aes(y = n, color = will_cat), position = "stack", alpha = 0.4, stat = "identity") + 
  guides(fill = guide_legend(title = "Willingness"),
         colour = guide_legend(title = "Willingness")) +
  xlab("Predicted category of open-ended survey question") +
  ylab("Frequency") + 
  scale_x_discrete(limits = rev(levels(surv_wide$open_ended_new))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1) +
  theme(axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14), # Adjust the y-axis font size
        axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
        legend.text = element_text(size = 14),       # Adjust the legend font size
        axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
        legend.title = element_text(size = 16)) +
  coord_flip()
ggsave("plots/stacked_open_q_by_will.eps", plot_by_will, device = cairo_ps, width = 10, height = 6)

plot_bio <- surv_long_plot %>%
  mutate(dtype.first = str_sub(block_order, 1 , 3),
         dtype.first = factor(dtype.first, labels = c("Biomarker", "Medical records", "Sensor data"))) %>%
  filter(!is.na(y),
         dtype.first=="Biomarker") %>% 
  group_by(will_cat, open_ended_new) %>% 
  count(open_ended_new) %>% 
  ggplot(., aes(x = open_ended_new, fill = will_cat, colour = will_cat)) + 
  geom_bar(aes(y = n, color = will_cat), position = "stack", alpha = 0.4, stat = "identity") + 
  guides(fill = guide_legend(title = "Willingness\nBiomarker"),
         colour = guide_legend(title = "Willingness\nBiomarker")) +
  xlab("Predicted category of open-ended survey question") +
  ylab("Frequency") + 
  scale_x_discrete(limits = rev(levels(surv_wide$open_ended_new))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1) +
  theme(axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14), # Adjust the y-axis font size
        axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
        legend.text = element_text(size = 14),       # Adjust the legend font size
        axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
        legend.title = element_text(size = 16)) +
  coord_flip()

ggsave("plots/stacked_open_q_bio.eps", plot_bio, device = cairo_ps, width = 10, height = 6)


plot_med <- surv_long_plot %>%
  mutate(dtype.first = str_sub(block_order, 1 , 3),
         dtype.first = factor(dtype.first, labels = c("Biomarker", "Medical records", "Sensor data"))) %>%
  filter(!is.na(y),
         dtype.first=="Medical records") %>% 
  group_by(will_cat, open_ended_new) %>% 
  count(open_ended_new) %>% 
  ggplot(., aes(x = open_ended_new, fill = will_cat, colour = will_cat)) + 
  geom_bar(aes(y = n, color = will_cat), position = "stack", alpha = 0.4, stat = "identity") + 
  guides(fill = guide_legend(title = "Willingness\nMedical records"),
         colour = guide_legend(title = "Willingness\nMedical records")) +
  xlab("Predicted category of open-ended survey question") +
  ylab("Frequency") + 
  scale_x_discrete(limits = rev(levels(surv_wide$open_ended_new))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1) +
  theme(axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14), # Adjust the y-axis font size
        axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
        legend.text = element_text(size = 14),       # Adjust the legend font size
        axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
        legend.title = element_text(size = 16)) +
  coord_flip()
ggsave("plots/stacked_open_q_med.eps", plot_med, device = cairo_ps, width = 10, height = 6)


plot_sens <- surv_long_plot %>%
  mutate(dtype.first = str_sub(block_order, 1 , 3),
         dtype.first = factor(dtype.first, labels = c("Biomarker", "Medical records", "Sensor data"))) %>%
  filter(!is.na(y),
         dtype.first=="Sensor data") %>%   group_by(will_cat, open_ended_new) %>% 
  count(open_ended_new) %>% 
  ggplot(., aes(x = open_ended_new, fill = will_cat, colour = will_cat)) + 
  geom_bar(aes(y = n, color = will_cat), position = "stack", alpha = 0.4, stat = "identity") + 
  guides(fill = guide_legend(title = "Willingness\nSensor data"),
         colour = guide_legend(title = "Willingness\nSensor data")) +
  xlab("Predicted category of open-ended survey question") +
  ylab("Frequency") + 
  scale_x_discrete(limits = rev(levels(surv_wide$open_ended_new))) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1) +
  theme(axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14), # Adjust the y-axis font size
        axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
        legend.text = element_text(size = 14),       # Adjust the legend font size
        axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
        legend.title = element_text(size = 16)) +
  coord_flip()
ggsave("plots/stacked_open_q_sens.eps", plot_sens, device = cairo_ps, width = 10, height = 6)

plot_bio
plot_sens
plot_med
plot_by_dtype
plot_by_purp
plot_by_rec
plot_by_will



### trust in science
x <- haven::read_dta(file = "data/ZA7759 - W51/ZA7759_v2-0-0.dta")
x$trust_science <- haven::as_factor(x$CF51097)
x <- x %>% 
  select(id_g, trust_science)

surv_long_2 <- merge(surv_long, x, by="id_g", all.x = T)
fix_missing <- function(x, na.value) {
  x[x == na.value] <- NA
  x
}
surv_long_2$trust_science <- fix_missing(surv_long_2$trust_science, "-90. item nonresponse") 
surv_long_2$trust_science <- fix_missing(surv_long_2$trust_science, "-99. Weiß nicht") 
table(surv_long_2$trust_science)
surv_long_2$trust_science <- factor(surv_long_2$trust_science)

### include trust in science
m_5 <- lmer(y ~ 1 + dtype + recipient + purpose + q_order + trust_science + (1|id_g),
            data = surv_long_2)
summary(m_5)



