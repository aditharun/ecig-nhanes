# Discordance between self-reported e-cigarette use and measured cotinine levels in non-smoking e-cigarette users
# Rishi Shah and Adith Arun
# July 21, 2024
# Study period is 2013-2020 and data source is the National Health and Nutrition Examination Survey: https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx

# load necessary libraries
library(nhanesA)
library(tidyverse)
library(survey)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(gtsummary)
library(tibble)
library(pROC)
library(boot)
library(srvyr)

# import data -----
# import demographic data
demo_data_13_14 <- nhanes("DEMO_H") # demographic data for 2013-2014
demo_data_15_16 <- nhanes("DEMO_I") # demographic data for 2015-2016
demo_data_17_20 <- nhanes("P_DEMO") # demographic data for 2017-2020

# import recent tobacco use data
smoke_data_13_14 <- nhanes("SMQRTU_H") # recent tobacco use 2013-2014
smoke_data_15_16 <- nhanes("SMQRTU_I") # recent tobacco use 2015-2016
smoke_data_17_20 <- nhanes("P_SMQRTU") # recent tobacco use 2017-2020

# import at home smoker data
shs_data_13_14 <- nhanes("SMQFAM_H") # household smoking data for 2013-2014
shs_data_15_16 <- nhanes("SMQFAM_I") # household smoking data for 2015-2016
shs_data_17_20 <- nhanes("P_SMQFAM") # household smoking data for 2017-2020

# import serum cotinine data
cot_data_13_14 <- nhanes("COT_H") # serum cotinine data for 2013-2014
cot_data_15_16 <- nhanes("COT_I") # serum cotinine data for 2015-2016
cot_data_17_20 <- nhanes("P_COT") # serum cotinine data for 2017-2020

# clean and merge data ------
# demographics data
# demographic variables
vars_13_16 <- c(
  "SEQN",       # Respondent sequence number
  "SDDSRVYR",   # Data release cycle
  "RIDSTATR",   # Interview/Examination status
  "RIAGENDR",   # Gender
  "RIDAGEYR",   # Age in years at screening
  "RIDRETH1",   # Race/Hispanic origin
  "RIDRETH3",   # Race/Hispanic origin w/ NH Asian
  "DMDEDUC2",   # Education level - Adults 20+
  "DMDMARTL",   # Marital status
  "INDFMPIR",   # Ratio of family income to poverty
  "SDMVPSU",    # Masked variance pseudo-PSU
  "SDMVSTRA",   # Masked variance pseudo-stratum
  "WTINT2YR",   # Full sample 2 year interview weight
  "WTMEC2YR"    # Full sample 2 year MEC exam weight
)

vars_17_20 <- c(
  "SEQN",       # Respondent sequence number
  "SDDSRVYR",   # Data release cycle
  "RIDSTATR",   # Interview/Examination status
  "RIAGENDR",   # Gender
  "RIDAGEYR",   # Age in years at screening
  "RIDRETH1",   # Race/Hispanic origin
  "RIDRETH3",   # Race/Hispanic origin w/ NH Asian
  "DMDEDUC2",   # Education level - Adults 20+
  "DMDMARTZ",   # Marital status
  "INDFMPIR",   # Ratio of family income to poverty
  "SDMVPSU",    # Masked variance pseudo-PSU
  "SDMVSTRA",   # Masked variance pseudo-stratum
  "WTINTPRP",   # Full sample interview weight
  "WTMECPRP"    # Full sample MEC exam weight
)

# match demographic variable names across survey cycles
demo_data_13_14 <- demo_data_13_14 %>%
  select(all_of(vars_13_16)) %>%
  rename(
    DMDMARTZ = DMDMARTL,  
    WTINTPRP = WTINT2YR,  
    WTMECPRP = WTMEC2YR   
  )

demo_data_15_16 <- demo_data_15_16 %>%
  select(all_of(vars_13_16)) %>%
  rename(
    DMDMARTZ = DMDMARTL,  
    WTINTPRP = WTINT2YR,  
    WTMECPRP = WTMEC2YR   
  )

demo_data_17_20 <- demo_data_17_20 %>%
  select(all_of(vars_17_20))


# adjust survey weights for pooled year analyses: https://www.cdc.gov/nchs/data/series/sr_02/sr02-190.pdf
demo_data_13_14 <- demo_data_13_14 %>% mutate(WTADJ = WTINTPRP * (2 / 7.2), WTADJMEC = WTMECPRP * (2 / 7.2))
demo_data_15_16 <- demo_data_15_16 %>% mutate(WTADJ = WTINTPRP * (2 / 7.2), WTADJMEC = WTMECPRP * (2 / 7.2))
demo_data_17_20 <- demo_data_17_20 %>% mutate(WTADJ = WTINTPRP * (3.2 / 7.2), WTADJMEC = WTMECPRP * (3.2 / 7.2))

# match cotinine data variables across survey cycles
cot_data_17_20 <- cot_data_17_20 %>% rename(LBXHCT = LBXHCOT, LBDHCTLC = LBDHCOLC)

# select shs data
shs_data_13_14 <- shs_data_13_14 %>% select("SEQN", "SMD470") # of people who smoke inside respondent's home
shs_data_15_16 <- shs_data_15_16 %>% select("SEQN", "SMD470") # of people who smoke inside respondent's home
shs_data_17_20 <- shs_data_17_20 %>% select("SEQN", "SMD470") # of people who smoke inside respondent's home

# smoking variables
vars <- c(
  "SEQN",    # Respondent sequence number
  "SMQ681",  # Smoked tobacco last 5 days?
  "SMQ690A", # Used last 5 days - Cigarettes
  "SMQ710",  # Number of days smoked cigarettes in the last 5 days
  "SMQ720",  # Number of cigarettes smoked per day
  "SMQ725",  # When did the respondent smoke the last cigarette?
  "SMQ690B", # Used last 5 days - Pipes
  "SMQ740",  # Number of days smoked pipe in the last 5 days
  "SMQ690C", # Used last 5 days - Cigars
  "SMQ770",  # Number of days smoked cigars in the last 5 days
  "SMQ690G", # Used last 5 days - Hookah, water pipes
  "SMQ845",  # Number of days smoked water pipe in the last 5 days
  "SMQ690H", # Used last 5 days - E-cigarettes
  "SMQ849",  # Number of days smoked e-cigarette in the last 5 days
  "SMQ851",  # Used smokeless tobacco last 5 days
  "SMQ863",  # Used nicotine replacement last 5 days
  "SMDANY"   # Used any tobacco product last 5 days
)

smoke_data_13_14 <- smoke_data_13_14 %>% select(all_of(vars))
smoke_data_15_16 <- smoke_data_15_16 %>% select(all_of(vars))
smoke_data_17_20 <- smoke_data_17_20 %>% select(all_of(vars))

# bind demographics data
demographics <- bind_rows(demo_data_13_14, demo_data_15_16, demo_data_17_20)

# bind serum cotinine data
cotinine <- bind_rows(cot_data_13_14, cot_data_15_16, cot_data_17_20)

# bind shs data
shs <- bind_rows(shs_data_13_14, shs_data_15_16, shs_data_17_20)

# bind recent tobacco use data
e_cigarette <- bind_rows(smoke_data_13_14, smoke_data_15_16, smoke_data_17_20)

# join all data
data <- left_join(demographics, cotinine, by = "SEQN")
data <- left_join(data, e_cigarette, by = "SEQN")
data <- left_join(data, shs, by = "SEQN")

# analysis ----
# exclude all smokers (other than e-cigarette users)
data <- data %>% mutate(ONLY_VAPES = case_when(!is.na(LBDCOTLC) & (SMQ863 == "No" | is.na(SMQ863)) & SMQ851 == "No" & SMQ681 == "Yes" & SMQ690H == 8 & is.na(SMQ690A) & is.na(SMQ690B) & is.na(SMQ690C) & is.na(SMQ690G) ~ 1,
                                              TRUE ~ 0)) # SMQ690A != 1 & SMQ690B != 2 & SMQ690C != 3 & SMQ690G != 7, also does smokeless tobacco matter?
                                                         # !is.na(LBDCOTLC) & LBDCOTLC == "At or above the detection limit" (23 NAs < 10% of 244, so do not need to adjust sample weights per NCHS recommendations: https://wwwn.cdc.gov/nchs/nhanes/tutorials/datasets.aspx)

data <- data %>% mutate(SHS_EXPOSURE = case_when(SMD470 %in% c("1 household member smokes inside the house", 
                                                               "2 household members smoke inside the house",
                                                               "2 or more household members smoke inside the house",
                                                               "3 or more household members smoke inside the house") ~ 1,
                                                 TRUE ~ 0))

data <- data %>% mutate(NON_SMOKER = case_when(SMQ681 == "No" & SMQ851 == "No" & SMQ863 == "No" ~ 1,
                                               TRUE ~ 0))

# cotinine level analysis
# create full survey design: https://wwwn.cdc.gov/nchs/nhanes/tutorials/VarianceEstimation.aspx
full_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTADJMEC,
  data = data,
  nest = TRUE
)

#SMQ690A - used cigarettes last 5 days

heavyestimates <- data %>% mutate(ecig_status = ifelse(ONLY_VAPES == 1, ifelse(SMQ849 >= 4, "Heavy", "Light") ,"Tobacco-Naive")) 
full_design_heavy <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTADJMEC,
  data = heavyestimates,
  nest = TRUE
)
svy_cotinine_heavy <- subset(full_design_heavy, ONLY_VAPES == 1 & SHS_EXPOSURE == 0) # data %>% filter(ONLY_VAPES == 1) # lower limit of detection for cotinine is 0.015 ng/mL
data_svy_heavy <- svyby(~LBXCOT, ~ecig_status, svy_cotinine_heavy, svyquantile, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), ci = TRUE)


svy_cotinine <- subset(full_design, ONLY_VAPES == 1 & SHS_EXPOSURE == 0) # data %>% filter(ONLY_VAPES == 1) # lower limit of detection for cotinine is 0.015 ng/mL


data_svy <- svyby(~LBXCOT, ~SMQ849, svy_cotinine, svyquantile, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), ci = TRUE)


data_cig <- data %>% mutate(cig_smoker = ifelse(is.na(SMQ690A), 0, 1)) %>% as_tibble() %>% relocate(cig_smoker, SMQ690A) %>% mutate(one = 1)
  
full_design_ecig <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTADJMEC,
  data = data_cig,
  nest = TRUE
)

svy_cig <- subset(full_design_ecig, cig_smoker == 1 & !is.na(LBXCOT))

data_svy_cig <- svyby(~LBXCOT, ~one, svy_cig, svyquantile, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), ci = TRUE)

data_svy <- as.data.frame(data_svy)
data_svy_cig <- as.data.frame(data_svy_cig)

# survey-adjusted distribution of cotinine levels by # of days last used e-cigarette in the last 5 days



# regression
# Q: What are factors associated with *detectable* e-cigarette use among the US non-actively tobacco-using population?
# covariates 

# race/ethnicity, RIDRETH1 is already factored
# gender, RIAGENDR is already factored

# age
data$AGECAT <- NA
data$AGECAT[data$RIDAGEYR >= 12 & data$RIDAGEYR <= 18] <- 1
data$AGECAT[data$RIDAGEYR >= 19 & data$RIDAGEYR <= 25] <- 2
data$AGECAT[data$RIDAGEYR >= 26] <- 3

levels <- c("12-18", "19-25", "≥26")
labels <- c(1, 2, 3)
data$AGECAT <- factor(data$AGECAT, levels = labels, labels = levels)

# income
# binarize low and middle/high family income
# low = < 200% of the federal poverty limit = 0
# middle/high = >= 200% of the federal poverty limit = 1
data$INC_BINARY <- NA
data$INC_BINARY[data$INDFMPIR <= 1.00] <- 0
data$INC_BINARY[data$INDFMPIR > 1.00] <- 1

levels <- c("Low income", "High income")
labels <- c(0, 1)
data$INC_BINARY <- factor(data$INC_BINARY, levels = labels, labels = levels)

# create sample summary table
# subset data to only include exclusive e-cigarette users and non-tobacco users
data_filtered <- data %>% filter(
  (ONLY_VAPES == 1 | NON_SMOKER == 1) & SHS_EXPOSURE == 0
)

data_filtered <- data_filtered %>% relocate(NON_SMOKER, ONLY_VAPES, SMQ849) %>% mutate(ecig_status = ifelse(ONLY_VAPES == 1, ifelse(SMQ849 >= 4, "Heavy", "Light") ,"Tobacco-Naive")) 


#for numerical values
t1_all_status_ecig <- (Table1_all_format <- tbl_summary(
  data = data_filtered %>% filter(ecig_status %in% c("Heavy", "Tobacco-Naive")) %>% filter(!is.na(LBXCOT)),
  by = "ecig_status",
  statistic = list(
    AGECAT ~ "{n} ({p})",
    RIAGENDR ~ "{n} ({p})",
    RIDRETH1 ~ "{n} ({p})",
    INC_BINARY ~ "{n} ({p})"),
  missing_text = "Missing",
  include = c(AGECAT, RIAGENDR, RIDRETH1, INC_BINARY)
)) 

# survey-ify data for percentages
#START HERE for table 1 percentages and n values
data2 <- as_survey(data %>% mutate(ecig_status = ifelse(ONLY_VAPES == 1, ifelse(SMQ849 >= 4, "Heavy", "Light") ,"Tobacco-Naive")) %>% filter(ecig_status %in% c("Heavy", "Tobacco-Naive")), 
                   id = SDMVPSU, 
                   weight = WTADJMEC,
                   strata = SDMVSTRA, nest = TRUE)
 

# make summary table with national estimates (abridged)
tbl_svysummary(subset(data2, (ONLY_VAPES == 1 | NON_SMOKER == 1) & SHS_EXPOSURE == 0), by = ecig_status, 
               include = c(AGECAT, RIAGENDR, RIDRETH1, INC_BINARY)) %>% add_p()



#Compare Heavy Ecig to Tobacco-Naive
data_filtered_heavy_tn <- data_filtered %>% as_tibble() %>% filter(ecig_status %in% c("Tobacco-Naive", "Heavy")) %>% mutate(status = ifelse(ecig_status == "Tobacco-Naive", 0, 1))
full_design_regression <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTADJMEC,
  data = data_filtered_heavy_tn,
  nest = TRUE
)

# fit regression model
fit <- svyglm(status ~ 
                relevel(RIDRETH1, ref = "Non-Hispanic White") + 
                relevel(RIAGENDR, ref = "Male") +
                relevel(INC_BINARY, ref = "High income") + 
                relevel(AGECAT, ref = "≥26"),
              design = full_design_regression,
              family = quasibinomial(link = 'logit'))

#logistic.display(fit)

epiDisplay::logistic.display(fit)$table %>% as.data.frame()

# roc analysis
# Q: What is the optimal cutpoint to distinguish between self-reported exclusive e-cig users from non–tobacco users?
data_filtered <- as_tibble(data_filtered)

df <- data_filtered %>% select(SHS_EXPOSURE, NON_SMOKER, ONLY_VAPES, LBXCOT, SMQ849, AGECAT, INC_BINARY, RIAGENDR, RIDRETH1, ecig_status)

# filter for non-missing cotinine levels
df <- df %>% filter(!is.na(LBXCOT))


df_h_tn <- df %>% filter(ecig_status %in% c("Heavy", "Tobacco-Naive")) %>% mutate(status = ifelse(ecig_status == "Tobacco-Naive", 0, 1))
df_l_tn <- df %>% filter(ecig_status %in% c("Light", "Tobacco-Naive")) %>% mutate(status = ifelse(ecig_status == "Tobacco-Naive", 0, 1))


rocanalysis <- function(df){
  # perform ROC analysis
  rocobj <- pROC::roc(df$status, df$LBXCOT, direction = "<", ci=TRUE)

  # extract ROC data
  df.roc <- data.frame(invspec = (1 - rocobj$specificities), sens = rocobj$sensitivities)

  # extract AUC and CI
  lb <- rocobj$ci[1] 
  auc <- rocobj$ci[2] 
  ub <- rocobj$ci[3]

  # create annotation text for AUC
  lab.text <- paste0("AUC: ", signif(auc, 2), " (", signif(lb, 2), " - ", signif(ub, 2), ")")
  ann.text <- data.frame(invspec = 0.25, sens = 0.05, label = lab.text, n = length(rocobj$cases))

  #compute youden optimal
  optimal <- coords(rocobj, "best")

  list(optimal = optimal, ann.text = ann.text, lab.text = lab.text, lb = lb, ub = ub, auc = auc, df.roc = df.roc)
}

cpal <- pal_jama("default")(7)

h_tn_roc <- rocanalysis(df_h_tn)
l_tn_roc <- rocanalysis(df_l_tn)

df.roc <- rbind(h_tn_roc$df.roc %>% as_tibble() %>% mutate(run = "Heavy-TN"),
l_tn_roc$df.roc %>% as_tibble() %>% mutate(run = "Light-TN"))

threshs <- rbind(h_tn_roc$optimal %>% mutate(threshold = round(threshold, 2)) %>% mutate(invspec = 1-specificity) %>% mutate(run = "Heavy-TN"), l_tn_roc$optimal %>% mutate(threshold = round(threshold, 2)) %>% mutate(invspec = 1-specificity) %>% mutate(run = "Light-TN")) %>% mutate(sens = sensitivity) %>% as_tibble()

# plot ROC curve
overall.roc <- ggplot(df.roc, aes(x=invspec, y=sens, color = run)) +
  geom_line(size = 0.75, alpha = 0.85) +
  scale_x_continuous(#expand = c(0, 0.01),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = as.character(c("0", ".25", ".50", ".75", "1.0"))) +
  scale_y_continuous(#expand = c(0, 0.01),
                     labels = as.character(c("0", ".25", ".50", ".75", "1.0"))) + theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 13),
    panel.grid = element_blank(), panel.border = element_rect(color = "black", fill = "transparent")) +  theme(axis.ticks = element_line(color = "black"), legend.text = element_text(size = 10), legend.title = element_text(size = 12)) + 
  labs(
    x = "1 - Specificity",
    y = "Sensitivity"
  ) + scale_color_manual(name = "E-cig group", values = c("Heavy-TN" = cpal[1], "Light-TN" = cpal[2]), labels = c("Heavy-TN"= "Heavy\n(\u2265 4d)\n", "Light-TN" = "Light\n(< 4d)")) + 
  geom_point(data = threshs, aes(x = invspec, y = sens, color = run), size = 2.3) + geom_text(data = threshs, aes(x = invspec+0.3, y = sens-0.02, color = run, label = paste0("Cotinine: ",threshold, " ng/mL (", round(invspec,2), ", ", round(sens, 2), ")")), size = 4, show.legend = FALSE)


#"Distinguishing between exclusive E-cig vape use\nand tobacco-naive individuals"

colnames(data_svy_cig)[1] <- "SMQ849"
data_svy_cig$SMQ849 <- 6
data_fig1a <- rbind(data_svy_cig, data_svy)

survey_cotinine <- ggplot(data_fig1a %>% filter(SMQ849 <= 6), aes(x = as.factor(SMQ849), y = LBXCOT.0.5)) +
  geom_boxplot(aes(
    lower = LBXCOT.0.25, middle = LBXCOT.0.5, upper = LBXCOT.0.75,
    ymin = LBXCOT.0.05, ymax = LBXCOT.0.95
  ), stat = "identity", alpha = 0.5, fill = "grey80", color = "grey30") + 
  scale_y_continuous(trans = "log10", 
                     breaks = c(0.01, 0.1, 1, 10, 100, 1000),
                     labels = c("0.01", "0.1", "1", "10", "100", "1000"), limits = c(0.01, 1000)) +
  labs(
    x = "Number of days used E-cig in last 5 days",
    y = "Serum cotinine levels (ng/mL)", # expression(paste("Log"[10], "-Transformed 
    fill = "Days of E-cig use"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none", panel.grid = element_blank(), panel.border = element_rect(color = "black", fill = "transparent"), axis.ticks = element_line(color = "black")
  ) +  theme(axis.title.x = element_text(hjust = 0.1)) + scale_x_discrete(labels = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "Cig"))



cowplot::plot_grid(survey_cotinine, overall.roc, nrow = 1, labels = c("A", "B"), label_size = 15, rel_widths = c(1, 1.4)) %>% ggsave("~/Downloads/ecig-fig1.pdf", plot = ., device= cairo_pdf, units = "in", width = 11, height = 5)

#"",#"Relationship between serum cotinine\nand recent E-cig use"


######

# concordance analysis
df.table <- df_h_tn %>% mutate(
  COTININE_CLASSIFICATION = ifelse(LBXCOT >= h_tn_roc$optimal$threshold, 1, 0)
)

# calculate concordance
# concordance is when self-reported e-cigarette use matches cotinine classification
df.table <- df.table %>% mutate(
  CONCORDANCE = case_when(
    ONLY_VAPES == 1 & COTININE_CLASSIFICATION == 1 ~ 1,
    NON_SMOKER == 1 & COTININE_CLASSIFICATION == 0 ~ 1,
    TRUE ~ 0
  )
)

# generate concordance summaries
calculate_concordance <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),
      concordant = sum(CONCORDANCE),
      percent_concordant = (concordant / n) * 100
    ) %>%
    mutate(group = group_var)
}

# non-tobacco users
non_smoker_concordance <- bind_rows(
  calculate_concordance(df.table %>% filter(NON_SMOKER == 1), "AGECAT"),
  calculate_concordance(df.table %>% filter(NON_SMOKER == 1), "RIAGENDR"),
  calculate_concordance(df.table %>% filter(NON_SMOKER == 1), "RIDRETH1"),
  calculate_concordance(df.table %>% filter(NON_SMOKER == 1), "INC_BINARY")
)

# exclusive e-cigarette  heavy users
vapers_concordance <- bind_rows(
  calculate_concordance(df.table %>% filter(ONLY_VAPES == 1), "AGECAT"),
  calculate_concordance(df.table %>% filter(ONLY_VAPES == 1), "RIAGENDR"),
  calculate_concordance(df.table %>% filter(ONLY_VAPES == 1), "RIDRETH1"),
  calculate_concordance(df.table %>% filter(ONLY_VAPES == 1), "INC_BINARY")
)

concordance_results <- bind_rows(
  non_smoker_concordance %>% mutate(group_type = "Non-Tobacco Users"),
  vapers_concordance %>% mutate(group_type = "Exclusive Heavy E-Cigarette Users")
)


concordance_pivot <- concordance_results %>%
  pivot_wider(
    names_from = group_type,
    values_from = c(n, concordant, percent_concordant),
    names_glue = "{group_type}_{.value}"
  )


