library(haven)
library(tidyverse)
library(mice)
library(glue)
library(Hmisc)
library(summarytools)
library(flextable)
library(officer)

setwd("D:/Next Steps 1-8/Projects/NEET Book")
rm(list=ls())

# 1. Set-Up ----
load("Data/mice.Rdata")

lbl_clean <- c(Any_NEET = "1+ Months NEET",
  Months_NEET = "Months NEET",
  cluster4 = "NEET Cluster",
  GHQ_W8_Likert = "GHQ-12 Likert",
  LifeSat_W8 = "Life Satisfaction",
  PoorHealth_W8 = "Poor Self-Rated Health",
  AUDIT_W8 = "Alcohol Use",
  Adult_W8 = "Adult Identity",
  LOC_Factor_W8 = "External Locus of Control",
  Employed_W8 = "Employed",
  LogPay_W8 = "(Log) Income",
  FinDiff_W8 = "Financial Difficulty",
  Precarious_W8 = "Precarious Work",
  ShiftWork_W8 = "Shift Work",
  Female = "Female",
  Child_W8 = "Has Child",
  FirstChild = "Age at First Birth",
  "Female#2.Child_W8" = "Female x Has Child",
  "Female#c.FirstChild" = "Female x Age at First Birth",
  NonWhite = "Non-White",
  ForLangHH_W1 = "Foreign Household Language",
  Any_VET = "1+ Months VET",
  Education_W8 = "Highest Qualification",
  GHQ_W2_Caseness = "GHQ-12 Caseness",
  GenHealth_W2 = "Self-Rated Health",
  Disabled_W2 = "Disabled",
  NSSEC3_W1 = "Family NS-SEC",
  IMD_W2 = "IMD",
  HHType_W1 = "Household Type",
  ParentEduc5_W1 = "Parental Education",
  GParentUni_W1 = "Grandparent Attended University",
  SchoolAtt_W2 = "Attitude to School",
  Risk_W1 = "Risk Behaviours",
  LOC_Factor_W2 = "Internal Locus of Control",
  "_cons" = "Intercept",
  n = "Observations") %>%
  enframe(name = "var", value = "var_clean") %>%
  mutate(lvls = map(var, 
                    function(x){
                      y <- (imp_long[[x]])
                      if (is.factor(y)) labs <- levels(y)
                      else labs <- x
                      enframe(labs, name = "level", value = "cat")
                    })) %>%
  unnest(lvls) %>%
  filter(!(level == 1 & cat == "No")) %>%
  mutate(cat_clean = ifelse(cat == "Yes" | var == cat, 
                            var_clean, cat)) %>%
  mutate(index = row_number(),
         var_clean = fct_inorder(var_clean),
         cat_clean = fct_inorder(cat_clean))
lbl_clean

# 2. Descriptive Statistics ----
get_freq <- function(df, norm = 40){
  mean_age <- complete(imp, "long") %>% 
    mutate(FirstChild = ifelse(Child_W8 == 'Yes', .FirstChild, NA)) %>%
    summarise(mean = wtd.mean(FirstChild, Survey_Weight_W8)) %>%
    pull(mean)
  df <- mutate(df, 
               FirstChild = ifelse(Child_W8 == 'No',
                                   NA, FirstChild + mean_age))
  
  df_freq <- select(df, -NSID) %>%
    select(where(is.factor)) %>%
    freq(weights = df$Survey_Weight_W8) %>%
    map_dfr(~ tb(.x) %>%  
          pivot_longer(1, names_to = "var", values_to = "cat")) %>%
    mutate(freq = freq /norm,
           across(c(freq, pct_tot), 
                  ~ round(.x, 2) %>% 
                    format(big.mark = ",") %>%
                    trimws()),
           string = glue("{freq} ({pct_tot}%)")) %>%
    select(var, cat, string)
  
  df_cont <- df %>%
    descr(weights = df$Survey_Weight_W8) %>%
    tb() %>%
    mutate(across(c(mean, sd), 
                  ~ round(.x, 2)),
           string = glue("{mean} ({sd})"),
           cat = variable) %>%
    select(var = variable, cat, string)
  
  bind_rows(df_freq, df_cont)
}

df_desc <- bind_rows(all = imp_long %>%
                       filter(imp>0) %>%
                       get_freq(),
                     neet = imp_long %>%
                       filter(imp>0, Months_NEET > 0) %>%
                       get_freq(),
                     not_neet = imp_long %>%
                       filter(imp>0, Months_NEET == 0) %>%
                       get_freq(),
                     .id = "sample") %>%
  filter(!(cat %in% c("<NA>", "No")), 
         !(var %in% c("imp", "cluster4", "Any_NEET")),
         str_detect(var, "W8", TRUE) | 
             str_detect(var, "(Child|Education)")) %>%
  pivot_wider(names_from = sample, values_from = string) %>%
  left_join(lbl_clean, by= c("var", "cat")) %>%
  arrange(index)


desc_tbl <- df_desc %>%
  select(var_clean, cat_clean, all, neet, not_neet) %>%
  mutate(across(c(var_clean, cat_clean), as.character)) %>%
  mutate(var_clean = ifelse(var_clean == cat_clean, "", var_clean)) %>%
  flextable() %>%
  merge_v(1) %>%
  set_header_labels(var_clean = "", cat_clean = "Variable",
                    all = "Full Sample", neet = "1+ Months NEET", 
                    not_neet = "0 Months NEET") %>%
  border_remove() %>%
  fontsize(size = 11, part = "all") %>%
  merge_h(part = "header") %>%
  border_inner_h(border = fp_border(color = "gray30", style = "dashed")) %>%
  hline_top(border = fp_border(color="black", width = 2), part = "all") %>%
  hline_bottom(border = fp_border(color="black", width = 2), part = "all") %>%
  fix_border_issues(part = "all") %>%
  align(j = 1:2, align="right", part = "all") %>%
  align(j = 3:5, align="center", part = "all") %>%
  valign(j = 1, valign = "top") %>%
  autofit()
desc_tbl
save_as_docx(desc_tbl, path = "Tables/descriptive_statistics.docx")

wtd_vars <- imp_long %>%
  filter(imp > 0) %>%
  select(where(is.numeric)) %>%
  pivot_longer(-Survey_Weight_W8) %>%
  group_by(name) %>%
  summarise(var = wtd.var(value, Survey_Weight_W8) %>% sqrt()) %>%
  deframe()


# 3. Regression Results ----
regsave <- read_dta("Data/regsave.dta") %>%
  filter(coef != 0, str_detect(var, "/", TRUE)) %>%
  separate(var, c("part", "var"), sep = ":", fill = "left") %>%
  mutate(part = ifelse(is.na(part), outcome, part),
         var = str_replace(var, "1.Any_NEET", "2.Any_NEET")) %>%
  separate(var, c("level", "var"), sep = "\\.", fill = "left", 
           extra = "merge", convert = TRUE) %>%
  mutate(level = ifelse(is.na(level), 1, level),
         level = ifelse(str_detect(var, "\\#"), 1, level)) %>%
  rename(beta = coef, se = stderr, p = pval, 
         lci = ci_lower, uci = ci_upper, n = N) %>%
  mutate(std = wtd_vars[var],
         std = ifelse(is.na(std), 1, std)) %>%
  filter(!(part == "Employed_W8" & str_detect(estimator, "^heck"))) %>%
  mutate(across(c(beta, se, lci, uci), ~ .x * std)) %>%
  mutate(across(c(beta, se, lci, uci),
                ~ ifelse(!(estimator %in% c("heckman", "reg")) |
                           part == "Employed_W8", exp(.x), .x))) %>%
  mutate(across(c(Any_NEET, Months_NEET, cluster4), as.logical), 
         string = glue("{round(beta, 2)}\n({round(lci, 2)}, {round(uci, 2)})")) %>%
  left_join(lbl_clean, by = c("var", "level")) %>%
  left_join(lbl_clean %>%
              select(outcome = var, 
                     outcome_clean = var_clean), by = "outcome")


# 3. Regression Plots ----
# a. Any NEET / Months NEET
neet_plot <- function(df, file_name = NULL, height = 9.9){
  hlines <- tibble(outcome_clean = c("1+ Months NEET", "Months NEET"),
                   yint = rep(1, 2))
  
  strip_labels <- df %>% 
    select(var_clean, cat_clean) %>% 
    distinct() %>%
    mutate(across(1:2, as.character),
           strip_label = ifelse(var_clean == cat_clean, "", var_clean)) %>%
    select(var_clean, strip_label) %>%
    deframe()
  
  p <- ggplot(df) +
    aes(x = cat_clean, y = beta, ymin = lci, ymax = uci,
        color = part) +
    geom_hline(data = hlines, aes(yintercept = yint)) +
    geom_pointrange(position = position_dodge(0.6)) +
    scale_color_brewer(palette = "Dark2") +
    coord_flip() +
    facet_grid(var_clean ~ outcome_clean, scales = "free", switch = "y",
               labeller = labeller(var_clean = strip_labels)) +
    labs(x = NULL, y = NULL) +
    guides(color = FALSE) +
    theme_minimal() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0))
  if (!is.null(file_name)){
    file_name <- paste0("Images/", file_name, ".png")
    ggsave(filename = file_name, plot = p,
           dpi = 600, height = height, width = 21, units = "cm")
  }
  p
}
df_temp <- regsave %>%
  filter(outcome %in% c("Months_NEET", "Any_NEET"),
         part != "inflate",
         type == "mi", var != "_cons")

df_temp %>%
  filter(str_detect(var, "Child") |
           str_detect(var, "Female")) %>%
  neet_plot("neet_gender_child")

df_temp %>%
  filter(var %in% 
           c("Any_VET", "NonWhite", "ForLangHH_W1", "HHType_W1")) %>%
  neet_plot("neet_vet_household")

df_temp %>%
  filter(var %in% 
           c("GHQ_W2_Caseness", "GenHealth_W2", "Disabled_W2")) %>%
  neet_plot("neet_health")

df_temp %>%
  filter(var %in% 
           c("IMD_W2", "ParentEduc5_W1", "GParentUni_W1", "NSSEC3_W1")) %>%
  neet_plot("neet_sep")

df_temp %>%
  filter(var %in% 
           c("Risk_W1" , "SchoolAtt_W2" ,  "LOC_Factor_W2")) %>%
  neet_plot("neet_risk")

df_temp %>%
  filter(var %in% 
           c("Education_W8")) %>%
  neet_plot("neet_edu")


# b. Cluster Membership 
cluster4_plot <- function(df, file_name = NULL, height = 9.9){
  strip_labels <- df %>% 
    select(var_clean, cat_clean) %>% 
    distinct() %>%
    mutate(across(1:2, as.character),
           strip_label = ifelse(var_clean == cat_clean, "", var_clean)) %>%
    select(var_clean, strip_label) %>%
    deframe()
  
  p <- ggplot(df) +
    aes(x = cat_clean, y = beta, ymin = lci, ymax = uci,
        color = str_replace_all(part, "_", " ")) +
    geom_hline(yintercept = 1) +
    geom_pointrange(position = position_dodge(0.6)) +
    scale_color_brewer(palette = "Dark2") +
    coord_flip() +
    facet_grid(var_clean ~ ., scales = "free", switch = "y",
               labeller = labeller(var_clean = strip_labels)) +
    labs(x = NULL, y = "Odds Ratio", color = "Cluster") +
    theme_minimal() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          legend.position = c(.85,.85))
  if (!is.null(file_name)){
    file_name <- paste0("Images/", file_name, ".png")
    ggsave(filename = file_name, plot = p,
           dpi = 600, height = height, width = 21, units = "cm")
  }
  p
}


df_temp <- regsave %>%
  filter(outcome == "cluster4", type == "mi",
         !(var %in% c("_cons", "Education_W8")))

df_temp %>%
  filter(str_detect(var, "Child") |
           str_detect(var, "Female")) %>%
  cluster4_plot("cluster4_gender_child")

df_temp %>%
  filter(var %in% 
           c("Any_VET", "NonWhite", "ForLangHH_W1", "HHType_W1")) %>%
  cluster4_plot("cluster4_vet_household")

df_temp %>%
  filter(var %in% 
           c("GHQ_W2_Caseness", "GenHealth_W2", "Disabled_W2")) %>%
  cluster4_plot("cluster4_health")

df_temp %>%
  filter(var %in% 
           c("IMD_W2", "ParentEduc5_W1", "GParentUni_W1", "NSSEC3_W1")) %>%
  cluster4_plot("cluster4_sep")

df_temp %>%
  filter(var %in% 
           c("Risk_W1" , "SchoolAtt_W2" ,  "LOC_Factor_W2")) %>%
  cluster4_plot("cluster4_edu_risk")
  
# c. Age 25 Outcomes
age25_plot <- function(df, file_name = NULL, height = 9.9){
  
  hlines <- df %>%
    mutate(intercept = ifelse(estimator %in% c("heckman", "reg"), 0, 1)) %>%
    select(outcome_clean, intercept) %>%
    distinct()
  
  p <- ggplot(df) +
    aes(x = cat_clean, y = beta, ymin = lci, ymax = uci,
        color = cat_clean) +
    geom_hline(data = filter(hlines, str_detect(outcome_clean, "NEET", TRUE)),
               mapping = aes(yintercept = intercept)) +
    geom_pointrange(position = position_dodge(0.5)) +
    facet_grid(outcome_clean ~ ., scales = "free", switch = "y") +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    guides(color = FALSE) +
    labs(x = NULL, y = NULL) +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          panel.spacing.y = unit(1.5, "lines"))
  
  if (!is.null(file_name)){
    file_name <- paste0("Images/", file_name, ".png")
    ggsave(filename = file_name, plot = p,
           dpi = 600, height = height, width = 21, units = "cm")
  }
  p
}

df_temp <- regsave %>%
  filter(var %in% c("cluster4", "Any_NEET"),
         type == "mi") %>%
  filter(Months_NEET)

df_temp$outcome %>% unique()

df_temp %>%
  filter(outcome %in% 
           c("Precarious_W8", "ShiftWork_W8",  "LogPay_W8", 
             "FinDiff_W8", "Employed_W8")) %>%
  age25_plot("age25_finance")

df_temp %>%
  filter(outcome %in% 
           c("PoorHealth_W8", "AUDIT_W8",
             "LifeSat_W8", "GHQ_W8_Likert")) %>%
  age25_plot("age25_health")

df_temp %>%
  filter(outcome %in% 
           c("Adult_W8", "LOC_Factor_W8")) %>%
  age25_plot("age25_adult")


# 4. Regression Tables ----
df_coefs <- regsave %>%
  select(string = n, part, outcome, estimator, Any_NEET, Months_NEET, cluster4, type) %>%
  distinct() %>%
  mutate(var = "n", level = 1, 
         string = format(string, big.mark = ",") %>% trimws()) %>%
  inner_join(lbl_clean, by = c("var", "level")) %>%
  left_join(lbl_clean %>%
              select(outcome = var, 
                     outcome_clean = var_clean), by = "outcome") %>%
  bind_rows(regsave) %>%
  mutate(outcome_clean = ifelse(part == "inflate", 
                                paste(outcome_clean, "(Inflate)"),
                                as.character(outcome_clean)),
         outcome_clean = ifelse(outcome == "cluster4",
                                str_replace_all(part, "_", " "),
                                outcome_clean)) %>%
  select(outcome, var_clean, cat_clean, outcome_clean, 
         string, index,  Any_NEET, Months_NEET, cluster4, index, type)

make_table <- function(df){
  flextable(df) %>%
    merge_v(1) %>%
    set_header_labels(var_clean = "", cat_clean = "Variable") %>%
    border_remove() %>%
    fontsize(size = 11, part = "all") %>%
    merge_h(part = "header") %>%
    border_inner_h(border = fp_border(color = "gray30", style = "dashed")) %>%
    hline_top(border = fp_border(color="black", width = 2), part = "all") %>%
    hline_bottom(border = fp_border(color="black", width = 2), part = "all") %>%
    fix_border_issues(part = "all") %>%
    align(j = 1:2, align="right", part = "all") %>%
    align(j = 3:ncol(df), align="center", part = "all") %>%
    valign(j = 1, valign = "top") %>%
    border(i = nrow(df), border.top = fp_border(color="black", width = 2), part = "body") %>%
    autofit()
}
get_coefs <- function(df, cols){
  df %>%
    distinct() %>%
    select(outcome_clean, var_clean, cat_clean, string, index) %>%
    pivot_wider(names_from = "outcome_clean", values_from = "string") %>%
    arrange(index) %>%
    mutate(across(1:2, as.character),
           var_clean = ifelse(var_clean == "Observations", " ", var_clean),
           var_clean = ifelse(var_clean == cat_clean, "", var_clean)) %>%
    select(cols)
}

# a. Predicting NEET
reg_tbl <- list()
reg_tbl$neet <- df_coefs %>%
  filter(outcome %in% c("Any_NEET", "Months_NEET", "cluster4"),
         type == "mi") %>%
  get_coefs(c(1, 2, 9, 8, 7, 4, 5, 6)) %>%
  make_table()
reg_tbl$neet
save_as_docx(reg_tbl$neet, path = "Tables/predict_neet.docx")


# b. Predicting Age 25 Outcomes
reg_tbl$age25_neet <- df_coefs %>%
  filter(!(outcome %in% c("Any_NEET", "Months_NEET", "cluster4")),
         type == "mi", Months_NEET, Any_NEET) %>%
  get_coefs(c(1, 2, 9, 6, 8, 5, 4, 7, 14, 13, 11, 10, 12)) %>%
  make_table()
reg_tbl$age25_neet
save_as_docx(reg_tbl$age25_neet, path = "Tables/predict_age25_neet.docx")

# c. Predicting Age 25 Outcomes
reg_tbl$age25_cluster <- df_coefs %>%
  filter(!(outcome %in% c("Any_NEET", "Months_NEET", "cluster4")),
         type == "mi", Months_NEET, cluster4) %>%
  get_coefs(c(1, 2, 9, 6, 8, 5, 4, 7, 14, 13, 11, 10, 12)) %>%
  make_table()
reg_tbl$age25_cluster
save_as_docx(reg_tbl$age25_cluster4, path = "Tables/predict_age25_cluster.docx")
