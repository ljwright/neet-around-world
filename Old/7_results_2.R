library(haven)
library(tidyverse)
library(mice)
library(glue)
library(Hmisc)
library(summarytools)
library(flextable)
library(officer)
library(gallimaufr)

rm(list=ls())

# 1. Set-Up ----
load("Data/mice.Rdata")
load("Data/mice_long.Rdata")


# Add Variables
df_child <- complete(imp, "long", TRUE) %>%
  as_tibble() %>%
  mutate(first_child = ifelse(Child_W8 != 'Yes', NA, FirstChild)) %>%
  select(NSID, imp = .imp, first_child)

imp_long <- imp_long %>%
  mutate(FemalexChild = case_when(Female == "No" & Child_W8 == "No" ~ "Male, No Child",
                                  Female == "Yes" & Child_W8 == "No" ~ "Female, No Child",
                                  Female == "No" & Child_W8 == "Yes" ~ "Male, with Child",
                                  Female == "Yes" & Child_W8 == "Yes" ~ "Female, with Child") %>%
           factor(c("Male, No Child", "Female, No Child", "Male, with Child", "Female, with Child"))) %>%
  select(-Status_W8) %>%
  left_join(df_child, by = c("NSID", "imp"))



# Clean Labels
lbl_clean <- c(n = "Observations",
               Any_NEET = "1+ Months NEET",
               Months_NEET = "Months NEET",
               neet_cluster = "NEET Cluster",
               neet_spells = "NEET Spells", mean_length = "NEET Spell Duration",
               n_spells = "Total Spells", entropy = "Sequence Entropy", 
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
               first_child = "Age at First Birth",
               FemalexChild = "Gender x Child",
               FirstChild = "Age at First Birth",
               FemalexFirstChild = "Female x Age at First Birth",
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
               "_cons" = "Intercept") %>%
  make_lbls(imp_long)

lbl_clean

# 2. Descriptive Statistics ----
desc <- list()

desc$all <- imp_long %>%
  filter(imp > 0) %>%
  get_desc("NSID", "imp", "Survey_Weight_W8") %>%
  select(var, cat, full_sample = string) %>%
  left_join(lbl_clean, by = c("var" = "var", "cat" = "desc_cat"))

desc$neet <- imp_long %>%
  filter(imp > 0) %>%
  get_desc("NSID", "imp", "Survey_Weight_W8", "Any_NEET") %>%
  select(-miss) %>%
  pivot_wider(names_from = group_var, values_from = string) %>%
  rename(neet_0 = No, neet_1 = Yes) %>%
  left_join(lbl_clean, by = c("var" = "var", "cat" = "desc_cat"))

desc$cluster <- imp_long %>%
  filter(imp > 0) %>%
  get_desc("NSID", "imp", "Survey_Weight_W8", "neet_cluster") %>%
  select(-miss) %>%
  pivot_wider(names_from = group_var, values_from = string) %>%
  left_join(lbl_clean, by = c("var" = "var", "cat" = "desc_cat"))

desc$tbl_1 <- left_join(desc$all, desc$neet) %>%
  arrange(index) %>%
  filter(!(level == 1 & levels == 2),
         var != "FirstChild",
         !(str_detect(var, "_W8") & !(var %in% c("Education_W8", "Child_W8")))) %>%
  mutate(cat_clean = ifelse(levels == 2 & cat_clean == "Yes", 
                            var_clean, cat_clean),
         var_clean = ifelse(var == cat | var_clean == cat_clean | (levels == 2 & level == 2),
                            "", var_clean)) %>%
  select(var_clean, cat_clean, `Full Sample` = full_sample, 
         `0 Months NEET` = neet_0, `1+ Months NEET` = neet_1) %>%
  drop_na() %>%
  make_flx(list(var_clean = "", cat_clean = "Variable"))
desc$tbl_1
save_as_docx(desc$tbl_1, path = "Tables/table_5_2.docx")

desc$tbl_2 <- left_join(desc$all, desc$cluster) %>%
  arrange(index) %>%
  filter(!(level == 1 & levels == 2),
         var != "FirstChild",
         !(str_detect(var, "_W8") & !(var %in% c("Education_W8", "Child_W8")))) %>%
  mutate(cat_clean = ifelse(levels == 2 & cat_clean == "Yes", 
                            var_clean, cat_clean),
         var_clean = ifelse(var == cat | var_clean == cat_clean | (levels == 2 & level == 2),
                            "", var_clean)) %>%
  select(var_clean, cat_clean, `Full Sample` = full_sample, 
         `Not NEET`:`Further Education`) %>%
  drop_na() %>%
  make_flx(list(var_clean = "", cat_clean = "Variable"))
desc$tbl_2
save_as_docx(desc$tbl_2, path = "Tables/table_5_3.docx")



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
  mutate(across(c(Any_NEET, Months_NEET, neet_cluster), as.logical), 
         string = glue("{round(beta, 2)}\n({round(lci, 2)}, {round(uci, 2)})")) %>%
  left_join(lbl_clean, by = c("var", "level")) %>%
  left_join(lbl_clean %>%
              select(outcome = var, 
                     outcome_clean = var_clean), by = "outcome")


cluster4_lvls <- levels(imp_long$cluster4)[2:5]

clean_part <- function(str){
  num <- str_sub(str, 1, 1) %>% as.numeric()
  cluster4_lvls[num]
}

df_ame <- bind_rows(read_dta("Data/regsave_ame.dta") %>%
                      filter(!(outcome %in% c("cluster4", "Months_NEET", "Any_NEET"))),
                    read_dta("Data/regsave_ame2.dta") %>%
                      filter(outcome %in% c("cluster4", "Months_NEET", "Any_NEET"))) %>% 
  filter(coef != 0, str_detect(var, "/", TRUE)) %>%
  separate(var, c("var", "part"), sep = ":", fill = "right") %>%
  mutate(part = ifelse(is.na(part), outcome, part),
         var = str_replace(var, "1.Any_NEET", "2.Any_NEET")) %>%
  separate(var, c("level", "var"), sep = "\\.", fill = "left", 
           extra = "merge", convert = TRUE) %>%
  mutate(level = ifelse(is.na(level), 1, level),
         level = ifelse(var == "FemalexChild", level+1, level),
         level = ifelse(str_detect(var, "\\#"), 1, level)) %>%
  rename(beta = coef, se = stderr, p = pval, 
         lci = ci_lower, uci = ci_upper, n = N) %>%
  filter(!(part == "Employed_W8" & str_detect(estimator, "^heck"))) %>%
  mutate(across(c(Any_NEET, Months_NEET, cluster4), as.logical), 
         string = glue("{round(beta, 2)}\n({round(lci, 2)}, {round(uci, 2)})")) %>%
  left_join(lbl_clean, by = c("var", "level")) %>%
  left_join(lbl_clean %>%
              select(outcome = var, 
                     outcome_clean = var_clean), by = "outcome") %>%
  mutate(part_x = clean_part(part),
         part = ifelse(outcome == "cluster4", part_x, part))

# 3. Regression Plots ----
# a. Any NEET / Months NEET
neet_plot <- function(df, file_name = NULL, height = 9.9, y_int = c(1, 1)){
  hlines <- tibble(outcome_clean = c("1+ Months NEET", "Months NEET"),
                   yint = y_int)
  
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
    facet_grid(var_clean ~ outcome_clean, scales = "free", 
               space = "free_y", switch = "y",
               labeller = labeller(var_clean = strip_labels)) +
    labs(x = NULL, y = NULL) +
    guides(color = FALSE) +
    theme_minimal() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0))
  if (!is.null(file_name)){
    file_name <- glue(file_name) %>%
      paste0("Images/", ., ".png")
    ggsave(filename = file_name, plot = p,
           dpi = 600, height = height, width = 21, units = "cm")
  }
  p
}

neet_plots <- function(df_temp, ame = FALSE){
  if (ame == TRUE){
    fle <- "ame_"
    y_int = c(0, 0)
  } else{
    fle <- ""
    y_int = c(1, 1)
  }
  
  df_temp %>%
    neet_plot(glue("neet_{fle}all_vars"), 29.7, y_int = y_int)
  
  df_temp %>%
    filter(str_detect(var, "Child") |
             str_detect(var, "Female")) %>%
    neet_plot(glue("neet_{fle}gender_child"), y_int = y_int)
  
  df_temp %>%
    filter(var %in% 
             c("Any_VET", "NonWhite", "ForLangHH_W1", "HHType_W1")) %>%
    neet_plot(glue("neet_{fle}vet_household"), y_int = y_int)
  
  df_temp %>%
    filter(var %in% 
             c("GHQ_W2_Caseness", "GenHealth_W2", "Disabled_W2")) %>%
    neet_plot(glue("neet_{fle}health"), y_int = y_int)
  
  df_temp %>%
    filter(var %in% 
             c("IMD_W2", "ParentEduc5_W1", "GParentUni_W1", "NSSEC3_W1")) %>%
    neet_plot(glue("neet_{fle}sep"), y_int = y_int)
  
  df_temp %>%
    filter(var %in% 
             c("Risk_W1" , "SchoolAtt_W2" ,  "LOC_Factor_W2")) %>%
    neet_plot(glue("neet_{fle}risk"), y_int = y_int)
  
  df_temp %>%
    filter(var %in% 
             c("Education_W8")) %>%
    neet_plot(glue("neet_{fle}edu"), y_int = y_int)
}


df_temp <- regsave %>%
  filter(outcome %in% c("Months_NEET", "Any_NEET"),
         part != "inflate",
         type == "mi", var != "_cons")
neet_plots(df_temp)


df_temp <- df_ame %>%
  filter(outcome %in% c("Months_NEET", "Any_NEET"),
         type == "mi", var != "_cons")
neet_plots(df_temp, TRUE)


# b. Cluster Membership 
cluster4_plot <- function(df, file_name = NULL, height = 9.9, y_int = 1, y_title = "Odds Ratio"){
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
    geom_hline(yintercept = y_int) +
    geom_pointrange(position = position_dodge(0.6)) +
    scale_color_brewer(palette = "Dark2") +
    coord_flip() +
    facet_grid(var_clean ~ ., scales = "free", switch = "y", space = "free_y",
               labeller = labeller(var_clean = strip_labels)) +
    labs(x = NULL, y = y_title, color = "Cluster") +
    theme_minimal() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          legend.position = "bottom")
  if (!is.null(file_name)){
    file_name <- glue(file_name) %>%
      paste0("Images/", ., ".png")
    ggsave(filename = file_name, plot = p,
           dpi = 600, height = height, width = 21, units = "cm")
  }
  p
}

cluster_plots <- function(df_temp, ame = FALSE){
  if (ame == TRUE){
    fle <- "ame_"
    y_int <- 0
    y_title <- NULL
  } else{
    fle <- ""
    y_int <- 1
    y_title <- "Odds Ratio"
  }
  
  df_temp %>%
    cluster4_plot(glue("cluster4_{fle}all_vars"), 29.7, 
                  y_int, y_title)
  
  df_temp %>%
    filter(str_detect(var, "Child") |
             str_detect(var, "Female")) %>%
    cluster4_plot(glue("cluster4_{fle}gender_child"),
                  y_int = y_int, y_title = y_title)
  
  df_temp %>%
    filter(var %in% 
             c("Any_VET", "NonWhite", "ForLangHH_W1", "HHType_W1")) %>%
    cluster4_plot(glue("cluster4_{fle}vet_household"),
                  y_int = y_int, y_title = y_title)
  
  df_temp %>%
    filter(var %in% 
             c("GHQ_W2_Caseness", "GenHealth_W2", "Disabled_W2")) %>%
    cluster4_plot(glue("cluster4_{fle}health"),
                  y_int = y_int, y_title = y_title)
  
  df_temp %>%
    filter(var %in% 
             c("IMD_W2", "ParentEduc5_W1", "GParentUni_W1", "NSSEC3_W1")) %>%
    cluster4_plot(glue("cluster4_{fle}sep"),
                  y_int = y_int, y_title = y_title)
  
  df_temp %>%
    filter(var %in% 
             c("Risk_W1" , "SchoolAtt_W2" ,  "LOC_Factor_W2")) %>%
    cluster4_plot(glue("cluster4_{fle}risk"),
                  y_int = y_int, y_title = y_title)
  
}

df_temp <- regsave %>%
  filter(outcome == "cluster4", type == "mi",
         !(var %in% c("_cons", "Education_W8")))
cluster_plots(df_temp)


df_temp <- df_ame %>%
  filter(outcome == "cluster4", type == "mi",
         !(var %in% c("_cons", "Education_W8")))
cluster_plots(df_temp, TRUE)


# c. Age 25 Outcomes
age25_plot <- function(df, file_name = NULL, height = 9.9, ame = FALSE){
  
  hlines <- df %>%
    mutate(intercept = ifelse(estimator %in% c("heckman", "reg"), 0, 1)) %>%
    select(outcome_clean, intercept) %>%
    distinct()
  
  if (ame == TRUE){
    hlines <- hlines %>%
      mutate(intercept = 0)
  }
  
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

age25_plots <- function(df_temp, ame = FALSE){
  if (ame == TRUE){
    fle <- "ame_"
  } else{
    fle <- ""
  }
  
  df_temp %>%
    age25_plot(glue("age25_{fle}all_vars"), 29.7, ame)
  
  df_temp %>%
    filter(outcome %in% 
             c("Precarious_W8", "ShiftWork_W8",  "LogPay_W8", 
               "FinDiff_W8", "Employed_W8")) %>%
    age25_plot(glue("age25_{fle}finance"), 19.8, ame)
  
  df_temp %>%
    filter(outcome %in% 
             c("PoorHealth_W8", "AUDIT_W8",
               "LifeSat_W8", "GHQ_W8_Likert")) %>%
    age25_plot(glue("age25_{fle}health"), 19.8, ame)
  
  df_temp %>%
    filter(outcome %in% 
             c("Adult_W8", "LOC_Factor_W8")) %>%
    age25_plot(glue("age25_{fle}adult"), ame = ame)
  
}
df_temp <- regsave %>%
  filter(var %in% c("cluster4", "Any_NEET"),
         type == "mi") %>%
  filter(Months_NEET)
age25_plots(df_temp)

df_temp <- df_ame %>%
  filter(var %in% c("cluster4", "Any_NEET"),
         type == "mi") %>%
  filter(Months_NEET)
age25_plots(df_temp, TRUE)


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
reg_tbl$age25_cluster4 <- df_coefs %>%
  filter(!(outcome %in% c("Any_NEET", "Months_NEET", "cluster4")),
         type == "mi", Months_NEET, cluster4) %>%
  get_coefs(c(1, 2, 9, 6, 8, 5, 4, 7, 14, 13, 11, 10, 12)) %>%
  make_table()
reg_tbl$age25_cluster4
save_as_docx(reg_tbl$age25_cluster4, path = "Tables/predict_age25_cluster.docx")


# 5. AME Plots ----
df_ame <- read_dta("Data/regsave_ame.dta") %>%
  filter(coef != 0, str_detect(var, "/", TRUE)) %>%
  separate(var, c("var", "part"), sep = ":", fill = "right") %>%
  mutate(part = ifelse(is.na(part), outcome, part),
         var = str_replace(var, "1.Any_NEET", "2.Any_NEET")) %>%
  separate(var, c("level", "var"), sep = "\\.", fill = "left", 
           extra = "merge", convert = TRUE) %>%
  mutate(level = ifelse(is.na(level), 1, level),
         level = ifelse(var == "FemalexChild", level+1, level),
         level = ifelse(str_detect(var, "\\#"), 1, level)) %>%
  rename(beta = coef, se = stderr, p = pval, 
         lci = ci_lower, uci = ci_upper, n = N) %>%
  filter(!(part == "Employed_W8" & str_detect(estimator, "^heck"))) %>%
  mutate(across(c(Any_NEET, Months_NEET, cluster4), as.logical), 
         string = glue("{round(beta, 2)}\n({round(lci, 2)}, {round(uci, 2)})")) %>%
  left_join(lbl_clean, by = c("var", "level")) %>%
  left_join(lbl_clean %>%
              select(outcome = var, 
                     outcome_clean = var_clean), by = "outcome")

df_temp <- df_ame %>%
  filter(outcome %in% c("Months_NEET", "Any_NEET"),
         part != "inflate",
         type == "mi", var != "_cons")


df_temp$outcome %>% unique()

df_temp %>%
  neet_plot()
# ADD HLINE OPTION; CHANGE FACET_GRID OPTION TO SPACE = FREE_Y
# standardize continuous?

# 6. Rename Files ----
