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

rm(imp, df_child)

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
save_as_docx(desc$tbl_1, path = "Tables/table_neet.docx")

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
save_as_docx(desc$tbl_2, path = "Tables/table_cluster.docx")


imp_long %>%
  mutate(sample = ifelse(imp == 0, 0, 1),
         wt = Survey_Weight_W8*n()/sum(Survey_Weight_W8)) %>%
  drop_na(sample, neet_cluster, wt) %>%
  count(sample, neet_cluster, wt = wt) %>%
  add_count(sample, wt = n, name = "total") %>%
  mutate(prop = n*100/total)


# 3. Regression Results ----
wtd_vars <- imp_long %>%
  filter(imp > 0) %>%
  select(where(is.numeric)) %>%
  pivot_longer(-Survey_Weight_W8) %>%
  group_by(name) %>%
  summarise(var = wtd.var(value, Survey_Weight_W8) %>% sqrt(),
            .groups = "drop") %>%
  deframe()

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
  mutate(std = 2 * wtd_vars[var],
         std = ifelse(is.na(std), 1, std)) %>%
  filter(!(part == "Employed_W8" & str_detect(estimator, "^heck"))) %>%
  mutate(across(c(beta, se, lci, uci), ~ .x * std),
         across(c(beta, se, lci, uci),
                ~ ifelse((estimator %in% c("logit", "mlogit", "zinb")) |
                           part == "Employed_W8", exp(.x), .x))) %>%
  mutate(across(c(Any_NEET, Months_NEET, neet_cluster), as.logical), 
         string = glue("{round(beta, 2)}\n({round(lci, 2)}, {round(uci, 2)})")) %>%
  left_join(lbl_clean, by = c("var", "level")) %>%
  left_join(lbl_clean %>%
              mutate(outcome_clean = fct_reorder(var_clean, index)) %>%
              select(outcome = var, outcome_clean) %>%
              distinct(), by = "outcome") %>%
  mutate(cat_clean = ifelse(levels == 2 & cat_clean == "Yes", 
                            var_clean, cat_clean),
         var_clean = ifelse(var == cat | var_clean == cat_clean | (levels == 2 & level == 2),
                            "", var_clean),
         var_ref = ifelse(var_clean == "", "", var_ref),
         var = fct_reorder(var, index),
         var_clean = fct_reorder(var_clean, index),
         cat_clean = fct_reorder(cat_clean, index))

neet_cluster_lvls <- levels(imp_long$neet_cluster)[-1]

# 3. Regression Plots ----
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

get_strip <- function(df){
  df %>% 
    select(var, cat, var_clean, cat_clean, var_ref) %>% 
    distinct() %>%
    mutate(across(everything(), as.character),
           strip_label = ifelse(var == cat, "", var_ref)) %>%
    select(var, strip_label) %>%
    deframe()
}

make_plot <- function(df, plot_func, file_name = NULL, height = 9.9){
  p <- plot_func(df)
  
  if (!is.null(file_name)){
    glue("Images/Regressions/{file_name}.png") %>%
      ggsave(plot = p, dpi = 1200, height = height,
             width = 21, units = "cm")
  }
  
  p
}


# a. Any NEET / Months NEET
neet_plot <- function(df){
  hlines <- tibble(outcome_clean = c("1+ Months NEET", "Months NEET"),
                   yint = c(1,1))
  
  ggplot(df) +
    aes(x = cat_clean, y = beta, ymin = lci, 
        ymax = uci, color = part) +
    geom_hline(data = hlines, aes(yintercept = yint)) +
    geom_pointrange(position = position_dodge(0.6)) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_log10() +
    coord_flip() +
    facet_grid(var ~ outcome_clean, scales = "free", 
               space = "free_y", switch = "y",
               labeller = labeller(var = get_strip(df))) +
    labs(x = NULL, y = NULL) +
    guides(color = "none") +
    theme_minimal() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          panel.spacing.x = unit(1.5, "lines"))
}

df_neet <- regsave %>%
  filter(outcome %in% c("Months_NEET", "Any_NEET"),
         part != "inflate",
         type == "mi", var != "_cons")

df_neet %>%
  make_plot(neet_plot, "neet_all_vars", 29.7)

df_neet %>%
  filter(str_detect(var, "Child") |
           str_detect(var, "Female")) %>%
  make_plot(neet_plot, "neet_gender_child")

df_neet %>%
  filter(var %in% c("Any_VET", "NonWhite", "ForLangHH_W1", "HHType_W1")) %>%
  make_plot(neet_plot, "neet_vet_household")

df_neet %>%
  filter(var %in% c("GHQ_W2_Caseness", "GenHealth_W2", "Disabled_W2")) %>%
  make_plot(neet_plot, "neet_health")

df_neet %>%
  filter(var %in% c("IMD_W2", "ParentEduc5_W1", "GParentUni_W1", "NSSEC3_W1")) %>%
  make_plot(neet_plot, "neet_sep")

df_neet %>%
  filter(var %in% c("Risk_W1", "SchoolAtt_W2", "LOC_Factor_W2")) %>%
  make_plot(neet_plot, "neet_risk")

df_neet %>%
  filter(var %in% c("Education_W8")) %>%
  make_plot(neet_plot, "neet_edu")


# b. Cluster Membership 
cluster_plot <- function(df){
  ggplot(df) +
    aes(x = cat_clean, y = beta, ymin = lci, ymax = uci,
        color = part, shape = part) +
    geom_hline(yintercept = 1) +
    geom_pointrange(position = position_dodge(0.6), size = 0.3) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_log10() +
    coord_flip() +
    facet_grid(var ~ ., scales = "free", switch = "y", space = "free_y",
               labeller = labeller(var =  get_strip(df))) +
    labs(x = NULL, y = "Odds Ratio", color = "Cluster", shape = "Cluster") +
    guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
    theme_minimal() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          legend.position = "bottom")
}

df_clust <- regsave %>%
  filter(outcome == "neet_cluster", type == "mi",
         !(var %in% c("_cons", "Education_W8"))) %>%
  mutate(part = str_replace(part, "Long_Term", "Long-Term") %>%
           str_replace("_", " ") %>%
           factor(levels(imp_long$neet_cluster)))

make_plot(df_clust, cluster_plot, "cluster_all_vars", 29.7)

df_clust %>%
  filter(str_detect(var, "Child") |
           str_detect(var, "Female")) %>%
  make_plot(cluster_plot, "cluster_gender_child")

df_clust %>%
  filter(var %in% c("Any_VET", "NonWhite", "ForLangHH_W1", "HHType_W1")) %>% 
  make_plot(cluster_plot, "cluster_vet_household")

df_clust %>%
  filter(var %in% c("GHQ_W2_Caseness", "GenHealth_W2", "Disabled_W2")) %>%
  make_plot(cluster_plot, "cluster_health")

df_clust %>%
  filter(var %in% c("IMD_W2", "ParentEduc5_W1", "GParentUni_W1", "NSSEC3_W1")) %>%
  make_plot(cluster_plot, "cluster_sep")

df_clust %>%
  filter(var %in% c("Risk_W1", "SchoolAtt_W2", "LOC_Factor_W2")) %>%
  make_plot(cluster_plot, "cluster_risk")


# c. Age 25 Outcomes
age25_plot <- function(df){
  hlines <- df %>%
    mutate(intercept = ifelse(estimator %in% c("heckman", "reg", "heckprobit", "tobit"), 0, 1)) %>%
    select(outcome_clean, intercept) %>%
    distinct()
  
  ggplot(df) +
    aes(x = cat_clean, y = beta, ymin = lci, ymax = uci) +
    geom_hline(data = filter(hlines, str_detect(outcome_clean, "NEET", TRUE)),
               mapping = aes(yintercept = intercept)) +
    geom_pointrange(position = position_dodge(0.5), color = cbbPalette[6]) +
    facet_grid(outcome_clean ~ ., scales = "free", switch = "y") +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0),
          panel.spacing.y = unit(1.5, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

df_25 <- regsave %>%
  filter(var %in% c("neet_cluster", "Any_NEET"),
         type == "mi",
         !(outcome %in% c("AUDIT_W8", "Adult_W8") & estimator == "reg")) %>%
  filter(Months_NEET == FALSE)

make_plot(df_25, age25_plot, "age25_all_vars", 29.7)

df_25 %>%
  filter(outcome %in% 
           c("Precarious_W8", "ShiftWork_W8",  "LogPay_W8", 
             "FinDiff_W8", "Employed_W8")) %>%
  make_plot(age25_plot, "age25_finance", 19.8)

df_25 %>%
  filter(outcome %in% 
           c("PoorHealth_W8", "AUDIT_W8",
             "LifeSat_W8", "GHQ_W8_Likert",
             "Adult_W8", "LOC_Factor_W8")) %>%
  make_plot(age25_plot, "age25_health", 21)


# 4. Rename Files ----
file.copy("Tables/table_neet.docx", "Tables/table_5_2.docx", overwrite = TRUE)
file.copy("Tables/table_cluster.docx", "Tables/table_5_3.docx", overwrite = TRUE)

file.copy("Images/Regressions/neet_all_vars.png", "Images/figure_5_5.png", overwrite = TRUE)

file.copy("Images/Sequences/seq_density_all.png", "Images/figure_5_6.png", overwrite = TRUE)
file.copy("Images/Sequences/seq_density_all_bw.png", "Images/figure_5_6_bw.png", overwrite = TRUE)
file.copy("Images/Sequences/seq_index_all.png", "Images/figure_5_7.png", overwrite = TRUE)
file.copy("Images/Sequences/seq_index_all_bw.png", "Images/figure_5_7_bw.png", overwrite = TRUE)

file.copy("Images/Sequences/seq_density_neet.png", "Images/figure_5_8.png", overwrite = TRUE)
file.copy("Images/Sequences/seq_density_neet_bw.png", "Images/figure_5_8_bw.png", overwrite = TRUE)
file.copy("Images/Sequences/seq_index_neet.png", "Images/figure_5_9.png", overwrite = TRUE)
file.copy("Images/Sequences/seq_index_neet_bw.png", "Images/figure_5_9_bw.png", overwrite = TRUE)

file.copy("Images/Regressions/cluster_gender_child.png", "Images/figure_5_10.png", overwrite = TRUE)
file.copy("Images/Regressions/cluster_sep.png", "Images/figure_5_11.png", overwrite = TRUE)
file.copy("Images/Regressions/cluster_vet_household.png", "Images/figure_5_12.png", overwrite = TRUE)
file.copy("Images/Regressions/cluster_health.png", "Images/figure_5_13.png", overwrite = TRUE)
file.copy("Images/Regressions/cluster_risk.png", "Images/figure_5_14.png", overwrite = TRUE)

file.copy("Images/Regressions/age25_finance.png", "Images/figure_5_15.png", overwrite = TRUE)
file.copy("Images/Regressions/age25_health.png", "Images/figure_5_16.png", overwrite = TRUE)