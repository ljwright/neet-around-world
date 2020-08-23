library(haven)
library(tidyverse)
library(mice)
library(lavaan)
library(glue)
library(Hmisc)
library(tictoc)

setwd("/cloud/project/NEET Book")
# setwd("D:/Next Steps 1-8/Projects/NEET Book")
rm(list=ls())

# 1. Load Files ----
clust_names <- c("Into Employment",
                 "Cyclers",
                 "Higher Education",
                 "Long-Term NEET")
df_clusters <- read_dta("Data/neet_clusters.dta") %>%
  select(NSID, cluster4) %>%
  mutate(cluster4 = factor(cluster4, labels = clust_names))

df_raw <- read_dta("Data/Dataset.dta") %>%
  as_factor() %>%
  left_join(df_clusters, by = "NSID")


# 2. Locus of Control ----
# Wave 2
df_loc <- df_raw %>%
  select(NSID, Survey_Weight_W2, matches("LOC_W2_Item")) %>%
  drop_na()

int <- paste0("Int_LOC_W2_Item", 1:3)
ext <- paste0("Ext_LOC_W2_Item", 1:3)
items <- c(ext, int)

int_cov <- combn(int, 2, simplify = FALSE) %>%
  map_chr(paste, collapse = " ~~ ") %>%
  paste(collapse = "\n")

loc_w2 <- glue("LOC_Factor_W2 =~ {paste(items, collapse = ' + ')}; {int_cov}") %>%
  cfa(df_loc, estimator = "DWLS", sampling.weights = "Survey_Weight_W2") %>%
  lavPredict() %>%
  as.data.frame() %>% as_tibble() %>%
  bind_cols(select(df_loc, NSID, Survey_Weight_W2)) %>%
  mutate(mean = wtd.mean(LOC_Factor_W2, Survey_Weight_W2),
         sd = wtd.var(LOC_Factor_W2, Survey_Weight_W2) %>% sqrt(),
         LOC_Factor_W2 = (LOC_Factor_W2 - mean) / sd) %>%
  select(NSID, LOC_Factor_W2)

# Wave 8
df_loc <- df_raw %>%
  select(NSID, Survey_Weight_W8, matches("LOC_W8")) %>%
  drop_na()
items <- glue("LOC_W8_Item_{1:4}")

loc_w8 <- glue("LOC_Factor_W8 =~ {paste(items, collapse = ' + ')}") %>%
  cfa(df_loc, estimator = "DWLS", sampling.weights = "Survey_Weight_W8") %>%
  lavPredict() %>%
  as.data.frame() %>% as_tibble() %>%
  bind_cols(select(df_loc, NSID, Survey_Weight_W8)) %>%
  mutate(mean = wtd.mean(LOC_Factor_W8, Survey_Weight_W8),
         sd = wtd.var(LOC_Factor_W8, Survey_Weight_W8) %>% sqrt(),
         LOC_Factor_W8 = (LOC_Factor_W8 - mean) / sd) %>%
  select(NSID, LOC_Factor_W8)

# Combine
df <- df_raw %>%
  left_join(loc_w2, by = "NSID") %>%
  left_join(loc_w8, by = "NSID") %>%
  select(-Survey_Weight_W2, -matches("Item"), - Any_NEET) %>%
  filter(!is.na(Survey_Weight_W8))

# 3. MICE set-up ----
f <- list()
f[".LogPay_W8"] <- "ifelse(Status_W8 == 'Employed', LogPay_W8, 0)"
f[".Precarious_W8"] <- "factor(ifelse(Status_W8 == 'Employed', as.character(Precarious_W8), 'No'))"
f[".ShiftWork_W8"] <- "factor(ifelse(Status_W8 == 'Employed', as.character(ShiftWork_W8), 'No'))"
f[".FirstChild"] <- "ifelse(Child_W8 == 'Yes', FirstChild, 0)"
f[".FirstChildxFemale"] <- "ifelse(Child_W8 == 'Yes' & Female=='Yes', FirstChild, 0)"
f[".cluster4"] <- "factor(ifelse(Months_NEET==0, 'Not NEET', as.character(cluster4)))"

df_mice <- map_dfc(f, ~ with(df, eval(parse(text = .x)))) %>%
  bind_cols(df, .)

meth <- make.method(df_mice)
meth[names(f)] <- paste0("~ I(",unlist(f),")")

pred <- make.predictorMatrix(df_mice)
pred[, "NSID"] <- 0

pred[, gsub("\\.","", names(f))] <- 0
diag(pred[gsub("\\.","", names(f))[1:3], names(f)[1:3]]) <- 0
pred[gsub("\\.","", names(f))[1:3], "Status_W8"] <- 0
pred["Status_W8", names(f)[1:3]] <- 0

pred[str_subset(names(df_mice), "Child"),
     str_subset(names(df_mice), "Child")] <- 0

pred["Months_NEET", ".cluster4"] <- 0

diag(pred) <- 0

post <- make.post(df_mice)

visit <- c("Months_NEET","cluster4",".cluster4",
           "Status_W8","LogPay_W8",".LogPay_W8",
           "Precarious_W8",".Precarious_W8",
           "ShiftWork_W8",".ShiftWork_W8",
           "FirstChild",".FirstChild",".FirstChildxFemale")
visit <- c(visit, names(df_mice)[!(names(df_mice) %in% visit)])


# 4. Run mice ----
# imp <- mice(df_mice,
# 				m = 1, meth = meth,
# 	  			pred = pred,visit=visit,
# 	  			post = post, print = TRUE,
# 	  			seed = 1,  maxit = 2)
set.seed(1)
tic()
imp <- parlmice(df_mice, 
                meth = meth, pred = pred, visit = visit,
                print = FALSE, maxit = 10, post = post,
                n.imp.core = 10, n.core= 4)
toc()
save(imp, file = "Data/mice.Rdata")

# 5. Export results ----
load("Data/mice.Rdata")
imp_long <- complete(imp, "long", TRUE) %>%
  as_tibble() %>%
  select(-all_of(str_replace(names(f), "\\.", "")),
         -.id, - matches("xFemale")) %>%
  rename_with(~ str_replace(.x, "\\.", "")) %>%
  mutate(across(c(Precarious_W8, ShiftWork_W8),
                ~ ifelse(Status_W8 != 'Employed', NA, as.character(.x)) %>%
                  factor()),
         LogPay_W8 = ifelse(Status_W8 != 'Employed', NA, LogPay_W8),
         cluster4 = ifelse(Months_NEET == 0, 'Not NEET', as.character(cluster4)) %>%
           factor(levels = c('Not NEET', clust_names)),
         Any_NEET = ifelse(Months_NEET == 0, 'No', 'Yes') %>%
           factor(),
         Employed_W8 = ifelse(Status_W8 == 'Employed', 'Yes', 'No') %>%
           factor()) %>%
  mutate(FirstChild = ifelse(Child_W8 != 'Yes', NA, FirstChild),
         FirstChild = FirstChild - wtd.mean(FirstChild, Survey_Weight_W8),
         FirstChild = ifelse(Child_W8 == 'No', 0, FirstChild))
save(imp, imp_long, file = "Data/mice.Rdata")
write_dta(imp_long, "Data/mice_long.dta")

