library(tidyverse)
library(haven)
library(WeightedCluster)
library(TraMineR)
library(glue)

rm(list=ls())

# 1. Load Files ----
df_seq <- read_dta("Data/Activity Sequences.dta") %>%
  arrange(NSID) %>%
  drop_na() %>%
  rename_with(~ glue("{str_sub(.x, -2)}.{str_sub(.x, 4, 7)}"), -NSID)

df_weight <- read_dta("Data/Dataset.dta") %>%
  arrange(NSID) %>% 
  select(NSID, Survey_Weight_W8, Any_NEET) %>%
  semi_join(df_seq, by = "NSID") %>%
  mutate(all_weight = Survey_Weight_W8*n()/sum(Survey_Weight_W8)) %>%
  group_by(Any_NEET) %>%
  mutate(neet_weight = Survey_Weight_W8*n()/sum(Survey_Weight_W8)) %>%
  ungroup()

# 2. Create TraMineR Objects ----
# Model Arguments
state_labels <- state_names <- c("Education", "Employment", "VET", "NEET")
state_short <- c("ED", "EMP", "VET", "NEET")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
state_cols <- cbPalette[c(4, 6, 7, 8)] # c("#F7F7F7", "#CCCCCC", "#969696", "#525252") 

# All Sequences
seq_all <- df_seq %>%
  select(-NSID) %>%
  seqdef(states = state_names, cpal = state_cols,
         labels = state_labels, missing = NA,
         missing.color = cbPalette[5], left = NA,
         gaps = NA, right = "DEL", xtstep = 12,
         weights = df_weight$all_weight)

png(filename = "Images/seq_all_index.png",
    width = 20, height = 20, units="in", res=300)
seqIplot(seq_all, sortv = "from.start", with.legend = FALSE)
dev.off()

png(filename = "Images/seq_all_density.png",
    width = 20, height = 20, units="in", res=300)
seqdplot(seq_all, sortv = "from.start", with.legend = FALSE)
dev.off()

seqlegend(seq_all)

# NEET Sequences
any_neet <- df_weight$Any_NEET == 1

seq_neet <- df_seq %>%
  filter(any_neet) %>%
  select(-NSID) %>%
  seqdef(states = state_names, cpal = state_cols,
         labels = state_labels, missing = NA,
         missing.color = cbPalette[5], left = NA,
         gaps = NA, right = "DEL", xtstep = 12,
         weights = df_weight$neet_weight[any_neet])

png(filename = "Images/seq_neet_index.png",
    width = 20, height = 20, units="in", res=300)
seqIplot(seq_neet, sortv = "from.start", with.legend = FALSE)
dev.off()

png(filename = "Images/seq_neet_density.png",
    width = 20, height = 20, units="in", res=300)
seqdplot(seq_neet, sortv = "from.start", with.legend = FALSE)
dev.off()


# 3. Sequence Analysis ----
seq_costs <- seqsubm(seq_neet, method = "CONSTANT")
seq_dists <- seqdist(seq_neet, method = "OM",
                     indel = 1, sm = seq_costs)
ward_clust <- hclust(as.dist(seq_dists), method="ward.D",
                     members = df_weight$neet_weight[any_neet])

# Cluster Quality
clust_qual <- as.clustrange(ward_clust, seq_dists,
                            weights = df_weight$neet_weight[any_neet],
                            ncluster = 8)

clustering <- as_tibble(clust_qual$clustering)
clustering$NSID <- df_weight$NSID[any_neet]
save(clustering, file="Data/neet_clusters.R")
write_dta(clustering,"Data/neet_clusters.dta")

plot(clust_qual)
plot(clust_qual$stats$ASW)
summary(clust_qual, max.rank = 2)

# Index and Density Plots
filepath <- "Images/Cluster Solutions 2/"

png(filename = paste0(filepath,"Legend.png"),width = 800, height = 600, pointsize=22)
seqlegend(neet_seq)
dev.off()

get_plots <- function(cluster){
  # cluster <- paste0("cluster", cluster)
  
  file <- paste0(filepath, cluster, "_index.png")
  png(filename = file,width = 20, height = 20, units="in", res=300)
  seqIplot(seq_neet, group = clustering[[cluster]], sortv = "from.start")
  dev.off()
  
  file <- paste0(filepath, cluster, "_density.png")
  png(filename = file, width = 20, height = 20, units="in", res=300)
  seqdplot(seq_neet, group = clustering[[cluster]], sortv = "from.start")
  dev.off()
}
clusters <- paste0("cluster", 3:8) %>%
  set_names()

map(clusters, get_plots)
clust_qual$ind <- map(clusters, 
                      ~  wcClusterQuality(seq_dists, clustering[[.x]],
                                          weights = df_weight$Survey_Weight_W8[any_neet]))


# 4. Cluster Solutions ----
# Choice: Cluster 4.
# Quality Measures
# Visual Inspection (Improvement on Cluster 3, Nothing Gained in Cluster 4)
# Final Plots
clust_names <- c("Into Employment", "Cyclers",
                 "Higher Education", "Long-Term NEET")
clust_solution <- factor(clustering$cluster4, labels = clust_names)

png(filename = "Images/cluster4_index.png", width = 20.592,
    height = 20.592, units = "cm", res = 300)
seqIplot(seq_neet, group = clust_solution, sortv = "from.start")
dev.off()

png(filename = "Images/cluster4_density.png", width = 20.592,
    height = 20.592, units = "cm", res = 300)
seqdplot(seq_neet, group = clust_solution, sortv = "from.start",border=NA)
dev.off()

png(filename = "Images/seq_all_index_publication.png", width = 20.592,
    height = 20.592, units = "cm", res = 300)
seqIplot(seq_all, sortv = "from.start", with.legend = TRUE)
dev.off()

png(filename = "Images/seq_all_density_publication.png", width = 20.592,
    height = 20.592, units = "cm", res = 300)
seqdplot(seq_all, sortv = "from.start", with.legend = TRUE, border = NA)
dev.off()


df_weight %>%
  left_join(df_seq, by = "NSID") %>%
  select(NSID, wt = neet_weight, matches("\\.")) %>%
  filter(any_neet) %>%
  left_join(clustering %>%
              select(NSID, cluster5), by = "NSID") %>%
  arrange(across(matches("\\."))) %>%
  group_by(cluster5, across(matches("\\."))) %>%
  summarise(id = cur_group_id(),
            wt = sum(wt),
            NSID = NSID[1],
            .groups = "drop") %>%
  group_by(cluster5) %>%
  mutate(ymax = cumsum(wt),
         ymin = ymax - wt) %>%
  ungroup() %>%
  pivot_longer(matches("\\."), names_to = "date", values_to = "activity") %>%
  group_by(id) %>%
  mutate(xmin = as.Date(glue("01.{date}"), "%d.%m.%Y"),
         xmax = ifelse(row_number() == n(), lead(xmin), xmin + 30) %>%
           as.Date(origin = "1970-01-01")) %>%
  ungroup() %>%
  mutate(activity = as_factor(activity)) %>%
  # filter(id %in% sample(unique(id), 100)) %>%
  ggplot() +
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
      fill = activity, color = activity) +
  facet_wrap(~ cluster5, scales = "free_y") + 
  geom_rect()


