library(tidyverse)
library(haven)
library(WeightedCluster)
library(TraMineR)
library(glue)
library(scales)

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
state_cols <- cbPalette[c(4, 6, 7, 8)]

# All Sequences
seq_all <- df_seq %>%
  select(-NSID) %>%
  seqdef(states = state_names, cpal = state_cols,
         labels = state_labels, missing = NA,
         missing.color = cbPalette[5], left = NA,
         gaps = NA, right = "DEL", xtstep = 12,
         weights = df_weight$all_weight)

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

plot(clust_qual)
plot(clust_qual$stats$ASW)
summary(clust_qual, max.rank = 2)

# Index and Density Plots
filepath <- "Images/Cluster Solutions/"

png(filename = paste0(filepath,"Legend.png"),width = 800, height = 600, pointsize=22)
seqlegend(neet_seq)
dev.off()

get_plots <- function(cluster){
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
                                          weights = df_weight$neet_weight[any_neet]))

# 4. Cluster Solutions ----
# Choice: Cluster 5
df_clust <- clustering %>%
  mutate(neet_cluster = factor(as.numeric(cluster5),
                               labels = c("Into Employment", "Unstable Employment",
                                          "Higher Education", "Further Education",
                                          "Long-Term NEET"))) %>%
  select(NSID, neet_cluster) %>%
  full_join(df_weight, by = "NSID")

df_clust <- df_clust %>%
  count(neet_cluster, wt = neet_weight, sort = TRUE) %>%
  drop_na() %>%
  mutate(neet_level = glue("{neet_cluster} ({round(n, 2)} seq.)")) %>%
  full_join(df_clust, by = "neet_cluster") %>%
  mutate(neet_level = fct_reorder(neet_level, n, .desc = TRUE),
         neet_cluster = fct_reorder(neet_cluster, n, .desc = TRUE)) %>%
  select(-n)

save(df_clust, file = "Data/neet_cluster.Rdata")
write_dta(df_clust,"Data/neet_cluster.dta")

# All Sequences
load("Data/neet_cluster.Rdata")

prep_data <- function(type = "all"){
  if (type == "all"){
    df <- df_clust %>%
      mutate(group_var = "All Sequences (6997 seq.)") %>%
      select(NSID, group_var, wt = all_weight)
  } else{
    df <- df_clust %>%
      filter(Any_NEET == 1) %>%
      select(NSID, group_var = neet_level, wt = neet_weight)
  }
  
  df %>%
    left_join(df_seq, by = "NSID") %>%
    as_factor() %>%
    arrange(across(matches("\\.")))
}

make_index <- function(type = "all"){
  facet <- type != "all"
  
  df_p <- prep_data(type) %>%
    group_by(group_var, across(matches("\\."))) %>%
    summarise(id = cur_group_id(),
              wt = sum(wt),
              NSID = NSID[1],
              .groups = "drop") %>%
    group_by(group_var) %>%
    mutate(ymax = cumsum(wt),
           ymin = ymax - wt) %>%
    ungroup() %>%
    pivot_longer(matches("\\."), names_to = "date", values_to = "activity") %>%
    group_by(id) %>%
    mutate(xmin = as.Date(glue("01.{date}"), "%d.%m.%Y"),
           xmax = ifelse(row_number() == n(), xmin + 30, lead(xmin)) %>%
             as.Date(origin = "1970-01-01")) %>%
    ungroup()
  
  plot_seq(df_p) +
    scale_y_continuous(labels = comma)
}

make_density <- function(type = "all"){
  facet <- type != "all"
  
  df_p <- prep_data(type) %>%
    pivot_longer(matches("\\."), names_to = "date", values_to = "activity") %>%
    mutate(date = as.Date(glue("01.{date}"), "%d.%m.%Y")) %>%
    arrange(group_var, date, activity) %>%
    count(group_var, date, activity, wt = wt) %>%
    group_by(group_var, date) %>%
    mutate(ymax = cumsum(n),
           ymin = ymax - n) %>%
    ungroup() %>%
    group_by(group_var, activity) %>%
    mutate(xmin = date,
           xmax = ifelse(row_number() == n(), xmin + 30, lead(xmin)) %>%
             as.Date(origin = "1970-01-01")) %>%
    ungroup() %>%
    group_by(group_var) %>%
    mutate(ymin = ymin/max(ymax),
           ymax = ymax/max(ymax))
  
  plot_seq(df_p) +
    scale_y_continuous(labels = percent)
}

plot_seq <- function(df){
  ggplot(df) +
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
        fill = activity, color = activity) +
    facet_wrap(~ group_var, scales = "free_y", ncol = 1) +
    geom_rect() +
    theme_minimal() +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    scale_x_date(breaks = glue("{2000:2015}-09-01") %>% as.Date(), 
                 date_labels = "%b %Y") +
    labs(x = NULL, y = NULL,
         color = NULL, fill = NULL) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# All Sequences
make_density("all")
ggsave("Images/Sequences/seq_density_all.png", dpi = 1200,
       width = 21, height = 16, units = "cm")

make_density("all") +
  scale_color_grey() +
  scale_fill_grey()
ggsave("Images/Sequences/seq_density_all_bw.png", dpi = 1200,
       width = 21, height = 16, units = "cm")

make_index("all")
ggsave("Images/Sequences/seq_index_all.png", dpi = 1200,
       width = 21, height = 16, units = "cm")

make_index("all") +
  scale_color_grey() +
  scale_fill_grey()
ggsave("Images/Sequences/seq_index_all_bw.png", dpi = 1200,
       width = 21, height = 16, units = "cm")

# NEET Sequences
make_density("neet")
ggsave("Images/Sequences/seq_density_neet.png", dpi = 1200,
       width = 21, height = 29.7, units = "cm")

make_density("neet") +
  scale_color_grey() +
  scale_fill_grey()
ggsave("Images/Sequences/seq_density_neet_bw.png", dpi = 1200,
       width = 21, height = 29.7, units = "cm")

make_index("neet")
ggsave("Images/Sequences/seq_index_neet.png", dpi = 1200,
       width = 21, height = 29.7, units = "cm")

make_index("neet") +
  scale_color_grey() +
  scale_fill_grey()
ggsave("Images/Sequences/seq_index_neet_bw.png", dpi = 1200,
       width = 21, height = 29.7, units = "cm")


# 5. NEET Variables ----
load("Data/neet_cluster.Rdata")

df_extra <- df_seq %>%
  pivot_longer(-NSID) %>%
  separate(name, c("m", "y"), "\\.") %>%
  mutate(across(m:y, as.numeric),
         my = y*12 + m,
         status = as_factor(value)) %>%
  select(NSID, status, my) %>%
  arrange(NSID, my) %>%
  group_by(NSID) %>%
  mutate(new_spell = ifelse(row_number() == 1 | status != lag(status), 1, 0),
         spell = cumsum(new_spell)) %>%
  group_by(NSID, spell) %>%
  summarise(status = status[1],
            length = my[n()] - my[1] + 1,
            .groups = "drop") %>%
  group_by(NSID, status) %>%
  summarise(status_spells = n(),
            mean_length = mean(length),
            .groups = "drop_last") %>%
  mutate(n_spells = sum(status_spells)) %>%
  ungroup() %>%
  complete(NSID, status) %>%
  group_by(NSID) %>%
  mutate(n_spells = max(n_spells, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(status == "NEET") %>%
  select(-status) %>%
  rename(neet_spells = status_spells) %>%
  mutate(entropy = seqient(seq_all) %>% as.numeric())

save(df_extra, file = "Data/df_extra.Rdata")
