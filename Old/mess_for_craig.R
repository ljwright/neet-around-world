df_spell <- clustering %>%
  select(NSID, cluster5) %>%
  left_join(df_seq, by = "NSID") %>%
  pivot_longer(-c(NSID, cluster5)) %>%
  separate(name, c("m", "y"), "\\.") %>%
  mutate(across(m:y, as.numeric),
         my = y*12 + m,
         status = as_factor(value)) %>%
  select(id = NSID, cluster5, status, my)

df_spell %>%
  arrange(id, my) %>%
  group_by(id) %>%
  mutate(new_spell = ifelse(row_number() == 1 | status != lag(status), 1, 0),
         spell = cumsum(new_spell)) %>%
  group_by(id, cluster5, spell) %>%
  summarise(status = status[1],
            length = my[n()] - my[1] + 1,
            .groups = "drop") %>%
  group_by(id, cluster5, status) %>%
  mutate(mean_length = mean(length)) %>%
  group_by(id, cluster5) %>%
  mutate(n_spells = n(),
         n_status = unique(status) %>% length()) %>%
  ungroup() %>%
  filter(status == "NEET") %>% # ADD EMPLOYMENT SPELLS TOO!
  group_by(id) %>%
  mutate(total_length = sum(length),
         neet_spells = n(),
         .groups = "drop") %>%
  distinct(id, cluster5, mean_length, n_spells, 
           n_status, total_length, neet_spells) %>%
  group_by(cluster5) %>%
  summarise(across(mean_length:neet_spells, mean),
            .groups = "drop")

seqient(seq_neet) %>% 
  as_tibble() %>% 
  bind_cols(clustering, .) %>% 
  as_tibble() %>%
  lm(Entropy ~ cluster5 - 1, .) %>%
  broom::tidy(conf.int = TRUE) %>%
  mutate(term = str_replace(term, "cluster5",)) %>%
  ggplot() +
  aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high) +
  # geom_hline(yintercept = 0) +
  geom_pointrange()


# NUMBER OF SPELLS; AVERAGE SPELL LENGTH BY CATEGORY; NUMBER OF NEET SPELLS; NUMBER OF EMPLOYMENT SPELLS
