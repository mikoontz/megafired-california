library(dplyr)
library(data.table)

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

out_fisher <- 
  lapply(X = biome_shortnames,
         FUN = function(biome_shortname) {
           d <- data.table::fread(input = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_fisher_", biome_shortname, ".csv"))
           d[, biome_shortname := ..biome_shortname]
           }) %>% 
  data.table::rbindlist()

out_fisher[out_fisher$biome_shortname == "tcf", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_fisher[out_fisher$biome_shortname == "mfws", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_fisher[out_fisher$biome_shortname == "tgss", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_fisher[out_fisher$biome_shortname == "dxs", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))

out_wilcox <- 
  lapply(X = biome_shortnames,
         FUN = function(biome_shortname) {
           d <- data.table::fread(input = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_wilcox_", biome_shortname, ".csv"))
           d[, biome_shortname := ..biome_shortname]
         }) %>% 
  data.table::rbindlist()

out_wilcox[out_wilcox$biome_shortname == "tcf", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_wilcox[out_wilcox$biome_shortname == "mfws", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_wilcox[out_wilcox$biome_shortname == "tgss", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_wilcox[out_wilcox$biome_shortname == "dxs", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))

out_t <- 
  lapply(X = biome_shortnames,
         FUN = function(biome_shortname) {
           d <- data.table::fread(input = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_t_", biome_shortname, ".csv"))
           d[, biome_shortname := ..biome_shortname]
         }) %>% 
  data.table::rbindlist()

out_t[out_t$biome_shortname == "tcf", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_t[out_t$biome_shortname == "mfws", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_t[out_t$biome_shortname == "tgss", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_t[out_t$biome_shortname == "dxs", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))

out_grouped_fisher <- 
  lapply(X = biome_shortnames,
         FUN = function(biome_shortname) {
           d <- data.table::fread(input = paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_fisher_", biome_shortname, ".csv"))
           d[, biome_shortname := ..biome_shortname]
         }) %>% 
  data.table::rbindlist()

out_grouped_fisher[out_grouped_fisher$biome_shortname == "tcf", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_grouped_fisher[out_grouped_fisher$biome_shortname == "mfws", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_grouped_fisher[out_grouped_fisher$biome_shortname == "tgss", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_grouped_fisher[out_grouped_fisher$biome_shortname == "dxs", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))

out_grouped_wilcox <- 
  lapply(X = biome_shortnames,
         FUN = function(biome_shortname) {
           d <- data.table::fread(input = paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_wilcox_", biome_shortname, ".csv"))
           d[, biome_shortname := ..biome_shortname]
         }) %>% 
  data.table::rbindlist()

out_grouped_wilcox[out_grouped_wilcox$biome_shortname == "tcf", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_grouped_wilcox[out_grouped_wilcox$biome_shortname == "mfws", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_grouped_wilcox[out_grouped_wilcox$biome_shortname == "tgss", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_grouped_wilcox[out_grouped_wilcox$biome_shortname == "dxs", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))

out_grouped_t <- 
  lapply(X = biome_shortnames,
         FUN = function(biome_shortname) {
           d <- data.table::fread(input = paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_t_", biome_shortname, ".csv"))
           d[, biome_shortname := ..biome_shortname]
         }) %>% 
  data.table::rbindlist()

out_grouped_t[out_grouped_t$biome_shortname == "tcf", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_grouped_t[out_grouped_t$biome_shortname == "mfws", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_grouped_t[out_grouped_t$biome_shortname == "tgss", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))
out_grouped_t[out_grouped_t$biome_shortname == "dxs", ] %>% filter(p.value <= 0.01) %>% arrange(desc(CPI))

