lts_t <-lts_in()
library(magrittr)
library(tidyr)
lts_c <- lts_summs_clean(lts_t)
lts_c$lts_clean_summs$lts_clean_summ_original
lts_c$lts_clean_summs$lts_clean_summ_ccf
lts_c$lts_clean_summs$lts_clean_summ_join



lts_pre_pca <- lts_c$lts_clean_summs$lts_clean_summ_join

# View(lts_pre_pca)

lts_pc_list <- lapply(lts_c$lts_clean_summs, lts_prcomp) #run pc analyis on each

str(lts_pc_list)

# lts_pc_list$lts_clean_summ_original$lts_all_pc

# lts_pc_list$lts_clean_summ_original$lts_labels_pc
# lts_pc_list$lts_clean_summ_join$lts_labels_pc
library(ggfortify)
# library(ggbio)


autoplot(lts_pc_list$lts_clean_summ_join$lts_pc_values, data = lts_pc_list$lts_clean_summ_join$lts_labels_pc, colour = "catchmentRegion"
         # shape = "season"
)+
  scale_color_manual(values = c("dodgerblue","darkorange"))+
  # scale_color_viridis(discrete = TRUE, option = "A")+
  theme_classic()



####
lts_pc_list$lts_clean_summ_original$lts_labels_pc
# lts_pc_list$lts_clean_summ_original$
p <- lapply(lts_pc_list, function(x) autoplot(x$lts_pc_values, data = lts_pc_list$lts_clean_summ_original$lts_labels_pc
, colour = "catchmentRegion")+
  scale_color_manual(values = c("dodgerblue","darkorange"))+
  # scale_color_viridis(discrete = TRUE, option = "A")+
  theme_classic()
)

p

library(grid)
do.call("grid.arrange", p)


  lts_prcomp(lts_pre_pca)

p <- lapply(lts_prcomp_list, function(x) autoplot(x$lts_pc_values, data = pcTest$lts_all_pc, colour = "catchmentRegion"
                                                  # shape = "season"
)+
  scale_color_manual(values = c("dodgerblue","darkorange"))+
  # scale_color_viridis(discrete = TRUE, option = "A")+
  theme_classic()
)

do.call("grid.arrange", p)
