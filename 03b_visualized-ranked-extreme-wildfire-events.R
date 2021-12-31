# visualizations of the 3-dimensional definition of megafire
library(sf)
library(dplyr)
library(plotly)
library(ggplot2)

ranked_ewe_out <- read.csv("data/out/extreme-wildfire-events-ranking.csv")
n_fires <- 2000

ranked_ewe_plot_data <-
  ranked_ewe_out %>% 
  dplyr::mutate(megafire = ifelse(mecdf >= 0.9, yes = "megafire", no = "non-megafire"))

ggplot(ranked_ewe_plot_data[1:n_fires, ], aes(x = size_rank, y = frp_rank, color = megafire)) +
  geom_point() +
  theme_bw() +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_color_manual(values = c("red", "black")) +
  labs(x = "Size rank (right is more extreme)",
       y = "Intensity rank (up is more extreme",
       color = "")

ggplot(ranked_ewe_plot_data[1:n_fires, ], aes(x = size_rank, y = aoir_rank, color = megafire)) +
  geom_point() +
  theme_bw() +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_color_manual(values = c("red", "black")) +
  labs(x = "Size rank (right is more extreme)",
       y = "Speed rank (up is more extreme",
       color = "")

ggplot(ranked_ewe_plot_data[1:n_fires, ], aes(x = aoir_rank, y = frp_rank, color = megafire)) +
  geom_point() +
  theme_bw() +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_color_manual(values = c("red", "black")) + 
  labs(x = "Speed rank (right is more extreme)",
       y = "Intensity rank (up is more extreme",
       color = "")


plot_ly(data = ranked_ewe_plot_data[1:n_fires, ], 
        x = ~size_rank, y = ~frp_rank, z = ~aoir_rank, 
        type = "scatter3d", mode = "markers", 
        color = ~megafire, colors = c("red", "black"))

ggplot(ranked_ewe, aes(x = mecdf, fill = (frp_90 == 0))) +
  geom_histogram(bins = 100, alpha = 0.25) +
  geom_vline(aes(xintercept = 0.90), color = "red") 

ggplot(mutate(ranked_ewe, frp_exists = frp_90 != 0), aes(x = mecdf)) +
  geom_histogram(bins = 100, alpha = 0.25) +
  geom_vline(aes(xintercept = 0.90), color = "red") +
  facet_wrap(facets = "frp_exists")
# 
# ranked_ewe %>% 
#   filter(mecdf >= 0.9)
# 
# ranked_ewe[100, ]
# 
# ranked_ewe %>% 
#   filter(frp_90 == 0)
# 
# ggplot(ranked_ewe_out, aes(x = ))
# 
# ggplot(ranked_ewe, aes(x = ignition_date, y = total_area_ha)) + 
#   geom_point() +
#   geom_smooth()
# 
# ggplot(ranked_ewe, aes(x = ignition_year, y = modeled_max_aoir)) + 
#   geom_point() +
#   geom_smooth()
# 
# ggplot(ranked_ewe, aes(x = ignition_date, y = frp_90)) + 
#   geom_point() +
#   geom_smooth() +
#   scale_y_log10()
# 
# ggplot(ranked_ewe, aes(x = ignition_date, y = mecdf)) + 
#   geom_point() +
#   geom_smooth()
# 
# ggplot(ranked_ewe, aes(x = ignition_month, y = mecdf)) + 
#   geom_point() +
#   geom_smooth()
# 
# ggplot(ranked_ewe, aes(x = mecdf)) +
#   geom_histogram()