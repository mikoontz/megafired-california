library(dplyr)
library(data.table)
library(broom)
library(ggplot2)
library(ggrepel)
library(pbapply)
library(lubridate)

megafire_drivers <- 
  read.csv(file = "data/out/megafire-drivers-summarized.csv") %>% 
  as_tibble()

# megafire_drivers[is.na(megafire_drivers$driver_wind_aspect_alignment_rad), "driver_wind_aspect_alignment_rad"] <- 0

pca_data <- 
  megafire_drivers %>% 
  dplyr::select(idx, dplyr::contains("driver_")) %>% 
  dplyr::filter(complete.cases(.))

complete_cases_idx <- pca_data$idx

pca <- 
  prcomp(pca_data[, -1], center = TRUE, scale = TRUE)

pca_augmented <-
  pca %>% 
  broom::augment(megafire_drivers[megafire_drivers$idx %in% complete_cases_idx, ])

ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = c("non-megafire", "megafire")[1 + megafire])) +
  geom_point() +
  scale_color_manual(values = c("red", "black")) +
  labs(color = "Megafire or no?",
       x = "PC1",
       y = "PC2") +
  theme_bw()

ggplot(pca_augmented %>% mutate(ewe_rank = ifelse(megafire, ewe_rank, NA)), aes(x = .fittedPC1, y = .fittedPC2, color = ewe_rank)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(color = "Megafire or no?",
       x = "PC1",
       y = "PC2") +
  theme_bw() +
  facet_wrap(facets = "megafire")

ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC3, color = c("non-megafire", "megafire")[1 + megafire])) +
  geom_point() +
  scale_color_manual(values = c("red", "black")) +
  labs(color = "Megafire or no?",
       x = "PC1",
       y = "PC3") +
  theme_bw()

ggplot(pca_augmented, aes(x = .fittedPC2, y = .fittedPC3, color = c("non-megafire", "megafire")[1 + megafire])) +
  geom_point() +
  scale_color_manual(values = c("red", "black")) +
  labs(color = "Megafire or no?",
       x = "PC2",
       y = "PC3") +
  theme_bw()



# ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = ewe_rank)) +
#   geom_point() +
#   scale_color_viridis_c()
# 
# ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = size_rank)) +
#   geom_point() +
#   scale_color_viridis_c()
# 
# ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = aoir_rank)) +
#   geom_point() +
#   scale_color_viridis_c()
# 
# ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = frp_rank)) +
#   geom_point() +
#   scale_color_viridis_c()

pca_rotation <-
  pca %>%
  broom::tidy(matrix = "rotation") %>%
  tidyr::pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value")

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix
ggplot(pca_rotation, aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-0.5, 0.5) + ylim(-0.5, 0.5) +
  coord_fixed() # fix aspect ratio to 1:1


ggplot(pca_rotation, aes(PC1, PC3)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-0.5, 0.5) + ylim(-0.5, 0.5) +
  coord_fixed() # fix aspect ratio to 1:1

ggplot(pca_rotation, aes(PC2, PC3)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-0.5, 0.5) + ylim(-0.5, 0.5) +
  coord_fixed() # fix aspect ratio to 1:1


pca$sdev / sum(pca$sdev)
cumsum(pca$sdev / sum(pca$sdev))
