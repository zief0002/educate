library(dplyr)
########################################################
# Load libraries and fonts
########################################################

library(ggplot2)
library(hexSticker)
library(showtext)
library(educate)

font_add_google("Open Sans", "open")
font_add_google("Oswald")


########################################################
# Create density plot
########################################################

lower_bound = min(education$salary, na.rm = TRUE)
upper_bound = max(education$salary, na.rm = TRUE)

densities_within = data.frame(bs = 1:1000) %>%
  dplyr::group_by(bs) %>%
  dplyr::mutate(
    data = list(education %>% dplyr::sample_frac(size = 1, replace = TRUE))
  ) %>%
  tidyr::unnest() %>%
  dplyr::group_by(bs) %>%
  dplyr::do(broom::tidy(density(.$salary, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE))) %>%
  dplyr::ungroup()

# Rename variables
names(densities_within)[2] = "X"
names(densities_within)[3] = "dens"

# Compute limits for conditional densities for better color gradient
M = mean(densities_within$dens)
SD = sd(densities_within$dens)
low_limit = M - 3 * SD
upp_limit = M + 3 * SD


my_dens = data.frame(
  X = density(education$salary, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE)$x,
  emp_dens = density(education$salary, from = lower_bound, to = upper_bound, n = 128, na.rm = TRUE)$y
)

# Join with empirical density and create color gradient levels
densities_within = densities_within %>%
  dplyr::left_join(my_dens, by = "X") %>%
  dplyr::filter(dens > low_limit, dens < upp_limit) %>%
  dplyr::group_by(X) %>%
  dplyr::mutate(
    dens.scaled = abs(dens - emp_dens),
    dens.scaled2 = dens.scaled / (max(dens.scaled) - min(dens.scaled))
  ) %>%
  dplyr::ungroup()

# Create multicolored watercolor normal density plot
p2 = ggplot(data = densities_within, aes(x = X)) +
  geom_path(ggplot2::aes(group = bs, y = dens, color = dens.scaled2)) +
  scale_color_gradientn("dens.scaled", colors = rev(RColorBrewer::brewer.pal(9, "YlGnBu"))) +
  scale_alpha_continuous(range = c(0.001, 1)) +
  stat_density(data = education, aes(x = salary), geom = "line", color = "black", size = 0.5) +
  theme_void() +
  guides(color = FALSE, alpha = FALSE)


########################################################
# Create sticker
########################################################

sticker(p2,
        # Package name, color, font family, size
        package = "educate", p_color = "#ffffff", p_family = "open", p_size = 8, p_y = 1.45,
        s_x = 1.1, s_y = 0.9, s_width = 1.2, s_height = 0.8,         # Subplot size
        h_color = "#002B3Cbb", h_size = 0.8, h_fill = "#002B3C", # Hexagon border

        url = "https://github.com/zief0002/educate", u_color = "#ffffff", u_family = "Oswald",
        filename="~/Desktop/educate_sticker.png"
        )
