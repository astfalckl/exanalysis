library(devtools)
library(tidyverse)
library(scico)
library(patchwork)
library(sp)
library(latex2exp)

devtools::install_github("astfalckl/exanalysis")
library(exanalysis)

# File path to save out images
file_path <- "paper_plots/"

# ------------------------------------------------------------------------------
# ------------------------- Do analysis in paper -------------------------------
# ------------------------------------------------------------------------------

sst_prior_params <- dplyr::tibble(tau = 6, c = 0.92, kappa = 1.61, alpha2 = 1)

h_list <- generate_Hx(model_data, sst_data)

sst_update <- calculate_sst_update(
  model_data, sst_data, h_list, sst_prior_params
)

spline_params <- list(
  bounds = c(-1.92, 10),
  knots = c(-1.15, -0.32, 1.44),
  degree = 1,
  prior_exp = c(0.16, 0.47, 0.27, 0.1, 0)
)

sic_prior_params <- list(
  corW_params = c(6, 4, 1, 0),
  varU_params = c(6, 4, 0.3, 0)
)

sst_field <- as.numeric(sst_update$update_tbl$Eadj2)

betais <- project_betais(sst_update, spline_params)

sic_update <- calculate_sic_update(
  sst_update, spline_params, sic_prior_params, betais, sst_field, sic_data
)

str(sst_update)
str(sic_update)

# ------------------------------------------------------------------------------
# ------------------------------- Figure 2 -------------------------------------
# ------------------------------------------------------------------------------

p_data_sst <- sst_data$data %>%
  filter(! lat %in% c(-90, 90, 80, -85)) %>%
  ggplot() +
  geom_line(
    data = wsi_extent,
    aes(x = lon, y = lat, group = hemisphere),
    linewidth = 0.8, linetype = "dashed", colour = "red"
  ) +
  geom_point(aes(x = lon, y = lat, color = sst_obs, shape = type), size = 1) +
  geom_polygon(
    data = coastlines,
    aes(x = long, y = lat, group = group),
    fill = "white"
  ) +
  geom_path(
    data = coastlines,
    aes(x = long, y = lat, group = group), size = 0.3
  ) +
  scale_color_scico(
    expression(degree ~ "C"),
    direction = -1,
    palette = "roma"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_fixed() +
  theme_bw() +
  guides(
    fill = guide_colourbar(barwidth = 100, barheight = 0.1),
    shape = "none"
  ) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave(
  paste0(file_path, "data_sst.pdf"), p_data_sst,
  width = 160, height = 100, units = "mm", dpi = 300
)

# ------------------------------------------------------------------------------
# ------------------------------- Figure 3 -------------------------------------
# ------------------------------------------------------------------------------

# Some global plotting parameters used below
var_lim <- c(0, 3.2)
var_breaks <- c(0, 1.5, 3)

# This recreates the data object but filters for the pseudo data. As this data
# is, in effect, a prior belief, we update by it and use the adjusted moments
# as our priors.
pseudo_data <- create_sst_data_object(
  filter(sst_data$data, sst_obs == -1.92),
  filter(sst_data$data, sst_obs == -1.92) %>% dplyr::select(lon, lat),
  matrix(0, nrow = 24),
  matrix(0, nrow = 24, ncol = 24)
)

# Update by the pseudo data
H_pseudo <- generate_Hx(model_data, pseudo_data)

sst_pseudo <- calculate_sst_update(
  model_data, pseudo_data, H_pseudo, sst_prior_params, TRUE
)

# ------------------------------- Figure 3a ------------------------------------

p_sst_adj_exp_first <- sst_update$update_tbl %>%
  filter(time == 1) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Eadj1),
      size = 0.5
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = -1, palette = "roma",
      limits = c(-2, 30), breaks = c(0, 10, 20, 30)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("a) Expectation, first update") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_blank()
    )

# ------------------------------- Figure 3b ------------------------------------

# The marginal variance plot assumes we have already updated by the pseudo data

p_sst_adj_var_first <- sst_pseudo$update_tbl %>%
  filter(time == 1) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Vadj2),
      size = 0.5
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "lajolla",
       limits = var_lim, breaks = var_breaks
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("b) SD, first update") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.title = element_blank()
    )

# ------------------------------- Figure 3c ------------------------------------

p_sst_adj_exp_second <- sst_update$update_tbl %>%
  filter(time == 1) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Eadj2),
      size = 0.5
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = -1, palette = "roma",
      limits = c(-2, 30), breaks = c(0, 10, 20, 30)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("c) Expectation, second update") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_blank()
    )

# ------------------------------- Figure 3d ------------------------------------

p_sst_adj_var_second <- sst_update$update_tbl %>%
  filter(time == 1) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Vadj2),
      size = 0.5
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "lajolla",
      limits = var_lim, breaks = var_breaks
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("d) SD, second update") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.title = element_blank()
    )

# ------------------------------- Figure 3e ------------------------------------

p_sst_adj_exp_diff <- sst_update$update_tbl %>%
  filter(time == 1) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Eadj2 - Eadj1),
      size = 0.5
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "vik",
      limits = c(-4, 4), breaks = c(-2, 0, 2)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("e) Contribution to Expectation") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_blank()
    )

# ------------------------------- Figure 3f ------------------------------------

p_sst_ensemble_sd <- sst_pseudo$simulation$data %>%
  filter(time == 1) %>%
  group_by(lat, lon) %>%
  summarise(var = var(sst)) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = sqrt(var)),
      size = 0.5
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "lajolla",
       limits = var_lim, breaks = var_breaks, na.value = "white"
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("f) Ensemble SD") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.title = element_blank()
    )

# --------------------------- Put them together --------------------------------

p_all_sst <- (p_sst_adj_exp_first + p_sst_adj_var_first) /
(p_sst_adj_exp_second + p_sst_adj_var_second) /
(p_sst_adj_exp_diff + p_sst_ensemble_sd)

ggsave(
  paste0(file_path, "sst_adj.pdf"), p_all_sst,
  width = 140, height = 165, units = "mm", dpi = 300
)

# ------------------------------------------------------------------------------
# ------------------------------- Figure 4 -------------------------------------
# ------------------------------------------------------------------------------

# --------------------------- Plot reference map -------------------------------

p_map <- ggplot(coords_select_all) +
  geom_polygon(
    data = coastlines,
    aes(x = long, y = lat, group = group),
    fill = "white"
  ) +
  geom_path(
    data = coastlines,
    aes(x = long, y = lat, group = group), size = 0.3
  ) +
  geom_point(aes(x = lon, y = lat), colour = "red", size = 2) +
  geom_text(
    aes(x = lon, y = lat, label = label), 
    hjust=-0.5, vjust=0.2, colour = "red"
  ) +
  theme_bw() +
  coord_fixed() +
  theme(axis.title = element_blank())

ggsave(
  paste0(file_path, "map.pdf"), p_map, width = 5, height = 2.7
)

# ------------------------------ Location A ------------------------------------

model_names <- unique(sst_update$simulation$data$model)

coords_select_all <- tibble(
  lon = c(-18.75, 180, -45, -157.5),
  lat = c(86.25, 48.75, 51.25, -53.75),
  label = c("A", "B", "C", "D")
)

coord_select <- coords_select_all[1, c(1, 2)]

betas <- sic_update$adj_exp_spline_tbl %>%
    filter(lon == coord_select$lon, lat == coord_select$lat) %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()

var_betas <- sic_update$adj_var_spline_tbl %>%
  filter(lon == coord_select$lon, lat == coord_select$lat) %>%
  ungroup() %>%
  dplyr::select(-lat, -lon) %>%
  as.matrix()

x_tmp <- seq(-1.92, 5, length = 1000)

p1 <- lapply(seq_along(model_names), function(i) {
  tibble(
    x = x_tmp,
    y = calc_X(x_tmp, spline_params) %*% matrix(betas[, i]) %>% as.numeric(),
    var = diag(calc_X(x_tmp, spline_params) %*%
      diag(var_betas[, i]) %*% t(calc_X(x_tmp, spline_params))),
    model = model_names[i]
  )
}) %>%
  bind_rows() %>%
  mutate(
    min = ifelse(y - 2 * sqrt(var) < 0, 0, y - 2 * sqrt(var)),
    max = ifelse(y + 2 * sqrt(var) > 1, 1, y + 2 * sqrt(var))
  ) %>%
  ggplot() +
  geom_ribbon(
      aes(
        x = x,
        ymin = min,
        ymax = max,
        group = model), alpha = 0.1) +
    geom_line(aes(x = x, y = y, group = model), alpha = 0.5) +
    ggtitle("A") +
    theme_bw() +
    scale_x_continuous(TeX("SST \\; [$\\degree$C]"), expand = c(0, 0)) +
    scale_y_continuous(TeX("SIC"), expand = c(0, 0), limits = c(0, 1)) +
    theme(aspect.ratio = 1)

# ------------------------------ Location B ------------------------------------

coord_select <- coords_select_all[2, c(1, 2)]

betas <- sic_update$adj_exp_spline_tbl %>%
    filter(lon == coord_select$lon, lat == coord_select$lat) %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()

var_betas <- sic_update$adj_var_spline_tbl %>%
  filter(lon == coord_select$lon, lat == coord_select$lat) %>%
  ungroup() %>%
  dplyr::select(-lat, -lon) %>%
  as.matrix()

p2 <- lapply(seq_along(model_names), function(i) {
  tibble(
    x = x_tmp,
    y = calc_X(x_tmp, spline_params) %*% matrix(betas[, i]) %>% as.numeric(),
    var = diag(calc_X(x_tmp, spline_params) %*%
      diag(var_betas[, i]) %*% t(calc_X(x_tmp, spline_params))),
    model = model_names[i]
  )
}) %>%
  bind_rows() %>%
  mutate(
    min = ifelse(y - 2 * sqrt(var) < 0, 0, y - 2 * sqrt(var)),
    max = ifelse(y + 2 * sqrt(var) > 1, 1, y + 2 * sqrt(var))
  ) %>%
  ggplot() +
  geom_ribbon(
      aes(
        x = x,
        ymin = min,
        ymax = max,
        group = model), alpha = 0.1) +
    geom_line(aes(x = x, y = y, group = model), alpha = 0.5) +
    ggtitle("B") +
    theme_bw() +
    scale_x_continuous(TeX("SST \\; [$\\degree$C]"), expand = c(0, 0)) +
    scale_y_continuous(TeX("SIC"), expand = c(0, 0), limits = c(0, 1)) +
    theme(
      aspect.ratio = 1,
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )

# ------------------------------ Location C ------------------------------------

coord_select <- coords_select_all[3, c(1, 2)]

betas <- sic_update$adj_exp_spline_tbl %>%
    filter(lon == coord_select$lon, lat == coord_select$lat) %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()

var_betas <- sic_update$adj_var_spline_tbl %>%
  filter(lon == coord_select$lon, lat == coord_select$lat) %>%
  ungroup() %>%
  dplyr::select(-lat, -lon) %>%
  as.matrix()

p3 <- lapply(seq_along(model_names), function(i) {
  tibble(
    x = x_tmp,
    y = calc_X(x_tmp, spline_params) %*% matrix(betas[, i]) %>% as.numeric(),
    var = diag(calc_X(x_tmp, spline_params) %*%
      diag(var_betas[, i]) %*% t(calc_X(x_tmp, spline_params))),
    model = model_names[i]
  )
}) %>%
  bind_rows() %>%
  mutate(
    min = ifelse(y - 2 * sqrt(var) < 0, 0, y - 2 * sqrt(var)),
    max = ifelse(y + 2 * sqrt(var) > 1, 1, y + 2 * sqrt(var))
  ) %>%
  ggplot() +
  geom_ribbon(
      aes(
        x = x,
        ymin = min,
        ymax = max,
        group = model), alpha = 0.1) +
    geom_line(aes(x = x, y = y, group = model), alpha = 0.5) +
    ggtitle("C") +
    theme_bw() +
    scale_x_continuous(TeX("SST \\; [$\\degree$C]"), expand = c(0, 0)) +
    scale_y_continuous(TeX("SIC"), expand = c(0, 0), limits = c(0, 1)) +
    theme(
      aspect.ratio = 1,
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )

# ------------------------------ Location D ------------------------------------

coord_select <- coords_select_all[4, c(1, 2)]

betas <- sic_update$adj_exp_spline_tbl %>%
    filter(lon == coord_select$lon, lat == coord_select$lat) %>%
    dplyr::select(starts_with("V")) %>%
    as.matrix()

var_betas <- sic_update$adj_var_spline_tbl %>%
  filter(lon == coord_select$lon, lat == coord_select$lat) %>%
  ungroup() %>%
  dplyr::select(-lat, -lon) %>%
  as.matrix()

p4 <- lapply(seq_along(model_names), function(i) {
  tibble(
    x = x_tmp,
    y = calc_X(x_tmp, spline_params) %*% matrix(betas[, i]) %>% as.numeric(),
    var = diag(calc_X(x_tmp, spline_params) %*%
      diag(var_betas[, i]) %*% t(calc_X(x_tmp, spline_params))),
    model = model_names[i]
  )
}) %>%
  bind_rows() %>%
  mutate(
    min = ifelse(y - 2 * sqrt(var) < 0, 0, y - 2 * sqrt(var)),
    max = ifelse(y + 2 * sqrt(var) > 1, 1, y + 2 * sqrt(var))
  ) %>%
  ggplot() +
  geom_ribbon(
      aes(
        x = x,
        ymin = min,
        ymax = max,
        group = model), alpha = 0.1) +
    geom_line(aes(x = x, y = y, group = model), alpha = 0.5) +
    ggtitle("D") +
    theme_bw() +
    scale_x_continuous(TeX("SST \\; [$\\degree$C]"), expand = c(0, 0)) +
    scale_y_continuous(TeX("SIC"), expand = c(0, 0), limits = c(0, 1)) +
    theme(
      aspect.ratio = 1,
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )

# ------------------------------ Put it together -------------------------------

plot_locations <- p1 | p2 | p3 | p4

ggsave(
  paste0(file_path, "ind_fits2.pdf"), plot_locations, height = 2.5, width = 7.5
)

# ------------------------------------------------------------------------------
# ------------------------------- Figure 5 -------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------- Figure 5a ------------------------------------

p_ice_adj_exp_first <- sic_update$update_tbl %>%
  filter(time == 8) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Eadj1),
      size = 0.5
    ) +
    geom_line(
      data = wsi_extent %>% filter(hemisphere == "S"),
      aes(x = lon, y = lat, group = hemisphere),
      size = 0.8, linetype = "dashed", colour = "red"
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "oslo", begin = 0.2, end = 0.9,
      limits = c(0, 1), breaks = c(0, 0.5, 1)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("a) Expectation, first update") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# ------------------------------- Figure 5b ------------------------------------

p_ice_adj_var_first <- sic_update$update_tbl %>%
  filter(time == 8) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Vadj1),
      size = 0.5
    ) +
    geom_line(
      data = wsi_extent %>% filter(hemisphere == "S"),
      aes(x = lon, y = lat, group = hemisphere),
      size = 0.8, linetype = "dashed", colour = "red"
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "lajolla",
      limits = c(0, 0.5), breaks = c(0, 0.25, 0.5)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("b) SD, first update") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# ------------------------------- Figure 5c ------------------------------------

p_ice_adj_exp_second <- sic_update$update_tbl %>%
  filter(time == 8) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Eadj2),
      size = 0.5
    ) +
    geom_line(
      data = wsi_extent %>% filter(hemisphere == "S"),
      aes(x = lon, y = lat, group = hemisphere),
      size = 0.8, linetype = "dashed", colour = "red"
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "oslo", begin = 0.2, end = 0.9,
      limits = c(0, 1), breaks = c(0, 0.5, 1)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("c) Expectation, second update") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# ------------------------------- Figure 5d ------------------------------------

p_ice_adj_var_second <- sic_update$update_tbl %>%
  filter(time == 8) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = sqrt(Vadj2)),
      size = 0.5
    ) +
    geom_line(
      data = wsi_extent %>% filter(hemisphere == "S"),
      aes(x = lon, y = lat, group = hemisphere),
      size = 0.8, linetype = "dashed", colour = "red"
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "lajolla",
      limits = c(0, 0.5), breaks = c(0, 0.25, 0.5)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("d) SD, second update") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# ------------------------------- Figure 5e ------------------------------------

p_ice_adj_exp_ensemble <- sst_update$simulation$data %>%
  filter(time == 8) %>%
  group_by(lat, lon) %>%
  summarise(exp = mean(ice)) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = exp),
      size = 0.5
    ) +
    geom_line(
      data = wsi_extent %>% filter(hemisphere == "S"),
      aes(x = lon, y = lat, group = hemisphere),
      size = 0.8, linetype = "dashed", colour = "red"
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "oslo", begin = 0.2, end = 0.9,
      limits = c(0, 1), breaks = c(0, 0.5, 1)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("e) Expectation, ensemble") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# ------------------------------- Figure 5f ------------------------------------

p_ice_adj_var_ensemble <- sst_update$simulation$data %>%
  filter(time == 8) %>%
  group_by(lat, lon) %>%
  summarise(var = var(ice)) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = sqrt(var)),
      size = 0.5
    ) +
    geom_line(
      data = wsi_extent %>% filter(hemisphere == "S"),
      aes(x = lon, y = lat, group = hemisphere),
      size = 0.8, linetype = "dashed", colour = "red"
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "lajolla",
      limits = c(0, 0.5), breaks = c(0, 0.25, 0.5)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    ggtitle("f) SD, ensemble") +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# --------------------------- Put them together --------------------------------

p_ice_adj <- (p_ice_adj_exp_first + p_ice_adj_var_first) /
  (p_ice_adj_exp_second + p_ice_adj_var_second) /
  (p_ice_adj_exp_ensemble + p_ice_adj_var_ensemble)

ggsave(
  paste0(file_path, "ice_adj.pdf"), p_ice_adj,
  width = 140, height = 165, units = "mm", dpi = 300
)

# ------------------------------------------------------------------------------
# ------------------------------- Figure 6 -------------------------------------
# ------------------------------------------------------------------------------

# Draw random SST and SIC field. I use a few sparse matrix tricks here to run it
# a bit faster.

sst_chol <- Matrix::chol(0.5 * (sst_update$Vs + Matrix::t(sst_update$Vs)))

set.seed(1)

sample <- Matrix::t(sst_chol) %*% Matrix::Matrix(rnorm(sst_update$simulation$n))

sym_tmp <- 0.5 *
  (sic_update$V_param_update + Matrix::t(sic_update$V_param_update))
sic_chol <- Matrix::chol(sym_tmp)

sample_ice <- Matrix::t(Matrix::Matrix(rnorm(sst_update$simulation$n * 5))) %*%
  sic_chol

psi_star <- left_join(
    sst_update$simulation$means,
    sst_update$simulation$coords %>% mutate(sample = -as.numeric(sample))
  ) %>%
  arrange(time, lon, lat) %>%
  create_psii(spline_params)

# ------------------------------ Plot SST Feb ----------------------------------

p_sst_feb <- sst_update$update_tbl %>%
  filter(time == 2) %>%
  arrange(lon, lat) %>%
  mutate(
    sample = -as.numeric(sample),
    plot = ifelse(sample + Eadj2 < -1.92, -1.92, sample + Eadj2)
  ) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = plot),
      size = 0.5
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = -1, palette = "roma",
      limits = c(-2, 31), breaks = c(0, 10, 20, 30)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    ggtitle("a) SST - February") +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# ------------------------------ Plot SST Aug ----------------------------------

p_sst_aug <- sst_update$update_tbl %>%
  filter(time == 8) %>%
  arrange(lon, lat) %>%
  mutate(
    sample = -as.numeric(sample),
    plot = ifelse(sample + Eadj2 < -1.92, -1.92, sample + Eadj2)
  ) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = plot),
      size = 0.5
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = -1, palette = "roma",
      limits = c(-2, 31), breaks = c(0, 10, 20, 30)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    ggtitle("b) SST - August") +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# ------------------------------ Plot SIC Feb ----------------------------------

p_ice_feb <- sic_update$update_tbl %>%
  mutate(
    Eadj2 = Eadj2 - as.numeric(psi_star %*% Matrix::t(sample_ice)),
    Eadj2 = ifelse(Eadj2 > 1, 1, Eadj2),
    Eadj2 = ifelse(Eadj2 < 0, 0, Eadj2)
  ) %>%
  filter(time == 2) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Eadj2),
      size = 0.5
    ) +
    geom_line(
      data = wsi_extent %>% filter(hemisphere == "N"),
      aes(x = lon, y = lat, group = hemisphere),
      size = 0.8, linetype = "dashed", colour = "red"
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "oslo", begin = 0.2, end = 0.9,
      limits = c(0, 1), breaks = c(0, 0.5, 1)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    ggtitle("c) SIC - February") +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# ------------------------------ Plot SIC Aug ----------------------------------

p_ice_aug <- sic_update$update_tbl %>%
  mutate(
    Eadj2 = Eadj2 - as.numeric(psi_star %*% Matrix::t(sample_ice)),
    Eadj2 = ifelse(Eadj2 > 1, 1, Eadj2),
    Eadj2 = ifelse(Eadj2 < 0, 0, Eadj2)
  ) %>%
  filter(time == 8) %>%
  ggplot() +
    geom_tile(
      aes(x = lon, y = lat, width = 4, height = 2.8, fill = Eadj2),
      size = 0.5
    ) +
    geom_line(
      data = wsi_extent %>% filter(hemisphere == "S"),
      aes(x = lon, y = lat, group = hemisphere),
      size = 0.8, linetype = "dashed", colour = "red"
    ) +
    geom_polygon(
      data = coastlines,
      aes(x = long, y = lat, group = group),
      fill = "white"
    ) +
    geom_path(
      data = coastlines,
      aes(x = long, y = lat, group = group), size = 0.1
    ) +
    scico::scale_fill_scico(
      NULL, direction = 1, palette = "oslo", begin = 0.2, end = 0.9,
      limits = c(0, 1), breaks = c(0, 0.5, 1)
    ) +
    coord_fixed() +
    theme_bw() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    ggtitle("d) SIC - August") +
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.5)) +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

# --------------------------- Put them together --------------------------------

p_samples <- (p_sst_feb | p_sst_aug) / (p_ice_feb | p_ice_aug)

ggsave(
  paste0(file_path, "samples.pdf"), p_samples,
  width = 140, height = 110, units = "mm", dpi = 300
)

# ------------------------------------------------------------------------------
# ------------------------------- Figure 7 -------------------------------------
# ------------------------------------------------------------------------------

# These results are from a coupled climate model and read in from csv files of
# the individual locations.

# ------------------------------ Reference Map ---------------------------------

location_plot <- ggplot() +
  geom_polygon(
    data = coastlines,
    aes(x = long, y = lat, group = group),
    fill = "white"
  ) +
  geom_path(
    data = coastlines,
    aes(x = long, y = lat, group = group), size = 0.3
  ) +
  geom_point(data = points, aes(x = lon, y = lat), colour = "red") +
  geom_text(
    data = points,
    aes(x = lon, y = lat, label = label),
    colour = "red", vjust = -0.5, hjust = -0.5
  ) +
  scale_y_continuous("Latitude", expand = c(0, 0), limits = c(42, 85)) +
  scale_x_continuous("Longitude", expand = c(0, 0), limits = c(-180, 0)) +
  theme_bw() +
  coord_fixed(ratio = 1.5) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 8)
  )

ggsave(
  paste0(file_path, "glacier_locations.pdf"), location_plot,
  width = 5, height = 2.1
)

# ------------------------------- Canada Plot ----------------------------------

canada <- read_csv("glacial_runs/canada.csv") %>%
  mutate(t = 1:500) %>%
  pivot_longer(-t)

canada_plot <- ggplot() +
  geom_line(
    data = canada %>% filter(name == "xovga"),
    aes(x = 10 * t, y = value), colour = "red"
  ) +
  geom_line(
    data = canada %>% filter(name != "xovga"),
    aes(x = 10 * t, y = value, group = name), alpha = 0.5
  ) +
  scale_y_continuous("Ice sheet height [m]") +
  scale_x_continuous("Years", expand = c(0, 0)) +
  ggtitle("Arctic Canada") +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 8)
  )

# ------------------------------ Greenland Plot --------------------------------

greenland <- read_csv("glacial_runs/greenland.csv") %>%
  mutate(t = 1:500) %>%
  pivot_longer(-t)

greenland_plot <- ggplot() +
  geom_line(
    data = greenland %>% filter(name == "xovga"),
    aes(x = 10 * t, y = value), colour = "red"
  ) +
  geom_line(
    data = greenland %>% filter(name != "xovga"),
    aes(x = 10 * t, y = value, group = name), alpha = 0.5
  ) +
  scale_y_continuous() +
  scale_x_continuous("Years", expand = c(0, 0)) +
  ggtitle("Central Greenland") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    aspect.ratio = 1,
    text = element_text(size = 8)
  )

# ----------------------------- Hudson Bay Plot --------------------------------

hudson <- read_csv("glacial_runs/hudson.csv") %>%
  mutate(t = 1:500) %>%
  pivot_longer(-t)

hudson_plot <- ggplot() +
  geom_line(
    data = hudson %>% filter(name == "xovga"),
    aes(x = 10 * t, y = value), colour = "red"
  ) +
  geom_line(
    data = hudson %>% filter(name != "xovga"),
    aes(x = 10 * t, y = value, group = name), alpha = 0.5
  ) +
  scale_y_continuous("Ice sheet height (m)") +
  scale_x_continuous("Years", expand = c(0, 0)) +
  ggtitle("Hudson Bay") +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 8),
    axis.title.y = element_blank()
  )

# ------------------------------ Pacific Plot ----------------------------------

pacific <- read_csv("glacial_runs/pacific.csv") %>%
  mutate(t = 1:500) %>%
  pivot_longer(-t)

pacific_plot <- ggplot() +
  geom_line(
    data = pacific %>% filter(name == "xovga"),
    aes(x = 10 * t, y = value), colour = "red"
  ) +
  geom_line(
    data = pacific %>% filter(name != "xovga"),
    aes(x = 10 * t, y = value, group = name), alpha = 0.5
  ) +
  scale_y_continuous() +
  scale_x_continuous("Years", expand = c(0, 0)) +
  ggtitle("Pacific Coast") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    aspect.ratio = 1,
    text = element_text(size = 8)
  )

points <- tibble(
  lat = c(46, 60, 78, 76),
  lon = c(-120, -86, -113, -40),
  label = c("D", "C", "A", "B")
)

# --------------------------- Put them together --------------------------------

callouts <- canada_plot | greenland_plot | hudson_plot | pacific_plot

ggsave(
  paste0(file_path, "glacier_callouts.pdf"), callouts,
  height = 2.5, width = 7.5
)