
# Figure out the hetero noise

fits_tmp <- lapply(1:13, function(i){
  cat(sprintf("\rIteration %i", i))

  Psi_tmp <- model_data$data %>%
    filter(model == model_names[i]) %>%
    arrange(time, lon, lat) %>%
    create_psii(spline_params)

  ice_tmp <- model_data$data %>%
    filter(model == model_names[i]) %>%
    arrange(time, lon, lat) %>%
    mutate(
      ice_hat = as.numeric(Psi_tmp %*% (betais$Theta %*% betais$betais[[i]] + betais$beta_mean))
    )
}) %>% bind_rows()

ggplot(fits_tmp %>% filter(sst < 10)) +
  geom_point(aes(x = sst, y = (ice - ice_hat)^2), alpha = 0.01) +
  facet_wrap(~model)

spline_tmp <- splines2::bSpline(
  seq(-1.92, 10, length = 100), 
  knot = spline_params$knots, degree = 2, intercept = TRUE, 
  Boundary.knots = c(-1.92, 10)
)

spline_tmp %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(x = seq(-1.92, 10, length = 100)) %>%
    tidyr::gather(spline, value, -x) %>%
    ggplot2::ggplot() + 
      ggplot2::geom_line(ggplot2::aes(x = x, y = value, colour = spline)) +
      ggplot2::theme_bw()

Xbspline <- splines2::bSpline(
  fits_tmp %>% filter(sst < 10) %>% pull(sst), 
  knot = spline_params$knots, degree = 2, intercept = TRUE, 
  Boundary.knots = c(-1.92, 10)
)

Ybspline <- fits_tmp %>% filter(sst < 10) %>% mutate(sq = (ice - ice_hat)^2) %>%
  pull(sq)

beta_bspline <- solve(t(Xbspline) %*% Xbspline) %*% t(Xbspline) %*% Ybspline
beta_bspline[c(5, 6)] <- 0

Yfit <- splines2::bSpline(
  seq(-1.92, 10, length = 100), 
  knot = spline_params$knots, degree = 2, intercept = TRUE, 
  Boundary.knots = c(-1.92, 10)
) %*% beta_bspline

ggplot(fits_tmp %>% filter(sst < 10)) +
  geom_point(aes(x = sst, y = (ice - ice_hat)^2), alpha = 0.01) +
  geom_line(
    data = tibble(x = seq(-1.92, 10, length = 100), y = as.numeric(Yfit)),
    mapping = aes(x = x, y = y), colour = "red"
  )


beta_bspline <- c(0.0022, 0.0157, 0.0320, 0.0538, 0, 0)

  var_Rhats <- lapply(1:m, function(i){

    cat(sprintf("\rIteration %i", i))

    Xtmp <- sst_update$simulation$data %>%
      filter(model == model_names[i]) %>%
      arrange(time, lon, lat) %>%
      pull(sst)

    Xtmp[which(Xtmp > 10)] <- 10

    Rdiag <- splines2::bSpline(
      Xtmp, 
      knot = spline_params$knots, degree = 2, intercept = TRUE, 
      Boundary.knots = spline_params$bounds
    ) %*% beta_bspline + 0.01

    Psi_tmp <- sst_update$simulation$data %>%
      filter(model == model_names[i]) %>%
      arrange(time, lon, lat) %>%
      create_psii(spline_params)    

    R <- t(betais$Theta) %*% t(Psi_tmp) %*% Diagonal(nrow(Rdiag), as.numeric(Rdiag)) %*%
      Psi_tmp %*% betais$Theta

    R

  })