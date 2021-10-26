
#' Generates the spatial and temporal incidence matrices
#'
#' @param simulation A sst_sim object containing the sst simulations
#' @param proxy A sst_data object containing the proxy data
#'
#' @return Returns a list of Hs, Hs_collapse (reshuffled Hs to make some 
#' calculation easier), Ht and H.
#' @export
generate_H <- function(simulation, proxy){

  type <- NULL

  # Calculate Hs

  cat(sprintf("\rCalculating Hs..."))

  tau_hat <- 6
  alpha_hat <- 1.3
  kappa_hat <- 9

  D_all <- fields::rdist.earth(
    rbind(as.matrix(simulation$coords), as.matrix(proxy$coords)),
    R = 1
  )

  D <- D_all[1:simulation$n, 1:simulation$n]
  Dx <- D_all[1:simulation$n, (simulation$n+1):(simulation$n + proxy$n)]

  K <- kappa_hat^2 * wendland(D, tau_hat, alpha_hat, 1, 1e-06)

  Kx <- kappa_hat^2 * wendland(Dx, tau_hat, alpha_hat, 1)

  Hs_tmp <- t(as.matrix(solve(K) %*% Kx))

  n_ann <- proxy$data %>% dplyr::filter(type == "ann") %>% nrow()
  n_jfm <- proxy$data %>% dplyr::filter(type == "jfm") %>% nrow()

  ann_idx <- which(proxy$data$type == "ann")
  jfm_idx <- which(proxy$data$type == "jfm")

  Hs1 <- Matrix::Matrix(Hs_tmp[ann_idx, ])
  Hs2 <- Matrix::Matrix(Hs_tmp[jfm_idx, ])

  match_idx <- match(seq(1, proxy$n, 1), c(ann_idx, jfm_idx))

  zero1 <- Matrix::Matrix(matrix(0, nrow = n_ann, ncol = simulation$n))
  zero2 <- Matrix::Matrix(matrix(0, nrow = n_jfm, ncol = simulation$n))

  Hs_unsrt <- rbind(cbind(Hs1, zero1), cbind(zero2, Hs2))
  Hs <- Hs_unsrt[match_idx, ]

  # Calculate Ht 

  cat(sprintf("\rCalculating Ht..."))

  Ht_ann <- Matrix::Matrix(
    kronecker(
      diag(1, simulation$n),
      matrix(1/simulation$ntime, nrow = 1, ncol = simulation$ntime)
    )
  )

  Ht_jfm <- Matrix::Matrix(
    kronecker(
      diag(1, simulation$n),
      matrix(c(rep(1/3, 3),rep(0,9)), nrow = 1, ncol = simulation$ntime)
    )
  )

  Ht <- rbind(Ht_ann, Ht_jfm)

  cat(sprintf("\rCalculating H... "))

  return(
    list(
      Hs = Hs,
      Hs_collapse = Hs_tmp,
      Ht = Ht,
      H = Hs %*% Ht
    )
  )

}