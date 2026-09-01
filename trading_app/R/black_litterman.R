# ===============================
# FUNCIONES PARA OPTIMIZACIÓN BLACK-LITTERMAN
# ===============================
calculate_black_litterman_weights <- function(returns_xts) {
  tryCatch({
    mu <- colMeans(returns_xts, na.rm = TRUE) * 252
    sigma <- cov(returns_xts, use = "complete.obs") * 252
    n_assets <- length(mu)
    
    if(n_assets < 2) {
      stop("Se necesitan al menos 2 activos para Black-Litterman")
    }
    
    market_caps <- runif(n_assets, 1e9, 1e12)
    market_weights <- market_caps / sum(market_caps)
    
    risk_aversion <- 2.5
    equilibrium_returns <- risk_aversion * sigma %*% market_weights
    
    P <- matrix(0, nrow = 2, ncol = n_assets)
    P[1, 1] <- 1
    P[2, 2] <- 1
    
    Q <- c(mu[1] * 1.1, mu[2] * 0.9)
    
    tau <- 0.025
    Omega <- tau * diag(diag(P %*% sigma %*% t(P)))
    
    tryCatch({
      tau_sigma_inv <- solve(tau * sigma)
      P_omega_P <- t(P) %*% solve(Omega) %*% P
      
      first_part <- tau_sigma_inv %*% equilibrium_returns
      second_part <- t(P) %*% solve(Omega) %*% Q
      
      combined_matrix <- tau_sigma_inv + P_omega_P
      
      bl_returns <- solve(combined_matrix) %*% (first_part + second_part)
      
      Dmat <- 2 * sigma
      dvec <- as.vector(bl_returns)
      Amat <- cbind(rep(1, n_assets), diag(n_assets))
      bvec <- c(1, rep(0, n_assets))
      
      solution <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
      weights <- solution$solution
      
      weights <- pmax(weights, 0)
      weights <- weights / sum(weights)
      
      return(list(
        weights = weights,
        bl_returns = as.vector(bl_returns),
        equilibrium_returns = as.vector(equilibrium_returns),
        mu = mu
      ))
      
    }, error = function(e) {
      weights <- rep(1/n_assets, n_assets)
      return(list(
        weights = weights,
        bl_returns = mu * 1.1,
        equilibrium_returns = mu * 0.9,
        mu = mu
      ))
    })
    
  }, error = function(e) {
    n_assets <- ncol(returns_xts)
    weights <- rep(1/n_assets, n_assets)
    mu <- colMeans(returns_xts, na.rm = TRUE) * 252
    return(list(
      weights = weights,
      bl_returns = mu * 1.1,
      equilibrium_returns = mu * 0.9,
      mu = mu
    ))
  })
}
