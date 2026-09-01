# ===============================
# FUNCIONES PARA SIMULACIÓN MONTE CARLO
# ===============================
monte_carlo_simulation <- function(returns_xts, n_simulations = 10000, horizon = 252, initial_value = 100000) {
  tryCatch({
    mu <- colMeans(returns_xts, na.rm = TRUE) * 252
    sigma <- cov(returns_xts, use = "complete.obs") * 252
    
    n_assets <- length(mu)
    
    weights_matrix <- matrix(runif(n_simulations * n_assets), nrow = n_simulations, ncol = n_assets)
    weights_matrix <- weights_matrix / rowSums(weights_matrix)
    
    port_returns <- numeric(n_simulations)
    port_volatilities <- numeric(n_simulations)
    final_values <- numeric(n_simulations)
    paths <- list()
    
    for(i in 1:n_simulations) {
      port_returns[i] <- sum(weights_matrix[i, ] * mu)
      port_volatilities[i] <- sqrt(t(weights_matrix[i, ]) %*% sigma %*% weights_matrix[i, ])
      
      daily_return <- port_returns[i] / 252
      daily_vol <- port_volatilities[i] / sqrt(252)
      
      price_path <- numeric(horizon + 1)
      price_path[1] <- initial_value
      
      for(t in 2:(horizon + 1)) {
        shock <- rnorm(1, 0, daily_vol)
        price_path[t] <- price_path[t-1] * exp(daily_return + shock - 0.5 * daily_vol^2)
      }
      
      final_values[i] <- price_path[horizon + 1]
      
      if(i <= 100) {
        paths[[i]] <- data.frame(
          Day = 1:length(price_path),
          Value = price_path,
          Simulation = i
        )
      }
    }
    
    returns_sim <- (final_values - initial_value) / initial_value
    var_95 <- quantile(returns_sim, 0.05)
    cvar_95 <- mean(returns_sim[returns_sim <= var_95])
    
    list(
      final_values = final_values,
      returns_sim = returns_sim,
      paths = bind_rows(paths),
      weights = weights_matrix,
      summary = list(
        mean_final_value = mean(final_values),
        median_final_value = median(final_values),
        sd_final_value = sd(final_values),
        var_95 = var_95,
        cvar_95 = cvar_95,
        probability_loss = mean(returns_sim < 0),
        probability_double = mean(final_values >= 2 * initial_value),
        best_case = max(final_values),
        worst_case = min(final_values)
      )
    )
    
  }, error = function(e) {
    cat("Error in Monte Carlo simulation:", e$message, "\n")
    return(NULL)
  })
}
