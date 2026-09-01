# ==============================================================================
# portfolio_analysis.R -- Portfolio returns, volatility, risk metrics
# ==============================================================================

# Función para análisis de portfolio
portfolio_analysis_pf <- function(returns_xts) {
  tryCatch({
    cum_returns <- apply(1 + returns_xts, 2, cumprod) - 1
    cum_returns_xts <- xts(cum_returns, order.by = index(returns_xts))
    
    quantiles <- apply(returns_xts, 2, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
    
    regressions <- map_dfr(colnames(returns_xts), function(sym) {
      tryCatch({
        returns_vec <- as.numeric(na.omit(returns_xts[, sym]))
        if(length(returns_vec) > 1) {
          lm_fit <- lm(returns_vec ~ seq_along(returns_vec))
          tibble(symbol = sym, slope = coef(lm_fit)[2], intercept = coef(lm_fit)[1])
        } else {
          tibble(symbol = sym, slope = NA, intercept = NA)
        }
      }, error = function(e) {
        tibble(symbol = sym, slope = NA, intercept = NA)
      })
    })
    
    list(
      cum_returns = cum_returns_xts,
      quantiles = quantiles,
      regressions = regressions
    )
    
  }, error = function(e) {
    list(
      cum_returns = returns_xts * 0,
      quantiles = matrix(NA, nrow = 5, ncol = ncol(returns_xts)),
      regressions = tibble(symbol = colnames(returns_xts), slope = NA, intercept = NA)
    )
  })
}

# Función para cálculo de volatilidad
calc_volatility_pf <- function(returns_xts, window = 30) {
  tryCatch({
    vol_list <- lapply(1:ncol(returns_xts), function(i) {
      rollapply(returns_xts[, i], width = window, FUN = sd, fill = NA, align = "right", na.rm = TRUE)
    })
    
    vol_matrix <- do.call(cbind, vol_list)
    colnames(vol_matrix) <- colnames(returns_xts)
    
    vol_xts <- xts(vol_matrix, order.by = index(returns_xts))
    return(vol_xts)
    
  }, error = function(e) {
    vol_fallback <- xts(matrix(NA, nrow = nrow(returns_xts), ncol = ncol(returns_xts)), 
                       order.by = index(returns_xts))
    colnames(vol_fallback) <- colnames(returns_xts)
    return(vol_fallback)
  })
}

# Función para métricas de riesgo
calc_risk_metrics_pf <- function(returns_xts, p = 0.05) {
  tryCatch({
    risk_metrics <- map_dfr(colnames(returns_xts), function(sym) {
      x <- na.omit(returns_xts[, sym])
      if(length(x) > 0) {
        var_val <- quantile(x, probs = p)
        cvar_val <- mean(x[x <= var_val])
        tibble(symbol = sym, VaR = var_val, CVaR = cvar_val)
      } else {
        tibble(symbol = sym, VaR = NA, CVaR = NA)
      }
    })
    return(risk_metrics)
  }, error = function(e) {
    tibble(symbol = colnames(returns_xts), VaR = NA, CVaR = NA)
  })
}
