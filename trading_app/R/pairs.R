# ==============================================================================
# pairs.R -- Pairs trading: hedge ratio, spread/z-score, signals, backtest
# ==============================================================================

# ===============================
# FUNCIONES PARA PAIRS TRADING COMPLETAMENTE REFORMULADAS
# ===============================

# Función para preparar datos de pares - MÁS ROBUSTA
prepare_pairs_data_robust <- function(asset1, asset2, from, to) {
  tryCatch({
    cat("🔄 Preparando datos para pares:", asset1, "y", asset2, "\n")
    
    # Obtener datos con múltiples fallbacks
    get_asset_data <- function(symbol) {
      tryCatch({
        # Intentar con quantmod primero
        data <- getSymbols(symbol, from = from, to = to, auto.assign = FALSE)
        cat("✅", symbol, "obtenido via quantmod\n")
        return(data)
      }, error = function(e) {
        tryCatch({
          # Fallback a tidyquant
          data <- tq_get(symbol, from = from, to = to, get = "stock.prices")
          cat("✅", symbol, "obtenido via tidyquant\n")
          return(data)
        }, error = function(e2) {
          cat("❌ Error obteniendo", symbol, ":", e2$message, "\n")
          return(NULL)
        })
      })
    }
    
    asset1_data <- get_asset_data(asset1)
    asset2_data <- get_asset_data(asset2)
    
    if(is.null(asset1_data) || is.null(asset2_data)) {
      stop("No se pudieron obtener datos para uno o ambos activos")
    }
    
    # Procesar datos de quantmod
    if(xts::is.xts(asset1_data)) {
      prices1 <- as.numeric(Ad(asset1_data))
      dates1 <- index(asset1_data)
    } else {
      # Procesar datos de tidyquant
      prices1 <- asset1_data$adjusted
      dates1 <- asset1_data$date
    }
    
    if(xts::is.xts(asset2_data)) {
      prices2 <- as.numeric(Ad(asset2_data))
      dates2 <- index(asset2_data)
    } else {
      prices2 <- asset2_data$adjusted
      dates2 <- asset2_data$date
    }
    
    # Encontrar fechas comunes
    if(xts::is.xts(asset1_data) && xts::is.xts(asset2_data)) {
      common_dates <- intersect(index(asset1_data), index(asset2_data))
      prices1_common <- as.numeric(Ad(asset1_data[common_dates]))
      prices2_common <- as.numeric(Ad(asset2_data[common_dates]))
    } else {
      common_dates <- intersect(dates1, dates2)
      prices1_common <- prices1[dates1 %in% common_dates]
      prices2_common <- prices2[dates2 %in% common_dates]
    }
    
    if(length(common_dates) < 30) {
      stop("Insuficientes datos comunes:", length(common_dates), "observaciones")
    }
    
    # Calcular retornos logarítmicos
    returns1 <- diff(log(prices1_common))
    returns2 <- diff(log(prices2_common))
    
    # Remover NA/Inf
    valid_returns <- is.finite(returns1) & is.finite(returns2)
    returns1_clean <- returns1[valid_returns]
    returns2_clean <- returns2[valid_returns]
    dates_clean <- common_dates[-1][valid_returns] # -1 porque diff reduce longitud
    
    if(length(returns1_clean) < 30) {
      stop("Insuficientes retornos válidos después de limpieza")
    }
    
    cat("✅ Datos preparados exitosamente\n")
    cat("📊 Retornos -", asset1, ":", length(returns1_clean), 
        asset2, ":", length(returns2_clean), "\n")
    
    return(list(
      returns_x = returns1_clean,
      returns_y = returns2_clean,
      prices_x = prices1_common,
      prices_y = prices2_common,
      dates = dates_clean,
      asset1 = asset1,
      asset2 = asset2
    ))
    
  }, error = function(e) {
    cat("❌ Error crítico preparando datos:", e$message, "\n")
    return(NULL)
  })
}

# Función SIMPLIFICADA para calcular hedge ratio - SIN KALMAN COMPLEJO
calculate_hedge_ratio_simple <- function(returns_x, returns_y, method = "rolling") {
  tryCatch({
    cat("🔍 Calculando hedge ratio con método:", method, "\n")
    
    n_obs <- length(returns_x)
    
    if(method == "rolling") {
      # Usar ventana rolling de 60 días para hedge ratio
      window_size <- min(60, n_obs %/% 2)
      hedge_ratio <- numeric(n_obs)
      
      for(i in window_size:n_obs) {
        window_returns_x <- returns_x[(i-window_size+1):i]
        window_returns_y <- returns_y[(i-window_size+1):i]
        
        # Regresión lineal simple
        lm_model <- lm(window_returns_y ~ window_returns_x - 1) # Sin intercepto
        hedge_ratio[i] <- coef(lm_model)[1]
      }
      
      # Rellenar valores iniciales
      hedge_ratio[1:(window_size-1)] <- hedge_ratio[window_size]
      
    } else if(method == "ols") {
      # OLS simple en todos los datos
      lm_model <- lm(returns_y ~ returns_x - 1)
      hedge_ratio <- rep(coef(lm_model)[1], n_obs)
      
    } else {
      # Método por defecto: correlation
      correlation <- cor(returns_x, returns_y)
      vol_ratio <- sd(returns_y) / sd(returns_x)
      static_hedge <- correlation * vol_ratio
      hedge_ratio <- rep(static_hedge, n_obs)
    }
    
    # Limitar valores extremos
    hedge_ratio <- pmin(pmax(hedge_ratio, 0.1), 10) # Entre 0.1 y 10
    
    cat("✅ Hedge ratio calculado - Rango:", round(range(hedge_ratio), 3), "\n")
    return(hedge_ratio)
    
  }, error = function(e) {
    cat("❌ Error calculando hedge ratio:", e$message, "\n")
    # Fallback a hedge ratio constante de 1
    return(rep(1, length(returns_x)))
  })
}

# Función para calcular spread y z-score
calculate_spread_zscore <- function(returns_x, returns_y, hedge_ratio) {
  tryCatch({
    # Calcular spread
    spread <- returns_y - hedge_ratio * returns_x
    
    # Calcular z-score con ventana rolling
    window_size <- min(60, length(spread) %/% 2)
    z_score <- numeric(length(spread))
    
    for(i in window_size:length(spread)) {
      spread_window <- spread[(i-window_size+1):i]
      z_score[i] <- (spread[i] - mean(spread_window)) / sd(spread_window)
    }
    
    # Rellenar valores iniciales
    if(window_size > 1) {
      z_score[1:(window_size-1)] <- z_score[window_size]
    }
    
    # Remover valores extremos
    z_score[is.na(z_score) | is.infinite(z_score)] <- 0
    z_score <- pmin(pmax(z_score, -5), 5) # Limitar entre -5 y 5
    
    cat("✅ Spread y z-score calculados\n")
    cat("📊 Z-score stats - Mean:", round(mean(z_score), 3), 
        "SD:", round(sd(z_score), 3), "\n")
    
    return(list(spread = spread, z_score = z_score))
    
  }, error = function(e) {
    cat("❌ Error calculando spread:", e$message, "\n")
    return(list(spread = returns_y - returns_x, 
                z_score = rep(0, length(returns_x))))
  })
}

# Función MEJORADA para generar señales
generate_pairs_signals_improved <- function(z_score, threshold = 2.0) {
  tryCatch({
    n <- length(z_score)
    signals <- rep("HOLD", n)
    position <- 0
    
    for(i in 2:n) {
      current_z <- z_score[i]
      prev_z <- z_score[i-1]
      
      # Señal LONG: z-score cruza por debajo de -threshold
      if(position <= 0 && current_z < -threshold && prev_z >= -threshold) {
        signals[i] <- "LONG"
        position <- 1
      }
      # Señal SHORT: z-score cruza por encima de +threshold  
      else if(position >= 0 && current_z > threshold && prev_z <= threshold) {
        signals[i] <- "SHORT"
        position <- -1
      }
      # Cerrar posición: z-score vuelve cerca de cero
      else if(position != 0 && abs(current_z) < 1.0) {
        signals[i] <- "CLOSE"
        position <- 0
      }
      # Mantener posición existente
      else if(position != 0) {
        signals[i] <- ifelse(position == 1, "HOLD_LONG", "HOLD_SHORT")
      }
    }
    
    cat("✅ Señales generadas - LONG:", sum(signals == "LONG"),
        "SHORT:", sum(signals == "SHORT"), 
        "CLOSE:", sum(signals == "CLOSE"), "\n")
    
    return(signals)
    
  }, error = function(e) {
    cat("❌ Error generando señales:", e$message, "\n")
    return(rep("HOLD", length(z_score)))
  })
}

# Función SIMPLIFICADA para backtest
backtest_pairs_simple <- function(returns_x, returns_y, signals, hedge_ratio) {
  tryCatch({
    n <- min(length(returns_x), length(returns_y), 
             length(signals), length(hedge_ratio))
    
    # Asegurar longitudes iguales
    returns_x <- returns_x[1:n]
    returns_y <- returns_y[1:n]
    signals <- signals[1:n]
    hedge_ratio <- hedge_ratio[1:n]
    
    strategy_returns <- numeric(n)
    position <- 0
    positions <- numeric(n)
    
    for(i in 1:n) {
      current_signal <- signals[i]
      
      # Actualizar posición basado en señal
      if(current_signal == "LONG") {
        position <- 1
      } else if(current_signal == "SHORT") {
        position <- -1
      } else if(current_signal == "CLOSE") {
        position <- 0
      }
      # HOLD_LONG y HOLD_SHORT mantienen la posición
      else if(current_signal == "HOLD_LONG") {
        position <- 1
      } else if(current_signal == "HOLD_SHORT") {
        position <- -1
      }
      
      positions[i] <- position
      
      # Calcular retorno
      if(position == 1) {
        # Largo en Y, corto en X
        strategy_returns[i] <- returns_y[i] - hedge_ratio[i] * returns_x[i]
      } else if(position == -1) {
        # Corto en Y, largo en X
        strategy_returns[i] <- -returns_y[i] + hedge_ratio[i] * returns_x[i]
      } else {
        strategy_returns[i] <- 0
      }
    }
    
    # Métricas de performance
    cum_returns <- cumsum(strategy_returns)
    total_return <- tail(cum_returns, 1)
    volatility <- sd(strategy_returns) * sqrt(252)
    sharpe_ratio <- if(volatility > 0) mean(strategy_returns) / sd(strategy_returns) * sqrt(252) else 0
    
    # Drawdown
    running_max <- cummax(cum_returns)
    drawdown <- cum_returns - running_max
    max_drawdown <- min(drawdown)
    
    # Estadísticas de trades
    trade_signals <- signals[signals %in% c("LONG", "SHORT")]
    n_trades <- length(trade_signals)
    
    cat("📊 Backtest completado - Return:", round(total_return, 4),
        "Sharpe:", round(sharpe_ratio, 3),
        "Trades:", n_trades, "\n")
    
    return(list(
      strategy_returns = strategy_returns,
      cum_returns = cum_returns,
      positions = positions,
      metrics = list(
        total_return = total_return,
        sharpe_ratio = sharpe_ratio,
        max_drawdown = max_drawdown,
        volatility = volatility,
        n_trades = n_trades
      )
    ))
    
  }, error = function(e) {
    cat("❌ Error en backtest:", e$message, "\n")
    n <- length(returns_x)
    return(list(
      strategy_returns = rep(0, n),
      cum_returns = rep(0, n),
      positions = rep(0, n),
      metrics = list(
        total_return = 0,
        sharpe_ratio = 0,
        max_drawdown = 0,
        volatility = 0,
        n_trades = 0
      )
    ))
  })
}

# Función PRINCIPAL de pairs trading reformulada
execute_pairs_trading <- function(asset1, asset2, from, to, threshold = 2.0) {
  tryCatch({
    cat("\n🎯 INICIANDO PAIRS TRADING REFORMULADO\n")
    cat("📈 Par:", asset1, "/", asset2, "\n")
    cat("📅 Rango:", as.character(from), "a", as.character(to), "\n")
    cat("🎚️ Umbral Z-score:", threshold, "\n")
    
    # 1. Preparar datos
    pairs_data <- prepare_pairs_data_robust(asset1, asset2, from, to)
    if(is.null(pairs_data)) {
      stop("Fallo en preparación de datos")
    }
    
    # 2. Calcular hedge ratio (SIMPLIFICADO - sin Kalman problemático)
    hedge_ratio <- calculate_hedge_ratio_simple(
      pairs_data$returns_x, 
      pairs_data$returns_y,
      method = "rolling"  # Usar método rolling en lugar de Kalman
    )
    
    # 3. Calcular spread y z-score
    spread_data <- calculate_spread_zscore(
      pairs_data$returns_x,
      pairs_data$returns_y, 
      hedge_ratio
    )
    
    # 4. Generar señales
    signals <- generate_pairs_signals_improved(spread_data$z_score, threshold)
    
    # 5. Backtest
    backtest_results <- backtest_pairs_simple(
      pairs_data$returns_x,
      pairs_data$returns_y,
      signals,
      hedge_ratio
    )
    
    # 6. Preparar resultados finales
    results <- list(
      hedge_ratio = hedge_ratio,
      spread = spread_data$spread,
      z_score = spread_data$z_score,
      signals = signals,
      backtest = backtest_results,
      dates = pairs_data$dates,
      asset1 = asset1,
      asset2 = asset2,
      data = pairs_data
    )
    
    cat("🎉 PAIRS TRADING COMPLETADO EXITOSAMENTE\n")
    return(results)
    
  }, error = function(e) {
    cat("❌ ERROR CRÍTICO EN PAIRS TRADING:", e$message, "\n")
    return(NULL)
  })
}
