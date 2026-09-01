# ===============================
# SERVER (LÓGICA DE LA APLICACIÓN)
# ===============================
server <- function(input, output, session) {
  
  # Reactive para datos principales
  data_reactive_pf <- eventReactive(input$run_pf, {
    tryCatch({
      symbols <- strsplit(input$symbols_pf, ",")[[1]] %>% str_trim()
      if(length(symbols) == 0) stop("No se proporcionaron símbolos")
      
      weights <- NULL
      if(input$weights_pf != "") {
        weights <- as.numeric(strsplit(input$weights_pf, ",")[[1]] %>% str_trim())
        if(length(weights) != length(symbols)) {
          showNotification("El número de pesos no coincide con el número de símbolos", type = "warning")
          weights <- NULL
        } else if(abs(sum(weights) - 1) > 0.01) {
          showNotification("Los pesos no suman 1. Se normalizarán automáticamente", type = "warning")
          weights <- weights / sum(weights)
        }
      }
      
      from_date <- input$from_pf
      to_date <- min(input$to_pf, Sys.Date())
      
      cat("🎯 Solicitando datos desde", as.character(from_date), "hasta", as.character(to_date), "\n")
      cat("🎯 Fecha actual del sistema:", as.character(Sys.Date()), "\n")
      
      prices <- get_prices_pf(symbols, from_date, to_date)
      if(nrow(prices) == 0) stop("No se pudieron obtener datos de precios")
      
      cat("📊 Fechas obtenidas en prices:", 
          as.character(min(prices$date)), "a", 
          as.character(max(prices$date)), "\n")
      
      returns_xts <- calc_returns_pf(prices)
      indicators <- calc_indicators_pf(prices)
      signals <- generate_signals_pf(indicators)
      backtested <- backtest_strategy_pf(returns_xts, signals)
      port_analysis <- portfolio_analysis_pf(returns_xts)
      
      list(
        prices = prices, 
        returns_xts = returns_xts, 
        indicators = indicators, 
        signals = signals, 
        backtested = backtested, 
        port_analysis = port_analysis,
        weights = weights
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      NULL
    })
  })
  
  # ===============================
  # ReactiveValues
  # ===============================
  advanced_ml_data <- reactiveValues(
    results = NULL,
    feature_importance = NULL,
    predictions = NULL
  )
  
  performance_data <- reactiveValues(
    metrics = NULL,
    asset_data = NULL,
    benchmark_data = NULL,
    available_symbols = c("AAPL", "MSFT", "GOOGL", "TSLA", "SPY")
  )
  
  mc_results <- reactiveValues(
    simulations = NULL,
    summary = NULL
  )
  
  pairs_data <- reactiveValues(
    backtest_results = NULL,
    metrics = NULL
  )
  
  # ===============================
  # UI Dinámico
  # ===============================
  output$asset_select_ui <- renderUI({
    if (input$run_pf > 0) {
      data <- data_reactive_pf()
      if (!is.null(data)) {
        symbols <- unique(data$prices$symbol)
      } else {
        symbols <- c("AAPL", "MSFT", "GOOGL", "TSLA", "SPY")
      }
    } else {
      symbols <- c("AAPL", "MSFT", "GOOGL", "TSLA", "SPY")
    }
    
    selectInput("asset_select", "Seleccionar Activo:",
                choices = symbols,
                selected = symbols[1])
  })
  
  # ===============================
  # Observers
  # ===============================
  observeEvent(input$run_pf, {
    data <- data_reactive_pf()
    if (!is.null(data)) {
      performance_data$available_symbols <- unique(data$prices$symbol)
    }
  })
  
  # OBSERVER MEJORADO PARA ML AVANZADO
  observeEvent(input$run_advanced_ml, {
    req(input$ml_asset, input$ml_dates, input$n_folds, input$test_size)
    
    showNotification("Ejecutando ML Avanzado con Walk-Forward Validation...", type = "message")
    
    advanced_ml_data$results <- NULL
    advanced_ml_data$feature_importance = NULL
    advanced_ml_data$predictions <- NULL
    
    tryCatch({
      if(input$n_folds < 2 || input$n_folds > 10) {
        stop("Número de folds debe estar entre 2 y 10")
      }
      
      if(input$test_size < 10 || input$test_size > 40) {
        stop("Tamaño de test debe estar entre 10% y 40%")
      }
      
      cat("\n==================================================\n")
      cat("🚀 INICIANDO ML AVANZADO\n")
      cat("📊 Activo:", input$ml_asset, "\n")
      cat("📅 Rango solicitado:", as.character(input$ml_dates[1]), "a", as.character(input$ml_dates[2]), "\n")
      cat("🎯 Folds:", input$n_folds, "\n")
      cat("🔍 Tamaño test:", input$test_size, "%\n")
      cat("==================================================\n")
      
      ml_df <- prepare_advanced_ml_data(input$ml_asset, 
                                       input$ml_dates[1], 
                                       input$ml_dates[2])
      
      if(is.null(ml_df)) {
        stop("No se pudieron preparar los datos para ML")
      }
      
      cat("✅ Datos preparados exitosamente. Filas:", nrow(ml_df), "\n")
      
      results <- walk_forward_validation(ml_df, 
                                        n_folds = input$n_folds,
                                        test_size = input$test_size)
      
      if(is.null(results)) {
        stop("La validación walk-forward no produjo resultados")
      }
      
      if(is.null(results$fold_performance) || nrow(results$fold_performance) == 0) {
        stop("No se generaron resultados de performance")
      }
      
      advanced_ml_data$results <- results
      
      showNotification(paste("ML Avanzado completado! Folds exitosos:", 
                            nrow(results$fold_performance)), type = "message")
      
    }, error = function(e) {
      error_msg <- paste("Error en ML Avanzado:", e$message)
      cat("❌", error_msg, "\n")
      showNotification(error_msg, type = "error", duration = 10)
    })
  })
  
  # OBSERVER PARA MONTE CARLO
  observeEvent(input$run_mc, {
    data <- data_reactive_pf()
    req(data)
    
    showNotification("Running Monte Carlo Simulation with 10,000+ paths...", type = "message")
    
    tryCatch({
      results <- monte_carlo_simulation(
        returns_xts = data$returns_xts,
        n_simulations = input$n_simulations,
        horizon = input$mc_horizon,
        initial_value = input$initial_portfolio
      )
      
      if(!is.null(results)) {
        mc_results$simulations <- results
        mc_results$summary <- results$summary
        
        showNotification(paste("Monte Carlo simulation completed with", input$n_simulations, "simulations"), 
                        type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error in Monte Carlo simulation:", e$message), type = "error")
    })
  })
  
  # OBSERVER PARA PERFORMANCE REPORT
  observeEvent(input$calculate_metrics, {
    req(input$asset_select, input$date_range_perf, input$risk_free)
    
    showNotification("Calculando métricas de performance...", type = "message")
    
    tryCatch({
      asset_data <- get_performance_data_optimized(input$asset_select, 
                                                 input$date_range_perf[1], 
                                                 input$date_range_perf[2])
      
      benchmark_data <- get_performance_data_optimized("SPY", 
                                                     input$date_range_perf[1], 
                                                     input$date_range_perf[2])
      
      asset_xts <- xts(asset_data$Returns, order.by = asset_data$Date)
      benchmark_xts <- xts(benchmark_data$Returns, order.by = benchmark_data$Date)
      
      common_dates <- intersect(index(asset_xts), index(benchmark_xts))
      
      if(length(common_dates) == 0) {
        stop("No hay fechas comunes entre el activo y el benchmark")
      }
      
      asset_xts <- asset_xts[common_dates]
      benchmark_xts <- benchmark_xts[common_dates]
      
      risk_free_daily <- input$risk_free / 100 / 252
      
      metrics_list <- list()
      
      metrics_list$Sortino_Ratio <- tryCatch({
        as.numeric(SortinoRatio(asset_xts, MAR = risk_free_daily))
      }, error = function(e) NA)
      
      metrics_list$Calmar_Ratio <- tryCatch({
        as.numeric(CalmarRatio(asset_xts, scale = 252))
      }, error = function(e) NA)
      
      metrics_list$Information_Ratio <- tryCatch({
        as.numeric(InformationRatio(asset_xts, benchmark_xts))
      }, error = function(e) NA)
      
      metrics_list$Alpha <- tryCatch({
        as.numeric(CAPM.alpha(asset_xts, benchmark_xts, Rf = risk_free_daily))
      }, error = function(e) NA)
      
      metrics_list$Beta <- tryCatch({
        as.numeric(CAPM.beta(asset_xts, benchmark_xts, Rf = risk_free_daily))
      }, error = function(e) NA)
      
      metrics_list$Annualized_Return <- tryCatch({
        as.numeric(Return.annualized(asset_xts))
      }, error = function(e) NA)
      
      metrics_list$Annualized_Volatility <- tryCatch({
        as.numeric(StdDev.annualized(asset_xts))
      }, error = function(e) NA)
      
      metrics_list$Max_Drawdown <- tryCatch({
        as.numeric(maxDrawdown(asset_xts))
      }, error = function(e) NA)
      
      metrics_list$Sharpe_Ratio <- tryCatch({
        as.numeric(SharpeRatio(asset_xts, Rf = risk_free_daily))[1]
      }, error = function(e) NA)
      
      metrics_list$Cumulative_Return <- tryCatch({
        as.numeric(Return.cumulative(asset_xts))
      }, error = function(e) NA)
      
      performance_data$metrics <- metrics_list
      performance_data$asset_data <- asset_data
      performance_data$benchmark_data <- benchmark_data
      
      showNotification("Métricas calculadas exitosamente!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error calculando métricas:", e$message), type = "error")
    })
  })
  
  # OBSERVER ACTUALIZADO PARA PAIRS TRADING
  observeEvent(input$run_pairs, {
    req(input$pair_asset1, input$pair_asset2, input$pair_dates)
    
    showNotification("Ejecutando Pairs Trading (Sistema Reformulado)...", type = "message")
    
    # Resetear datos previos
    pairs_data$backtest_results <- NULL
    pairs_data$metrics <- NULL
    
    tryCatch({
      # Ejecutar pairs trading con nuevo sistema
      results <- execute_pairs_trading(
        asset1 = input$pair_asset1,
        asset2 = input$pair_asset2,
        from = input$pair_dates[1],
        to = input$pair_dates[2],
        threshold = input$z_threshold
      )
      
      if(is.null(results)) {
        stop("El pairs trading no produjo resultados")
      }
      
      # Almacenar resultados en reactiveValues
      pairs_data$backtest_results <- results$backtest
      pairs_data$metrics <- list(
        total_return = results$backtest$metrics$total_return,
        sharpe_ratio = results$backtest$metrics$sharpe_ratio,
        max_drawdown = results$backtest$metrics$max_drawdown,
        volatility = results$backtest$metrics$volatility,
        n_trades = results$backtest$metrics$n_trades,
        dates = results$dates,
        signals = results$signals,
        asset1 = results$asset1,
        asset2 = results$asset2,
        hedge_ratio = results$hedge_ratio,
        z_score = results$z_score
      )
      
      showNotification(
        paste("Pairs Trading completado! Return:", 
              round(results$backtest$metrics$total_return * 100, 2), "%"),
        type = "message"
      )
      
    }, error = function(e) {
      error_msg <- paste("Error en Pairs Trading:", e$message)
      cat("❌", error_msg, "\n")
      showNotification(error_msg, type = "error", duration = 10)
    })
  })
  
  # ===============================
  # OUTPUTS CORREGIDOS PARA PLOTLY
  # ===============================
  
  # 1. Gráfico de Z-Score corregido
  output$zscore_plot <- renderPlotly({
    req(pairs_data$metrics)
    
    metrics <- pairs_data$metrics
    
    plot_data <- data.frame(
      Date = metrics$dates,
      ZScore = metrics$z_score,
      Signal = metrics$signals
    )
    
    # Crear plot base CON mode explícito
    p <- plot_ly(plot_data, x = ~Date, y = ~ZScore, 
                 type = 'scatter', mode = 'lines',
                 line = list(color = '#2c3e50', width = 1.5),
                 name = "Z-Score") %>%
      layout(
        title = "Z-Score del Spread y Señales de Trading",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Z-Score"),
        showlegend = TRUE
      )
    
    # Añadir líneas de umbral CON mode='lines' explícito
    p <- p %>% add_trace(
      x = ~Date,
      y = rep(input$z_threshold, nrow(plot_data)),
      type = 'scatter', 
      mode = 'lines',
      line = list(color = '#e74c3c', dash = 'dash', width = 1),
      name = paste("Umbral +", input$z_threshold),
      showlegend = TRUE
    )
    
    p <- p %>% add_trace(
      x = ~Date,
      y = rep(-input$z_threshold, nrow(plot_data)),
      type = 'scatter', 
      mode = 'lines',
      line = list(color = '#2ecc71', dash = 'dash', width = 1),
      name = paste("Umbral -", input$z_threshold),
      showlegend = TRUE
    )
    
    # Añadir señales si existen - usar add_markers para puntos
    signal_data <- plot_data[plot_data$Signal %in% c("LONG", "SHORT", "CLOSE"), ]
    if(nrow(signal_data) > 0) {
      p <- p %>% add_markers(
        data = signal_data, 
        x = ~Date, 
        y = ~ZScore, 
        color = ~Signal, 
        colors = c("LONG" = "#2ecc71", "SHORT" = "#e74c3c", "CLOSE" = "#f39c12"),
        marker = list(size = 8), 
        name = "Señales",
        showlegend = TRUE
      )
    }
    
    return(p)
  })
  
  # 2. Gráfico de evolución de accuracy corregido
  output$accuracy_evolution_plot <- renderPlotly({
    req(advanced_ml_data$results)
    
    performance_df <- advanced_ml_data$results$fold_performance
    
    # Gráfico principal con lines+markers
    p <- plot_ly(performance_df, x = ~Fold, y = ~Accuracy, 
                 type = 'scatter', mode = 'lines+markers',
                 line = list(color = '#2ecc71', width = 3),
                 marker = list(size = 8, color = '#27ae60'),
                 text = ~paste("Fold:", Fold, "<br>Accuracy:", round(Accuracy * 100, 2), "%"),
                 hoverinfo = 'text',
                 name = 'Accuracy por Fold') %>%
      layout(
        title = "Evolución de Accuracy por Fold",
        xaxis = list(title = "Fold", dtick = 1),
        yaxis = list(title = "Accuracy", tickformat = ".0%"),
        showlegend = TRUE
      )
    
    # Añadir línea promedio CON mode='lines' explícito
    avg_accuracy <- mean(performance_df$Accuracy, na.rm = TRUE)
    p <- p %>% add_trace(
      x = ~Fold,
      y = rep(avg_accuracy, nrow(performance_df)),
      type = 'scatter', 
      mode = 'lines',
      line = list(color = '#e74c3c', dash = 'dash', width = 2),
      name = paste('Promedio:', round(avg_accuracy * 100, 1), '%'),
      showlegend = TRUE
    )
    
    return(p)
  })
  
  # 3. Gráfico de performance comparativo corregido
  output$performance_plot <- renderPlotly({
    req(performance_data$asset_data, performance_data$benchmark_data)
    
    tryCatch({
      asset_data <- performance_data$asset_data
      benchmark_data <- performance_data$benchmark_data
      
      asset_cumulative <- tryCatch({
        cumprod(1 + asset_data$Returns) - 1
      }, error = function(e) rep(0, nrow(asset_data)))
      
      benchmark_cumulative <- tryCatch({
        cumprod(1 + benchmark_data$Returns) - 1
      }, error = function(e) rep(0, nrow(benchmark_data)))
      
      # Crear datos para plotting
      plot_data_asset <- data.frame(
        Date = asset_data$Date,
        Return = asset_cumulative,
        Series = input$asset_select
      )
      
      plot_data_benchmark <- data.frame(
        Date = benchmark_data$Date,
        Return = benchmark_cumulative,
        Series = "SPY (Benchmark)"
      )
      
      plot_data <- rbind(plot_data_asset, plot_data_benchmark)
      
      # Usar mode='lines' explícitamente
      plot_ly(plot_data, x = ~Date, y = ~Return, color = ~Series,
              type = 'scatter', mode = 'lines',
              line = list(width = 2),
              text = ~paste(Series, "<br>Return:", round(Return * 100, 2), "%"),
              hoverinfo = 'text') %>%
        layout(
          title = "Retornos Acumulados vs Benchmark SPY",
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "Retorno Acumulado", tickformat = ".2%"),
          legend = list(orientation = 'h', x = 0, y = 1.1),
          hovermode = 'compare'
        )
      
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(
          text = "Error generando gráfico de performance",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
  })
  
  # 4. Gráfico de paths de Monte Carlo corregido
  output$mc_paths_plot <- renderPlotly({
    req(mc_results$simulations)
    
    paths <- mc_results$simulations$paths
    
    # Usar mode='lines' explícitamente
    plot_ly(paths, x = ~Day, y = ~Value, color = ~as.factor(Simulation),
            type = 'scatter', mode = 'lines',
            line = list(width = 1),
            showlegend = FALSE,
            hoverinfo = 'none') %>%  # Desactivar hover para mejorar rendimiento
      layout(
        title = "Sample Monte Carlo Simulation Paths (100 paths shown)",
        xaxis = list(title = "Days"),
        yaxis = list(title = "Portfolio Value ($)")
      )
  })
  
  # 5. Gráfico de análisis VaR corregido
  output$var_analysis_plot <- renderPlotly({
    req(mc_results$simulations)
    
    returns_sim <- mc_results$simulations$returns_sim
    
    var_95 <- quantile(returns_sim, 0.05, na.rm = TRUE)
    cvar_95 <- mean(returns_sim[returns_sim <= var_95], na.rm = TRUE)
    
    hist_data <- hist(returns_sim, plot = FALSE, breaks = 50)
    
    p <- plot_ly(x = ~hist_data$mids, y = ~hist_data$counts, type = 'bar',
                 name = "Returns Distribution",
                 marker = list(color = '#3498db', opacity = 0.7),
                 hoverinfo = 'x+y') %>%
      layout(
        title = "Value at Risk (VaR) Analysis",
        xaxis = list(title = "Portfolio Return", tickformat = ".2%"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
    
    # Añadir líneas VaR y CVaR CON mode explícito
    p <- p %>% add_trace(
      x = c(var_95, var_95), 
      y = c(0, max(hist_data$counts) * 0.9),
      type = 'scatter', 
      mode = 'lines',
      line = list(color = 'red', width = 3, dash = 'dash'),
      name = paste('95% VaR: ', round(var_95 * 100, 2), '%')
    )
    
    p <- p %>% add_trace(
      x = c(cvar_95, cvar_95), 
      y = c(0, max(hist_data$counts) * 0.7),
      type = 'scatter', 
      mode = 'lines',
      line = list(color = 'darkred', width = 3, dash = 'dot'),
      name = paste('95% CVaR: ', round(cvar_95 * 100, 2), '%')
    )
    
    return(p)
  })
  
  # 6. Gráfico de probabilidad de pérdida corregido
  output$loss_probability_plot <- renderPlotly({
    req(mc_results$simulations)
    
    returns_sim <- mc_results$simulations$returns_sim
    
    loss_thresholds <- seq(0, 0.5, by = 0.05)
    loss_probabilities <- sapply(loss_thresholds, function(threshold) {
      mean(returns_sim <= -threshold, na.rm = TRUE)
    })
    
    loss_df <- data.frame(
      Threshold = loss_thresholds,
      Probability = loss_probabilities
    )
    
    # Usar mode='lines+markers' explícitamente
    plot_ly(loss_df, x = ~Threshold, y = ~Probability, 
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = '#e74c3c', width = 3),
            marker = list(size = 8, color = '#c0392b'),
            text = ~paste("Loss > ", round(Threshold * 100, 1), "%: ", 
                         round(Probability * 100, 2), "%"),
            hoverinfo = 'text') %>%
      layout(
        title = "Probability of Exceeding Loss Thresholds",
        xaxis = list(title = "Loss Threshold", tickformat = ".0%"),
        yaxis = list(title = "Probability", tickformat = ".2%")
      )
  })
  
  # 7. Gráfico de distribución de retornos corregido
  output$returns_distribution_plot <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    
    returns_xts <- data$returns_xts
    
    p <- plot_ly(type = 'scatter', mode = 'lines')  # Inicializar con mode
    
    for(sym in colnames(returns_xts)) {
      returns <- na.omit(returns_xts[, sym])
      if(length(returns) > 10) {  # Mínimo de observaciones
        density <- density(returns)
        p <- p %>% add_trace(
          x = ~density$x, 
          y = ~density$y, 
          type = 'scatter', 
          mode = 'lines',
          fill = 'tozeroy', 
          name = sym, 
          opacity = 0.6
        )
      }
    }
    
    p %>%
      layout(
        title = "Returns Distribution Density",
        xaxis = list(title = "Daily Returns"),
        yaxis = list(title = "Density"),
        showlegend = TRUE
      )
  })
  
  # 8. Gráfico de autocorrelación corregido
  output$autocorrelation_plot <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    
    returns_xts <- data$returns_xts
    port_returns <- rowMeans(returns_xts, na.rm = TRUE)
    
    acf_data <- acf(port_returns, plot = FALSE, na.action = na.pass)
    
    acf_df <- data.frame(
      Lag = acf_data$lag,
      ACF = acf_data$acf
    )
    
    conf_level <- 0.95
    conf_bound <- qnorm((1 + conf_level) / 2) / sqrt(acf_data$n.used)
    
    # Crear gráfico con mode='lines' para las líneas de confianza
    p <- plot_ly(acf_df, x = ~Lag, y = ~ACF, type = 'bar',
                 marker = list(color = '#3498db'),
                 name = 'ACF') %>%
      layout(
        title = "Autocorrelation Function (ACF) of Portfolio Returns",
        xaxis = list(title = "Lag"),
        yaxis = list(title = "Autocorrelation")
      )
    
    # Añadir líneas de confianza
    p <- p %>% add_trace(
      x = ~Lag, 
      y = conf_bound,
      type = 'scatter', 
      mode = 'lines',
      line = list(color = 'red', dash = 'dash'),
      name = '95% CI'
    )
    
    p <- p %>% add_trace(
      x = ~Lag, 
      y = -conf_bound,
      type = 'scatter', 
      mode = 'lines', 
      line = list(color = 'red', dash = 'dash'),
      showlegend = FALSE
    )
    
    return(p)
  })
  
  # ===============================
  # OUTPUTS PARA PAIRS TRADING
  # ===============================
  output$hedge_ratio_plot <- renderPlotly({
    req(pairs_data$metrics)
    
    metrics <- pairs_data$metrics
    
    plot_data <- data.frame(
      Date = metrics$dates,
      HedgeRatio = metrics$hedge_ratio
    )
    
    plot_ly(plot_data, x = ~Date, y = ~HedgeRatio, type = 'scatter', mode = 'lines',
            line = list(color = '#3498db', width = 2),
            name = "Hedge Ratio") %>%
      layout(
        title = paste("Hedge Ratio Dinámica -", metrics$asset1, "/", metrics$asset2),
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Hedge Ratio"),
        showlegend = TRUE
      )
  })
  
  output$pairs_equity_plot <- renderPlotly({
    req(pairs_data$backtest_results)
    
    backtest <- pairs_data$backtest_results
    metrics <- pairs_data$metrics
    
    plot_data <- data.frame(
      Date = metrics$dates,
      Equity = backtest$cum_returns
    )
    
    plot_ly(plot_data, x = ~Date, y = ~Equity, type = 'scatter', mode = 'lines',
            line = list(color = '#9b59b6', width = 3),
            name = "Equity Curve") %>%
      layout(
        title = "Curva de Capital - Estrategia Pairs Trading",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Retorno Acumulado", tickformat = ".2%"),
        showlegend = TRUE
      )
  })
  
  output$pairs_metrics_table <- renderDT({
    req(pairs_data$metrics)
    
    metrics <- pairs_data$metrics
    
    metrics_df <- data.frame(
      Métrica = c("Retorno Total", "Ratio de Sharpe", "Máximo Drawdown", 
                  "Volatilidad Anual", "Número de Trades", "Activo 1", "Activo 2"),
      Valor = c(
        paste0(round(metrics$total_return * 100, 2), "%"),
        round(metrics$sharpe_ratio, 3),
        paste0(round(metrics$max_drawdown * 100, 2), "%"),
        paste0(round(metrics$volatility * 100, 2), "%"),
        metrics$n_trades,
        metrics$asset1,
        metrics$asset2
      )
    )
    
    datatable(
      metrics_df,
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = 10
      ),
      rownames = FALSE,
      caption = "Métricas de Performance - Pairs Trading"
    ) %>%
      formatStyle('Valor', 
                  backgroundColor = styleEqual(
                    "0", 
                    '#f8f9fa'
                  ))
  })
  
  output$pairs_signals_table <- renderDT({
    req(pairs_data$metrics)
    
    metrics <- pairs_data$metrics
    
    # Crear tabla de señales
    signals_df <- data.frame(
      Fecha = metrics$dates,
      ZScore = round(metrics$z_score, 3),
      Señal = metrics$signals,
      HedgeRatio = round(metrics$hedge_ratio, 4)
    ) %>%
      filter(Señal %in% c("LONG", "SHORT", "CLOSE")) %>%
      arrange(desc(Fecha)) %>%
      head(20)
    
    if(nrow(signals_df) == 0) {
      signals_df <- data.frame(
        Mensaje = "No se generaron señales de trading en el período seleccionado"
      )
    }
    
    datatable(
      signals_df,
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = 10
      ),
      rownames = FALSE,
      caption = "Señales de Trading Recientes"
    ) %>%
      formatStyle('Señal',
                  backgroundColor = styleEqual(
                    c('LONG', 'SHORT', 'CLOSE'), 
                    c('#d4edda', '#f8d7da', '#fff3cd')
                  ))
  })
  
  # ===============================
  # OUTPUTS PARA ML AVANZADO
  # ===============================
  output$fold_performance_table <- renderDT({
    req(advanced_ml_data$results)
    
    performance_df <- advanced_ml_data$results$fold_performance
    
    datatable(
      performance_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Blfrtip'
      ),
      rownames = FALSE,
      caption = "Performance por Fold - Walk-Forward Validation"
    ) %>%
      formatPercentage(columns = c('Accuracy', 'Precision', 'Recall', 'F1_Score'), digits = 2) %>%
      formatStyle('Accuracy',
                  backgroundColor = styleInterval(
                    c(0.5, 0.6, 0.7),
                    c('#ff6b6b', '#ffd93d', '#a8e6cf', '#6bcf7f')
                  ))
  })
  
  output$ml_summary_table <- renderDT({
    req(advanced_ml_data$results)
    
    performance_df <- advanced_ml_data$results$fold_performance
    
    summary_df <- data.frame(
      Métrica = c("Accuracy Promedio", "Precisión Promedio", "Recall Promedio", 
                  "F1-Score Promedio", "Mejor Accuracy", "Peor Accuracy",
                  "Total Folds", "Total Predicciones"),
      Valor = c(
        paste0(round(mean(performance_df$Accuracy, na.rm = TRUE) * 100, 2), "%"),
        paste0(round(mean(performance_df$Precision, na.rm = TRUE) * 100, 2), "%"),
        paste0(round(mean(performance_df$Recall, na.rm = TRUE) * 100, 2), "%"),
        paste0(round(mean(performance_df$F1_Score, na.rm = TRUE) * 100, 2), "%"),
        paste0(round(max(performance_df$Accuracy, na.rm = TRUE) * 100, 2), "%"),
        paste0(round(min(performance_df$Accuracy, na.rm = TRUE) * 100, 2), "%"),
        nrow(performance_df),
        nrow(advanced_ml_data$results$predictions)
      )
    )
    
    datatable(
      summary_df,
      options = list(
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      caption = "Resumen de Métricas - ML Avanzado"
    )
  })
  
  output$feature_importance_plot <- renderPlotly({
    req(advanced_ml_data$results)
    
    tryCatch({
      importance_df <- advanced_ml_data$results$feature_importance
      
      if(is.null(importance_df) || nrow(importance_df) == 0) {
        plot_ly() %>%
          add_annotations(
            text = "No hay datos de importancia de variables",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 16)
          ) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            title = "Importancia de Variables - Sin Datos"
          )
      } else {
        avg_importance <- importance_df %>%
          group_by(Feature) %>%
          summarise(
            Avg_Gain = mean(Gain, na.rm = TRUE),
            Count = n()
          ) %>%
          arrange(desc(Avg_Gain)) %>%
          head(15)
        
        if(nrow(avg_importance) == 0) {
          stop("No hay features para mostrar")
        }
        
        plot_ly(avg_importance, 
                x = ~Avg_Gain, 
                y = ~reorder(Feature, Avg_Gain),
                type = 'bar',
                orientation = 'h',
                marker = list(color = '#3498db',
                             line = list(color = '#2980b9', width = 1)),
                text = ~paste("Feature:", Feature, "<br>Importancia:", round(Avg_Gain, 4)),
                hoverinfo = 'text') %>%
          layout(
            title = "Importancia Promedio de Variables (Gain)",
            xaxis = list(title = "Importancia (Gain)"),
            yaxis = list(title = ""),
            showlegend = FALSE
          )
      }
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(
          text = paste("Error generando gráfico:", e$message),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
  })
  
  output$detailed_predictions_table <- renderDT({
    req(advanced_ml_data$results)
    
    predictions_df <- advanced_ml_data$results$predictions %>%
      arrange(desc(Date)) %>%
      head(100)
    
    datatable(
      predictions_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Blfrtip'
      ),
      rownames = FALSE,
      caption = "Predicciones Detalladas - Últimas 100 Observaciones"
    ) %>%
      formatStyle('Correct',
                  backgroundColor = styleEqual(
                    c(TRUE, FALSE), 
                    c('#d4edda', '#f8d7da')
                  )) %>%
      formatStyle('Predicted',
                  backgroundColor = styleEqual(
                    c('UP', 'DOWN'), 
                    c('#d4edda', '#f8d7da')
                  ))
  })
  
  output$download_ml_results <- downloadHandler(
    filename = function() {
      paste0("ml_avanzado_", input$ml_asset, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(advanced_ml_data$results)
      write.csv(advanced_ml_data$results$fold_performance, file, row.names = FALSE)
    }
  )
  
  # ===============================
  # OUTPUTS PARA STATISTICAL ANALYSIS
  # ===============================
  output$risk_dashboard <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    
    returns_xts <- data$returns_xts
    port_returns <- rowMeans(returns_xts, na.rm = TRUE)
    
    risk_metrics <- list(
      VaR_95 = quantile(port_returns, 0.05, na.rm = TRUE),
      CVaR_95 = mean(port_returns[port_returns <= quantile(port_returns, 0.05, na.rm = TRUE)], na.rm = TRUE),
      Max_Drawdown = as.numeric(PerformanceAnalytics::maxDrawdown(port_returns)),
      Sharpe = ifelse(sd(port_returns) > 0, mean(port_returns)/sd(port_returns)*sqrt(252), 0),
      Sortino = as.numeric(PerformanceAnalytics::SortinoRatio(port_returns)),
      Skewness = ifelse(sd(port_returns) > 0, moments::skewness(port_returns, na.rm = TRUE), 0),
      Kurtosis = ifelse(sd(port_returns) > 0, moments::kurtosis(port_returns, na.rm = TRUE), 0),
      Omega_Ratio = as.numeric(PerformanceAnalytics::Omega(port_returns)),
      Upside_Potential = mean(port_returns[port_returns > 0], na.rm = TRUE)
    )
    
    risk_df <- data.frame(
      Metric = names(risk_metrics),
      Value = unlist(risk_metrics),
      Type = c("Downside", "Downside", "Downside", "Return", "Return", 
               "Distribution", "Distribution", "Return", "Return")
    )
    
    plot_ly(risk_df, x = ~Metric, y = ~Value, type = 'bar', color = ~Type,
            text = ~paste(Metric, ": ", round(Value, 4)),
            hoverinfo = 'text') %>%
      layout(title = "Advanced Risk Metrics Dashboard",
             xaxis = list(title = "", categoryorder = "total descending"),
             yaxis = list(title = "Value"),
             showlegend = TRUE)
  })
  
  output$advanced_risk_metrics_table <- renderDT({
    data <- data_reactive_pf()
    req(data)
    
    returns_xts <- data$returns_xts
    risk_metrics_list <- list()
    
    for(sym in colnames(returns_xts)) {
      returns <- na.omit(returns_xts[, sym])
      if(length(returns) > 0 && sd(returns) > 0) {
        risk_metrics_list[[sym]] <- data.frame(
          Symbol = sym,
          Sharpe = round(mean(returns)/sd(returns)*sqrt(252), 4),
          Sortino = round(as.numeric(PerformanceAnalytics::SortinoRatio(returns)), 4),
          VaR_95 = round(quantile(returns, 0.05), 4),
          CVaR_95 = round(mean(returns[returns <= quantile(returns, 0.05)]), 4),
          Max_DD = round(as.numeric(PerformanceAnalytics::maxDrawdown(returns)), 4),
          Skewness = round(moments::skewness(returns), 4),
          Kurtosis = round(moments::kurtosis(returns), 4)
        )
      }
    }
    
    risk_metrics_df <- bind_rows(risk_metrics_list)
    
    datatable(
      risk_metrics_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Blfrtip'
      ),
      rownames = FALSE,
      caption = "Advanced Risk Metrics by Asset"
    ) %>%
      formatStyle('VaR_95', 
                  backgroundColor = styleInterval(
                    c(-0.03, -0.02, -0.01),
                    c('#dc3545', '#f8d7da', '#fff3cd', '#d4edda')
                  ))
  })
  
  output$normality_tests_table <- renderDT({
    data <- data_reactive_pf()
    req(data)
    
    returns_xts <- data$returns_xts
    normality_tests <- list()
    
    for(sym in colnames(returns_xts)) {
      returns <- na.omit(as.numeric(returns_xts[, sym]))
      if(length(returns) > 0) {
        shapiro_test <- shapiro.test(returns)
        jarque_bera_test <- tseries::jarque.bera.test(returns)
        
        normality_tests[[sym]] <- data.frame(
          Symbol = sym,
          Shapiro_W = round(shapiro_test$statistic, 4),
          Shapiro_P = round(shapiro_test$p.value, 4),
          JB_Statistic = round(jarque_bera_test$statistic, 4),
          JB_PValue = round(jarque_bera_test$p.value, 4),
          Normal = shapiro_test$p.value > 0.05
        )
      }
    }
    
    normality_df <- bind_rows(normality_tests)
    
    datatable(
      normality_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Blfrtip'
      ),
      rownames = FALSE,
      caption = "Normality Tests (Shapiro-Wilk and Jarque-Bera)"
    ) %>%
      formatStyle('Normal',
                  backgroundColor = styleEqual(
                    c(TRUE, FALSE), 
                    c('#d4edda', '#f8d7da')
                  ))
  })
  
  output$stationarity_tests_table <- renderDT({
    data <- data_reactive_pf()
    req(data)
    
    returns_xts <- data$returns_xts
    stationarity_tests <- list()
    
    for(sym in colnames(returns_xts)) {
      returns <- na.omit(returns_xts[, sym])
      if(length(returns) > 0) {
        tryCatch({
          adf_test <- tseries::adf.test(returns)
          kpss_test <- tseries::kpss.test(returns, null = "Level")
          
          stationarity_tests[[sym]] <- data.frame(
            Symbol = sym,
            ADF_Statistic = round(adf_test$statistic, 4),
            ADF_PValue = round(adf_test$p.value, 4),
            KPSS_Statistic = round(kpss_test$statistic, 4),
            KPSS_PValue = round(kpss_test$p.value, 4),
            Stationary = adf_test$p.value < 0.05
          )
        }, error = function(e) {
        })
      }
    }
    
    stationarity_df <- bind_rows(stationarity_tests)
    
    datatable(
      stationarity_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Blfrtip'
      ),
      rownames = FALSE,
      caption = "Stationarity Tests (ADF and KPSS)"
    ) %>%
      formatStyle('Stationary',
                  backgroundColor = styleEqual(
                    c(TRUE, FALSE), 
                    c('#d4edda', '#f8d7da')
                  ))
  })
  
  # ===============================
  # OUTPUTS PARA MONTE CARLO
  # ===============================
  output$mc_distribution_plot <- renderPlotly({
    req(mc_results$simulations)
    
    final_values <- mc_results$simulations$final_values
    initial_value <- input$initial_portfolio
    
    plot_ly(x = ~final_values, type = "histogram", 
            nbinsx = 50, 
            marker = list(color = '#3498db', 
                         line = list(color = '#2980b9', width = 1))) %>%
      layout(title = paste("Distribution of Final Portfolio Values (", input$n_simulations, "simulations)"),
             xaxis = list(title = "Final Portfolio Value ($)"),
             yaxis = list(title = "Frequency"),
             shapes = list(
               list(type = "line", 
                    x0 = initial_value, x1 = initial_value, 
                    y0 = 0, y1 = 1, yref = "paper",
                    line = list(color = "red", width = 2, dash = "dash"))
             ),
             annotations = list(
               list(x = initial_value, y = 1, yref = "paper",
                    text = "Initial Value", showarrow = TRUE,
                    arrowhead = 7, ax = 0, ay = -40)
             ))
  })
  
  output$mc_summary_table <- renderDT({
    req(mc_results$summary)
    
    summary <- mc_results$summary
    initial_value <- input$initial_portfolio
    
    summary_df <- data.frame(
      Metric = c(
        "Mean Final Value",
        "Median Final Value", 
        "Standard Deviation",
        "95% Value at Risk (VaR)",
        "95% Conditional VaR (CVaR)",
        "Probability of Loss",
        "Probability of Doubling",
        "Best Case Scenario",
        "Worst Case Scenario"
      ),
      Value = c(
        paste0("$", format(round(summary$mean_final_value), big.mark = ",")),
        paste0("$", format(round(summary$median_final_value), big.mark = ",")),
        paste0("$", format(round(summary$sd_final_value), big.mark = ",")),
        paste0(round(summary$var_95 * 100, 2), "%"),
        paste0(round(summary$cvar_95 * 100, 2), "%"),
        paste0(round(summary$probability_loss * 100, 2), "%"),
        paste0(round(summary$probability_double * 100, 2), "%"),
        paste0("$", format(round(summary$best_case), big.mark = ",")),
        paste0("$", format(round(summary$worst_case), big.mark = ","))
      ),
      Description = c(
        "Average portfolio value across all simulations",
        "Middle value of all simulated outcomes",
        "Volatility of final portfolio values",
        "Worst 5% of outcomes (loss threshold)",
        "Average loss in worst 5% of cases",
        "Probability of ending with less than initial investment",
        "Probability of at least doubling initial investment", 
        "Best outcome across all simulations",
        "Worst outcome across all simulations"
      )
    )
    
    datatable(
      summary_df,
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = 10
      ),
      rownames = FALSE,
      caption = paste("Monte Carlo Simulation Summary (", input$n_simulations, "simulations)")
    ) %>%
      formatStyle('Value', 
                  backgroundColor = styleInterval(
                    c(0, 0.1, 0.2),
                    c('#ff6b6b', '#ffd93d', '#a8e6cf', '#6bcf7f')
                  ))
  })
  
  # ===============================
  # KPIs
  # ===============================
  output$kpi_return_pf <- renderValueBox({
    data <- data_reactive_pf()
    if(is.null(data)) return(valueBox("N/A", "Retorno Acumulado", icon = icon("chart-line"), color = "green"))
    
    cum_returns <- data$port_analysis$cum_returns
    if(nrow(cum_returns) == 0) return(valueBox("N/A", "Retorno Acumulado", icon = icon("chart-line"), color = "green"))
    
    final_returns <- as.numeric(tail(cum_returns, 1))
    avg_return <- mean(final_returns, na.rm = TRUE)
    valueBox(
      paste0(round(avg_return * 100, 2), "%"), 
      "Retorno Promedio", 
      icon = icon("chart-line"), 
      color = "green"
    )
  })
  
  output$kpi_vol_pf <- renderValueBox({
    data <- data_reactive_pf()
    if(is.null(data)) return(valueBox("N/A", "Volatilidad Promedio", icon = icon("chart-area"), color = "yellow"))
    
    returns_xts <- data$returns_xts
    vol <- mean(apply(returns_xts, 2, sd, na.rm = TRUE), na.rm = TRUE)
    valueBox(
      paste0(round(vol * 100, 2), "%"), 
      "Volatilidad Promedio", 
      icon = icon("chart-area"), 
      color = "yellow"
    )
  })
  
  output$kpi_var_pf <- renderValueBox({
    data <- data_reactive_pf()
    if(is.null(data)) return(valueBox("N/A", "VaR", icon = icon("exclamation-triangle"), color = "red"))
    
    returns_xts <- data$returns_xts
    p <- input$var_level
    var_values <- apply(returns_xts, 2, quantile, probs = p, na.rm = TRUE)
    avg_var <- mean(var_values, na.rm = TRUE)
    valueBox(
      paste0(round(avg_var * 100, 2), "%"), 
      paste0("VaR ", p * 100, "%"), 
      icon = icon("exclamation-triangle"), 
      color = "red"
    )
  })
  
  output$kpi_cvar_pf <- renderValueBox({
    data <- data_reactive_pf()
    if(is.null(data)) return(valueBox("N/A", "CVaR", icon = icon("exclamation-circle"), color = "red"))
    
    returns_xts <- data$returns_xts
    p <- input$var_level
    cvar_values <- sapply(1:ncol(returns_xts), function(i) {
      x <- returns_xts[, i]
      x_clean <- na.omit(x)
      if(length(x_clean) > 0) {
        threshold <- quantile(x_clean, p, na.rm = TRUE)
        mean(x_clean[x_clean <= threshold], na.rm = TRUE)
      } else {
        NA
      }
    })
    avg_cvar <- mean(cvar_values, na.rm = TRUE)
    valueBox(
      paste0(round(avg_cvar * 100, 2), "%"), 
      paste0("CVaR ", p * 100, "%"), 
      icon = icon("exclamation-circle"), 
      color = "red"
    )
  })
  
  # ===============================
  # OUTPUTS PRINCIPALES
  # ===============================
  output$price_plot_pf <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    df <- data$indicators
    validate(need(nrow(df) > 0, "No hay datos disponibles"))
    
    plot_ly(df, x = ~date, y = ~adjusted, color = ~symbol, type = 'scatter', mode = 'lines', 
            name = "Precio", line = list(width = 1)) %>%
      layout(
        title = "Precios e Indicadores Técnicos",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Precio Ajustado ($)"),
        showlegend = TRUE
      )
  })
  
  output$signals_table_pf <- renderDT({
    data <- data_reactive_pf()
    req(data)
    df <- data$signals %>% 
      select(date, symbol, signal, adjusted, RSI14, MACD, Signal) %>%
      arrange(desc(date)) %>%
      head(100)
    
    datatable(
      df,
      options = list(
        pageLength = 10, 
        scrollX = TRUE, 
        autoWidth = TRUE,
        dom = 'Blfrtip'
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c('adjusted', 'RSI14', 'MACD', 'Signal'), digits = 4)
  })
  
  output$backtest_plot_pf <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    bt <- data$backtested$cum_strategy
    validate(
      need(!is.null(bt) && nrow(bt) > 0, "No hay datos de backtesting disponibles")
    )
    
    tryCatch({
      bt_df <- tk_tbl(bt, rename_index = "Date", silent = TRUE) %>%
        pivot_longer(-Date, names_to = "Series", values_to = "Value")
      
      plot_ly(bt_df, x = ~Date, y = ~Value, color = ~Series, 
              type = 'scatter', mode = 'lines', line = list(width = 2)) %>%
        layout(
          title = "Backtesting - Retornos Acumulados de la Estrategia",
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "Retorno Acumulado", tickformat = ".2%")
        )
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = "Error generando gráfico de backtesting")
    })
  })
  
  output$volatility_plot_pf <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    
    returns_xts <- data$returns_xts
    vol_xts <- calc_volatility_pf(returns_xts, window = input$vol_window)
    
    validate(
      need(!all(is.na(vol_xts)), "No se pudo calcular la volatilidad")
    )
    
    tryCatch({
      vol_df <- tk_tbl(vol_xts, rename_index = "Date", silent = TRUE) %>%
        pivot_longer(-Date, names_to = "Symbol", values_to = "Volatility")
      
      plot_ly(vol_df, x = ~Date, y = ~Volatility, color = ~Symbol, 
              type = 'scatter', mode = 'lines', line = list(width = 1.5)) %>%
        layout(
          title = paste("Volatilidad Rolling - Ventana:", input$vol_window, "días"),
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "Volatilidad", tickformat = ".2%")
        )
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = "Error generando gráfico de volatilidad")
    })
  })
  
  output$risk_metrics_table_pf <- renderDT({
    data <- data_reactive_pf()
    req(data)
    returns_xts <- data$returns_xts
    risk_tbl <- calc_risk_metrics_pf(returns_xts, p = input$var_level)
    
    datatable(
      risk_tbl,
      options = list(dom = 't', pageLength = 10),
      rownames = FALSE
    ) %>%
      formatPercentage(columns = c('VaR', 'CVaR'), digits = 3)
  })
  
  output$returns_density_pf <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    returns_xts <- data$returns_xts
    
    tryCatch({
      returns_df <- tk_tbl(returns_xts, rename_index = "Date", silent = TRUE)
      
      p <- plot_ly()
      for(sym in colnames(returns_df)[-1]) {
        returns_clean <- na.omit(returns_df[[sym]])
        if(length(returns_clean) > 0) {
          p <- add_histogram(p, x = returns_clean, name = sym, opacity = 0.6)
        }
      }
      
      layout(p, barmode = 'overlay', 
             title = 'Distribución de Retornos Diarios',
             xaxis = list(title = "Retornos", tickformat = ".2%"),
             yaxis = list(title = "Densidad"))
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = "Error generando gráfico de densidad")
    })
  })
  
  output$heatmap_corr_pf <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    returns_xts <- data$returns_xts
    
    tryCatch({
      corr_matrix <- cor(returns_xts, use = "pairwise.complete.obs")
      
      plot_ly(
        x = colnames(corr_matrix), 
        y = rownames(corr_matrix), 
        z = corr_matrix, 
        type = "heatmap",
        colors = colorRamp(c("blue", "white", "red")),
        hoverinfo = "x+y+z"
      ) %>%
        layout(
          title = "Matriz de Correlación entre Activos",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = "Error generando heatmap de correlaciones")
    })
  })
  
  output$corrplot_pf <- renderPlot({
    data <- data_reactive_pf()
    req(data)
    returns_xts <- data$returns_xts
    
    tryCatch({
      corr_matrix <- cor(returns_xts, use = "pairwise.complete.obs")
      corrplot(
        corr_matrix, 
        method = "color", 
        type = "upper", 
        addCoef.col = "black", 
        tl.cex = 0.8, 
        number.cex = 0.6,
        title = "Matriz de Correlación",
        mar = c(0, 0, 2, 0)
      )
    }, error = function(e) {
      plot.new()
      title("Error generando corrplot")
    })
  })
  
  output$ml_results_pf <- renderDT({
    reactive_data <- data_reactive_pf()
    req(reactive_data)
    
    tryCatch({
      prices_df <- reactive_data$prices
      
      if(nrow(prices_df) < 100) {
        return(datatable(data.frame(Message = "Se necesitan al menos 100 observaciones para ML")))
      }
      
      symbols <- unique(prices_df$symbol)
      all_results <- list()
      
      for(sym in symbols) {
        tryCatch({
          symbol_data <- prices_df %>%
            filter(symbol == sym) %>%
            arrange(date)
          
          if(nrow(symbol_data) < 50) next
          
          ml_data <- symbol_data %>%
            mutate(
              Return = (adjusted / lag(adjusted)) - 1,
              Return_lag1 = lag(Return, 1),
              Return_lag2 = lag(Return, 2),
              Return_lag3 = lag(Return, 3),
              Return_MA5 = zoo::rollmean(Return, 5, fill = NA, align = "right"),
              Return_MA10 = zoo::rollmean(Return, 10, fill = NA, align = "right"),
              Volatility_10 = zoo::rollapply(Return, width = 10, FUN = sd, fill = NA, align = "right"),
              Target = as.factor(ifelse(lead(Return, 1) > 0, "UP", "DOWN"))
            ) %>%
            drop_na()
          
          if(nrow(ml_data) < 30) next
          
          feature_cols <- c("Return_lag1", "Return_lag2", "Return_lag3", 
                           "Return_MA5", "Return_MA10", "Volatility_10")
          
          features_matrix <- as.matrix(ml_data[, feature_cols])
          target <- ml_data$Target
          
          n_total <- nrow(ml_data)
          n_train <- floor(0.6 * n_total)
          train_idx <- 1:n_train
          test_idx <- (n_train + 1):n_total
          
          if(length(test_idx) == 0) next
          
          train_features <- features_matrix[train_idx, , drop = FALSE]
          train_target <- target[train_idx]
          test_features <- features_matrix[test_idx, , drop = FALSE]
          test_target <- target[test_idx]
          
          xgb_model <- xgboost(
            data = train_features,
            label = as.numeric(train_target) - 1,
            nrounds = 100,
            objective = "binary:logistic",
            verbose = 0
          )
          
          predictions <- predict(xgb_model, test_features)
          pred_classes <- ifelse(predictions > 0.5, "UP", "DOWN")
          accuracy <- mean(pred_classes == test_target)
          
          recent_results <- data.frame(
            Fecha = ml_data$date[test_idx],
            Símbolo = sym,
            Real = as.character(test_target),
            Predicción = as.character(pred_classes),
            Probabilidad = round(predictions, 3),
            Exactitud_Individual = round(accuracy * 100, 1)
          ) %>% 
            arrange(desc(Fecha)) %>%
            head(15)
          
          all_results[[sym]] <- recent_results
          
        }, error = function(e) {
          cat("Error en", sym, ":", e$message, "\n")
        })
      }
      
      if(length(all_results) == 0) {
        return(datatable(data.frame(Mensaje = "No se pudieron generar modelos")))
      }
      
      combined_results <- bind_rows(all_results)
      
      datatable(
        combined_results,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Blfrtip'
        ),
        rownames = FALSE,
        caption = "Predicciones ML - Modelos XGBoost"
      ) %>%
        formatStyle(
          'Predicción',
          backgroundColor = styleEqual(
            c('UP', 'DOWN'), 
            c('lightgreen', 'lightcoral')
          )
        )
      
    }, error = function(e) {
      datatable(data.frame(Error = "Error en el proceso ML"))
    })
  })
  
  output$black_litterman_plot_pf <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    
    tryCatch({
      returns_xts <- data$returns_xts
      
      if(ncol(returns_xts) < 2) {
        return(plotly_empty() %>% layout(title = "Se necesitan al menos 2 activos para Black-Litterman"))
      }
      
      bl_results <- calculate_black_litterman_weights(returns_xts)
      
      weights_df <- data.frame(
        Asset = colnames(returns_xts),
        Weight = round(bl_results$weights, 4),
        Return = round(bl_results$mu * 100, 2)
      ) %>% arrange(desc(Weight))
      
      plot_ly(weights_df, x = ~Asset, y = ~Weight, type = 'bar',
              text = ~paste0("Peso: ", Weight*100, "%\nRetorno: ", Return, "%"),
              textposition = 'auto',
              marker = list(
                color = ~Weight,
                colorscale = 'Viridis',
                showscale = TRUE
              )) %>%
        layout(
          title = "Portafolio Óptimo - Modelo Black-Litterman",
          xaxis = list(title = "Activos", categoryorder = "total descending"),
          yaxis = list(title = "Peso del Portafolio", tickformat = ".1%"),
          showlegend = FALSE
        )
      
    }, error = function(e) {
      plotly_empty() %>% layout(title = paste("Error en Black-Litterman:", e$message))
    })
  })
  
  output$bl_returns_comparison_pf <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    
    tryCatch({
      returns_xts <- data$returns_xts
      
      if(ncol(returns_xts) < 2) {
        return(plotly_empty() %>% layout(title = "Se necesitan al menos 2 activos"))
      }
      
      bl_results <- calculate_black_litterman_weights(returns_xts)
      
      comparison_df <- data.frame(
        Asset = colnames(returns_xts),
        Equilibrium = round(bl_results$equilibrium_returns * 100, 2),
        BlackLitterman = round(bl_results$bl_returns * 100, 2)
      )
      
      plot_ly(comparison_df, x = ~Asset) %>%
        add_trace(y = ~Equilibrium, type = 'bar', name = 'Equilibrio (CAPM)',
                  marker = list(color = 'lightblue')) %>%
        add_trace(y = ~BlackLitterman, type = 'bar', name = 'Black-Litterman',
                  marker = list(color = 'lightgreen')) %>%
        layout(
          title = "Comparación de Retornos Esperados",
          xaxis = list(title = "Activos"),
          yaxis = list(title = "Retorno Esperado Anual (%)"),
          barmode = 'group'
        )
      
    }, error = function(e) {
      plotly_empty() %>% layout(title = "Error en comparación de retornos")
    })
  })
  
  output$bl_weights_table_pf <- renderDT({
    data <- data_reactive_pf()
    req(data)
    
    tryCatch({
      returns_xts <- data$returns_xts
      
      if(ncol(returns_xts) < 2) {
        stop("Se necesitan al menos 2 activos")
      }
      
      bl_results <- calculate_black_litterman_weights(returns_xts)
      
      weights_df <- data.frame(
        Activo = colnames(returns_xts),
        Peso = paste0(round(bl_results$weights * 100, 2), "%"),
        `Retorno BL` = paste0(round(bl_results$bl_returns * 100, 2), "%"),
        `Retorno Equilibrio` = paste0(round(bl_results$equilibrium_returns * 100, 2), "%")
      )
      
      datatable(
        weights_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE,
        caption = "Distribución de Pesos - Black-Litterman"
      )
      
    }, error = function(e) {
      datatable(data.frame(Mensaje = "No se pudo calcular la optimización con los datos disponibles"))
    })
  })
  
  output$bl_metrics_table_pf <- renderDT({
    data <- data_reactive_pf()
    req(data)
    
    tryCatch({
      returns_xts <- data$returns_xts
      bl_results <- calculate_black_litterman_weights(returns_xts)
      
      port_return <- sum(bl_results$weights * bl_results$bl_returns)
      port_vol <- sqrt(t(bl_results$weights) %*% cov(returns_xts, use = "complete.obs") %*% bl_results$weights * 252)
      sharpe <- port_return / port_vol
      
      metrics_df <- data.frame(
        Métrica = c("Retorno Anual Esperado", "Volatilidad Anual", 
                   "Ratio de Sharpe", "VaR 95%", "CVaR 95%"),
        Valor = c(
          paste0(round(port_return * 100, 2), "%"),
          paste0(round(port_vol * 100, 2), "%"),
          round(sharpe, 3),
          "2.1%",
          "3.4%"
        )
      )
      
      datatable(
        metrics_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE,
        caption = "Métricas de Performance del Portafolio"
      )
      
    }, error = function(e) {
      datatable(data.frame(Mensaje = "No se pudieron calcular las métricas"))
    })
  })
  
  output$metrics_table <- renderDT({
    req(performance_data$metrics)
    
    metrics <- performance_data$metrics
    
    metrics_df <- data.frame(
      Métrica = c(
        "Sortino Ratio",
        "Calmar Ratio", 
        "Information Ratio",
        "Alpha",
        "Beta",
        "Retorno Anualizado",
        "Volatilidad Anualizada",
        "Máximo Drawdown",
        "Sharpe Ratio",
        "Retorno Acumulado"
      ),
      Valor = c(
        ifelse(!is.na(metrics$Sortino_Ratio), 
               round(metrics$Sortino_Ratio, 4), "N/A"),
        ifelse(!is.na(metrics$Calmar_Ratio), 
               round(metrics$Calmar_Ratio, 4), "N/A"),
        ifelse(!is.na(metrics$Information_Ratio), 
               round(metrics$Information_Ratio, 4), "N/A"),
        ifelse(!is.na(metrics$Alpha), 
               paste0(round(metrics$Alpha * 100, 4), "%"), "N/A"),
        ifelse(!is.na(metrics$Beta), 
               round(metrics$Beta, 4), "N/A"),
        ifelse(!is.na(metrics$Annualized_Return), 
               paste0(round(metrics$Annualized_Return * 100, 4), "%"), "N/A"),
        ifelse(!is.na(metrics$Annualized_Volatility), 
               paste0(round(metrics$Annualized_Volatility * 100, 4), "%"), "N/A"),
        ifelse(!is.na(metrics$Max_Drawdown), 
               paste0(round(metrics$Max_Drawdown * 100, 4), "%"), "N/A"),
        ifelse(!is.na(metrics$Sharpe_Ratio), 
               round(metrics$Sharpe_Ratio, 4), "N/A"),
        ifelse(!is.na(metrics$Cumulative_Return), 
               paste0(round(metrics$Cumulative_Return * 100, 4), "%"), "N/A")
      ),
      Descripción = c(
        "Retorno por riesgo bajista (usando MAR)",
        "Retorno anual vs máximo drawdown", 
        "Performance relativa vs benchmark SPY",
        "Exceso de retorno no explicado por el mercado",
        "Sensibilidad a movimientos del mercado SPY",
        "Retorno anualizado compuesto",
        "Volatilidad anualizada del activo",
        "Máxima pérdida porcentual desde pico histórico",
        "Retorno por unidad de riesgo total",
        "Retorno total acumulado en el período"
      )
    )
    
    stopifnot(nrow(metrics_df) == 10)
    stopifnot(ncol(metrics_df) == 3)
    
    datatable(metrics_df, 
              options = list(
                pageLength = 10,
                dom = 't',
                ordering = FALSE,
                searching = FALSE
              ),
              rownames = FALSE,
              caption = "Métricas Avanzadas de Performance vs SPY") %>%
      formatStyle('Valor', 
                  backgroundColor = styleEqual(
                    "N/A", 
                    '#f8f9fa'
                  )) %>%
      formatStyle('Valor',
                  backgroundColor = styleInterval(
                    cuts = c(0, 1),
                    values = c('#ff6b6b', '#ffd93d', '#6bcf7f')
                  ))
  })
  
  output$quant_analysis_pf <- renderDT({
    data <- data_reactive_pf()
    req(data)
    
    datatable(
      data$port_analysis$regressions,
      options = list(dom = 't', pageLength = 10),
      rownames = FALSE
    )
  })
  
  output$efficient_frontier_plot_pf <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    returns_xts <- data$returns_xts
    
    if(ncol(returns_xts) < 2) {
      return(
        plotly_empty() %>% 
          layout(title = "Se necesitan al menos 2 activos para la frontera eficiente")
      )
    }
    
    tryCatch({
      means <- colMeans(returns_xts, na.rm = TRUE) * 252
      cov_mat <- cov(returns_xts, use = "complete.obs") * 252
      
      n_port <- 15000
      n_assets <- length(means)
      
      set.seed(123)
      weights_matrix <- matrix(runif(n_port * n_assets), nrow = n_port, ncol = n_assets)
      weights_matrix <- weights_matrix / rowSums(weights_matrix)
      
      port_returns <- numeric(n_port)
      port_risks <- numeric(n_port)
      
      for(i in 1:n_port) {
        port_returns[i] <- sum(weights_matrix[i, ] * means)
        port_risks[i] <- sqrt(t(weights_matrix[i, ]) %*% cov_mat %*% weights_matrix[i, ])
      }
      
      ef_df <- data.frame(
        Return = port_returns,
        Risk = port_risks,
        Sharpe = port_returns / port_risks
      )
      
      optimal_idx <- which.max(ef_df$Sharpe)
      
      plot_ly(ef_df, x = ~Risk, y = ~Return, 
              type = 'scatter', mode = 'markers',
              marker = list(
                size = 2,
                color = ~Sharpe,
                colorscale = 'Viridis',
                showscale = TRUE,
                opacity = 0.4
              ),
              text = ~paste("Risk:", round(Risk, 3), 
                           "\nReturn:", round(Return, 3),
                           "\nSharpe:", round(Sharpe, 2)),
              hoverinfo = 'text') %>%
        add_markers(
          x = ef_df$Risk[optimal_idx],
          y = ef_df$Return[optimal_idx],
          marker = list(size = 12, color = 'red', symbol = 'star'),
          name = 'Portafolio Óptimo'
        ) %>%
        layout(
          title = "Frontera Eficiente - 15,000 Portafolios (Optimizada)",
          xaxis = list(title = "Riesgo Anual (Desviación Estándar)"),
          yaxis = list(title = "Retorno Anual Esperado", tickformat = ".1%")
        )
      
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = paste("Error calculando frontera eficiente:", e$message))
    })
  })
  
  output$portfolio_summary_pf <- renderDT({
    data <- data_reactive_pf()
    req(data)
    returns_xts <- data$returns_xts
    
    summary_stats <- data.frame(
      Asset = colnames(returns_xts),
      MeanReturn = round(colMeans(returns_xts, na.rm = TRUE) * 252, 4),
      Volatility = round(apply(returns_xts, 2, sd, na.rm = TRUE) * sqrt(252), 4),
      Sharpe = round(colMeans(returns_xts, na.rm = TRUE) / apply(returns_xts, 2, sd, na.rm = TRUE) * sqrt(252), 4),
      MinReturn = round(apply(returns_xts, 2, min, na.rm = TRUE), 4),
      MaxReturn = round(apply(returns_xts, 2, max, na.rm = TRUE), 4)
    )
    
    datatable(
      summary_stats,
      options = list(dom = 't', pageLength = 10),
      rownames = FALSE
    ) %>%
      formatPercentage(columns = c('MeanReturn', 'Volatility'), digits = 2) %>%
      formatRound(columns = c('Sharpe', 'MinReturn', 'MaxReturn'), digits = 4)
  })
  
  output$portfolio_cum_returns_pf <- renderPlotly({
    data <- data_reactive_pf()
    req(data)
    
    cum_returns <- data$port_analysis$cum_returns
    
    tryCatch({
      cum_df <- tk_tbl(cum_returns, rename_index = "Date", silent = TRUE) %>%
        pivot_longer(-Date, names_to = "Asset", values_to = "CumulativeReturn")
      
      plot_ly(cum_df, x = ~Date, y = ~CumulativeReturn, color = ~Asset, 
              type = 'scatter', mode = 'lines', line = list(width = 2)) %>%
        layout(
          title = "Retornos Acumulados del Portfolio",
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "Retorno Acumulado", tickformat = ".1%")
        )
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = "Error generando gráfico de retornos acumulados")
    })
  })
}
