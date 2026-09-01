# ==============================================================================
# indicators.R -- Technical indicators, signals, backtesting
# ==============================================================================

# Función para calcular indicadores técnicos
calc_indicators_pf <- function(prices_df) {
  tryCatch({
    prices_df %>%
      group_by(symbol) %>%
      arrange(date) %>%
      mutate(
        SMA20 = TTR::SMA(adjusted, n = 20),
        EMA50 = TTR::EMA(adjusted, n = 50),
        RSI14 = TTR::RSI(adjusted, n = 14),
        MACD = TTR::MACD(adjusted, nFast = 12, nSlow = 26, nSig = 9)[,1],
        Signal = TTR::MACD(adjusted, nFast = 12, nSlow = 26, nSig = 9)[,2]
      ) %>%
      ungroup()
  }, error = function(e) {
    prices_df %>% mutate(SMA20 = NA, EMA50 = NA, RSI14 = NA, MACD = NA, Signal = NA)
  })
}

# Función para generar señales de trading
generate_signals_pf <- function(indicators_df) {
  indicators_df %>%
    mutate(
      signal = case_when(
        !is.na(MACD) & !is.na(Signal) & MACD > Signal & !is.na(RSI14) & RSI14 < 70 ~ "BUY",
        !is.na(MACD) & !is.na(Signal) & MACD < Signal & !is.na(RSI14) & RSI14 > 30 ~ "SELL",
        TRUE ~ "HOLD"
      )
    ) %>%
    filter(!is.na(SMA20) & !is.na(EMA50))
}

# Función para backtesting de estrategia
backtest_strategy_pf <- function(returns_xts, signals_df) {
  tryCatch({
    signals_wide <- signals_df %>%
      select(date, symbol, signal) %>%
      pivot_wider(names_from = symbol, values_from = signal, values_fill = "HOLD") %>%
      mutate(date = as.Date(date))
    
    signals_xts <- xts(signals_wide[-1], order.by = signals_wide$date)
    
    common_dates <- intersect(index(returns_xts), index(signals_xts))
    
    if(length(common_dates) == 0) {
      return(list(
        strategy_returns = returns_xts * 0,
        cum_strategy = returns_xts * 0
      ))
    }
    
    returns_aligned <- returns_xts[common_dates, ]
    signals_aligned <- signals_xts[common_dates, ]
    
    all_symbols <- unique(c(colnames(returns_aligned), colnames(signals_aligned)))
    
    for(sym in all_symbols) {
      if(!sym %in% colnames(returns_aligned)) {
        returns_aligned <- cbind(returns_aligned, xts(rep(0, nrow(returns_aligned)), order.by = index(returns_aligned)))
        colnames(returns_aligned)[ncol(returns_aligned)] <- sym
      }
      if(!sym %in% colnames(signals_aligned)) {
        signals_aligned <- cbind(signals_aligned, xts(rep("HOLD", nrow(signals_aligned)), order.by = index(signals_aligned)))
        colnames(signals_aligned)[ncol(signals_aligned)] <- sym
      }
    }
    
    returns_aligned <- returns_aligned[, all_symbols]
    signals_aligned <- signals_aligned[, all_symbols]
    
    signals_numeric <- apply(signals_aligned, 2, function(x) {
      ifelse(x == "BUY", 1, ifelse(x == "SELL", -1, 0))
    })
    
    signals_numeric_xts <- xts(signals_numeric, order.by = index(signals_aligned))
    
    signals_lagged <- lag(signals_numeric_xts, 1)
    signals_lagged[is.na(signals_lagged)] <- 0
    
    strategy_returns <- returns_aligned * signals_lagged
    
    cum_strategy <- cumsum(strategy_returns)
    
    list(
      strategy_returns = strategy_returns,
      cum_strategy = cum_strategy
    )
    
  }, error = function(e) {
    zeros_xts <- returns_xts * 0
    list(
      strategy_returns = zeros_xts,
      cum_strategy = zeros_xts
    )
  })
}
