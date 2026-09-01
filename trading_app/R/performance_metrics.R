# ===============================
# FUNCIONES PARA PERFORMANCE REPORT
# ===============================
get_performance_data_optimized <- function(symbol, from, to) {
  tryCatch({
    stock_data <- getSymbols(symbol, from = from, to = to, auto.assign = FALSE)
    
    prices <- Ad(stock_data)
    returns <- dailyReturn(prices)
    
    data.frame(
      Date = index(returns),
      Returns = as.numeric(returns),
      Symbol = symbol
    ) %>% na.omit()
    
  }, error = function(e) {
    stop(paste0("No se pudieron obtener datos de performance para '", symbol, "': ", e$message))
  })
}
