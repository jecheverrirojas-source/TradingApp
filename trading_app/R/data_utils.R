# ==============================================================================
# data_utils.R -- Price fetching, returns calculation
# ==============================================================================

# Función para obtener precios del portfolio
get_prices_pf <- function(symbols, from, to) {
  tryCatch({
    to_corrected <- min(as.Date(to), Sys.Date())
    
    if (as.Date(to_corrected) < Sys.Date() - 7) {
      cat("⚠️ Fecha 'to' es antigua. Ajustando para obtener datos recientes...\n")
      from_corrected <- max(as.Date(from), Sys.Date() - 365)
      to_corrected <- Sys.Date()
    } else {
      from_corrected <- as.Date(from)
    }
    
    cat("📥 Descargando datos desde", as.character(from_corrected), "hasta", as.character(to_corrected), "\n")
    
    all_prices <- list()
    
    for(sym in symbols) {
      tryCatch({
        cat("Descargando", sym, "...\n")
        sym_data <- tq_get(sym, from = from_corrected, to = to_corrected, get = "stock.prices")
        
        if(!is.null(sym_data) && nrow(sym_data) > 0) {
          sym_data_clean <- sym_data %>%
            select(symbol, date, adjusted) %>%
            mutate(date = as.Date(date))
          all_prices[[sym]] <- sym_data_clean
          cat("✅", sym, "descargado (", nrow(sym_data_clean), "filas)\n")
        }
      }, error = function(e) {
        cat("❌ Error descargando", sym, ":", e$message, "\n")
        tryCatch({
          cat("Intentando descargar", sym, "con quantmod...\n")
          sym_data <- getSymbols(sym, from = from_corrected, to = to_corrected, auto.assign = FALSE)
          sym_data_clean <- data.frame(
            symbol = sym,
            date = index(sym_data),
            adjusted = as.numeric(Ad(sym_data))
          )
          all_prices[[sym]] <- sym_data_clean
          cat("✅", sym, "descargado con quantmod (", nrow(sym_data_clean), "filas)\n")
        }, error = function(e2) {
          cat("❌ Error descargando", sym, "con quantmod:", e2$message, "\n")
        })
      })
    }
    
    if(length(all_prices) == 0) {
      stop("No se pudieron descargar datos para ningún símbolo")
    }
    
    prices <- bind_rows(all_prices) %>%
      filter(date <= Sys.Date())
    
    cat("✅ Datos descargados exitosamente\n")
    cat("📅 Rango de fechas en datos descargados:", 
        as.character(min(prices$date)), "a", 
        as.character(max(prices$date)), "\n")
    cat("📊 Símbolos obtenidos:", toString(unique(prices$symbol)), "\n")
    cat("📈 Total de observaciones:", nrow(prices), "\n")
    
    return(prices)
    
  }, error = function(e) {
    cat("❌ Error crítico descargando precios:", e$message, "\n")
    stop(paste("Error obteniendo precios:", e$message))
  })
}

# Función para calcular retornos
calc_returns_pf <- function(prices_df) {
  returns_df <- prices_df %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", col_rename = "returns")
  
  returns_wide <- returns_df %>%
    pivot_wider(names_from = symbol, values_from = returns) %>%
    mutate(date = as.Date(date))
  
  returns_xts <- xts(returns_wide[-1], order.by = returns_wide$date)
  return(returns_xts)
}
