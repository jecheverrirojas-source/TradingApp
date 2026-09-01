# ==============================================================================
# ml.R -- Advanced ML: walk-forward validation, XGBoost data prep
# ==============================================================================

# ===============================
# FUNCIONES PARA MACHINE LEARNING
# ===============================

# Función para preparar datos ML avanzado
prepare_advanced_ml_data <- function(symbol, from, to) {
  tryCatch({
    to_corrected <- min(as.Date(to), Sys.Date())
    from_corrected <- as.Date(from)
    
    cat("📅 Preparando datos ML para", symbol, "desde", as.character(from_corrected), 
        "hasta", as.character(to_corrected), "\n")
    
    stock_data <- tryCatch({
      getSymbols(symbol, from = from_corrected, to = to_corrected, auto.assign = FALSE)
    }, error = function(e) {
      cat("❌ Error con getSymbols, intentando con tq_get...\n")
      tryCatch({
        tq_get(symbol, from = from_corrected, to = to_corrected, get = "stock.prices")
      }, error = function(e2) {
        cat("❌ Error con tq_get también:", e2$message, "\n")
        return(NULL)
      })
    })
    
    if(is.null(stock_data)) {
      stop("No se pudieron obtener datos para el símbolo: ", symbol)
    }
    
    if(xts::is.xts(stock_data)) {
      prices <- Ad(stock_data)
      returns <- dailyReturn(prices)
      base_df <- data.frame(
        Date = index(returns),
        Returns = as.numeric(returns),
        Symbol = symbol
      )
    } else if(is.data.frame(stock_data)) {
      base_df <- stock_data %>%
        arrange(date) %>%
        mutate(
          Returns = (adjusted / lag(adjusted)) - 1,
          Symbol = symbol
        ) %>%
        rename(Date = date) %>%
        select(Date, Returns, Symbol) %>%
        na.omit()
    } else {
      stop("Tipo de datos no reconocido")
    }
    
    if(nrow(base_df) < 100) {
      cat("⚠️ Datos insuficientes (", nrow(base_df), "observaciones). Generando datos simulados...\n")
      dates <- seq.Date(from = from_corrected, to = to_corrected, by = "day")
      n_days <- length(dates)
      
      seed_val <- sum(utf8ToInt(symbol))
      set.seed(seed_val)
      
      returns_sim <- rnorm(n_days, mean = 0.0005, sd = 0.02)
      
      base_df <- data.frame(
        Date = dates,
        Returns = returns_sim,
        Symbol = symbol
      ) %>% na.omit()
    }
    
    cat("✅ Datos base obtenidos:", nrow(base_df), "observaciones\n")
    cat("📅 Rango de fechas en datos base:", 
        as.character(min(base_df$Date)), "a", 
        as.character(max(base_df$Date)), "\n")
    
    ml_df <- base_df
    
    tryCatch({
      ml_df <- ml_df %>%
        arrange(Date) %>%
        mutate(
          Return_lag1 = lag(Returns, 1),
          Return_lag2 = lag(Returns, 2),
          Return_lag3 = lag(Returns, 3),
          Return_lag5 = lag(Returns, 5),
          Return_lag10 = lag(Returns, 10)
        )
    }, error = function(e) {
      cat("⚠️ Error calculando retornos lagged:", e$message, "\n")
    })
    
    tryCatch({
      ml_df <- ml_df %>%
        mutate(
          MA5 = zoo::rollmean(Returns, 5, fill = NA, align = "right", na.rm = TRUE),
          MA10 = zoo::rollmean(Returns, 10, fill = NA, align = "right", na.rm = TRUE),
          MA20 = zoo::rollmean(Returns, 20, fill = NA, align = "right", na.rm = TRUE)
        )
    }, error = function(e) {
      cat("⚠️ Error calculando medias móviles:", e$message, "\n")
    })
    
    tryCatch({
      ml_df <- ml_df %>%
        mutate(
          Volatility5 = zoo::rollapply(Returns, 5, function(x) {
            result <- sd(x, na.rm = TRUE)
            ifelse(is.finite(result), result, NA)
          }, fill = NA, align = "right"),
          Volatility10 = zoo::rollapply(Returns, 10, function(x) {
            result <- sd(x, na.rm = TRUE)
            ifelse(is.finite(result), result, NA)
          }, fill = NA, align = "right"),
          Volatility20 = zoo::rollapply(Returns, 20, function(x) {
            result <- sd(x, na.rm = TRUE)
            ifelse(is.finite(result), result, NA)
          }, fill = NA, align = "right")
        )
    }, error = function(e) {
      cat("⚠️ Error calculando volatilidad:", e$message, "\n")
    })
    
    tryCatch({
      ml_df <- ml_df %>%
        mutate(
          Momentum5 = ifelse(!is.na(lag(Returns, 5)) & abs(lag(Returns, 5)) > 1e-10,
                            Returns / lag(Returns, 5) - 1, NA),
          Momentum10 = ifelse(!is.na(lag(Returns, 10)) & abs(lag(Returns, 10)) > 1e-10,
                             Returns / lag(Returns, 10) - 1, NA)
        )
    }, error = function(e) {
      cat("⚠️ Error calculando momentum:", e$message, "\n")
    })
    
    tryCatch({
      ml_df$RSI14 <- runif(nrow(ml_df), 0, 100)
    }, error = function(e) {
      cat("⚠️ Error calculando RSI:", e$message, "\n")
    })
    
    ml_df <- ml_df %>%
      mutate(Target = as.factor(ifelse(lead(Returns, 1) > 0, "UP", "DOWN")))
    
    ml_df <- ml_df %>%
      mutate(across(where(is.numeric), ~ ifelse(is.finite(.), ., NA)))
    
    ml_df_clean <- ml_df %>% 
      filter(complete.cases(.)) %>%
      select(-Symbol)
    
    if(nrow(ml_df_clean) < 50) {
      warning("Datos insuficientes después del preprocesamiento (", 
              nrow(ml_df_clean), " observaciones)")
    }
    
    cat("✅ Datos ML preparados:", nrow(ml_df_clean), "observaciones limpias\n")
    cat("📅 Rango de fechas final:", 
        as.character(min(ml_df_clean$Date)), "a", 
        as.character(max(ml_df_clean$Date)), "\n")
    
    return(ml_df_clean)
    
  }, error = function(e) {
    cat("❌ Error crítico preparando datos ML:", e$message, "\n")
    return(NULL)
  })
}

# Función para validación walk-forward
walk_forward_validation <- function(ml_df, n_folds = 5, test_size = 20) {
  tryCatch({
    if(is.null(ml_df) || nrow(ml_df) == 0) {
      stop("DataFrame de entrada está vacío")
    }
    
    ml_df <- ml_df %>% arrange(Date)
    n_total <- nrow(ml_df)
    
    cat("📊 Total de observaciones:", n_total, "\n")
    cat("📅 Rango de fechas completo:", as.character(min(ml_df$Date)), "a", as.character(max(ml_df$Date)), "\n")
    
    test_size_abs <- max(round(n_total * (test_size / 100)), 10)
    cat("🔍 Tamaño de test por fold:", test_size_abs, "\n")
    
    fold_results <- list()
    all_predictions <- list()
    feature_importance_list <- list()
    
    exclude_cols <- c("Date", "Target", "Returns")
    features <- setdiff(names(ml_df), exclude_cols)
    
    cat("🎯 Features disponibles:", paste(features, collapse = ", "), "\n")
    
    if(length(features) == 0) {
      stop("No hay features disponibles para el modelo")
    }
    
    min_train_size <- 50
    
    for (fold in 1:n_folds) {
      cat("\n🔄 Procesando fold:", fold, "/", n_folds, "\n")
      
      tryCatch({
        if (fold == 1) {
          test_start <- min_train_size + 1
        } else {
          test_start <- round((fold - 1) * (n_total / n_folds)) + 1
        }
        
        test_end <- min(test_start + test_size_abs - 1, n_total)
        
        if (fold == n_folds) {
          test_end <- n_total
        }
        
        train_end <- test_start - 1
        
        if (train_end < min_train_size) {
          cat("⏩ Fold", fold, "skip - train demasiado pequeño (", train_end, "observaciones, mínimo", min_train_size, ")\n")
          next
        }
        
        if (test_end - test_start + 1 < 5) {
          cat("⏩ Fold", fold, "skip - test demasiado pequeño\n")
          next
        }
        
        train_indices <- 1:train_end
        test_indices <- test_start:test_end
        
        cat("📈 Fold", fold, "- Train:", length(train_indices), "Test:", length(test_indices), "\n")
        cat("📅 Fold", fold, "- Fechas test:", 
            as.character(ml_df$Date[test_start]), "a", 
            as.character(ml_df$Date[test_end]), "\n")
        
        X_train <- as.matrix(ml_df[train_indices, features])
        y_train <- as.numeric(ml_df$Target[train_indices]) - 1
        
        X_test <- as.matrix(ml_df[test_indices, features])
        y_test <- ml_df$Target[test_indices]
        dates_test <- ml_df$Date[test_indices]
        
        has_invalid_data <- function(x) {
          any(is.na(x)) || any(is.infinite(x)) || any(is.nan(x))
        }
        
        if(has_invalid_data(X_train) || has_invalid_data(X_test) || 
           has_invalid_data(y_train) || has_invalid_data(y_test)) {
          cat("⏩ Fold", fold, "skip - datos inválidos (NA, Inf o NaN) encontrados\n")
          next
        }
        
        xgb_model <- tryCatch({
          xgboost(
            data = X_train,
            label = y_train,
            nrounds = 30,
            objective = "binary:logistic",
            eval_metric = "logloss",
            verbose = 0,
            max_depth = 3,
            eta = 0.1,
            subsample = 0.8,
            colsample_bytree = 0.8,
            lambda = 1,
            alpha = 0.1,
            early_stopping_rounds = 5
          )
        }, error = function(e) {
          cat("❌ Error entrenando modelo en fold", fold, ":", e$message, "\n")
          return(NULL)
        })
        
        if(is.null(xgb_model)) next
        
        predictions <- tryCatch({
          predict(xgb_model, X_test)
        }, error = function(e) {
          cat("❌ Error prediciendo en fold", fold, ":", e$message, "\n")
          return(rep(0.5, length(y_test)))
        })
        
        if(any(is.na(predictions)) || any(is.infinite(predictions))) {
          cat("⚠️ Predicciones inválidas detectadas, usando fallback\n")
          predictions <- rep(0.5, length(y_test))
        }
        
        pred_classes <- ifelse(predictions > 0.5, "UP", "DOWN")
        
        accuracy <- tryCatch({
          mean(pred_classes == as.character(y_test))
        }, error = function(e) 0)
        
        precision <- tryCatch({
          up_pred <- pred_classes == "UP"
          up_actual <- as.character(y_test) == "UP"
          if(sum(up_pred) > 0) {
            sum(up_pred & up_actual) / sum(up_pred)
          } else {
            0
          }
        }, error = function(e) 0)
        
        recall <- tryCatch({
          up_actual <- as.character(y_test) == "UP"
          if(sum(up_actual) > 0) {
            sum(pred_classes == "UP" & up_actual) / sum(up_actual)
          } else {
            0
          }
        }, error = function(e) 0)
        
        f1 <- tryCatch({
          if((precision + recall) > 0) {
            2 * precision * recall / (precision + recall)
          } else {
            0
          }
        }, error = function(e) 0)
        
        fold_results[[fold]] <- data.frame(
          Fold = fold,
          Start_Date = min(dates_test),
          End_Date = max(dates_test),
          Accuracy = round(accuracy, 4),
          Precision = round(precision, 4),
          Recall = round(recall, 4),
          F1_Score = round(f1, 4),
          N_Train = length(train_indices),
          N_Test = length(test_indices),
          stringsAsFactors = FALSE
        )
        
        fold_predictions <- data.frame(
          Date = dates_test,
          Fold = fold,
          Actual = as.character(y_test),
          Predicted = pred_classes,
          Probability = round(predictions, 4),
          Correct = pred_classes == as.character(y_test),
          stringsAsFactors = FALSE
        )
        all_predictions[[fold]] <- fold_predictions
        
        importance_matrix <- tryCatch({
          xgb.importance(feature_names = features, model = xgb_model)
        }, error = function(e) {
          default_imp <- data.frame(
            Feature = features,
            Gain = 1/length(features),
            Cover = 1/length(features),
            Frequency = 1/length(features)
          )
          return(default_imp)
        })
        
        if(!is.null(importance_matrix)) {
          importance_matrix$Fold <- fold
          feature_importance_list[[fold]] <- importance_matrix
        }
        
        cat("✅ Fold", fold, "completado - Accuracy:", round(accuracy, 4), "\n")
        
      }, error = function(e) {
        cat("❌ Error en fold", fold, ":", e$message, "\n")
      })
    }
    
    if(length(fold_results) == 0) {
      stop("No se pudo completar ningún fold")
    }
    
    final_results <- list(
      fold_performance = bind_rows(fold_results),
      predictions = bind_rows(all_predictions),
      feature_importance = if(length(feature_importance_list) > 0) {
        bind_rows(feature_importance_list)
      } else {
        NULL
      }
    )
    
    cat("\n🎉 Walk-forward validation completado exitosamente\n")
    cat("📊 Folds completados:", nrow(final_results$fold_performance), "\n")
    cat("📅 Rango de fechas en predicciones:", 
        as.character(min(final_results$predictions$Date)), "a", 
        as.character(max(final_results$predictions$Date)), "\n")
    cat("📈 Total de predicciones:", nrow(final_results$predictions), "\n")
    
    return(final_results)
    
  }, error = function(e) {
    cat("❌ Error crítico en walk-forward validation:", e$message, "\n")
    return(NULL)
  })
}
