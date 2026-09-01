# ===============================
# INTERFAZ DE USUARIO (UI)
# ===============================
ui <- dashboardPage(
  title = "Trading PRO",
  skin = "blue",
  dashboardHeader(title = tagList(icon("chart-line"), "Trading PRO")),
  dashboardSidebar(
    tags$div(class = "sidebar-section-title", icon("sliders-h"), "Configuración del Portfolio"),
    textInput("symbols_pf", "Activos (coma separados):", value = "AAPL,MSFT,GOOGL"),
    textInput("weights_pf", "Pesos (coma separados, ej: 0.6,0.3,0.1):", value = ""),
    dateInput("from_pf", "Fecha inicio:", value = Sys.Date()-365),
    dateInput("to_pf", "Fecha fin:", value = Sys.Date()),
    tags$hr(class = "sidebar-divider"),
    tags$div(class = "sidebar-section-title", icon("exclamation-triangle"), "Parámetros de Riesgo"),
    sliderInput("vol_window", "Ventana volatilidad:", min = 5, max = 100, value = 30),
    sliderInput("var_level", "Nivel VaR/CVaR:", min = 0.01, max = 0.1, value = 0.05, step = 0.01),
    tags$hr(class = "sidebar-divider"),
    actionButton("run_pf", "Ejecutar análisis", icon = icon("play"), class = "btn-primary")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      valueBoxOutput("kpi_return_pf"),
      valueBoxOutput("kpi_vol_pf"),
      valueBoxOutput("kpi_var_pf"),
      valueBoxOutput("kpi_cvar_pf")
    ),
    fluidRow(
      tabBox(width = 12,
             tabPanel(tagList(icon("chart-line"), "Precios e Indicadores"), value = "Precios e Indicadores",
                      plotlyOutput("price_plot_pf")),
             tabPanel(tagList(icon("signal"), "Señales y Backtesting"), value = "Señales y Backtesting",
                      DTOutput("signals_table_pf"), plotlyOutput("backtest_plot_pf")),
             tabPanel(tagList(icon("exclamation-triangle"), "Riesgo y Volatilidad"), value = "Riesgo y Volatilidad",
                      plotlyOutput("volatility_plot_pf"), DTOutput("risk_metrics_table_pf"), plotlyOutput("returns_density_pf")),
             tabPanel(tagList(icon("table"), "Correlaciones"), value = "Correlaciones",
                      plotlyOutput("heatmap_corr_pf"), plotOutput("corrplot_pf")),
             tabPanel(tagList(icon("chart-area"), "Frontera Eficiente"), value = "Frontera Eficiente",
                      plotlyOutput("efficient_frontier_plot_pf")),
             tabPanel(tagList(icon("brain"), "Machine Learning"), value = "Machine Learning",
                      DTOutput("ml_results_pf")),
             tabPanel(tagList(icon("robot"), "ML Avanzado"), value = "ML Avanzado",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          selectInput("ml_asset", "Seleccionar Activo:",
                                      choices = c("AAPL", "MSFT", "GOOGL", "TSLA", "SPY"),
                                      selected = "AAPL"),
                          dateRangeInput("ml_dates", "Rango de Fechas:",
                                        start = Sys.Date() - 730,
                                        end = Sys.Date()),
                          numericInput("n_folds", "Número de Folds:",
                                      value = 5, min = 2, max = 10, step = 1),
                          numericInput("test_size", "Tamaño Test (%):",
                                      value = 20, min = 10, max = 40, step = 5),
                          actionButton("run_advanced_ml", "Ejecutar ML Avanzado",
                                      icon = icon("play"), class = "btn-primary"),
                          br(), br(),
                          downloadButton("download_ml_results", "Descargar Resultados")
                        ),
                        mainPanel(
                          width = 9,
                          h3("Machine Learning Avanzado - Walk-Forward Validation"),

                          fluidRow(
                            column(6,
                                   h4("Performance por Fold"),
                                   DTOutput("fold_performance_table")
                            ),
                            column(6,
                                   h4("Métricas Resumen"),
                                   DTOutput("ml_summary_table")
                            )
                          ),

                          br(),

                          fluidRow(
                            column(6,
                                   h4("Importancia de Variables"),
                                   plotlyOutput("feature_importance_plot")
                            ),
                            column(6,
                                   h4("Evolución de Accuracy"),
                                   plotlyOutput("accuracy_evolution_plot")
                            )
                          ),

                          br(),

                          h4("Predicciones Detalladas"),
                          DTOutput("detailed_predictions_table")
                        )
                      )
             ),
             tabPanel(tagList(icon("sliders-h"), "Optimización Avanzada"), value = "Optimización Avanzada",
                      fluidRow(
                        column(6, plotlyOutput("black_litterman_plot_pf")),
                        column(6, plotlyOutput("bl_returns_comparison_pf"))
                      ),
                      fluidRow(
                        column(6, DTOutput("bl_weights_table_pf")),
                        column(6, DTOutput("bl_metrics_table_pf"))
                      )
             ),
             tabPanel(tagList(icon("chart-pie"), "Performance Report"), value = "Performance Report",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          uiOutput("asset_select_ui"),
                          dateRangeInput("date_range_perf", "Rango de Fechas:",
                                        start = Sys.Date() - 365,
                                        end = Sys.Date()),
                          numericInput("risk_free", "Tasa Libre de Riesgo (%):",
                                      value = 2.5, min = 0, max = 10, step = 0.1),
                          actionButton("calculate_metrics", "Calcular Métricas",
                                      icon = icon("play"), class = "btn-primary")
                        ),
                        mainPanel(
                          width = 9,
                          h3("Métricas de Performance Avanzadas"),
                          DTOutput("metrics_table"),
                          br(),
                          h4("Análisis Comparativo"),
                          plotlyOutput("performance_plot")
                        )
                      )
             ),
             tabPanel(tagList(icon("calculator"), "Statistical Analysis"), value = "Statistical Analysis",
                      fluidRow(
                        column(6,
                               h4("Distribution Analysis"),
                               plotlyOutput("returns_distribution_plot"),
                               DTOutput("normality_tests_table")
                        ),
                        column(6,
                               h4("Time Series Properties"),
                               plotlyOutput("autocorrelation_plot"),
                               DTOutput("stationarity_tests_table")
                        )
                      ),
                      fluidRow(
                        column(12,
                               h4("Advanced Risk Metrics Dashboard"),
                               plotlyOutput("risk_dashboard"),
                               DTOutput("advanced_risk_metrics_table")
                        )
                      )),
             tabPanel(tagList(icon("dice"), "Monte Carlo Simulation"), value = "Monte Carlo Simulation",
                      fluidRow(
                        column(4,
                               wellPanel(
                                 h4("Simulation Parameters"),
                                 numericInput("n_simulations", "Number of Simulations:",
                                             value = 10000, min = 1000, max = 100000, step = 1000),
                                 numericInput("mc_horizon", "Time Horizon (days):",
                                             value = 252, min = 30, max = 2520),
                                 numericInput("initial_portfolio", "Initial Portfolio Value ($):",
                                             value = 100000, min = 1000, max = 1000000),
                                 actionButton("run_mc", "Run Monte Carlo Simulation",
                                             icon = icon("play"), class = "btn-primary")
                               ),
                               DTOutput("mc_summary_table")
                        ),
                        column(8,
                               h4("Portfolio Value Distribution"),
                               plotlyOutput("mc_distribution_plot"),
                               h4("Sample Simulation Paths"),
                               plotlyOutput("mc_paths_plot")
                        )
                      ),
                      fluidRow(
                        column(6,
                               h4("Value at Risk Analysis"),
                               plotlyOutput("var_analysis_plot")
                        ),
                        column(6,
                               h4("Probability of Loss"),
                               plotlyOutput("loss_probability_plot")
                        )
                      )),
             tabPanel(tagList(icon("link"), "Pairs Trading"), value = "Pairs Trading",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          selectInput("pair_asset1", "Activo 1:",
                                      choices = c("AAPL", "MSFT", "GOOGL", "TSLA", "SPY"),
                                      selected = "AAPL"),
                          selectInput("pair_asset2", "Activo 2:",
                                      choices = c("AAPL", "MSFT", "GOOGL", "TSLA", "SPY"),
                                      selected = "MSFT"),
                          dateRangeInput("pair_dates", "Rango de Fechas:",
                                        start = Sys.Date() - 365,
                                        end = Sys.Date()),
                          numericInput("z_threshold", "Umbral Z-Score:",
                                      value = 2.0, min = 1.0, max = 3.0, step = 0.1),
                          actionButton("run_pairs", "Ejecutar Pairs Trading",
                                      icon = icon("play"), class = "btn-primary"),
                          br(), br(),
                          h5("Instrucciones:"),
                          p("• Selecciona dos activos correlacionados"),
                          p("• Se calculará la hedge ratio dinámica"),
                          p("• Señales: LONG cuando Z-Score < -umbral, SHORT cuando Z-Score > umbral")
                        ),
                        mainPanel(
                          width = 9,
                          h3("Pairs Trading - Estrategia Reformulada"),

                          fluidRow(
                            column(6,
                                   h4("Hedge Ratio Dinámica"),
                                   plotlyOutput("hedge_ratio_plot")
                            ),
                            column(6,
                                   h4("Z-Score y Señales"),
                                   plotlyOutput("zscore_plot")
                            )
                          ),

                          br(),

                          fluidRow(
                            column(12,
                                   h4("Equity Curve - Backtest"),
                                   plotlyOutput("pairs_equity_plot")
                            )
                          ),

                          fluidRow(
                            column(6,
                                   h4("Métricas de Performance"),
                                   DTOutput("pairs_metrics_table")
                            ),
                            column(6,
                                   h4("Señales de Trading"),
                                   DTOutput("pairs_signals_table")
                            )
                          )
                        )
                      )
             ),
             tabPanel(tagList(icon("chart-bar"), "Análisis Avanzado"), value = "Análisis Avanzado",
                      DTOutput("quant_analysis_pf")),
             tabPanel(tagList(icon("briefcase"), "Portfolio Summary"), value = "Portfolio Summary",
                      DTOutput("portfolio_summary_pf"), plotlyOutput("portfolio_cum_returns_pf"))
      )
    )
  )
)
