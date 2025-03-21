
# --------------------------------------------------------------------------------
#             0) Libraries
# --------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)
library(tseries)
library(corrplot)
library(dplyr)
library(zoo)
library(TTR)
library(sde)
library(knitr)
library(yuima)
library(rugarch)
library(plotly)

# --------------------------------------------------------------------------------
#             1) Functions
# --------------------------------------------------------------------------------
calculate_zscore <- function(price, lookback = 20) {
  n <- length(price)
  zscore <- rep(NA, n)
  
  for (i in (lookback + 1):n) {
    window <- price[(i - lookback):(i - 1)]
    if (any(is.na(window))) {
      zscore[i] <- NA
    } else {
      roll_mean <- mean(window, na.rm = TRUE)
      roll_sd <- sd(window, na.rm = TRUE)
      if (roll_sd == 0) {
        zscore[i] <- 0
      } else {
        zscore[i] <- (price[i] - roll_mean) / roll_sd
      }
    }
  }
  return(zscore)
}

ou_trading_strategy <- function(prices, lookback = 20, entry_threshold = 2, exit_threshold = 0.5, 
                                stop_loss = 0.05, take_profit = 0.1) {
  n <- length(prices)
  log_prices <- log(prices)
  zscore <- calculate_zscore(log_prices, lookback)
  position <- rep(0, n)
  entry_price <- rep(NA, n)
  pnl <- rep(0, n)
  cumulative_pnl <- rep(0, n)
  
  for (i in (lookback + 1):n) {
    if (is.na(zscore[i])) {
      if (i > 1) {
        position[i] <- position[i - 1]
        if (!is.na(entry_price[i - 1])) {
          entry_price[i] <- entry_price[i - 1]
        }
      }
      next
    }
    
    if (i == 1 || position[i - 1] == 0) {
      if (zscore[i] > entry_threshold) {
        position[i] <- -1
        entry_price[i] <- prices[i]
      } else if (zscore[i] < -entry_threshold) {
        position[i] <- 1
        entry_price[i] <- prices[i]
      } else {
        position[i] <- 0
      }
    } else if (position[i - 1] == 1) {
      if (zscore[i] > exit_threshold ||
          prices[i] <= entry_price[i - 1] * (1 - stop_loss) ||
          prices[i] >= entry_price[i - 1] * (1 + take_profit)) {
        position[i] <- 0
        pnl[i] <- prices[i] / entry_price[i - 1] - 1
      } else {
        position[i] <- position[i - 1]
        entry_price[i] <- entry_price[i - 1]
      }
    } else if (position[i - 1] == -1) {
      if (zscore[i] < -exit_threshold ||
          prices[i] >= entry_price[i - 1] * (1 + stop_loss) ||
          prices[i] <= entry_price[i - 1] * (1 - take_profit)) {
        position[i] <- 0
        pnl[i] <- 1 - prices[i] / entry_price[i - 1]
      } else {
        position[i] <- position[i - 1]
        entry_price[i] <- entry_price[i - 1]
      }
    }
    
    cumulative_pnl[i] <- if (i > 1 && !is.na(cumulative_pnl[i - 1])) 
      cumulative_pnl[i - 1] + pnl[i] 
    else 
      pnl[i]
  }
  
  return(list(
    position = position, 
    entry_price = entry_price, 
    pnl = pnl, 
    cumulative_pnl = cumulative_pnl, 
    zscore = zscore
  ))
}

analyze_performance <- function(prices, strategy_results) {
  returns <- diff(log(prices))
  strategy_returns <- strategy_results$pnl
  strategy_returns[is.na(strategy_returns)] <- 0
  
  total_return <- sum(strategy_returns, na.rm = TRUE)
  annualized_return <- total_return / (length(prices) / 252)
  sharpe_ratio <- mean(strategy_returns, na.rm = TRUE) / sd(strategy_returns, na.rm = TRUE) * sqrt(252)
  
  peak <- cummax(strategy_results$cumulative_pnl)
  drawdown <- peak - strategy_results$cumulative_pnl
  max_drawdown <- max(drawdown, na.rm = TRUE)
  
  trades <- strategy_returns[strategy_returns != 0]
  win_rate <- sum(trades > 0, na.rm = TRUE) / length(trades)
  profit_factor <- sum(trades[trades > 0], na.rm = TRUE) / abs(sum(trades[trades < 0], na.rm = TRUE))
  
  return(list(
    total_return = total_return,
    annualized_return = annualized_return,
    sharpe_ratio = sharpe_ratio,
    max_drawdown = max_drawdown,
    win_rate = win_rate,
    profit_factor = profit_factor,
    num_trades = length(trades)
  ))
}

plot_strategy_results <- function(prices, strategy_results, symbol) {
  df <- data.frame(
    Date = seq_along(prices),
    Price = as.numeric(prices),
    Position = strategy_results$position,
    ZScore = strategy_results$zscore,
    CumulativePnL = strategy_results$cumulative_pnl
  )
  
  # Color palette
  theme_custom <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 12),
      panel.grid.major = element_line(color = "#e0e0e0"),
      panel.grid.minor = element_line(color = "#f0f0f0"),
      plot.background = element_rect(fill = "#fafafa", color = NA),
      legend.background = element_rect(fill = "#fafafa", color = NA)
    )
  
  p1 <- ggplot(df, aes(x = Date, y = Price)) +
    geom_line(color = "#3366CC", size = 0.8) +
    geom_point(data = subset(df, Position == 1), aes(x = Date, y = Price), color = "#00CC66", size = 2) +
    geom_point(data = subset(df, Position == -1), aes(x = Date, y = Price), color = "#FF6666", size = 2) +
    labs(title = paste("Price Chart with Trading Signals -", symbol),
         x = "Period", y = "Price") +
    theme_custom
  
  p2 <- ggplot(df, aes(x = Date, y = ZScore)) +
    geom_line(color = "#3366CC", size = 0.8) +
    geom_hline(yintercept = 2, color = "#FF6666", linetype = "dashed", size = 0.8) +
    geom_hline(yintercept = -2, color = "#00CC66", linetype = "dashed", size = 0.8) +
    geom_hline(yintercept = 0, color = "gray50", size = 0.5) +
    labs(title = paste("Z-Score Mean Reversion Signal -", symbol),
         x = "Period", y = "Z-Score") +
    theme_custom
  
  p3 <- ggplot(df, aes(x = Date, y = CumulativePnL)) +
    geom_line(color = "#FF9900", size = 1) +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 0.5) +
    labs(title = paste("Cumulative P&L -", symbol),
         x = "Period", y = "Cumulative P&L") +
    theme_custom
  
  list(price_plot = p1, zscore_plot = p2, pnl_plot = p3)
}

backtest_ou_strategy <- function(data, symbols, 
                                 lookback = 20, entry_threshold = 2, exit_threshold = 0.5, 
                                 stop_loss = 0.05, take_profit = 0.1) {
  results <- list()
  performance_summary <- data.frame(
    Symbol = character(),
    Total_Return = numeric(),
    Annualized_Return = numeric(),
    Sharpe_Ratio = numeric(),
    Max_Drawdown = numeric(),
    Win_Rate = numeric(),
    Profit_Factor = numeric(),
    Num_Trades = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(symbols)) {
    symbol <- symbols[i]
    prices <- data[, i]  # Price vector
    
    strategy_results <- ou_trading_strategy(
      prices, lookback, entry_threshold, exit_threshold, stop_loss, take_profit
    )
    performance <- analyze_performance(prices, strategy_results)
    
    performance_summary <- rbind(performance_summary, data.frame(
      Symbol = symbol,
      Total_Return = performance$total_return,
      Annualized_Return = performance$annualized_return,
      Sharpe_Ratio = performance$sharpe_ratio,
      Max_Drawdown = performance$max_drawdown,
      Win_Rate = performance$win_rate,
      Profit_Factor = performance$profit_factor,
      Num_Trades = performance$num_trades
    ))
    
    results[[symbol]] <- list(strategy_results = strategy_results,
                              performance = performance)
  }
  
  return(list(results = results, performance_summary = performance_summary))
}

compare_strategies <- function(symbols, data, strategy_results) {
  comparison_df <- data.frame(
    Symbol = character(),
    OU_Strategy_Return = numeric(),
    Buy_Hold_Return = numeric(),
    OU_Strategy_Sharpe = numeric(),
    Buy_Hold_Sharpe = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (symbol in symbols) {
    symbol_index <- which(symbols == symbol)
    prices <- data[, symbol_index]
    
    ou_return <- strategy_results$results[[symbol]]$performance$total_return
    ou_sharpe <- strategy_results$results[[symbol]]$performance$sharpe_ratio
    
    buy_hold_return <- prices[length(prices)] / prices[1] - 1
    buy_hold_returns <- diff(log(prices))
    buy_hold_sharpe <- mean(buy_hold_returns, na.rm = TRUE) / sd(buy_hold_returns, na.rm = TRUE) * sqrt(252)
    
    comparison_df <- rbind(comparison_df, data.frame(
      Symbol = symbol,
      OU_Strategy_Return = ou_return,
      Buy_Hold_Return = buy_hold_return,
      OU_Strategy_Sharpe = ou_sharpe,
      Buy_Hold_Sharpe = buy_hold_sharpe
    ))
  }
  
  return(comparison_df)
}

# --------------------------------------------------------------------------------
#             2) User interface with Shinydashboard and themes
# --------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Forex Mean Reversion Trading",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Parameters", tabName = "params", icon = icon("sliders")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("file-alt"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box {
          border-radius: 5px;
          box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
        }
        .bg-light-blue {
          background-color: #f8f9fa;
        }
        .nav-tabs-custom .nav-tabs li.active {
          border-top-color: #3c8dbc;
        }
        .main-header .logo {
          font-weight: bold;
          font-size: 20px;
        }
        .content-wrapper {
          background-color: #f8f9fa;
        }
        .small-box {
          border-radius: 5px;
        }
        .intro-text, .conclusion-text {
          font-size: 16px;
          line-height: 1.6;
          text-align: justify;
        }
        .section-title {
          border-bottom: 1px solid #ddd;
          padding-bottom: 10px;
          margin-bottom: 20px;
          color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # Introduction Tab
      tabItem(tabName = "intro",
              fluidRow(
                column(width = 12,
                       box(
                         width = 12,
                         title = "Project Overview",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         div(class = "intro-text",
                             HTML("
                  <h3 class='section-title'>1. Introduction</h3>
                  <p>This project focuses on implementing a mean-reversion trading strategy using the Ornstein-Uhlenbeck (OU) process in the foreign exchange (Forex) market. The strategy is based on the assumption that currency prices tend to revert to a mean over time, and it leverages Z-score calculations to identify trading opportunities. The approach is fully implemented in an interactive Shiny application, allowing users to configure parameters, test the strategy, and analyze results.</p>
                  <br>
                  
                  <h3 class='section-title'>2. Key Features</h3>
                  <ul>
                    <li> Statistical modeling using the Ornstein-Uhlenbeck process.</li>
                    <li> Mean reversion-based trading in Forex.</li>
                    <li> Z-score based entry/exit strategy.</li>
                    <li> Interactive Shiny application for real-time testing.</li>
                    <li> Risk management features, including stop-loss and take-profit mechanisms.</li>
                  </ul>
                  <br>
                  
                  
                  <h3 class='section-title'>3. Theoretical Foundations</h3>
                  <p><strong>The Ornstein-Uhlenbeck (OU) process</strong> is a stochastic differential equation (SDE) widely used to model mean-reverting behaviors. It is defined as: </p>

                  <p style='text-align: center; font-size: 15px;'><p style='text-align: center; font-size: 15px;'><strong>dX<sub>t</sub> = θ(μ - X<sub>t</sub>)dt + σdW<sub>t</sub></strong></p>

                  <p>où :</p>
                  <ul>
                    <li><strong>X<sub>t</sub></strong> : represents the asset price or spread.</li>
                    <li><strong>μ</strong> : the long-term equilibrium level.</li>
                    <li><strong>θ</strong> : is the mean reversion speed, which determines how fast the process reverts to μ </li>
                    <li><strong>σ</strong> : represents the volatility of the fluctuations.</li>
                    <li><strong>W<sub>t</sub></strong> : is a Wiener process representing random noise.</li>
                  </ul>
                  
                  <p>The analytical solution of this model is given by : </p>
                  <p style='text-align: center; font-size: 15px;'><p style='text-align: center; font-size: 15px;'><strong>X<sub>t</sub> = X<sub>0</sub> e<sup>-θt</sup> + μ(1 - e<sup>-θt</sup>) + σ &#8747;<sub>0</sub><sup>t</sup> e<sup>-θ(t-s)</sup> dW<sub>s</sub></strong></p></p>
                  <p>This show that, over time,  X<sub>t</sub> trends to revert to μ at a speed proportional to θ.</p>
                  <br>
                  
                  
                  <p><strong>Z-Score Calculation for Trading Signals</strong> </p>
                  <p style='text-align: center; font-size: 15px;'><p style='text-align: center; font-size: 15px;'><strong>Z<sub>t</sub> = (X<sub>t</sub> - μ) / σ </strong></p>
                  <br>
                  

                  <h3 class='section-title'>4. Implemented Strategy</h3>
                  <p><strong>Trading Strategy</strong></p>
                  <p>The trading strategy follows these rules:</p>
                  <ol>
                    <li>Identify mean-reverting assets: Forex pairs exhibiting stable historical mean-reversion patterns.</li>
                    <li>Compute Z-score based on log-price returns.</li>
                    <li>Entry conditions:</li>
                    <ul>
                      <li>Buy (Long): When Z-score < Entry Threshold <em>(e.g., -2.0)</em>.</li>
                      <li>Buy (Long): When Z-score < Entry Threshold <em>(e.g., -2.0)</em>.</li>
                    </ul>
                    <li>Exit conditions:</li>
                    <ul>
                      <li>Exit position when Z-score returns toward 0.</li>
                      <li>Stop-loss and take-profit thresholds are applied.</li>
                    </ul>
                  </ol>
                  
                  
                  <p><strong>Risk management </strong></p>
                  <ol>
                    <li>Stop-Loss: Positions are closed if losses exceed a certain percentage.</li>
                    <li>Take-Profit: Profits are secured when price moves favorably by a set percentage.</li>
                    <li>Dynamic position sizing: Exposure is adjusted based on volatility.</li>
                  </ol>                
                  <br>


                  <h3 class='section-title'>5. User Guide for the App</h3>
                  <p><strong>Overview </strong></p>

                  <p>The Shiny application provides an interactive interface to explore and test the mean-reversion trading strategy. It allows users to:</p>
                  <ul>
                    <li>Upload Forex data.</li>
                    <li>Configure trading parameters.</li>
                    <li>Entry conditions:</li>
                    <li>Run backtests and analyze performance.</li>
                  </ul>
                  
                  <p><strong>How to Use </strong></p>
                  <ol>
                    <li>Navigate to the Parameters Tab: Adjust lookback period, entry/exit thresholds, stop-loss, and take-profit levels.</li>
                    <li>Run the Backtest: Click the button to execute the strategy over historical data.</li>
                    <li>Entry conditions:</li>
                    <li>Analyze Results:</li>
                    <ul>
                      <li>Performance Metrics: View Sharpe ratio, drawdown, total return.</li>
                      <li>Trade Signals: Visualize buy/sell signals on price charts.</li>
                      <li>Comparison with Buy & Hold: See if the strategy outperforms passive holding.</li>
                    </ul>
                  </ol>
                  <br>
                  
                  
                  <h3 class='section-title'>6. Conclusion</h3>
                  <p>This project demonstrates the application of the Ornstein-Uhlenbeck process to Forex trading, using a Z-score-based mean reversion approach. The Shiny app allows users to dynamically test and refine the strategy, providing an intuitive interface for quantitative trading analysis.</p>
                  ")
                         )
                       )
                )
              ),
              fluidRow(
                valueBoxOutput("total_pairs_box", width = 4),
                valueBoxOutput("period_box", width = 4),
                valueBoxOutput("strategy_box", width = 4)
              )
      ),
      
      # Parameters tab
      tabItem(tabName = "params",
              fluidRow(
                column(width = 12,
                       box(
                         width = 12,
                         title = "Strategy Parameters",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         column(width = 6,
                                sliderInput("lookback", "Lookback period :", 
                                            min = 10, max = 50, value = 20, step = 1),
                                sliderInput("entry_threshold", "Entry zone (Z-score) :", 
                                            min = 1, max = 3, value = 2, step = 0.1)
                         ),
                         column(width = 6,
                                sliderInput("exit_threshold", "Exit zone (Z-score) :", 
                                            min = 0, max = 1, value = 0.5, step = 0.1),
                                sliderInput("stop_loss", "Stop-Loss (%) :", 
                                            min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                                sliderInput("take_profit", "Take-Profit (%) :", 
                                            min = 0.01, max = 0.2, value = 0.1, step = 0.01),
                                dateRangeInput("date_range", "Analysis period :",
                                               start = "2010-01-01", 
                                               end = Sys.Date())
                         ),
                         column(width = 12, align = "center",
                                actionButton("run_btn", "Run backtest", 
                                             class = "btn-lg btn-primary", 
                                             icon = icon("play"))
                         )
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         width = 12,
                         title = "Parameter Selection Guide",
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         HTML("
                  <ul>
                    <li><strong>Lookback Period: </sub></strong> Defines the window size for calculating moving averages and Z-scores. A longer period smooths out noise but may slow down reaction time.</li>
                    <li><strong> Entry Zone (Z-score):</strong> Determines how far the price must deviate from the mean before a trade is triggered. Higher values reduce the frequency of trades but increase confidence in mean reversion.</li>
                    <li><strong>Exit Zone (Z-score):</strong> Sets the threshold at which positions are closed. A lower exit Z-score ensures quicker exits, reducing holding time and risk.</li>
                    <li><strong>Take-Profit (%): </strong> The percentage gain at which a trade is exited to secure profits. Higher values increase potential gains but may miss reversal points.</li>
                    <li><strong>Analysis Period: </strong> Defines the historical data range for backtesting, impacting strategy evaluation and parameter optimization.</li>
                  </ul>
              ")
                       )
                )
              )
      ),
      
      # Result tab
      tabItem(tabName = "results",
              tabBox(
                width = 12,
                title = "Analysis results",
                id = "result_tabs",
                tabPanel("Performance Summary", 
                         fluidRow(
                           column(width = 12,
                                  box(
                                    width = 12,
                                    status = "primary",
                                    DTOutput("perf_table")
                                  )
                           )
                         )
                ),
                tabPanel("Top 5 Pairs", 
                         fluidRow(
                           column(width = 12,
                                  box(
                                    width = 12,
                                    title = "The 5 Best Pairs (by Sharpe Ratio)",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    uiOutput("plots_top5")
                                  )
                           )
                         )
                ),
                tabPanel("Correlation Matrix",
                         fluidRow(
                           column(width = 12,
                                  box(
                                    width = 12,
                                    title = "Correlations Between Currency Pairs",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    plotOutput("correlation_plot", height = "600px")
                                  )
                           )
                         )
                ),
                tabPanel("Strategy Comparison",
                         fluidRow(
                           column(width = 12,
                                  box(
                                    width = 12,
                                    title = "Ornstein-Uhlenbeck vs Buy & Hold",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    DTOutput("comparison_table")
                                  )
                           )
                         )
                ),
                tabPanel("Combined Portfolio",
                         fluidRow(
                           column(width = 8,
                                  box(
                                    width = 12,
                                    title = "Combined Portfolio Performance",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    plotOutput("portfolio_plot", height = "400px")
                                  )
                           ),
                           column(width = 4,
                                  box(
                                    width = 12,
                                    title = "Portfolio Statistics",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    tableOutput("portfolio_stats")
                                  )
                           )
                         )
                )
              )
      ),
      
      # Conclusion tab
      tabItem(tabName = "conclusion",
              fluidRow(
                column(width = 12,
                       box(
                         width = 12,
                         title = "Conclusion and Interpretation of Results",
                         status = "primary",
                         solidHeader = TRUE,
                         div(class = "conclusion-text",
                             HTML("
                  <h3 class='section-title'>Global conclusion</h3>
                  <p>The mean reversion strategy based on the Ornstein-Uhlenbeck process demonstrates its ability to generate profits by exploiting price fluctuations around a mean. It provides effective risk management through stop-loss and take-profit mechanisms, ensuring protection against excessive losses.</p>
                  <p>The strategy’s performance is highly dependent on the selected parameters (lookback period, Z-score thresholds, stop-loss, and take-profit levels), highlighting the importance of careful configuration and adaptation to market conditions.</p>
                  <p>Overall, the approach remains robust and applicable to various currency pairs, offering a structured framework for systematic trading while maintaining a controlled risk exposure.</p>

                  <dr>
                  
                  <h3 class='section-title'>Strengths of the Strategy</h3>
                  <p>The Ornstein-Uhlenbeck strategy offers several advantages:</p>
                  <ul>
                    <li>Ability to generate profits in markets without a clear trend</li>
                    <li>Automatic exit mechanism when the price reverts to the mean</li>
                    <li>Protection against losses through stop-loss and take-profit mechanisms</li>
                    <li>Natural diversification across different currency pairs</li>
                  </ul>
                  <dr>
                  
                  
                  <h3 class='section-title'>Limitations and Risks</h3>
                  <p>Some limitations must be considered:</p>
                  <ul>
                    <li>Limited performance in strongly trending markets</li>
                    <li>Sensitivity to parameters (lookback period, entry/exit thresholds)</li>
                    <li>Risk of false signals in highly volatile markets</li>
                    <li>Need to optimize parameters for each currency pair</li>
                  </ul>
                  <dr>
                  
                  
                  <h3 class='section-title'>Improvement Perspectives</h3>
                  <p>To further enhance this strategy, we could consider:</p>
                  <ul>
                    <li>Incorporating trend filters to avoid trades against the main trend</li>
                    <li>Developing a dynamic capital allocation system between pairs</li>
                    <li>Integrating additional technical indicators to confirm signals</li>
                    <li>Optimizing parameters for each pair individually</li>
                    <li>Implementing a machine learning mechanism for parameter selection</li>
                  </ul>
                  ")
                         )
                       )
                )
              )
      )
    )
  )
)

# --------------------------------------------------------------------------------
#                           3)  Shiny Server
# --------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Info boxes for the Introduction tab
  output$total_pairs_box <- renderValueBox({
    valueBox(
      "21", "Currency Pairs",
      icon = icon("money-bill-wave"),
      color = "aqua"
    )
  })
  
  output$period_box <- renderValueBox({
    valueBox(
      "10+ years", "Analysis Period",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  output$strategy_box <- renderValueBox({
    valueBox(
      "Mean Reversion", "Strategy Type",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  # Download / prepare the data
  symbols <- c(
    "EURUSD=X", "GBPUSD=X", "USDJPY=X", "USDCAD=X", "NZDUSD=X", 
    "EURGBP=X", "EURJPY=X", "GBPJPY=X", "AUDJPY=X", "CADJPY=X", 
    "CHFUSD=X", "EURCHF=X", "GBPCHF=X", "AUDCHF=X", "USDCHF=X", 
    "EURAUD=X", "GBPAUD=X", "USDZAR=X", "USDSEK=X", "USDKRW=X", 
    "USDCNY=X"
  )
  
  # Use reactive dates for downloading  
  data_reactive <- reactive({
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    data_list <- lapply(symbols, function(sym) {
      getSymbols(sym, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)[, 6]
    })
    names(data_list) <- symbols
    data_merged <- do.call(merge, data_list)
    data_merged <- na.omit(data_merged)
    
    forex_data <- coredata(data_merged)
    return(forex_data)
  })
  
  # Event Reactive to launch the backtest when the button is clicked
  strategy_results <- eventReactive(input$run_btn, {
    # Notification to indicate that the calculation is in progress
    showNotification("Backtest calculation in progress...", type = "message", duration = NULL, id = "calc_notif")
    
    # Retrieve parameters
    lookback <- input$lookback
    entry_threshold <- input$entry_threshold
    exit_threshold <- input$exit_threshold
    stop_loss <- input$stop_loss
    take_profit <- input$take_profit
    
    # Retrieve datas
    forex_data <- data_reactive()
    
    # Launch the backtest
    result <- backtest_ou_strategy(
      forex_data, 
      symbols,
      lookback = lookback,
      entry_threshold = entry_threshold,
      exit_threshold = exit_threshold,
      stop_loss = stop_loss,
      take_profit = take_profit
    )
    
    # Close the notification
    removeNotification(id = "calc_notif")
    
    # Sucess notification
    showNotification("Backtest completed successfully!", type = "message", duration = 3)
    
    return(result)
  })
  
  # Performance Summary (table)
  output$perf_table <- renderDT({
    req(strategy_results())
    perf_df <- strategy_results()$performance_summary
    
    # Enhance data formating
    perf_df$Total_Return <- paste0(round(perf_df$Total_Return * 100, 2), "%")
    perf_df$Annualized_Return <- paste0(round(perf_df$Annualized_Return * 100, 2), "%")
    perf_df$Max_Drawdown <- paste0(round(perf_df$Max_Drawdown * 100, 2), "%")
    perf_df$Win_Rate <- paste0(round(perf_df$Win_Rate * 100, 2), "%")
    
    datatable(
      perf_df, 
      colnames = c(
        "Pair", 
        "Total Return", 
        "Annualized Return", 
        "Sharpe Ratio", 
        "Drawdown Maximum", 
        "Win rate", 
        "Profit Factor", 
        "Number of trades"
      ),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        searching = TRUE,
        ordering = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      extensions = 'Buttons',
      class = "display compact cell-border stripe hover"
    ) %>%
      formatStyle(
        'Sharpe_Ratio',
        color = styleInterval(c(0, 1, 2), c('red', 'orange', 'lightgreen', 'darkgreen')),
        fontWeight = 'bold'
      )
  })
  
  
  # --------------------------------------------------------------------------------
  #             Strategy Comparison 
  # --------------------------------------------------------------------------------
  output$comparison_table <- renderDT({
    req(strategy_results())
    forex_data <- data_reactive()
    
    comparison <- compare_strategies(symbols, forex_data, strategy_results())
    
    comparison$OU_Strategy_Return <- paste0(round(comparison$OU_Strategy_Return * 100, 2), "%")
    comparison$Buy_Hold_Return <- paste0(round(comparison$Buy_Hold_Return * 100, 2), "%")
    
    datatable(
      comparison,
      colnames = c(
        "Pair", 
        "Return OU", 
        "Return Buy & Hold", 
        "Sharpe OU", 
        "Sharpe Buy & Hold"
      ),
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE,
        scrollX = TRUE
      ),
      class = "display compact cell-border stripe hover"
    ) %>%
      formatStyle(
        'OU_Strategy_Return',
        color = styleInterval(0, c('red', 'green')),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Buy_Hold_Return',
        color = styleInterval(0, c('red', 'green')),
        fontWeight = 'bold'
      )
  })
  
  
  # --------------------------------------------------------------------------------
  #             Correlation between pairs
  # --------------------------------------------------------------------------------
  output$correlation_plot <- renderPlot({
    req(strategy_results())
    forex_data <- data_reactive()
    
    # Compute daily returns 
    returns <- apply(forex_data, 2, function(x) diff(log(x)))
    
    # Compute the correlation matrix
    cor_matrix <- cor(returns, use = "pairwise.complete.obs")
    
    # Visualization with corrplot 
    corrplot(
      cor_matrix, 
      method = "color",
      type = "upper", 
      order = "hclust",
      tl.col = "black", 
      tl.srt = 45,
      col = colorRampPalette(c("#FF4136", "#FFFFFF", "#0074D9"))(200),
      addCoef.col = "black",
      number.cex = 0.7,
      tl.cex = 0.8,
      title = "Correlation Matrix of Returns",
      mar = c(0, 0, 2, 0)
    )
  })
  
  
  # --------------------------------------------------------------------------------
  #             Combined Portfolio 
  # --------------------------------------------------------------------------------
  output$portfolio_plot <- renderPlot({
    req(strategy_results())
    
    results <- strategy_results()$results
    symbols <- names(results)
    
    # Extract cumulative PnLs for each symbol 
    cumulative_pnls <- lapply(symbols, function(sym) {
      results[[sym]]$strategy_results$cumulative_pnl
    })
    
    # Compute the average PnL (equally weighted portfolio)  
    n_obs <- length(cumulative_pnls[[1]])
    portfolio_pnl <- numeric(n_obs)
    
    for (i in seq_len(n_obs)) {
      values <- sapply(cumulative_pnls, function(pnl) pnl[i])
      portfolio_pnl[i] <- mean(values, na.rm = TRUE)
    }
    
    # Plot the portfolio's PnL 
    plot_data <- data.frame(
      Day = seq_len(n_obs),
      PnL = portfolio_pnl
    )
    
    ggplot(plot_data, aes(x = Day, y = PnL)) +
      geom_line(size = 1, color = "#3366CC") +
      geom_area(alpha = 0.2, fill = "#3366CC") +
      labs(
        title = "Performance of the Combined Portfolio (Equally Weighted)",
        x = "Trading day",
        y = "Cumulative P&L"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 12),
        panel.grid.major = element_line(color = "#e0e0e0"),
        panel.grid.minor = element_line(color = "#f0f0f0"),
        legend.position = "none"
      )
  })
  
  
  # --------------------------------------------------------------------------------
  #             Portfolio Statistics 
  # --------------------------------------------------------------------------------
  output$portfolio_stats <- renderTable({
    req(strategy_results())
    
    results <- strategy_results()$results
    symbols <- names(results)
    
    # Extract PnLs for each symbol 
    pnls <- lapply(symbols, function(sym) {
      results[[sym]]$strategy_results$pnl
    })
    
    # Compute portfolio returns 
    n_obs <- length(pnls[[1]])
    portfolio_returns <- numeric(n_obs)
    
    for (i in seq_len(n_obs)) {
      values <- sapply(pnls, function(pnl) pnl[i])
      portfolio_returns[i] <- mean(values, na.rm = TRUE)
    }
    
    # Compute statistics
    total_return <- sum(portfolio_returns, na.rm = TRUE)
    annualized_return <- total_return / (n_obs / 252)
    sharpe_ratio <- mean(portfolio_returns, na.rm = TRUE) / 
      sd(portfolio_returns, na.rm = TRUE) * sqrt(252)
    
    cumulative_returns <- cumsum(portfolio_returns)
    max_drawdown <- max(cummax(cumulative_returns) - cumulative_returns, na.rm = TRUE)
    
    positive_days <- sum(portfolio_returns > 0, na.rm = TRUE)
    total_days <- sum(!is.na(portfolio_returns))
    win_rate <- positive_days / total_days
    
    # Formatted results
    data.frame(
      Statistique = c(
        "Total return", 
        "Annualized return", 
        "Sharpe Ratio", 
        "Drawdown Maximum", 
        "Win rate"
      ),
      Valeur = c(
        paste0(round(total_return * 100, 2), "%"),
        paste0(round(annualized_return * 100, 2), "%"),
        round(sharpe_ratio, 2),
        paste0(round(max_drawdown * 100, 2), "%"),
        paste0(round(win_rate * 100, 2), "%")
      )
    )
  },
  striped = TRUE, 
  bordered = TRUE, 
  hover = TRUE, 
  spacing = 'l',
  align = 'c', 
  width = "100%"
  )
  
  
  # --------------------------------------------------------------------------------
  #             Top 5 Pairs (by Sharpe Ratio) 
  # --------------------------------------------------------------------------------
  output$plots_top5 <- renderUI({
    req(strategy_results())
    
    # Retrieve performance data
    perf_df <- strategy_results()$performance_summary
    
    # Select top 5 pairs by Sharpe Ratio 
    top5 <- perf_df %>%
      arrange(desc(Sharpe_Ratio)) %>%
      head(5)
    
    # Retrieve top 5 symbols 
    top5_symbols <- top5$Symbol
    
    # Dynamically generate plot blocks 
    plot_outputs <- lapply(seq_along(top5_symbols), function(i) {
      if (i <= length(top5_symbols)) {
        symbol <- top5_symbols[i]
        symbol_index <- which(symbols == symbol)
        prices <- data_reactive()[, symbol_index]
        
        strategy_result <- strategy_results()$results[[symbol]]$strategy_results
        plots <- plot_strategy_results(prices, strategy_result, symbol)
        
        tagList(
          h4(paste("Paire #", i, ":", symbol)),
          fluidRow(
            column(
              width = 6, 
              plotOutput(
                outputId = paste0("price_plot_", i), 
                height = "250px"
              )
            ),
            column(
              width = 6, 
              plotOutput(
                outputId = paste0("pnl_plot_", i), 
                height = "250px"
              )
            )
          ),
          tags$hr()
        )
      }
    })
    
    # Create outputs for plots
    for (i in seq_along(top5_symbols)) {
      local({
        local_i <- i
        symbol <- top5_symbols[local_i]  # Set the correct pair for this iteration
        symbol_index <- which(symbols == symbol)
        prices <- data_reactive()[, symbol_index]
        strategy_result <- strategy_results()$results[[symbol]]$strategy_results
        plots <- plot_strategy_results(prices, strategy_result, symbol)
        
        force(symbol)  # Ensure the captured value is correct  
        
        output[[paste0("price_plot_", local_i)]] <- renderPlot({
          plot_strategy_results(prices, strategy_results()$results[[symbol]]$strategy_results, symbol)$price_plot
        })
        
        output[[paste0("pnl_plot_", local_i)]] <- renderPlot({
          plot_strategy_results(prices, strategy_results()$results[[symbol]]$strategy_results, symbol)$pnl_plot
        })
      })
    }
    
    # Return of the UI
    tagList(plot_outputs)
  })
}

# App launch
shinyApp(ui = ui, server = server)
  
  
  
  
  
  
  
  
  
  