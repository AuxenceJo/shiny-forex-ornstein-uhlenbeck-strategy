# 📈 Shiny App: Forex Mean Reversion Strategy using Ornstein-Uhlenbeck Process

This project implements a **mean reversion trading strategy** based on the **Ornstein-Uhlenbeck (OU) process** applied to **Forex currency pairs**. The app is built with R and **Shiny**, offering an interactive platform to test, visualize, and evaluate the strategy's performance.

## 🚀 Features

- 📊 Mean reversion strategy based on Z-score signals  
- ⚙️ Fully interactive Shiny dashboard  
- 🧠 Statistical modeling with the OU process  
- 🛑 Risk management via stop-loss and take-profit  
- 🧮 Backtesting with performance metrics (Sharpe, drawdown, win rate)  
- 🔄 Comparison with Buy & Hold strategy  
- 📈 Portfolio view combining all pairs  
- 🔗 Correlation analysis between currency pairs  

---

## 📚 Theoretical Background

The strategy assumes that certain currency pairs revert to a **mean value** over time. The **Ornstein-Uhlenbeck process** is used to model this behavior:

**OU Process (SDE):**

```math
dX_t = \theta (\mu - X_t) \, dt + \sigma \, dW_t
```

Where:
- `X_t`: the price or log-price
- `μ`: long-term mean
- `θ`: mean reversion speed
- `σ`: volatility
- `W_t`: Brownian motion

**Trading Signal (Z-score):**

```math
Z_t = \frac{X_t - \mu}{\sigma}
```

The strategy takes **long positions** when Z-score is low (e.g. < -2) and **short positions** when Z-score is high (e.g. > +2), expecting the price to revert to the mean.

---

## 🧪 How to Use the App

1. Upload or select Forex pairs  
2. Choose strategy parameters:
   - Lookback window
   - Entry/exit Z-score thresholds
   - Stop-loss / Take-profit levels  
3. Run the backtest  
4. View performance metrics, trade signals, comparisons....
5. Analyze correlations and combined portfolio returns  

---

## 🛠 Installation

Make sure you have R (≥ 4.0) and the following R packages installed:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinythemes", "DT", "quantmod", 
  "PerformanceAnalytics", "ggplot2", "reshape2", "tseries", 
  "corrplot", "dplyr", "zoo", "TTR", "sde", "knitr", "yuima", 
  "rugarch", "plotly"
))
```

---

## ▶️ Running Locally

```r
# Clone the repository
git clone https://github.com/AuxenceJo/shiny-forex-ornstein-uhlenbeck-strategy.git

# Open the project in RStudio
setwd("shiny-forex-ornstein-uhlenbeck-strategy")

# Run the app
shiny::runApp()
```

---

## 🌐 Live Demo

You can try the app online via [shinyapps.io]([https://your_username.shinyapps.io/your_app_name](https://auxencejo.shinyapps.io/Ornstein_Ulhenbeck_Mean_Reversion_Start/))

---

## 📂 Project Structure

```
📁 shiny-forex-ou-strategy/
│
├── app.R                 # Main Shiny app file
├── README.md             # Project overview
```

---

## 📈 Example Outputs

- Price charts with buy/sell signals  
- Z-score visualization  
- Cumulative P&L for each pair  
- Top 5 currency pairs by Sharpe ratio  
- Correlation matrix of returns  
- Portfolio performance and statistics  

---

## 🧠 Potential Improvements

- Add trend filters to avoid counter-trend trades  
- Machine learning for parameter optimization  
- Dynamic capital allocation between pairs  
- Additional confirmation indicators (e.g. RSI, Bollinger)  

---

## 🧑‍💻 Author

**Auxence [@AuxenceJo]**

If you use or modify this project, a ⭐ on GitHub would be appreciated!

If you have any advice please do not hesitate ton contact me via github or [linkedin.com](https://www.linkedin.com/in/auxence-jovignot/)

---

## 📜 License

MIT License. Free to use and adapt with attribution.
