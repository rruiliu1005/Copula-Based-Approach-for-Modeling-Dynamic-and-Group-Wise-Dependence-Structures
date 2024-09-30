load("/Users/ruiliu/Desktop/research/results/model_5_ADA.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_ATOM.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_AVAX.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_BCH.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_BNB.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_BTC.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_DOGE.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_DOT.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_EOS.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_ETC.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_ETH.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_FIL.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_FTM.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_LINK.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_LTC.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_MATIC.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_NEAR.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_SOL.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_TRX.RData")
load("/Users/ruiliu/Desktop/research/results/model_5_XRP.RData")

# Set up the plotting layout for 5 rows and 4 columns
jpeg("/Users/ruiliu/Desktop/research/plots/modelling_dependence_parameter_model_5.jpeg", 
     width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4))  # Create a grid with 5 rows and 4 columns

# List of models and their actual values
models <- list(model_5_ADA, model_5_ATOM, model_5_AVAX, model_5_BCH, model_5_BNB,
               model_5_BTC, model_5_DOGE, model_5_DOT, model_5_EOS, model_5_ETC,
               model_5_ETH, model_5_FIL, model_5_FTM, model_5_LINK, model_5_LTC,
               model_5_MATIC, model_5_NEAR, model_5_SOL, model_5_TRX, model_5_XRP)

# Corresponding actual values (replace these with actual values from your dataframe)
actual_vars <- list(regression_df$ADAUSDT, regression_df$ATOMUSDT, regression_df$AVAXUSDT,
                    regression_df$BCHUSDT, regression_df$BNBUSDT, regression_df$BTCUSDT,
                    regression_df$DOGEUSDT, regression_df$DOTUSDT, regression_df$EOSUSDT,
                    regression_df$ETCUSDT, regression_df$ETHUSDT, regression_df$FILUSDT,
                    regression_df$FTMUSDT, regression_df$LINKUSDT, regression_df$LTCUSDT,
                    regression_df$MATICUSDT, regression_df$NEARUSDT, regression_df$SOLUSDT,
                    regression_df$TRXUSDT, regression_df$XRPUSDT)

# Model names for plotting
model_names <- c("ADA", "ATOM", "AVAX", "BCH", "BNB", "BTC", "DOGE", "DOT", "EOS", "ETC", 
                 "ETH", "FIL", "FTM", "LINK", "LTC", "MATIC", "NEAR", "SOL", "TRX", "XRP")

# Loop over each model and plot
for (i in 1:length(models)) {
  # Extract fitted values and actual values
  fitted_values <- fitted(models[[i]])
  actual_values <- log(actual_vars[[i]] - 1)
  
  # Plot actual vs fitted values
  plot(actual_values, fitted_values,
       xlab = paste("Actual log(", model_names[i], " - 1)", sep = ""), 
       ylab = "Fitted Values", 
       main = paste("Fitted vs Actual for", model_names[i]),
       pch = 19, col = "blue")
  
  # Add a reference line (y = x) for perfect prediction
  abline(a = 0, b = 1, col = "red", lwd = 2)
}

# Close the JPEG device
dev.off()
