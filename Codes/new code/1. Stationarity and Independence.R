########################################################################################################
#pre-processing data 
########################################################################################################
library(dplyr)
library(tseries)
library(tidyr)
library(ggplot2)
data = read.csv('/Users/ruiliu/Desktop/research/data/USDT_perp_futures_0902_daily.csv')

#convert datetime format 
data$openTime = as.POSIXct(data$openTime/1000, origin="1970-01-01")
data$openTime = as.Date(data$openTime)

#keep coins that has the longest trading history and the largest average trading volume 
startdates = data %>%
  group_by(inst) %>%
  summarise(min_time = min(openTime))

startdates$start_date_rank = rank(startdates$min_time)

trading_vol = data %>%
  group_by(inst)%>%
  summarise(average_volume = mean(QuoteAssetVolume))

trading_vol$trading_vol_rank = rank(-trading_vol$average_volume) 

summary = inner_join(startdates, trading_vol, by = 'inst')

summary$total_rank = (summary$start_date_rank*0.5 + summary$trading_vol_rank*0.5)

print(summary[order(summary$total_rank, decreasing = FALSE), ], n = 30)

#keep selected 20 coins
inst_to_keep = summary[order(summary$total_rank, decreasing = FALSE), ][1:20,]$inst
tmp = data[,"inst"]
data = data[tmp %in% inst_to_keep, ]
print("Assets used for analysis")
print(inst_to_keep)
#inst_to_keep = c('BTCUSDT', 'ETHUSDT', 'BNBUSDT', 'XRPUSDT', 'ADAUSDT', 'DOGEUSDT', 'SOLUSDT', 'XLMUSDT', 'MATICUSDT', 'TRXUSDT', 
#                 'LTCUSDT', 'DOTUSDT', 'AVAXUSDT', 'NEARUSDT', 'FILUSDT', 'ATOMUSDT', 'DASHUSDT', 'LINKUSDT', 'UNIUSDT', 'XMRUSDT')

#calculate daily returns
data <- data[, c("openTime", "inst", "Close")]
data <- data %>% arrange(inst, openTime) 
data = unique(data) #remove duplicated rows
daily_returns <- data %>%
  group_by(inst) %>%
  mutate(daily_return = (Close / lag(Close) - 1))
#calculate log returns
daily_returns['log_returns'] = log(daily_returns['daily_return']+1)
daily_returns <- na.omit(daily_returns)
#make sure the data all have the same dates
tmp = daily_returns %>%  group_by(inst) %>% summarise(max_openTime = max(openTime))
tmp = min(tmp$max_openTime)
tmp1 = daily_returns %>%  group_by(inst) %>% summarise(min_openTime = min(openTime))
tmp1 = max(tmp1$min_openTime)
daily_returns = daily_returns[(daily_returns$openTime >= tmp1)&(daily_returns$openTime <= tmp), ]

########################################################################################################
#Plot returns plot 
########################################################################################################
p = ggplot(daily_returns, aes(x = openTime, y = daily_return)) +
  geom_line(color = "blue") +  # Set the line color to blue for all plots
  facet_wrap(~ inst, ncol = 2, nrow = 10, scales = "free_x") +  # 2 columns and 10 rows
  labs(x = "Date",
       y = "Daily Returns") +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(legend.position = "none") + 
  ylim(-0.3, 0.3) # Remove the legend

# Save the plot to fit an A4 page
ggsave("/Users/ruiliu/Desktop/research/plots/daily_returns.jpg", plot = p, width = 8, height = 11, units = "in")


########################################################################################################
#Stationarity Tests
########################################################################################################

#Null hypothesis is that a unit root is present in a time series, the series is non-stationary and exhibits a systematic pattern
library(knitr)
library(ggplot2)
library(ggpmisc)

# Create the ADF results table
adf_results <- daily_returns %>%
  group_by(inst) %>%
  summarise(adf_statistic = round(adf.test(log_returns, alternative = "stationary")$statistic, 2),
            p_value = adf.test(log_returns, alternative = "stationary")$p.value,
            .groups = 'drop') %>%
  arrange(p_value)

# Create a ggplot object with the table as a layer
p <- ggplot() +
  geom_table(aes(0.5, 0.5, label = list(adf_results))) +
  theme_void()  # Removes axes and other plot elements

# Save the plot as a PNG image
ggsave("/Users/ruiliu/Desktop/research/plots/adf_results_table.png", plot = p, width = 8.27, height = 11.69, units = "in")

########################################################################################################
#ACF and PACF Plots
########################################################################################################
library(forecast)
library(ggplot2)
library(dplyr)
library(gridExtra)

plot_acf_pacf_ts <- function(data, save_dir) {
  # Create lists to store the plots
  acf_plots <- list()
  pacf_plots <- list()
  
  # Loop through each instrument
  for (inst_name in unique(data$inst)) {
    # Subset the data for the specific inst
    sub_data <- filter(data, inst == inst_name)
    
    # Check if there's enough data to plot
    if (nrow(sub_data) > 1) {
      # Convert log_returns column to a ts object
      log_returns_ts <- ts(sub_data$log_returns, frequency = 365)
      
      # Plot ACF using ggAcf
      acf_plot <- ggAcf(log_returns_ts, lag.max = 30) + ggtitle(inst_name) +
        theme(plot.title = element_text(size = 8), axis.title.x = element_text(size = 6),  
              axis.title.y = element_text(size = 6))
      
      # Plot PACF using ggPacf
      pacf_plot <- ggPacf(log_returns_ts, lag.max = 30) + ggtitle(inst_name)+
        theme(plot.title = element_text(size = 8), axis.title.x = element_text(size = 6),  
              axis.title.y = element_text(size = 6))
      
      # Store the plots in their respective lists
      acf_plots[[paste0(inst_name, "_acf")]] <- acf_plot
      pacf_plots[[paste0(inst_name, "_pacf")]] <- pacf_plot
    } else {
      cat(paste("Not enough data to plot for", inst_name, "\n"))
    }
  }
  
  # Arrange the ACF plots into a grid of 10 rows by 2 columns
  combined_acf_plot <- marrangeGrob(grobs = acf_plots, nrow = 10, ncol = 2)
  
  # Arrange the PACF plots into a grid of 10 rows by 2 columns
  combined_pacf_plot <- marrangeGrob(grobs = pacf_plots, nrow = 10, ncol = 2)
  
  # Save the combined ACF plot
  ggsave(file.path(save_dir, "combined_acf_plots.jpg"), combined_acf_plot, width = 8, height = 11, units = "in")
  
  # Save the combined PACF plot
  ggsave(file.path(save_dir, "combined_pacf_plots.jpg"), combined_pacf_plot, width = 8, height = 11, units = "in")
}

plot_acf_pacf_ts(daily_returns, save_dir = "/Users/ruiliu/Desktop/research/plots/acfpacf")



########################################################################################################
#Ljung-Box test for indenpendence 
########################################################################################################
#Null hypothesis: there are no autocorrelations in the series at any of the lags tested.

library(gridExtra)
library(grid)
library(dplyr)

# Calculate the Box-Ljung p-values and the number of lags used
Box_results <- daily_returns %>%
  group_by(inst) %>%
  summarise(
    Box_p_value = if (length(log_returns) > 10) 
      Box.test(log_returns, lag = min(10, length(log_returns) - 1), type = "Ljung-Box")$p.value 
    else 
      NA_real_,
    n_lags = if (length(log_returns) > 10) 
      min(10, length(log_returns) - 1) 
    else 
      NA_integer_,  # Adding the number of lags used
    .groups = 'drop'
  )

# Round the p-values to two decimal places
Box_results <- Box_results %>%
  mutate(Box_p_value = sprintf("%.2f", Box_p_value))

# Arrange results by p-value
Box_results <- Box_results %>%
  arrange(Box_p_value)

# Split the data into two equal parts for formatting into 10x4 layout
Box_results_part1 <- Box_results[1:10, ]
Box_results_part2 <- Box_results[11:20, ]

# Combine the two parts into a single data frame for the table
Box_results_matrix <- cbind(Box_results_part1, Box_results_part2)
colnames(Box_results_matrix) <- c("asset", "p-value", "lags", "asset", "p-value", "lags")

# Convert to a table grob (graphical object), ensuring no row names
table_grob <- tableGrob(Box_results_matrix, rows = NULL)  # Setting rows = NULL removes row numbers

# Calculate the width and height of the table
table_width <- convertWidth(sum(table_grob$widths), "in", valueOnly = TRUE)
table_height <- convertHeight(sum(table_grob$heights), "in", valueOnly = TRUE)

# Save the table as a JPEG image with the exact dimensions of the table
jpeg("/Users/ruiliu/Desktop/research/plots/Box_results_table.jpg", width = table_width, height = table_height, units = "in", res = 300)
grid.draw(table_grob)  # Draw the table on the graphic device
dev.off()  # Close the

########################################################################################################
#Fit ARIMA and GARCH model
########################################################################################################

library(dplyr)
library(purrr)
library(rugarch)

library(rugarch)

fit_garch_and_test <- function(residuals) {
  max_p <- 3  # Max GARCH(p,q) orders
  max_q <- 3
  max_AR_p <- 2
  max_MA_q <- 2
  
  # Initialize variables to store the best model
  best_bic <- Inf
  best_model <- NULL
  best_order <- NULL
  garch_std_residuals <- NA  # Initialize to NA in case no model is valid
  lb_test <- NA
  lb_test_squared <- NA
  
  # Number of observations
  n <- length(residuals)
  
  for (ARp in 0:max_AR_p) {
    for (MAq in 0:max_MA_q) {
      for (p in 1:max_p) {
        for (q in 0:max_q) {
          
          # Specify and fit the GARCH model
          spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
                             mean.model = list(armaOrder = c(ARp, MAq), include.mean = FALSE))
          
          # Fit model using tryCatch to handle errors
          garch_fit <- tryCatch(
            ugarchfit(spec, unlist(residuals), solver = 'hybrid'),
            error = function(e) return(NULL),
            warning = function(w) return(NULL)  # Also handle warnings
          )
          
          # Proceed only if the model converged and is not NULL
          if (!is.null(garch_fit)) {
            # Extract log-likelihood and number of parameters, handle NA values
            log_likelihood <- tryCatch(likelihood(garch_fit), error = function(e) NA)
            num_params <- tryCatch(length(coef(garch_fit)), error = function(e) NA)
            
            # Skip models where log-likelihood or num_params is NA or NULL
            if (!is.na(log_likelihood) && !is.null(log_likelihood) && !is.na(num_params) && !is.null(num_params)) {
              # Calculate BIC manually
              bic_value <- -2 * log_likelihood + num_params * log(n)
              
              # Compare and store the model if it's the best so far
              if (bic_value < best_bic) {
                best_bic <- bic_value
                best_model <- garch_fit
                best_order <- c(ARp, MAq, p, q)
                
                # Calculate standardized residuals and Ljung-Box test results
                garch_std_residuals <- tryCatch(as.numeric(residuals(best_model, standardize = TRUE)), error = function(e) NA)
                lb_test <- tryCatch(Box.test(tryCatch(as.numeric(residuals(best_model, standardize = FALSE)), error = function(e) NA), lag = 10, type = "Ljung-Box")$p.value, error = function(e) NA)
                lb_test_squared <- tryCatch(Box.test(garch_std_residuals^2, lag = 10, type = "Ljung-Box")$p.value, error = function(e) NA)
              }
            }
          }
        }
      }
    }
  }
  
  # Return the best model's order, BIC value, and standardized residuals, handling potential NULL values
  return(list(
    order = best_order, 
    bic = best_bic, 
    std_residuals = garch_std_residuals, 
    lb_test = lb_test, 
    lb_test_squared = lb_test_squared
  ))
}



# Applying the function to each group
log_returns_df <- data.frame(daily_returns[,c('openTime', 'inst', 'log_returns')] %>%
  pivot_wider(names_from = inst, values_from = log_returns))

AR_results = lapply(log_returns_df[,2:ncol(log_returns_df)], fit_garch_and_test)

order_and_BIC <- data.frame(
  ARMA_p = sapply(AR_results, function(x) x$order[1]),
  ARMA_q = sapply(AR_results, function(x) x$order[2]),
  GARCH_p = sapply(AR_results, function(x) x$order[3]),    
  GARCH_q = sapply(AR_results, function(x) x$order[4]),
  BIC = sapply(AR_results, function(x) x$bic),
  #lb_residuals_p_value = sapply(AR_results, function(x) x$lb_test),
  standardized_residuals_squared_p_value = sapply(AR_results, function(x) x$lb_test_squared)
)

order_and_BIC[, c("BIC", "standardized_residuals_squared_p_value")] <- lapply(
  order_and_BIC[, c("BIC", "standardized_residuals_squared_p_value")],
  function(x) formatC(x, format = "f", digits = 4)
)



# Create a table grob (graphical object)
table <- tableGrob(order_and_BIC)
# Convert the table to a ggplot object to use ggsave
p <- ggplot() + annotation_custom(table) + theme_void()
ggsave("/Users/ruiliu/Desktop/research/plots/GARCH_results_table.jpg", plot = p, width = 10, height = 6)


# Extract standardized residuals as a data frame with each inst as a column
std_residuals_df <- data.frame(
  openTime = log_returns_df$openTime  # Assuming all series are the same length
)

for (inst in names(AR_results)) {
  std_residuals_df[[inst]] <- AR_results[[inst]]$std_residuals
}

########################################################################################################
#Fit Marginal Distribution
########################################################################################################


library(MASS)
library(sn)
library(fBasics)
library(VGAM)
library(fitdistrplus)
library(dplyr)
library(sn)

data = std_residuals_df[,2:21]

fit_distributions <- function(data) {
  fits <- list()
  n <- length(data)  # Number of observations
  
  # Normal Distribution
  normal_fit <- MASS::fitdistr(data, "normal")
  fits$normal <- c(AIC = AIC(normal_fit), BIC = BIC(normal_fit), params = normal_fit$estimate)
  
  # t-Distribution (provide degrees of freedom estimate if needed)
  t_fit <- fitdistrplus::fitdist(data, "t", start = list(df = 10))
  fits$t <- c(AIC = t_fit$aic, BIC = t_fit$bic, params = t_fit$estimate)
  
  # # Skew-normal distribution
  sn_fit = selm(X ~ 1, data= data.frame(X = data), family = 'SN')
  #sn_fit <- sn::selm(data ~ 1, family = sn::SN())
  logLik_sn <- logLik(sn_fit)
  k_sn <- length(coef(sn_fit))
  fits$sn <- c(AIC = 2 * k_sn - 2 * logLik_sn, BIC = log(n) * k_sn - 2 * logLik_sn, params = coef(sn_fit))
  
  # st_fit = selm(X ~ 1, data= data.frame(X = data), family = 'ST', param.type = 'DP')
  # logLik_st <- logLik(st_fit)
  # k_st <- length(coef(st_fit))
  # if (!is.null(coef(st_fit))){
  #   fits$st <- c(AIC = 2 * k_st - 2 * logLik_st, BIC = log(n) * k_st - 2 * logLik_st, params = coef(st_fit))
  # }
  
  logistic_fit <- fitdistr(data, densfun = "logistic")
  fits$logistic <- c(AIC = AIC(logistic_fit), BIC = BIC(logistic_fit), params = logistic_fit$estimate)
  
  dlaplace <- function(x, location, scale) {
    1/(2 * scale) * exp(-abs(x - location) / scale)
  }
  laplace_fit <- fitdistr(data, dlaplace, start = list(location = 0, scale = 1))
  fits$laplace = c(AIC = AIC(laplace_fit), BIC = BIC(laplace_fit), params = coef(laplace_fit))
  
  cauchy_fit = fitdistr(data, "cauchy")
  fits$cauchy = c(AIC = AIC(cauchy_fit), BIC = BIC(cauchy_fit), params = coef(cauchy_fit))
  return(fits)
}

# Apply the function to each row and store the results
results <- lapply(data, fit_distributions)
# Print the resulting dataframe
#print(results)

find_best_model <- function(model_list) {
  # Initialize a variable to store the minimum BIC and the best model name
  min_bic <- Inf
  best_model <- NULL
  
  # Loop through each model in the list
  for (model_name in names(model_list)) {
    current_bic <- model_list[[model_name]]['BIC']
    # Check if the current model's BIC is lower than the current minimum
    if (current_bic < min_bic) {
      min_bic <- current_bic
      best_model <- model_name
    }
  }
  
  return(best_model)
}

# Apply the function to each set of models in the results list
best_distributions <- lapply(results, find_best_model)

# Extract the instrument names (names of the list)
instruments <- names(best_distributions)

# Extract the distributions (values of the list)
distributions <- unlist(best_distributions)

# Create a data frame with two columns: instruments and distributions
best_distributions_df <- data.frame(
  instrument = instruments,
  distribution = distributions,
  stringsAsFactors = FALSE
)

best_distributions_df <- best_distributions_df[order(best_distributions_df$distribution), ]

# Split the dataframe into two parts
first_half <- best_distributions_df[1:10, ] # First 10 rows
second_half <- best_distributions_df[11:20, ] # Next 10 rows

# Remove row names from both parts
rownames(first_half) <- NULL
rownames(second_half) <- NULL

# Create table grobs for each part
table1 <- tableGrob(first_half, rows = rep("", nrow(first_half)))
table2 <- tableGrob(second_half, rows = rep("", nrow(second_half)))

# Combine the two tables side by side using grid.arrange
p <- grid.arrange(table1, table2, ncol = 2)

# Save the combined tables as a jpg
ggsave("/Users/ruiliu/Desktop/research/plots/Best_Fitting_Distributions.jpg", plot = p, width = 6, height = 3.5)

#################################################################################################
#Plot Histogram and fitted distribution
#################################################################################################
plot_data <- list()

for (i in 1:length(best_distributions)) {
  best <- best_distributions[[i]]
  model <- results[[i]][[best]]
  x <- seq(-5, 5, by = 0.01)
  
  if (best == 'normal') {
    mean <- model['params.mean']
    sd <- model['params.sd']  # corrected parameter name
    plot_data[[i]] <- dnorm(x = x, mean = mean, sd = sd)
  } else if (best == 't') {
    df <- model['params.df']
    plot_data[[i]] <- dt(x = x, df = df)
  } else if (best == 'sn') {
    mean <- model['params.mean']
    sd <- model['params.s.d']
    gamma1 <- model['params.gamma1']
    plot_data[[i]] <- dsn(x = x, xi = mean, omega = sd, alpha = gamma1)
  } else if (best == 'st') {
    mean <- model['params.mean']
    sd <- model['params.s.d']
    gamma1 <- model['params.gamma1']
    gamma2 <- model['params.gamma2']
    plot_data[[i]] <- dst(x = x, omega = sd, alpha = gamma1, nu = gamma2)
  } else if (best == 'logistic') {
    location <- model['params.location']
    scale <- model['params.scale']
    plot_data[[i]] <- dlogis(x = x, location = location, scale = scale)
  } else if (best == 'laplace') {
    location <- model['params.location']
    scale <- model['params.scale']
    plot_data[[i]] <- dlaplace(x = x, location = location, scale = scale)
  } else if (best == 'cauchy') {
    location <- model['params.location']
    scale <- model['params.scale']
    plot_data[[i]] <- dcauchy(x = x, location = location, scale = scale)
  }
}

library(ggplot2)
library(gridExtra)
library(grid)

# Create a list to hold the individual plots
plot_list <- list()

# Loop through each distribution and generate the plot
for (i in 1:length(best_distributions)) {
  
  # Create histogram data
  hist_data <- hist(AR_results[[i]]$std_residuals, plot = FALSE, breaks = 100)
  
  # Create a data frame for the histogram
  df_hist <- data.frame(x = hist_data$mids, y = hist_data$density)
  
  # Create a data frame for the line plot using the original x and y values
  df_line <- data.frame(x = x, y = plot_data[[i]])
  
  # Generate the plot
  p <- ggplot() +
    geom_histogram(aes(x = AR_results[[i]]$std_residuals, y = ..density..), 
                   bins = 100, fill = "lightgray", color = "black") +
    geom_line(data = df_line, aes(x = x, y = y), color = "blue", size = 0.5) +
    labs(title = paste(names(AR_results)[i], ": ", best_distributions[[i]]),
         x = "Standardized Residuals",
         y = "Density") +
    xlim(-10, 10) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),   
      axis.title.x = element_text(size = 6),                
      axis.title.y = element_text(size = 6), 
      axis.text.x = element_text(size = 5),                  # Adjust x-axis label size
      axis.text.y = element_text(size = 5)               
    )
  
  # Add the plot to the list
  plot_list[[i]] <- p
}

# Arrange the plots into a grid of 10 rows by 2 columns
combined_plot <- marrangeGrob(plot_list, nrow = 10, ncol = 2)


jpeg("/Users/ruiliu/Desktop/research/plots/fitted_distributions.jpg", width = 8, height = 11, units = "in", res = 300)
grid.draw(combined_plot)
dev.off()


######################################################
#plot qq plot of data with theoretical quantiles 
######################################################
# Loop through each residual set and distribution to create QQ plots
for (i in 1:length(best_distributions)) {
  
  # Extract standardized residuals for current iteration
  residuals <- AR_results[[i]]$std_residuals
  
  # Generate the QQ plot using ggplot2
  p <- ggplot(data = data.frame(residuals = residuals), aes(sample = residuals)) +
    stat_qq(color = "grey") +            # Scatter points in grey
    stat_qq_line(color = "red") +        # Line in red
    labs(title = paste(names(AR_results)[i], ": QQ Plot for", best_distributions[[i]]),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    xlim(-5, 5) +
    ylim(-5, 5) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),
      axis.title.x = element_text(size = 6),
      axis.title.y = element_text(size = 6),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5)
    )
  
  # Add the plot to the list
  plot_list[[i]] <- p
}

# Arrange the QQ plots into a grid of 10 rows by 2 columns
combined_plot <- marrangeGrob(plot_list, nrow = 10, ncol = 2)

# Save the combined QQ plot as a JPEG file
jpeg("/Users/ruiliu/Desktop/research/plots/qqplots_distribution.jpg", width = 8, height = 11, units = "in", res = 300)
grid.draw(combined_plot)
dev.off()

################################################################################################################
# uniform data 
################################################################################################################

unif_data <- list()

for (i in 1:length(best_distributions)) {
  best <- best_distributions[[i]]
  model <- results[[i]][[best]]
  
  if (best == 'normal') {
    mean <- model['params.mean']
    sd <- model['params.sd']
    unif_data[[i]] <- pnorm(q = std_residuals_df[,i+1], mean = mean, sd = sd)
  } else if (best == 't') {
    df <- model['params.df']
    unif_data[[i]] <- pt(q = std_residuals_df[,i+1], df = df)
  } else if (best == 'sn') {
    mean <- model['params.mean']
    sd <- model['params.s.d']
    gamma1 <- model['params.gamma1']
    unif_data[[i]] <- psn(x = std_residuals_df[,i+1], xi = mean, omega = sd, alpha = gamma1)
  } else if (best == 'st') {
    mean <- model['params.mean']
    sd <- model['params.s.d']
    gamma1 <- model['params.gamma1']
    gamma2 <- model['params.gamma2']
    unif_data[[i]] <- pst(x = std_residuals_df[,i+1], omega = sd, alpha = gamma1, nu = gamma2)
  } else if (best == 'logistic') {
    location <- model['params.location']
    scale <- model['params.scale']
    unif_data[[i]] <- plogis(q = std_residuals_df[,i+1], location = location, scale = scale)
  } else if (best == 'laplace') {
    location <- model['params.location']
    scale <- model['params.scale']
    unif_data[[i]] <- plaplace(q = std_residuals_df[,i+1], location = location, scale = scale)
  } else if (best == 'cauchy') {
    location <- model['params.location']
    scale <- model['params.scale']
    unif_data[[i]] <- pcauchy(q = std_residuals_df[,i+1], location = location, scale = scale)
  }
}


################################################################################################################################
#QQ plot of transformed data 
################################################################################################################################

library(ggplot2)
library(gridExtra)
library(grid)

# Create a list to hold the individual plots
plot_list <- list()

# Loop through each unif_data set and generate the QQ plot
for (i in 1:length(unif_data)) {
  
  # Generate the theoretical quantiles (uniform distribution)
  theoretical_quantiles <- qunif(ppoints(length(unif_data[[i]])))
  
  # Sort the sample data to match with theoretical quantiles
  sample_quantiles <- sort(unif_data[[i]])
  
  # Create a data frame for ggplot
  df <- data.frame(theoretical_quantiles = theoretical_quantiles, 
                   sample_quantiles = sample_quantiles)
  
  # Generate the QQ plot
  p <- ggplot(df, aes(x = theoretical_quantiles, y = sample_quantiles)) +
    geom_point(color = "grey") +
    geom_abline(slope = 1, intercept = 0, color = "red") +  # 45-degree line
    labs(title =  names(best_distributions)[i], 
         x = "Theoretical Quantiles (Uniform)", 
         y = "Sample Quantiles") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),    # Adjust title size and style
      axis.title.x = element_text(size = 6),              
      axis.title.y = element_text(size = 6), 
      axis.text.x = element_text(size = 5),          
      axis.text.y = element_text(size = 5)     
    )
  
  # Add the plot to the list
  plot_list[[i]] <- p
}

# Arrange the plots into a grid of 10 rows by 2 columns
combined_plot <- marrangeGrob(plot_list, nrow = 10, ncol = 2)


# Save the combined plot as a JPEG file
jpeg("/Users/ruiliu/Desktop/research/plots/combined_qq_plots.jpg", width = 8, height = 11, units = "in", res = 300)
grid.draw(combined_plot)
dev.off()



###########
#Factor Analysis 
###########

library(psych)
library(factoextra)
unif_df = data.frame(do.call(cbind, unif_data))
colnames(unif_df) = names(best_distributions)

#aa = lapply(AR_results$garch_standardized_resid, qnorm)

cor_mat_unif_df = cor(unif_df[1:20])
fa_result_new = fa(cor_mat_unif_df, nfactors = min(ncol(unif_df), 8), rotate = "varimax", fm = "principal")

# Filter out negative eigenvalues for visualization
positive_eigenvalues <- fa_result_new$values[fa_result_new$values > 0]

# Create a vector for the number of factors
factors <- 1:length(positive_eigenvalues)

jpeg("/Users/ruiliu/Desktop/research/plots/scree_plot.jpg", width = 800, height = 600)
p = plot(factors, positive_eigenvalues, type = "b", xlab = "Factor Number", ylab = "Eigenvalues", main = "Scree Plot")
abline(h = 1, col = "red", lty = 2)  # Add a line at eigenvalue = 1
dev.off() 

unif_df['openTime'] = sort(unique(daily_returns$openTime))
write.csv(unif_df, '/Users/ruiliu/Desktop/research/data/unif_df.csv', row.names = FALSE)

##################################################################
# Correlation plots 
##################################################################

plot_dependence_heat_map = function(df){
  cor_matrix <- as.matrix(df)
  
  # Set the row names of the matrix (assuming the row and column names are the same for correlation)
  rownames(cor_matrix) <- colnames(cor_matrix)
  
  # Melt the matrix into long format for ggplot2
  melted_cor_matrix <- melt(cor_matrix, varnames = c("Variable1", "Variable2"))
  
  # Plot the heatmap using ggplot2
  ggplot(data = melted_cor_matrix, aes(x = Variable1, y = Variable2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
          axis.text.y = element_text(size = 10)) +
    coord_fixed() 
}

correlation_mat_kendall = cor(unif_df[,1:(ncol(unif_df)-1)], method = 'kendall')
correlation_mat_spearman = cor(unif_df[,1:(ncol(unif_df)-1)], method = 'spearman')

p1 = plot_dependence_heat_map(correlation_mat_kendall) + 
  labs(title = "Heatmap of Kendall's tau", x = "Asset Pairs", y = "Asset Pairs")

p2 = plot_dependence_heat_map(lower_tail_dependence_gaussian) + 
  labs(title = "Heatmap of Spearman's rho", x = "Asset Pairs", y = "Asset Pairs")

combined_plot <- grid.arrange(p1, p2, ncol = 2, nrow = 1)

# Save the combined plot as a jpg
ggsave("/Users/ruiliu/Desktop/research/plots/heatmap_correlation_data.jpg", plot = combined_plot, width = 12, height = 5, dpi = 300)


library(GGally)
unif_norm_df = data.frame(apply(unif_df[,1:(ncol(unif_df))-1], 1:2, qnorm, mean = 0, sd = 1))
#ggpairs(unif_norm_df[,1:10])
p1 = ggpairs(
  unif_norm_df[,1:20],
  upper = list(continuous = wrap("points", size = 0.5)),
  lower = list(continuous = wrap("points", size = 0.5)))

#ggsave("/Users/ruiliu/Desktop/research/plots/data_normal_plots.png", plot = p1, width = 15, height = 10)


spearmans_rho_transform = function(rho_s){
  rho_c = 2*sin(pi/6*rho_s)
  return(rho_c)
}

correlation_mat_copula = apply(correlation_mat_spearman, 1:2, spearmans_rho_transform)
simulated_gaussian = MASS::mvrnorm(n = 1000, mu = rep(0, nrow(correlation_mat_copula)), Sigma = correlation_mat_copula)

p2 = ggpairs(
  simulated_gaussian[,1:20],
  upper = list(continuous = wrap("points", size = 0.5)),
  lower = list(continuous = wrap("points", size = 0.5)))
ggsave("/Users/ruiliu/Desktop/research/plots/simulated_normal_plots.png", plot = p2, width = 15, height = 10)


# Tail weighted Measure of Dependence 

resid_df = std_residuals_df
unif_score = function(coin_column){
  out = rank(coin_column)
  out = (out-0.5)/length(out)
  return(out)
}

power_func = function(coin, d, p){
  out = 1 - coin/p
  out = out^d
  return(out)
}

power_tail_dependence = function(df, d, p){
  coin_columns = colnames(df)
  #power_func_df = as.data.frame(lapply(df, function(col) lapply(col, power_func, d = d, p = p)))
  df[df > 0.5] = NA
  power_func_df = as.data.frame(apply(df,1:2, power_func, d = d, p = p))
  colnames(power_func_df) = colnames(df)
  power_tail_dependence_df = data.frame(matrix(NA, nrow = length(coin_columns), ncol = length(coin_columns)))
  colnames(power_tail_dependence_df) = coin_columns
  rownames(power_tail_dependence_df) = coin_columns
  
  for (i in 1:length(coin_columns)){
    #cat(sprintf("Completed %d out of %d iterations\n", i, length(coin_columns)))
    for (j in 1:length(coin_columns)){
      tmp1 = power_func_df[, coin_columns[i]]
      tmp2 = power_func_df[, coin_columns[j]]
      
      na_positions <- is.na(tmp1) | is.na(tmp2)
      tmp1 <- tmp1[!na_positions]
      tmp2 <- tmp2[!na_positions]
      power_tail_dependence_df[coin_columns[i], coin_columns[j]] = cor(tmp1, tmp2)
    }
  }
  return(power_tail_dependence_df)
}


library(ggplot2)
library(reshape2)

# Convert the correlation dataframe to a matrix


############################################################################################################
lower_tail_dependence_data1 = power_tail_dependence(df = unif_df[,1:(ncol(unif_df)-1)], p = 0.5, d = 6)
upper_tail_dependence_data1 = power_tail_dependence(df = 1-unif_df[,1:(ncol(unif_df)-1)], p = 0.5, d = 6)

############################################################################################################
cdf_simulated_gaussian = apply(simulated_gaussian, 1:2, pnorm)
lower_tail_dependence_gaussian = power_tail_dependence(df = cdf_simulated_gaussian, p = 0.5, d = 6)
upper_tail_dependence_gaussian = power_tail_dependence(df = 1-cdf_simulated_gaussian, p = 0.5, d = 6)

p1 = plot_dependence_heat_map(lower_tail_dependence_data1) + 
  labs(title = "Heatmap of lower tail dependence data", x = "Asset Pairs", y = "Asset Pairs")

p2 = plot_dependence_heat_map(lower_tail_dependence_gaussian) + 
  labs(title = "Heatmap of lower tail dependence simulated gaussian", x = "Asset Pairs", y = "Asset Pairs")

p3 = plot_dependence_heat_map(upper_tail_dependence_data1) + 
  labs(title = "Heatmap of upper tail dependence data", x = "Asset Pairs", y = "Asset Pairs")

p4 = plot_dependence_heat_map(upper_tail_dependence_gaussian) + 
  labs(title = "Heatmap of upper tail dependence simulated gaussian", x = "Asset Pairs", y = "Asset Pairs")

combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Save the combined plot as a jpg
ggsave("/Users/ruiliu/Desktop/research/plots/heatmap_tail_dependence_data.jpg", plot = combined_plot, width = 12, height = 10, dpi = 300)
