library(quantmod)
library(MASS)
library(zoo)
library(mgcv)
library(pls)

getSymbols("^VIX", src = "yahoo", from = "2020-10-20", to = Sys.Date())
vix_data <- data.frame(VIX)
vix_data$Date = rownames(vix_data)

getSymbols("CL=F", src = "yahoo", from = "2020-10-20", to = Sys.Date()) #(WTI Crude)
oil_data <- `CL=F`
oil_data <- data.frame(`CL=F`)
oil_data$Date = rownames(oil_data)

getSymbols("GC=F", src = "yahoo", from = "2020-10-20", to = Sys.Date())
gold_data <- `GC=F`
gold_data <- data.frame(`GC=F`)
gold_data$Date = rownames(gold_data)


# load("/Users/ruiliu/Desktop/research/results/result_gaussian_180_days.RData")
# load("/Users/ruiliu/Desktop/research/results/result_clayton_180_days.RData")
# load("/Users/ruiliu/Desktop/research/results/result_gumbel_180_days.RData")
load("/Users/ruiliu/Desktop/research/results/result_rgumbel_180_days.RData")
unif_df = read.csv('/Users/ruiliu/Desktop/research/data/unif_df.csv')

# gaussian_df <- result_gaussian_180_days[[2]]
# gaussian_df <- do.call(rbind, gaussian_df)
# clayton_df <- result_clayton_180_days[[2]]
# clayton_df <- do.call(rbind, clayton_df)
# gumbel_df <- result_gumbel_180_days[[2]]
# gumbel_df <- do.call(rbind, gumbel_df)
rgumbel_df = result_rgumbel_180_days[[2]]
rgumbel_df = do.call(rbind, rgumbel_df)
rgumbel_df = data.frame(rgumbel_df) 
colnames(rgumbel_df) = colnames(unif_df)[1:20]
rgumbel_df$Date = sort(unif_df$openTime)[181:length(unif_df$openTime)]

############
# 1. Regression based on log transformation 
external_df = vix_data[,c('VIX.Close', 'Date')]
external_df = merge(external_df, oil_data[,c('CL.F.Close', 'Date')], by = 'Date')
external_df = merge(external_df, gold_data[,c('GC.F.Close', 'Date')], by = 'Date')
colnames(external_df) = c('Date','VIX', 'Oil', 'Gold')
regression_df = merge(rgumbel_df, external_df, by = 'Date', all = FALSE)
regression_df = na.locf(regression_df, fromLast = TRUE)
model_1_ADA <- lm(log(ADAUSDT-1) ~ log(VIX) + log(Oil) + log(Gold), data = regression_df)
summary(model_1_ADA)
model_1_BTC <- lm(log(BTCUSDT-1) ~ log(VIX) + log(Oil) + log(Gold), data = regression_df)
summary(model_1_BTC)

############
# 2. Regression based on powers of log transformation 
regression_df = merge(rgumbel_df, external_df, by = 'Date', all = FALSE)
regression_df = na.locf(regression_df, fromLast = TRUE)
model_2_ADA <- lm(log(ADAUSDT-1) ~ poly(log(VIX),3) + poly(log(Oil),3) + poly(log(Gold),3), data = regression_df)
summary(model_2_ADA)
model_2_BTC <- lm(log(BTCUSDT-1) ~ poly(log(VIX),3) + poly(log(Oil),3) + poly(log(Gold),3), data = regression_df)
summary(model_2_BTC)
model_2_ETH <- lm(log(ETHUSDT-1) ~ poly(log(VIX),3) + poly(log(Oil),3) + poly(log(Gold),3), data = regression_df)
summary(model_2_ETH)
############
# 3. Regression transforming theta using log((theta-1)/(5-theta+1))
regression_df = merge(rgumbel_df, external_df, by = 'Date', all = FALSE)
regression_df = na.locf(regression_df, fromLast = TRUE)
model_3_ADA <- lm(log((ADAUSDT-1)/(5-ADAUSDT+1)) ~ poly(log(VIX),3) + poly(log(Oil),3) + poly(log(Gold),3), data = regression_df)
summary(model_3_ADA)
model_3_BTC <- lm(log((BTCUSDT-1)/(5-BTCUSDT+1)) ~ poly(log(VIX),3) + poly(log(Oil),3) + poly(log(Gold),3), data = regression_df)
summary(model_3_BTC)
model_3_ETH <- lm(log((ETHUSDT-1)/(5-ETHUSDT+1)) ~ poly(log(VIX),3) + poly(log(Oil),3) + poly(log(Gold),3), data = regression_df)
summary(model_3_ETH)

# 4. Regression based on exponential transformation 
model_4_ADA <- lm(exp(ADAUSDT) ~ VIX + Oil + Gold, data = regression_df)
summary(model_4_ADA)
model_4_BTC <- lm(exp(BTCUSDT) ~ VIX + Oil + Gold, data = regression_df)
summary(model_4_BTC)
model_4_ETH <- lm(exp(ETHUSDT) ~ VIX + Oil + Gold, data = regression_df)
summary(model_4_ETH)

############
# 5. Box-Cox transformation of theta
boxcox_model <- boxcox(lm(log(ADAUSDT-1) ~ log(VIX) + log(Gold) + log(Oil), data = regression_df))
lambda <- boxcox_model$x[which.max(boxcox_model$y)]
transformed_theta_df <- (rgumbel_df[,1:20]^lambda - 1) / lambda
transformed_theta_df$Date = rgumbel_df$Date
regression_df_transformed = merge(transformed_theta_df, external_df, by = 'Date', all = FALSE)
regression_df_transformed = na.locf(regression_df_transformed, fromLast = TRUE)

model <- lm(ADAUSDT ~ poly(log(VIX), 3) + poly(log(Gold), 3) + poly(log(Oil), 3), data = regression_df_transformed)
summary(model)

############
# 5. Interaction terms 
regression_df = merge(rgumbel_df, external_df, by = 'Date', all = FALSE)
regression_df = na.locf(regression_df, fromLast = TRUE)
model_5_ADA <- lm(log(ADAUSDT-1) ~ poly(log(VIX),3) + poly(log(Oil),3) + poly(log(Gold),3) + 
                    log(VIX) * log(Oil) + log(Oil) * log(Gold) + log(VIX)*log(Gold)
                  , data = regression_df)
summary(model_5_ADA)

model_5_BTC <- lm(log(BTCUSDT-1) ~ poly(log(VIX),3) + poly(log(Oil),3) + poly(log(Gold),3) + 
                    log(VIX) * log(Oil) + log(Oil) * log(Gold) + log(VIX)*log(Gold)
                  , data = regression_df)
summary(model_5_BTC)

model_5_ETH <- lm(log(ETHUSDT-1) ~ poly(log(VIX),3) + poly(log(Oil),3) + poly(log(Gold),3) + 
                    log(VIX) * log(Oil) + log(Oil) * log(Gold) + log(VIX)*log(Gold)
                  , data = regression_df)
summary(model_5_ETH)

############
# 6. Generalized Additive Models with Thin Plate Regression Splines
model_6_ADA <- gam(ADAUSDT ~ s(log(VIX)) + s(log(Oil)) + s(log(Gold)), data = regression_df)
summary(model_6_ADA)
gam.check(model_6_ADA)

model_6_BTC <- gam(BTCUSDT ~ s(log(VIX)) + s(log(Oil)) + s(log(Gold)), data = regression_df)
summary(model_6_BTC)
gam.check(model_6_BTC)

model_6_ETH <- gam(ETHUSDT ~ s(log(VIX)) + s(log(Oil)) + s(log(Gold)), data = regression_df)
summary(model_6_ETH)
gam.check(model_6_ETH)
############
# 7. Generalized Additive Models with Cubic Regression Splines
model_7_ADA <- gam(ADAUSDT ~ s(log(VIX), bs = "cr") + s(log(Oil), bs = "cr") + s(log(Gold), bs = "cr"), data = regression_df)
summary(model_7_ADA)
gam.check(model_7_ADA)

model_7_BTC <- gam(BTCUSDT ~ s(log(VIX), bs = "cr") + s(log(Oil), bs = "cr") + s(log(Gold), bs = "cr"), data = regression_df)
summary(model_7_BTC)
gam.check(model_7_BTC)

model_7_ETH <- gam(ETHUSDT ~ s(log(VIX), bs = "cr") + s(log(Oil), bs = "cr") + s(log(Gold), bs = "cr"), data = regression_df)
summary(model_7_ETH)
gam.check(model_7_ETH)

