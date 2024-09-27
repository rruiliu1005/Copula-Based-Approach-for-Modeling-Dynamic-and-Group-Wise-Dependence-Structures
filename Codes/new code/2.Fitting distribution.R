
library(fGarch)


############BTC#####################################################
BTC = read.csv('/Users/ruiliu/Desktop/Melbourne/research/data/BTC.csv')
BTC_fit = sstdFit(BTC$standardized_log_returns)
BTC_fit
fitted_BTC = psstd(BTC$standardized_log_returns,mean =  BTC_fit$estimate[1], 
                   sd = BTC_fit$estimate[2], nu = BTC_fit$estimate[3], xi = BTC_fit$estimate[4])
plot(BTC$standardized_log_returns, fitted_BTC)
hist(fitted_BTC, breaks = 20)


############ETH#####################################################
ETH = read.csv('/Users/ruiliu/Desktop/Melbourne/research/data/ETH.csv')
ETH_fit = sstdFit(ETH$standardized_log_returns)
ETH_fit
fitted_ETH = psstd(ETH$standardized_log_returns,mean =  ETH_fit$estimate[1], 
                   sd = ETH_fit$estimate[2], nu = ETH_fit$estimate[3], xi = ETH_fit$estimate[4])
plot(ETH$standardized_log_returns, fitted_ETH)
hist(fitted_ETH, breaks = 20)


############LINK#####################################################
LINK = read.csv('/Users/ruiliu/Desktop/Melbourne/research/data/LINK.csv')
LINK_fit = sstdFit(LINK$standardized_log_returns)
LINK_fit
fitted_LINK = psstd(LINK$standardized_log_returns,mean =  LINK_fit$estimate[1], 
                    sd = LINK_fit$estimate[2], nu = LINK_fit$estimate[3], xi = LINK_fit$estimate[4])
plot(LINK$standardized_log_returns, fitted_LINK)
hist(fitted_LINK, breaks = 20)

############AAVE#####################################################
AAVE = read.csv('/Users/ruiliu/Desktop/Melbourne/research/data/AAVE.csv')
AAVE_fit = sstdFit(AAVE$standardized_log_returns)
AAVE_fit
fitted_AAVE = psstd(AAVE$standardized_log_returns,mean =  AAVE_fit$estimate[1], 
                    sd = AAVE_fit$estimate[2], nu = AAVE_fit$estimate[3], xi = AAVE_fit$estimate[4])
plot(AAVE$standardized_log_returns, fitted_AAVE)
hist(fitted_AAVE, breaks = 20)


############UNI#####################################################
UNI = read.csv('/Users/ruiliu/Desktop/Melbourne/research/data/UNI.csv')
UNI_fit = sstdFit(UNI$standardized_log_returns)
UNI_fit
fitted_UNI = psstd(UNI$standardized_log_returns,mean =  UNI_fit$estimate[1], 
                   sd = UNI_fit$estimate[2], nu = UNI_fit$estimate[3], xi = UNI_fit$estimate[4])
plot(UNI$standardized_log_returns, fitted_UNI)
hist(fitted_UNI, breaks = 20)

############MKR#####################################################
MKR = read.csv('/Users/ruiliu/Desktop/Melbourne/research/data/MKR.csv')
MKR_fit = sstdFit(MKR$standardized_log_returns)
MKR_fit
fitted_MKR = psstd(MKR$standardized_log_returns,mean =  MKR_fit$estimate[1], 
                   sd = MKR_fit$estimate[2], nu = MKR_fit$estimate[3], xi = MKR_fit$estimate[4])
plot(MKR$standardized_log_returns, fitted_MKR)
hist(fitted_MKR, breaks = 20)

############IMX#####################################################
IMX = read.csv('/Users/ruiliu/Desktop/Melbourne/research/data/IMX.csv')
IMX_fit = sstdFit(IMX$standardized_log_returns)
IMX_fit
fitted_IMX = psstd(IMX$standardized_log_returns,mean =  IMX_fit$estimate[1], 
                   sd = IMX_fit$estimate[2], nu = IMX_fit$estimate[3], xi = IMX_fit$estimate[4])
plot(IMX$standardized_log_returns, fitted_IMX)
hist(fitted_IMX, breaks = 20)


############DOGE#####################################################
DOGE = read.csv('/Users/ruiliu/Desktop/Melbourne/research/data/DOGE.csv')
DOGE_fit = sstdFit(DOGE$standardized_log_returns)
DOGE_fit
fitted_DOGE = psstd(DOGE$standardized_log_returns,mean =  DOGE_fit$estimate[1], 
                    sd = DOGE_fit$estimate[2], nu = DOGE_fit$estimate[3], xi = DOGE_fit$estimate[4])
plot(DOGE$standardized_log_returns, fitted_DOGE)
hist(fitted_DOGE, breaks = 20)




