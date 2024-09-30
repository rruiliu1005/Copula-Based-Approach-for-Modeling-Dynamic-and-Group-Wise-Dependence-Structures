unif_df = read.csv('/Users/ruiliu/Desktop/research/data/unif_df.csv')
group_1_1 = unif_df[, c("BTCUSDT", "ETHUSDT", "BNBUSDT", "XRPUSDT", "LTCUSDT")]
group_1_2 = unif_df[, c("ADAUSDT", "SOLUSDT", "DOTUSDT", "ATOMUSDT", "AVAXUSDT")]
group_1_3 = unif_df[, c("DOGEUSDT", "BCHUSDT", "TRXUSDT", "ETCUSDT", "MATICUSDT", "FTMUSDT", "FILUSDT", "NEARUSDT", "LINKUSDT", "EOSUSDT")]

group_2_1 = unif_df[, c("ETHUSDT", "SOLUSDT", "DOTUSDT", "ATOMUSDT", "AVAXUSDT", "MATICUSDT", "LINKUSDT")]
group_2_2 = unif_df[, c("BTCUSDT", "BNBUSDT", "XRPUSDT", "LTCUSDT", "BCHUSDT")]
group_2_3 = unif_df[, c("DOGEUSDT", "ADAUSDT", "TRXUSDT", "ETCUSDT", "EOSUSDT", "FTMUSDT", "FILUSDT", "NEARUSDT")]


load("/Users/ruiliu/Desktop/research/results/group_1_1_result_gaussian_90_days.RData")
result_gaussian_90_days_1_1 = result_gaussian_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_gaussian_180_days.RData")
result_gaussian_180_days_1_1 = result_gaussian_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_gaussian_360_days.RData")
result_gaussian_360_days_1_1 = result_gaussian_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_gaussian_720_days.RData")
result_gaussian_720_days_1_1 = result_gaussian_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_2_result_gaussian_90_days.RData")
result_gaussian_90_days_1_2 = result_gaussian_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_gaussian_180_days.RData")
result_gaussian_180_days_1_2 = result_gaussian_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_gaussian_360_days.RData")
result_gaussian_360_days_1_2 = result_gaussian_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_gaussian_720_days.RData")
result_gaussian_720_days_1_2 = result_gaussian_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_3_result_gaussian_90_days.RData")
result_gaussian_90_days_1_3 = result_gaussian_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_gaussian_180_days.RData")
result_gaussian_180_days_1_3 = result_gaussian_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_gaussian_360_days.RData")
result_gaussian_360_days_1_3 = result_gaussian_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_gaussian_720_days.RData")
result_gaussian_720_days_1_3 = result_gaussian_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_1_result_gaussian_90_days.RData")
result_gaussian_90_days_2_1 = result_gaussian_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_gaussian_180_days.RData")
result_gaussian_180_days_2_1 = result_gaussian_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_gaussian_360_days.RData")
result_gaussian_360_days_2_1 = result_gaussian_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_gaussian_720_days.RData")
result_gaussian_720_days_2_1 = result_gaussian_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_2_result_gaussian_90_days.RData")
result_gaussian_90_days_2_2 = result_gaussian_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_gaussian_180_days.RData")
result_gaussian_180_days_2_2 = result_gaussian_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_gaussian_360_days.RData")
result_gaussian_360_days_2_2 = result_gaussian_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_gaussian_720_days.RData")
result_gaussian_720_days_2_2 = result_gaussian_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_3_result_gaussian_90_days.RData")
result_gaussian_90_days_2_3 = result_gaussian_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_gaussian_180_days.RData")
result_gaussian_180_days_2_3 = result_gaussian_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_gaussian_360_days.RData")
result_gaussian_360_days_2_3 = result_gaussian_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_gaussian_720_days.RData")
result_gaussian_720_days_2_3 = result_gaussian_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_1_result_clayton_90_days.RData")
result_clayton_90_days_1_1 = result_clayton_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_clayton_180_days.RData")
result_clayton_180_days_1_1 = result_clayton_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_clayton_360_days.RData")
result_clayton_360_days_1_1 = result_clayton_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_clayton_720_days.RData")
result_clayton_720_days_1_1 = result_clayton_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_2_result_clayton_90_days.RData")
result_clayton_90_days_1_2 = result_clayton_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_clayton_180_days.RData")
result_clayton_180_days_1_2 = result_clayton_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_clayton_360_days.RData")
result_clayton_360_days_1_2 = result_clayton_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_clayton_720_days.RData")
result_clayton_720_days_1_2 = result_clayton_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_3_result_clayton_90_days.RData")
result_clayton_90_days_1_3 = result_clayton_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_clayton_180_days.RData")
result_clayton_180_days_1_3 = result_clayton_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_clayton_360_days.RData")
result_clayton_360_days_1_3 = result_clayton_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_clayton_720_days.RData")
result_clayton_720_days_1_3 = result_clayton_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_1_result_clayton_90_days.RData")
result_clayton_90_days_2_1 = result_clayton_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_clayton_180_days.RData")
result_clayton_180_days_2_1 = result_clayton_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_clayton_360_days.RData")
result_clayton_360_days_2_1 = result_clayton_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_clayton_720_days.RData")
result_clayton_720_days_2_1 = result_clayton_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_2_result_clayton_90_days.RData")
result_clayton_90_days_2_2 = result_clayton_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_clayton_180_days.RData")
result_clayton_180_days_2_2 = result_clayton_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_clayton_360_days.RData")
result_clayton_360_days_2_2 = result_clayton_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_clayton_720_days.RData")
result_clayton_720_days_2_2 = result_clayton_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_3_result_clayton_90_days.RData")
result_clayton_90_days_2_3 = result_clayton_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_clayton_180_days.RData")
result_clayton_180_days_2_3 = result_clayton_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_clayton_360_days.RData")
result_clayton_360_days_2_3 = result_clayton_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_clayton_720_days.RData")
result_clayton_720_days_2_3 = result_clayton_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_1_result_gumbel_90_days.RData")
result_gumbel_90_days_1_1 = result_gumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_gumbel_180_days.RData")
result_gumbel_180_days_1_1 = result_gumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_gumbel_360_days.RData")
result_gumbel_360_days_1_1 = result_gumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_gumbel_720_days.RData")
result_gumbel_720_days_1_1 = result_gumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_2_result_gumbel_90_days.RData")
result_gumbel_90_days_1_2 = result_gumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_gumbel_180_days.RData")
result_gumbel_180_days_1_2 = result_gumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_gumbel_360_days.RData")
result_gumbel_360_days_1_2 = result_gumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_gumbel_720_days.RData")
result_gumbel_720_days_1_2 = result_gumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_3_result_gumbel_90_days.RData")
result_gumbel_90_days_1_3 = result_gumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_gumbel_180_days.RData")
result_gumbel_180_days_1_3 = result_gumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_gumbel_360_days.RData")
result_gumbel_360_days_1_3 = result_gumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_gumbel_720_days.RData")
result_gumbel_720_days_1_3 = result_gumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_1_result_gumbel_90_days.RData")
result_gumbel_90_days_2_1 = result_gumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_gumbel_180_days.RData")
result_gumbel_180_days_2_1 = result_gumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_gumbel_360_days.RData")
result_gumbel_360_days_2_1 = result_gumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_gumbel_720_days.RData")
result_gumbel_720_days_2_1 = result_gumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_2_result_gumbel_90_days.RData")
result_gumbel_90_days_2_2 = result_gumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_gumbel_180_days.RData")
result_gumbel_180_days_2_2 = result_gumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_gumbel_360_days.RData")
result_gumbel_360_days_2_2 = result_gumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_gumbel_720_days.RData")
result_gumbel_720_days_2_2 = result_gumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_3_result_gumbel_90_days.RData")
result_gumbel_90_days_2_3 = result_gumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_gumbel_180_days.RData")
result_gumbel_180_days_2_3 = result_gumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_gumbel_360_days.RData")
result_gumbel_360_days_2_3 = result_gumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_gumbel_720_days.RData")
result_gumbel_720_days_2_3 = result_gumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_1_result_rgumbel_90_days.RData")
result_rgumbel_90_days_1_1 = result_rgumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_rgumbel_180_days.RData")
result_rgumbel_180_days_1_1 = result_rgumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_rgumbel_360_days.RData")
result_rgumbel_360_days_1_1 = result_rgumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_1_result_rgumbel_720_days.RData")
result_rgumbel_720_days_1_1 = result_rgumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_2_result_rgumbel_90_days.RData")
result_rgumbel_90_days_1_2 = result_rgumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_rgumbel_180_days.RData")
result_rgumbel_180_days_1_2 = result_rgumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_rgumbel_360_days.RData")
result_rgumbel_360_days_1_2 = result_rgumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_2_result_rgumbel_720_days.RData")
result_rgumbel_720_days_1_2 = result_rgumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_1_3_result_rgumbel_90_days.RData")
result_rgumbel_90_days_1_3 = result_rgumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_rgumbel_180_days.RData")
result_rgumbel_180_days_1_3 = result_rgumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_rgumbel_360_days.RData")
result_rgumbel_360_days_1_3 = result_rgumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_1_3_result_rgumbel_720_days.RData")
result_rgumbel_720_days_1_3 = result_rgumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_1_result_rgumbel_90_days.RData")
result_rgumbel_90_days_2_1 = result_rgumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_rgumbel_180_days.RData")
result_rgumbel_180_days_2_1 = result_rgumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_rgumbel_360_days.RData")
result_rgumbel_360_days_2_1 = result_rgumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_1_result_rgumbel_720_days.RData")
result_rgumbel_720_days_2_1 = result_rgumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_2_result_rgumbel_90_days.RData")
result_rgumbel_90_days_2_2 = result_rgumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_rgumbel_180_days.RData")
result_rgumbel_180_days_2_2 = result_rgumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_rgumbel_360_days.RData")
result_rgumbel_360_days_2_2 = result_rgumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_2_result_rgumbel_720_days.RData")
result_rgumbel_720_days_2_2 = result_rgumbel_720_days

load("/Users/ruiliu/Desktop/research/results/group_2_3_result_rgumbel_90_days.RData")
result_rgumbel_90_days_2_3 = result_rgumbel_90_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_rgumbel_180_days.RData")
result_rgumbel_180_days_2_3 = result_rgumbel_180_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_rgumbel_360_days.RData")
result_rgumbel_360_days_2_3 = result_rgumbel_360_days
load("/Users/ruiliu/Desktop/research/results/group_2_3_result_rgumbel_720_days.RData")
result_rgumbel_720_days_2_3 = result_rgumbel_720_days


############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_gaussian_90_days_group_1.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_gaussian_90_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
second_element <- result_gaussian_90_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
second_element <- result_gaussian_90_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)


dates = as.Date(unif_df[91:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_gaussian_90_days_group_2.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_gaussian_90_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
second_element <- result_gaussian_90_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
second_element <- result_gaussian_90_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)

dates = as.Date(unif_df[91:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}

for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}

for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}

dev.off()


############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_gaussian_180_days_group_1.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_gaussian_180_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
second_element <- result_gaussian_180_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
second_element <- result_gaussian_180_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)

dates = as.Date(unif_df[181:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}

dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_gaussian_180_days_group_2.jpeg", width = 8, height = 10, units = "in", res = 300)
par(mfrow = c(5, 4)) 
second_element <- result_gaussian_180_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
second_element <- result_gaussian_180_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
second_element <- result_gaussian_180_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)
dates = as.Date(unif_df[181:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}

for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}

for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}
dev.off()


############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_gaussian_360_days_group_1.jpeg", width = 8, height = 10, units = "in", res = 300)
# Set up the plotting layout
par(mfrow = c(5, 4)) 
second_element <- result_gaussian_360_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
second_element <- result_gaussian_360_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
second_element <- result_gaussian_360_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)

dates = as.Date(unif_df[361:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}
dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_gaussian_360_days_group_2.jpeg", width = 8, height = 10, units = "in", res = 300)
par(mfrow = c(5, 4)) 
second_element <- result_gaussian_360_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
second_element <- result_gaussian_360_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
second_element <- result_gaussian_360_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)

dates = as.Date(unif_df[361:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}

for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}

for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}
dev.off()


############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_clayton_90_days_group_1.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_clayton_90_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
second_element <- result_clayton_90_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
second_element <- result_clayton_90_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)


dates = as.Date(unif_df[91:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_clayton_90_days_group_2.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_clayton_90_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
second_element <- result_clayton_90_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
second_element <- result_clayton_90_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)

dates = as.Date(unif_df[91:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}

for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}

for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}

dev.off()


############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_clayton_180_days_group_1.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_clayton_180_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
second_element <- result_clayton_180_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
second_element <- result_clayton_180_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)

dates = as.Date(unif_df[181:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}

dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_clayton_180_days_group_2.jpeg", width = 8, height = 10, units = "in", res = 300)
par(mfrow = c(5, 4)) 
second_element <- result_clayton_180_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
second_element <- result_clayton_180_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
second_element <- result_clayton_180_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)
dates = as.Date(unif_df[181:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}

for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}

for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}
dev.off()


############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_clayton_360_days_group_1.jpeg", width = 8, height = 10, units = "in", res = 300)
# Set up the plotting layout
par(mfrow = c(5, 4)) 
second_element <- result_clayton_360_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
second_element <- result_clayton_360_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
second_element <- result_clayton_360_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)

dates = as.Date(unif_df[361:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}
dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_clayton_360_days_group_2.jpeg", width = 8, height = 10, units = "in", res = 300)
par(mfrow = c(5, 4)) 
second_element <- result_clayton_360_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
second_element <- result_clayton_360_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
second_element <- result_clayton_360_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)

dates = as.Date(unif_df[361:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}

for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}

for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}
dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_rgumbel_90_days_group_1_tau.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_rgumbel_90_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
matrix_data_1_1 <- 1 - 1/matrix_data_1_1

second_element <- result_rgumbel_90_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
matrix_data_1_2 <- 1 - 1/matrix_data_1_2

second_element <- result_rgumbel_90_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)
matrix_data_1_3 <- 1 - 1/matrix_data_1_3



dates = as.Date(unif_df[91:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

jpeg("/Users/ruiliu/Desktop/research/plots/result_rgumbel_90_days_group_2_tau.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_rgumbel_90_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
matrix_data_2_1 <- 1 - 1/matrix_data_2_1

second_element <- result_rgumbel_90_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
matrix_data_2_2 <- 1 - 1/matrix_data_2_2

second_element <- result_rgumbel_90_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)
matrix_data_2_3 <- 1 - 1/matrix_data_2_3



dates = as.Date(unif_df[91:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_rgumbel_180_days_group_1_tau.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_rgumbel_180_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
matrix_data_1_1 <- 1 - 1/matrix_data_1_1

second_element <- result_rgumbel_180_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
matrix_data_1_2 <- 1 - 1/matrix_data_1_2

second_element <- result_rgumbel_180_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)
matrix_data_1_3 <- 1 - 1/matrix_data_1_3



dates = as.Date(unif_df[181:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

jpeg("/Users/ruiliu/Desktop/research/plots/result_rgumbel_180_days_group_2_tau.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_rgumbel_180_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
matrix_data_2_1 <- 1 - 1/matrix_data_2_1

second_element <- result_rgumbel_180_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
matrix_data_2_2 <- 1 - 1/matrix_data_2_2

second_element <- result_rgumbel_180_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)
matrix_data_2_3 <- 1 - 1/matrix_data_2_3



dates = as.Date(unif_df[181:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_rgumbel_360_days_group_1_tau.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_rgumbel_360_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
matrix_data_1_1 <- 1 - 1/matrix_data_1_1

second_element <- result_rgumbel_360_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
matrix_data_1_2 <- 1 - 1/matrix_data_1_2

second_element <- result_rgumbel_360_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)
matrix_data_1_3 <- 1 - 1/matrix_data_1_3



dates = as.Date(unif_df[361:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

jpeg("/Users/ruiliu/Desktop/research/plots/result_rgumbel_360_days_group_2_tau.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_rgumbel_360_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
matrix_data_2_1 <- 1 - 1/matrix_data_2_1

second_element <- result_rgumbel_360_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
matrix_data_2_2 <- 1 - 1/matrix_data_2_2

second_element <- result_rgumbel_360_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)
matrix_data_2_3 <- 1 - 1/matrix_data_2_3



dates = as.Date(unif_df[361:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

############################################################
jpeg("/Users/ruiliu/Desktop/research/plots/result_rgumbel_720_days_group_1_tau.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_rgumbel_720_days_1_1[[2]]
matrix_data_1_1 <- do.call(rbind, second_element)
matrix_data_1_1 <- 1 - 1/matrix_data_1_1

second_element <- result_rgumbel_720_days_1_2[[2]]
matrix_data_1_2 <- do.call(rbind, second_element)
matrix_data_1_2 <- 1 - 1/matrix_data_1_2

second_element <- result_rgumbel_720_days_1_3[[2]]
matrix_data_1_3 <- do.call(rbind, second_element)
matrix_data_1_3 <- 1 - 1/matrix_data_1_3



dates = as.Date(unif_df[721:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:5) {
  plot(dates, matrix_data_1_1[, i], type = "l", main = colnames(group_1_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_1_2[, i], type = "l", main = colnames(group_1_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:10) {
  plot(dates, matrix_data_1_3[, i], type = "l", main = colnames(group_1_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

jpeg("/Users/ruiliu/Desktop/research/plots/result_rgumbel_720_days_group_2_tau.jpeg", width = 8, height = 10, units = "in", res = 300)

par(mfrow = c(5, 4)) 

second_element <- result_rgumbel_720_days_2_1[[2]]
matrix_data_2_1 <- do.call(rbind, second_element)
matrix_data_2_1 <- 1 - 1/matrix_data_2_1

second_element <- result_rgumbel_720_days_2_2[[2]]
matrix_data_2_2 <- do.call(rbind, second_element)
matrix_data_2_2 <- 1 - 1/matrix_data_2_2

second_element <- result_rgumbel_720_days_2_3[[2]]
matrix_data_2_3 <- do.call(rbind, second_element)
matrix_data_2_3 <- 1 - 1/matrix_data_2_3



dates = as.Date(unif_df[721:nrow(unif_df), 21], format = "%Y-%m-%d")
for (i in 1:7) {
  plot(dates, matrix_data_2_1[, i], type = "l", main = colnames(group_2_1)[i], 
       xlab = "Time", ylab = "Value", col = 'red')
}
for (i in 1:5) {
  plot(dates, matrix_data_2_2[, i], type = "l", main = colnames(group_2_2)[i], 
       xlab = "Time", ylab = "Value", col = 'blue')
}
for (i in 1:8) {
  plot(dates, matrix_data_2_3[, i], type = "l", main = colnames(group_2_3)[i], 
       xlab = "Time", ylab = "Value", col = 'purple')
}


dev.off()

