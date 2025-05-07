# Finance Course 4
# Mini-Cases 1
setwd("C:/WU/SBWL Finance/Course 4 - Risk Management/HW1")

# MC 1.1
load("C:/WU/SBWL Finance/Course 4 - Risk Management/HW1/Mini_Case_1_1.Rdata")

# quadratic utility function
mean_A <- mean(A)
mean_B <- mean(B)
round(mean_A,5) == round(mean_B,5)
# means (expected payoffs) are the same
sd_A <- sd(A)
sd_B <- sd(B)
cat("SD(A):", sd_A,
    "\n\nSD(B):", sd_B)
# B has lower volatility => choose investment B

# 5% VaR
ordered_A <- sort(A, decreasing = FALSE)
var_A_0.05 <- quantile(ordered_A, probs = 0.05, type = 6)

ordered_B <- sort(B, decreasing = FALSE)
var_B_0.05 <- quantile(ordered_B, probs = 0.05, type = 6)
cat("VaR at 5%: \n
A:", -var_A_0.05,
    "  B:", -var_B_0.05)

# => choose investment B as it has lower value at risk at 5%

# 0.01% VaR
var_A_0.001 <- quantile(ordered_A, probs = 0.001, type = 6)

var_B_0.001 <- quantile(ordered_B, probs = 0.001, type = 6)
cat("VaR at 0.1%: \n
A:", -var_A_0.001,
    "  B:", -var_B_0.001)

# => choose investment A as it has lower value at risk at 0.01%

#histograms
A_hist <- A[A > -4 & A < 6]
B_hist <- B[B > -4 & B < 6]

par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))
hist(A_hist, breaks = 23, xlim = c(-4, 6), ylim = c(0,2000),
     main = "A",
     xlab = "Payoffs")
legend("topright", legend = c("5% VaR", "0.1% VaR"), col = c("red", "green"), 
       lty = 1, cex = 0.8)
abline(v = var_A_0.05, col = 'red')
abline(v = var_A_0.001, col = 'green')

hist(B_hist, breaks = 23, xlim = c(-4, 6), ylim = c(0,2000),
     main = "B",
     xlab = "Payoffs")
legend("topright", legend = c("5% VaR", "0.1% VaR"), col = c("red", "green"), 
       lty = 1, cex = 0.8)
abline(v = var_B_0.05, col = 'red')
abline(v = var_B_0.001, col = 'green')

mtext("Histograms of the payoffs", outer = TRUE, cex = 1.5)

# A's histogram is more spread out, meaning that it has higher
# variance which explains the first answer 
# 

# MC 1.2
# in the notebook

# MC 1.3
# download data
library(quantmod)

start_date <- "2015-01-01"
end_date <- "2020-07-02"
today <- "2020-01-01"

getSymbols("SPY", src = "yahoo", from = start_date,
           to = end_date)

#kurva <- as.data.frame(SPY)
#dates <- index(data_3)
#kurva$Date <- dates
#write.csv(kurva, file = "sp500_data.csv", row.names = F)

# a) daily returns 
data_3 <- as.data.frame(SPY$SPY.Close)
for (i in 2:nrow(data_3)) {
  data_3$Return[i] <- (data_3$SPY.Close[i]-data_3$SPY.Close[i-1])/data_3$SPY.Close[i-1]
}

head(data_3)

# b) volatility from 2015-01-01 up to “today” 2020-01-01
data_3_b <- data_3[rownames(data_3) < today, ]
volatility_3_b <- sd(data_3_b$Return, na.rm = T)
cat("Volatility is", volatility_3_b*100, "%")

# c) 
w <- 100
alpha <- 0.05
var_3_c <- -w * volatility_3_b * qnorm(0.05)

# d)
data_3_d <- data_3[!rownames(data_3) %in% rownames(data_3_b), ]

cat("The number of trading days between 'today' and the 'future' are",
    nrow(data_3_d))

data_3_d$worse <- ifelse(data_3_d$Return < (-var_3_c/100),
                         1, 0)
cat("We expected to see", ceiling(0.05*(nrow(data_3_d))),
    "days with worse returns than the VaR.",
    "\n\nWe actualy saw",sum(data_3_d$worse), "days with worse returns than the VaR.")


# e)
par(mfrow = c(1,1))
data_3 <- na.omit(data_3)
plot(as.Date(rownames(data_3)), data_3$Return,
     main = "Return Time Series",
     xlab = "Date",
     ylab = "Return")
abline(h = -var_3_c/100, col = 'red')

## MC 1.5
load("C:/WU/SBWL Finance/Course 4 - Risk Management/HW1/Mini_Case_1_5.Rdata")
shares_vector <- as.vector(c(6000,4000,2000,1000,5000))
names(shares_vector) <- c("EBS","VER","VOE","ANDR","OMV")
returns <- prices

for (i in 2:6) {
  for (j in 2:nrow(prices)) {
    returns[j,i] <- (prices[j,i]-prices[j-1,i])/prices[j-1,i]
  }
}
returns <- returns[-1, ]

sigma <- var(returns[,2:6])

current_prices <- as.matrix(prices[nrow(prices), 2:6])

delta <- shares_vector*current_prices
w <- sum(delta)

volatility <- sqrt(as.vector(delta)%*%as.matrix(sigma)%*%t(t(as.vector(delta))))

var <- -volatility * qnorm(0.005)

# MC 1.6
load("C:/WU/SBWL Finance/Course 4 - Risk Management/HW1/Mini_Case_1_6.Rdata")
shares_vector <- as.vector(c(7000,12000,7000,4000,2000))
names(shares_vector) <- c("EBS","VER","VOE","ANDR","OMV")

returns <- prices
for (i in 2:6) {
  for (j in 2:nrow(prices)) {
    returns[j,i] <- (prices[j,i]-prices[j-1,i])/prices[j-1,i]
  }
}
returns <- returns[-1, ]

current_prices <- as.matrix(prices[nrow(prices), 2:6])

returns$portfolio <- rep(NA, nrow(returns))

for (i in 1:nrow(returns)) {
  returns$portfolio[i] <- sum(
    shares_vector["EBS"] * (1 + returns$EBS[i]) * current_prices[, "EBS"], 
    shares_vector["VER"] * (1 + returns$VER[i]) * current_prices[, "VER"],
    shares_vector["VOE"] * (1 + returns$VOE[i]) * current_prices[, "VOE"],
    shares_vector["ANDR"] * (1 + returns$ANDR[i]) * current_prices[, "ANDR"],
    shares_vector["OMV"]  * (1 + returns$OMV[i])  * current_prices[, "OMV"]
  )
}

current_values <- current_prices*shares_vector
current_portfolio <- sum(current_values)

returns$pnl <- returns$portfolio-current_portfolio

var_0.05 <- quantile(returns$pnl, probs = 0.05, type = 6)
var_0.01 <- quantile(returns$pnl, probs = 0.01, type = 6)

cat("VaR at 0.5%: \n", 
    -var_0.05,
    "\n\nVaR at 0.1%: \n", -var_0.01)

hist(returns$pnl, breaks = 60)
legend("topleft",
       legend = c("VaR at 5%", "VaR at 1%"),
       col = c("red", "green"),
       lty = 1, lwd = 2)
abline(v = var_0.05, col = 'red', lwd = 2)
abline(v = var_0.01, col = 'green', lwd = 2)

