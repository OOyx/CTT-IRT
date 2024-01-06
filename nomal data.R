#一维正态随机数

# 安装和加载truncnorm包
install.packages("truncnorm")
library(truncnorm)

# 生成2000个截尾正态分布的随机数
x <- rtruncnorm(n = 2000, a = 0, b = 100, mean = 60, sd = 15)
X <- round(x)

# 生成第二组随机数
Y <- rtruncnorm(n = 2000, a = 0, b = 100, mean = 65, sd = 17)
Y <- round(Y)


# 将数据整合成一个数据框
data <- data.frame(X, Y)

# 将数据保存为CSV文件
write.csv(data, "random_numbers.csv", row.names = FALSE)

#多维正态随机数
# 安装并加载tmvtnorm包
install.packages("tmvtnorm")
library(tmvtnorm)

# 设置均值向量
mu <- c(60, 12)

# 设置协方差矩阵
sigma <- matrix(c(15^2, 0, 0, 3^2), nrow = 2)

# 设置截断区间
lower <- c(0, 0)
upper <- c(100, 20)

# 生成100个二维截尾正态分布的随机数
xv <- rtmvnorm(n = 2000, mean = mu, sigma = sigma, lower = lower, upper = upper)
xv<-round(xv)
xv
# 查看随机数的统计描述
summary(xv)