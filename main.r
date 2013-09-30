# --------- 步骤1： 随机向量发生：多变量正态分布。--------- 

# 维数，可以改变，比如为500
n = 2
library(mvtnorm)
# 协方差矩阵
s = matrix(rep(0.9,n),n,n)+diag(0.1,n,n)
m = diag(2,n,n)
set.seed(1)
# 这里我希望做一点儿更多各种个样的数据，所以改变
x <- rmvnorm(n=500, mean=c(rep(-4,n-1),0), sigma=s)
y <- rmvnorm(n=500, mean=rep(0,n), sigma=solve(s)/5)
z <- rmvnorm(n=500, mean=c(rep(4,n-1),0), sigma=s)
u <- rmvnorm(n=3000, mean=c(rep(10,n-1),2), sigma=m)
Data <- rbind(x,y,z,u)
# 如果用散点图看它，会看到好像“ ~o ”的二维的
plot(Data)
#（一共发生了4500个随机变量。1500对尾巴，3000对头）


# --------- 步骤2：特征提取：主成分分析。---------
X = data.frame(Data)
Y = princomp(X)
# 求出最高方差的数据的一维投影
Z <- as.matrix(X) %*% as.matrix(Y$loadings[, 1])
# 如果想看看这一个主成分的方差的比例(%)：
((Y$sd^2)[1]/sum(Y$sd^2))*100
plot(Z)

# --------- # 步骤3：参数分布拟合：矩法估计。---------

# 做分布拟合之前用 factor 区别这两个分类，我们只要一个：3000个“头”的向量。
f <- factor(c(rep(1, 1500), rep(2,3000)))
rez <- split(Z,f)
library(fitdistrplus)
# 假定rez$`2`（3000个“头”的向量）服从正态分布，因为我用正态分布发生器发生了这些数据。（+但是我们这里应该做正态性检验，我以后做）
# 用最大似然估计，做了分布拟合：
fg.mme <- fitdist(rez$`2`,'norm',method='mme') # 想用最大似然估计的话，用 method='mle' （+一定要用适合度检测，以后做）
# 可以发生新的观测：rnorm(100,fg.mme$estimate['mean'],fg.mme$estimate['sd'])
print(fg.mme)
a = seq(-15,-5,by=0.05)
b = dnorm(x, fg.mme$estimate['mean'],fg.mme$estimate['sd'])

# --------- 步骤4：非参数分布求出：核密度估计 ---------

# 假定rez$`2`（1500个”尾巴“的向量）服从什么不认识的分布。
# R有很有用的函数 density() ，它求出核密度估计：
fit <- density(rez$`1`,kernel='gaussian')
# 但是这个核密度估计没有概率密度函数。我发现了其实 fit <- density(x) 会给我 fit$x 和 fit$y 的值。这些 fit$y 就是核密度估计的向量。
plot(fit$x, fit$y)
# 那，我应用这核密度估计的向量，和相对应 fit$x 定义域的值的向量，写了个函数：
QL <- function(ft,x) {
    i = which.min(abs(ft$x - x))
    f = fit$y[i]
    return(f)
}
# 这个函数求出相对应和密度函数的函数参数 x 最近的 fit$x 的 fit$y 值，比如 QL(fit,0.5) = 相对应最近 x = 0.5 的 fit$x 值的 fit$y 值。
QL(fit,-5)

 
# --------- 步骤5：分类：贝叶斯分类器。---------

O = rmvnorm(n=1, mean=c(rep(6,n-1),0), sigma=s)
# 有了新的观测 O 之后实行特征提取：
o = as.matrix(O) %*% as.matrix(Y$loadings[, 1]); print(o)
 
# 为了应用贝叶斯规则，求出先验概率。假定先验概率等于相对次数：1500尾巴的向量/4500， 3000头的向量/4500，就是 1/3 和 2/3。
prior_A = 1500/4500
prior_B = 3000/4500
 
# 为了求出条件概率的值，计算相对应 o 的两个求出的（条件）密度函数的密度值：
density_A = QL(fit, o) # ”尾巴“为条件的时候的密度的值
density_B = dnorm(o, fg.mme$estimate['mean'],fg.mme$estimate['sd']) # ”头“的时候的密度的值
 
# 判定规则：选择最大的期望值的分类：
# 尾巴的期望值：
A = prior_A * density_A; print(A)
# 头的期望值：
B = prior_B * density_B; print(B)
# 判定：
if (A > B) { print("尾巴"); } else if (B > A) { print("头"); } else { print("不知到"); }
 
# 两个分布的密度的图：
plot(density(Z))
