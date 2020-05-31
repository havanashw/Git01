rm(list = ls())
# 各种颜色都可以选！
library(RColorBrewer)
library(scales)
display.brewer.all(type = "all")
display.brewer.pal(8, "Spectral")
mycolors<-brewer.pal(8, "Spectral")
mycolors
##########################################################################
#--------------------------------PART ONE--------------------------------#
##########################################################################
# 读取包中湖北以及全国疫情数据
library(nCov2019)
x <- load_nCov2019()
data <- x[]
## 湖北数据
data_hubei <- subset(data, province == "Hubei", select = c(-country, -city))
## 全国数据
data_china <- subset(data, country == "China", select = c(-province, -city))
## 数据分组函数
library(dplyr)
group <- function(data){
  merge <- group_by(data, time)
  data_merge <- summarise(merge,
                     cum_confirm = sum(cum_confirm),
                     cum_heal = sum(cum_heal),
                     cum_dead = sum(cum_dead))
  return(data_merge)
}
hubei <- group(data_hubei)
china <- group(data_china)
## 调整后的湖北数据
hubei_adj <- hubei
hubei_adj[which(hubei_adj$time == '2020/02/05'),"cum_confirm"] <- hubei_adj[which(hubei_adj$time == '2020/02/05'),"cum_confirm"] + 3346
hubei_adj[which(hubei_adj$time == '2020/02/06'),"cum_confirm"] <- hubei_adj[which(hubei_adj$time == '2020/02/06'),"cum_confirm"] + 4666
hubei_adj[which(hubei_adj$time == '2020/02/07'),"cum_confirm"] <- hubei_adj[which(hubei_adj$time == '2020/02/07'),"cum_confirm"] + 5753
hubei_adj[which(hubei_adj$time == '2020/02/08'),"cum_confirm"] <- hubei_adj[which(hubei_adj$time == '2020/02/08'),"cum_confirm"] + 7503
hubei_adj[which(hubei_adj$time == '2020/02/09'),"cum_confirm"] <- hubei_adj[which(hubei_adj$time == '2020/02/09'),"cum_confirm"] + 8789
hubei_adj[which(hubei_adj$time == '2020/02/10'),"cum_confirm"] <- hubei_adj[which(hubei_adj$time == '2020/02/10'),"cum_confirm"] + 10382
hubei_adj[which(hubei_adj$time == '2020/02/11'),"cum_confirm"] <- hubei_adj[which(hubei_adj$time == '2020/02/11'),"cum_confirm"] + 12186
# 清洗及处理数据
treatdata <- function(data){
  data$cur_confirm <- data$cum_confirm - data$cum_heal - data$cum_dead
  data$new_confirm <- 0
  data$new_heal <- 0
  data$new_dead <- 0
  for (i in 2:(dim(data)[1])) {
    data$new_confirm[i] <- data$cum_confirm[i] - data$cum_confirm[i-1]
    data$new_heal[i] <- data$cum_heal[i] - data$cum_heal[i-1]
    data$new_dead[i] <- data$cum_dead[i] - data$cum_dead[i-1]
  }
  data$heal_rate <- 0
  data$dead_rate <- 0
  for (i in 2:(dim(data)[1])) {
    data$heal_rate[i] <- data$new_heal[i]/data$cur_confirm[i-1]
    data$dead_rate[i] <- data$new_dead[i]/data$cur_confirm[i-1]
  }
  data$E <- 0
  data$I <- 0
  for (i in 1:(dim(data)[1] - 10)) {
    data$E[i] <- sum(data$new_confirm[(i+1):(i+10)])
  }
  for (i in 1:(dim(data)[1] - 3)) {
    data$I[i] <- sum(data$new_confirm[(i+1):(i+3)])
  }
  return(data)
}
hubei <- treatdata(hubei)
china <- treatdata(china)
hubei_adj <- treatdata(hubei_adj)
##########################################################################
#--------------------------------PART TWO--------------------------------#
##########################################################################
# 一些疫情可视化
library(ggplot2)
library(scales)

## 全国疫情可视化
df <- summary(x)
df$cur_confirm <- df$cum_confirm - df$cum_heal - df$cum_dead
df1 <- df[min(which(df$time == '2020/01/15')):max(which(df$time == '2020/04/30')),]
df2 <- subset(df1, province != "Hubei")
## 各省累积确诊人数折线图除去湖北省
library(ggrepel)
ggplot(df2,aes(as.Date(time), cum_confirm, group=province, color=province)) +
  geom_point(shape=18, size=2) + geom_line() +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  geom_text_repel(aes(label=province), data=df2[df2$time == '2020/04/30', ], hjust=1) +
  theme_classic() + theme(legend.position='none') +
  labs(title = "Cumulative Confirmed Cases except for Hubei", 
       x = "Date", y = "Population")
## 各省每天确诊人数折线图除去湖北省
library(ggrepel)
ggplot(df2,aes(as.Date(time), cur_confirm, group=province, color=province)) +
  geom_point(shape=18, size=2) + geom_line() +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  geom_text_repel(aes(label=province), data=df2[df2$time == '2020/04/30', ], hjust=1) +
  theme_classic() + theme(legend.position='none') +
  labs(title = "Current Confirmed Cases except for Hubei", 
       x = "Date", y = "Population")

## 湖北省累积确诊人数
df_hubei <- hubei[which(hubei$time == '2020/01/10'):which(hubei$time == '2020/04/30'),]
ggplot(df_hubei,aes(as.Date(time), cum_confirm, group=1)) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  geom_point(shape=18, size=2, color="blue") + geom_line(linetype="dashed", color="red") +
  theme_classic() + theme(legend.position='none') +
  labs(title = "Cumulative Confirmed Cases in Hubei", 
       x = "Date", y = "Population")
## 湖北省每天确诊人数
ggplot(df_hubei,aes(as.Date(time), cur_confirm, group=1)) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  geom_point(shape=18, size=2, color="blue") + geom_line(linetype="dashed", color="red") +
  theme_classic() + theme(legend.position='none') +
  labs(title = "Current Confirmed Cases in Hubei", 
       x = "Date", y = "Population")
## 湖北省新增确诊人数
ggplot(df_hubei,aes(as.Date(time), new_confirm, group=1)) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  geom_point(shape=18, size=2, color="blue") + geom_line(linetype="dashed", color="red") +
  theme_classic() + theme(legend.position='none') +
  labs(title = "New Confirmed Cases in Hubei", 
       x = "Date", y = "Population")

## 将湖北累积确诊和当前确诊放到一张图上
p <- ggplot() +
  geom_point(data=df_hubei,aes(as.Date(time), cur_confirm, color="1")) +
  geom_point(data=df_hubei,aes(as.Date(time), cum_confirm, color="2")) +
  geom_point(data=df_hubei,aes(as.Date(time), cum_heal, color="3")) +
  geom_point(data=df_hubei,aes(as.Date(time), cum_dead, color="4")) +
  geom_line(data=df_hubei,aes(as.Date(time),cur_confirm,color="1")) +
  geom_line(data=df_hubei,aes(as.Date(time),cum_confirm,color="2")) +
  geom_line(data=df_hubei,aes(as.Date(time),cum_heal,color="3")) +
  geom_line(data=df_hubei,aes(as.Date(time),cum_dead,color="4")) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  scale_color_manual(name = "",
                     values = c("1"="#CC3300", "2"="#807DBA", "3"="#99CC99", "4"="#999999"), 
                     labels = c("current confirm", "cumulative confirm", "cumulative heal", "cumulative dead")) +
  theme_classic() + theme(legend.position="top") +
  labs(title = "Covid-19 Trend in Hubei", 
       x = "Date", y = "Population")
p

# 世界地图
require(nCov2019)
x <- load_nCov2019()
plot(x, date = "2020-01-20", continuous_scale=F)
plot(x, date = "2020-02-15", continuous_scale=F)
plot(x, date = "2020-03-05", continuous_scale=F)
plot(x, date = "2020-03-25", continuous_scale=F)

# 画中国地图
# remotes::install_github("GuangchuangYu/chinamap")
require(chinamap)
require(mapproj)
cn <- get_map_china()
cn$province <- trans_province(cn$province)
plot(x, region="china", chinamap=cn, continuous_scale=F, date = "2020-01-10", font.size=2.5)
plot(x, region="china", chinamap=cn, continuous_scale=F, date = "2020-01-20", font.size=2.5)
plot(x, region="china", chinamap=cn, continuous_scale=F, date = "2020-02-10", font.size=2.5)
plot(x, region="china", chinamap=cn, continuous_scale=F, date = "2020-02-20", font.size=2.5)
plot(x, region="china", chinamap=cn, continuous_scale=F, date = "2020-03-15", font.size=2.5)
plot(x, region="china", chinamap=cn, continuous_scale=F, date = "2020-03-25", font.size=2.5)
plot(x, region="china", chinamap=cn, continuous_scale=F, date = "2020-04-05", font.size=2.5)
plot(x, region="china", chinamap=cn, continuous_scale=F, date = "2020-04-20", font.size=2.5)

############################################################################
#--------------------------------PART THREE--------------------------------#
############################################################################

# 未调整的数据
## 估计死亡率和治愈率
start <- which(hubei$time == '2020/1/23')
end <- which(hubei$time == '2020/2/29')
hubei_est <- hubei[start:end,]
hubei_est$t <- seq(0, (end - start), 1)
## 将死亡率和治愈率为0的取周围两天数据的均值
mean0 <- function(data, rate){
  pos_dead <- which(data$dead_rate == 0)
  data$dead_rate[pos_dead] <- (data$dead_rate[pos_dead-1]+data$dead_rate[pos_dead+1])/2
  pos_heal <- which(data$heal_rate == 0)
  data$heal_rate[pos_heal] <- (data$heal_rate[pos_heal-1]+data$heal_rate[pos_heal+1])/2
  return(data)
}
hubei_est <- mean0(hubei_est)

## 估计死亡率
### 拟合
attach(hubei_est)
lm1_dead <- lm(dead_rate~t)
summary(lm1_dead)
# lm2_dead <- lm(dead_rate~t+I(t^2))
# summary(lm2_dead)
lm3_dead <- lm(log(dead_rate) ~ t)
summary(lm3_dead)
lm4_dead <- nls(dead_rate~a*t^b,start=list(a=1.3,b=0.5),trace = T)
summary(lm4_dead)

lm3_dead <- lm(log(dead_rate) ~ t)
summary(lm3_dead)
x=hubei_adj_est$t
lm4_dead<-nls(dead_rate~a*x^b,start=list(a=0.01,b=-0.3))
summary(lm4_dead)
f1=exp(lm3_dead$coefficients[1]+lm3_dead$coefficients[2]*x)
f=0.029400*x^-0.609829
sum((f-dead_rate)^2)
sum((f1-dead_rate)^2)


library(ggplot2)
library(ggrepel)
p <- ggplot(hubei_est, aes(t, dead_rate)) +
  geom_point(size=3) +
  geom_line(aes(t, fitted(lm1_dead),color='1')) +
  geom_text_repel(aes(t,0.027,label=paste("SSE","=",round(1,3),sep = " ")),
                  size=4,color="#3288BD",data=hubei_est[t == 1,]) +
  # geom_line(aes(t, fitted(lm2_dead),color='2')) +
  geom_line(aes(t, exp(fitted(lm3_dead)),color='3')) +
  geom_text_repel(aes(t,0.01,label=paste("SSE","=",round(1,3),sep = " ")),
                  size=4,color="#D53E4F",data=hubei_est[t == 15,]) +
  geom_line(aes(t, fitted(lm4_dead),color='4')) +
  geom_text_repel(aes(t,0.002,label=paste("SSE","=",round(1,3),sep = " ")),
                  size=4,color="black",data=hubei_est[t == 30,]) +
  scale_color_manual(name = "fit_method",
                     values = c("1"="#D53E4F","3"="black","4"="#3288BD"), 
                     labels = c("Linear Regression", "Exponential Regression", "Power Regression"))
# "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#E6F598" "#ABDDA4" "#66C2A5" "#3288BD"
p


# fit <- function(par,z){
#   a <- par[1]; b <- par[2]
#   t <- z[,1]
#   n <- z[,2]
#   tmp <- par[1]*t^{par[2]}
#   sumsq <- sum((n - tmp)^2)
# }
# y<-cbind(hubei_est$t,hubei_est$dead_rate)
# a.guess <- 1.3
# b.guess <- 0.5 
# par <- c(a.guess,b.guess)
# y.fit <- optim(par,fit,z=y)
# y.fit$par



### 画图
plot(t,dead_rate,
     main='Basic Scatter plot and Fitting Curve of DEAD RATE vs TIME',
     xlab='TIME',
     ylab='DEAD RATE',
     pch=19)
x <- t
yfit1 <- lm2_dead$coef[1] +lm2_dead$coef[2]*x +lm2_dead$coef[3]*I(x^2)
yfit2 <- exp(lm3_dead$coefficients[1]+lm3_dead$coefficients[2]*x)
abline(lm1_dead,col="#E41A1C",lwd=2,lty=1)
lines(x,yfit1,col="#377EB8",lwd=2,lty=2)
lines(x,yfit2,col= "#4DAF4A",lwd=2,lty=2)
legend("top", c("Linear Regression", "Quadratic Polynomial Regression","Exponential Regression"), 
       col = c("#E41A1C","#377EB8", "#4DAF4A"), lty = 1:3, lwd = rep(3,4), bg="grey", text.col = 1)

## 估计治愈率
### 拟合
lm1_heal <- lm(heal_rate~t)
summary(lm1_heal)
lm2_heal <- lm(heal_rate~t+I(t^2))
summary(lm2_heal)
lm3_heal <- lm(log(heal_rate) ~ t)
summary(lm3_heal)

### 画图
plot(t,heal_rate,
     pin = c(2,2),
     main='Basic Scatter plot and Fitting Curve of HEAL RATE vs TIME',
     xlab='TIME',
     ylab='HEAL RATE',
     pch=19)
yfit3 <- lm2_heal$coef[1] +lm2_heal$coef[2]*x +lm2_heal$coef[3]*I(x^2)
yfit4 <- exp(lm3_heal$coefficients[1]+lm3_heal$coefficients[2]*x)
abline(lm1_heal,col="#E41A1C",lwd=2,lty=1)
lines(x,yfit3,col="#377EB8",lwd=2,lty=2)
lines(x,yfit4,col= "#4DAF4A",lwd=2,lty=2)
legend("top", c("Linear Regression", "Quadratic Polynomial Regression","Exponential Regression"), 
       col = c("#E41A1C","#377EB8", "#4DAF4A"), lty = 1:3, lwd = rep(3,4), bg="grey", text.col = 1)
detach(hubei_est)

### 保存回归系数
dead_coef <- lm3_dead$coefficients
heal_coef <- lm3_heal$coefficients

###########################################################################
#--------------------------------PART FOUR--------------------------------#
###########################################################################

# 调整的数据
## 估计死亡率和治愈率
hubei_adj_est <- hubei_adj[start:end,]
hubei_adj_est$t <- seq(0, (end - start), 1)
hubei_adj_est <- mean0(hubei_adj_est)

## 估计死亡率
### 拟合
attach(hubei_adj_est)
lm1_dead <- lm(dead_rate~t)
summary(lm1_dead)
lm2_dead <- lm(dead_rate~t+I(t^2))
summary(lm2_dead)
lm3_dead <- lm(log(dead_rate) ~ t)
summary(lm3_dead)

### 画图
plot(t,dead_rate,
     main='Basic Scatter plot and Fitting Curve of DEAD RATE vs TIME',
     xlab='TIME',
     ylab='DEAD RATE',
     pch=19)
x <- t
yfit1 <- lm2_dead$coef[1] +lm2_dead$coef[2]*x +lm2_dead$coef[3]*I(x^2)
yfit2 <- exp(lm3_dead$coefficients[1]+lm3_dead$coefficients[2]*x)
abline(lm1_dead,col="#E41A1C",lwd=2,lty=1)
lines(x,yfit1,col="#377EB8",lwd=2,lty=2)
lines(x,yfit2,col="#4DAF4A",lwd=2,lty=2)
legend("top", c("Linear Regression", "Quadratic Polynomial Regression","Exponential Regression"), 
       col = c("#E41A1C","#377EB8", "#4DAF4A"), lty = 1:3, lwd = rep(3,4), bg="grey", text.col = 1)

## 估计治愈率
### 拟合
lm1_heal <- lm(heal_rate~t)
summary(lm1_heal)
lm2_heal <- lm(heal_rate~t+I(t^2))
summary(lm2_heal)
lm3_heal <- lm(log(heal_rate) ~ t)
summary(lm3_heal)

### 画图
plot(t,heal_rate,
     pin = c(2,2),
     main='Basic Scatter plot and Fitting Curve of HEAL RATE vs TIME',
     xlab='TIME',
     ylab='HEAL RATE',
     pch=19)
yfit3 <- lm2_heal$coef[1] +lm2_heal$coef[2]*x +lm2_heal$coef[3]*I(x^2)
yfit4 <- exp(lm3_heal$coefficients[1]+lm3_heal$coefficients[2]*x)
abline(lm1_heal,col="#E41A1C",lwd=2,lty=1)
lines(x,yfit3,col="#377EB8",lwd=2,lty=2)
lines(x,yfit4,col="#4DAF4A",lwd=2,lty=2)
legend("top", c("Linear Regression", "Quadratic Polynomial Regression","Exponential Regression"), 
       col = c("#E41A1C","#377EB8", "#4DAF4A"), lty = 1:3, lwd = rep(3,4), bg="grey", text.col = 1)
detach(hubei_adj_est)

### 保存回归系数
dead_adj_coef <- lm3_dead$coefficients
heal_adj_coef <- lm3_heal$coefficients

###########################################################################
#--------------------------------PART FIVE--------------------------------#
###########################################################################

# 估计covid_seir_model部分参数
# 未调整的数据
require(deSolve)
hubei$t <- seq(0, (dim(hubei)[1]-1), 1)
covid <- hubei[,c("time","t","cur_confirm","cum_heal","cum_dead")]
names(covid) <- c("day","time","Q","R","D")
## c-seir模型
c_seir <- function(t, x, parms){
  S <- x[1]
  P <- x[2]
  E <- x[3]
  Q <- x[4]
  I <- x[5]
  R <- x[6]
  D <- x[7]
  N <- x[8]
  with(as.list(parms),
       {
         dS <- -lambda1*S - beta*(E + I) - lambda2*S + (P*(1 - p1))/14
         dP <- lambda2*S - (P*(1 - p1))/(14) - P*alpha*p1 + E*gamma
         dE <- beta*(E + I) - E*gamma - E*alpha
         dQ <- P*alpha*p1 + I*m - Q*(exp(heal_coef[1] + heal_coef[2]*t)) - Q*(exp(dead_coef[1]+dead_coef[2]*t))
         dI <- E*alpha - I*m - I*d2
         dR <- Q*(exp(heal_coef[1] + heal_coef[2]*t))
         dD <- Q*(exp(dead_coef[1]+dead_coef[2]*t)) + I*d2
         dN <- dS + dP + dE + dQ + dI + dR + dD
         res <- c(dS,dP,dE,dQ,dI,dR,dD,dN)
         list(res)
       })
}

## 拟合实际数据
start <- which(hubei$time == '2020/1/23')
end <- which(hubei$time == '2020/3/31')
time <- seq(0, end-start, 1)
R <- covid$R[start:end]
D <- covid$D[start:end]
Q <- covid$Q[start:end]

## 已知初始参数
para_init <- hubei[start,]

## 构造似然函数估计参数
require(bbmle)
### likelihood function
sirLL <- function(llambda1,llambda2,lp1,lgamma,lbeta,ld2) {
  parms <- c(lambda1 = plogis(llambda1),
             lambda2 = plogis(llambda2),
             p1 = plogis(lp1),
             gamma = plogis(lgamma),
             beta = plogis(lbeta),
             alpha = 1/7,
             m = 1/3,
             d2 = plogis(ld2))
  x0 <- c(S = 59170000,
          P = 3000,
          E = para_init$E,
          Q = para_init$cur_confirm,
          I = para_init$I,
          R = para_init$cum_heal,
          D = para_init$cum_dead,
          N = 59170000)
  out <- ode(y=x0, time, c_seir, parms)
  colnames(out) <- c("time","S","P","E","Q","I","R","D","N")
  SD_R <- sqrt(sum( (R-out[,"R"])^2)/length(time) )
  SD_Q <- sqrt(sum( (Q-out[,"Q"])^2)/length(time) )
  SD_D <- sqrt(sum( (D-out[,"D"])^2)/length(time) )
  weight <- c(2, 5, 3)
  likehood <- -weight[1]*sum(dnorm(R, mean=out[,"R"], sd=SD_R, log=TRUE))+
    -weight[2]*sum(dnorm(Q, mean=out[,"Q"], sd=SD_Q, log=TRUE))+
    -weight[3]*sum(dnorm(D, mean=out[,"D"], sd=SD_D, log=TRUE))
  return(likehood)
}

### minimize negative-log-likelihood
fit <- mle2(sirLL, 
            start = list(llambda1=qlogis(1e-2),
                         llambda2=qlogis(1e-3),
                         lp1=qlogis(1e-2),
                         lgamma=qlogis(1e-1),
                         lbeta=qlogis(1e-1),
                         ld2=qlogis(1e-3)),
            method="Nelder-Mead",
            control=list(maxit=1E5,trace=2),
            trace=FALSE)

summary(fit)
# 参数估计结果
theta <- as.numeric(c(plogis(coef(fit)[1:6])))
theta_df <- data.frame('lambda1' = theta[1],
                          'lambda2' = theta[2],
                          'p1' = theta[3],
                          'gamma' = theta[4],
                          'beta' = theta[5],
                          'alpha' = 1/7,
                          'm' = 1/3,
                          'd2' = theta[6])
theta_df
write.csv(theta_df,"initial_theta.csv",row.names = F)
## 与实际对比
parms <- c(lambda1 = theta[1],
           lambda2 = theta[2],
           p1 = theta[3],
           gamma = theta[4],
           beta = theta[5],
           alpha = 1/7,
           m = 1/3,
           d2 = theta[6])
times <- time
# times <- seq(0,100,1)
x0 <- c(S = 59170000,
        P = 3000,
        E = para_init$E,
        Q = para_init$cur_confirm,
        I = para_init$I,
        R = para_init$cum_heal,
        D = para_init$cum_dead,
        N = 59170000)
stateMatrix1 <- ode(y=x0, times, c_seir, parms)
colnames(stateMatrix1) <- c("time","S","P","E","Q","I","R","D","N")

df_est <- as.data.frame(stateMatrix1)
df_real <- covid[start:(start+length(times)-1),]
df_est$day <- df_real$day

p <- ggplot() +
  geom_point(data=df_real,aes(as.Date(day),R,color="green")) +
  geom_line(data=df_est,aes(as.Date(day),R,linetype="dashed")) +
  scale_x_date(breaks=date_breaks("3 days"), labels=date_format("%m/%d")) +
  theme_classic() + 
  scale_color_manual(name = "",
                     values = c("green" = "green"), 
                     breaks = c("green"),
                     labels = c("cum_heal")) +
  scale_linetype_manual(name = "",
                        values = c("dashed" = "dashed"), 
                        breaks = c("dashed"),
                        labels = c("fit_cum_heal")) +
  labs(title = "Fit the Cumulative Heal", 
       x = "Date", y = "Population")
p

p <- ggplot() +
  geom_point(data=df_real,aes(as.Date(day),Q,color="red")) +
  geom_line(data=df_est,aes(as.Date(day),Q,linetype="twodash")) +
  scale_x_date(breaks=date_breaks("3 days"), labels=date_format("%m/%d")) +
  theme_classic() + 
  scale_color_manual(name = "",
                     values = c("red" = "red"), 
                     breaks = c("red"),
                     labels = c("cur_confirm")) +
  scale_linetype_manual(name = "",
                        values = c("twodash" = "twodash"), 
                        breaks = c("twodash"),
                        labels = c("fit_cur_confirm")) +
  labs(title = "Fit the Current Confirm", 
       x = "Date", y = "Population")
p

p <- ggplot() +
  geom_point(data=df_real,aes(as.Date(day),D,color="black")) +
  geom_line(data=df_est,aes(as.Date(day),D,linetype="longdash")) +
  scale_x_date(breaks=date_breaks("3 days"), labels=date_format("%m/%d")) +
  theme_classic() + 
  scale_color_manual(name = "",
                     values = c("black" = "black"), 
                     breaks = c("black"),
                     labels = c("cum_dead")) +
  scale_linetype_manual(name = "",
                        values = c("longdash" = "longdash"), 
                        breaks = c("longdash"),
                        labels = c("fit_cum_dead")) +
  labs(title = "Fit the Cumulative Dead", 
       x = "Date", y = "Population")
p
## 将三个图合并在一起
p <- ggplot() +
  geom_point(data=df_real,aes(as.Date(day),R,color='a')) +
  geom_line(data=df_est,aes(as.Date(day),R,linetype="1")) +
  geom_point(data=df_real,aes(as.Date(day),Q,color='b')) +
  geom_line(data=df_est,aes(as.Date(day),Q,linetype="2")) +
  geom_point(data=df_real,aes(as.Date(day),D,color='c')) +
  geom_line(data=df_est,aes(as.Date(day),D,linetype="3")) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  theme_classic() + 
  scale_color_manual(name = "",
                     values=c('a'='#99CC99','b'='#CC3300','c'='#999999'),
                     labels = c("cum_heal","cur_confirm","cum_dead")) +
  scale_linetype_manual(name = "",
                        values = c("1"="solid","2"="twodash","3"="longdash"),
                        labels = c("fit_cum_heal","fit_cur_confirm","fit_cum_dead")) +
  labs(title = "Fit the Covid-19 Trend", 
       x = "Date", y = "Population")
p

##########################################################################
#--------------------------------PART SIX--------------------------------#
##########################################################################

# 估计covid_seir_model部分参数
# 调整的数据
require(deSolve)
hubei_adj$t <- seq(0, (dim(hubei_adj)[1]-1), 1)
covid <- hubei_adj[,c("time","t","cur_confirm","cum_heal","cum_dead")]
names(covid) <- c("day","time","Q","R","D")

# 数据调整前后对比
df_hubei <- hubei[which(hubei$time == '2020/01/10'):which(hubei$time == '2020/04/30'),]
df_hubei_adj <- hubei_adj[which(hubei$time == '2020/01/10'):which(hubei$time == '2020/04/30'),]
p <- ggplot() +
  geom_point(data=df_hubei,aes(as.Date(time), cur_confirm, color="1")) +
  geom_point(data=df_hubei_adj,aes(as.Date(time), cur_confirm, color="2")) +
  geom_line(data=df_hubei,aes(as.Date(time),cur_confirm,color="1")) +
  geom_line(data=df_hubei_adj,aes(as.Date(time),cur_confirm,color="2")) +
  scale_x_date(breaks=date_breaks("7 days"), labels=date_format("%m/%d")) +
  scale_color_manual(name = "",
                     values = c("1"="#CC3300", "2"="#FDD0A2"), 
                     labels = c("initial data", "adjusted data")) +
  theme_classic() + theme(legend.position="top") +
  labs(title = "Current Confirm (Initial Data vs Adjusted Data)", 
       x = "Date", y = "Population")
p


## c-seir模型
c_seir <- function(t, x, parms){
  S <- x[1]
  P <- x[2]
  E <- x[3]
  Q <- x[4]
  I <- x[5]
  R <- x[6]
  D <- x[7]
  N <- x[8]
  with(as.list(parms),
       {
         dS <- -lambda1*S - beta*(E + I) - lambda2*S + (P*(1 - p1))/14
         dP <- lambda2*S - (P*(1 - p1))/(14) - P*alpha*p1 + E*gamma
         dE <- beta*(E + I) - E*gamma - E*alpha
         dQ <- P*alpha*p1 + I*m - Q*(exp(heal_adj_coef[1] + heal_adj_coef[2]*t)) - Q*(exp(dead_adj_coef[1]+dead_adj_coef[2]*t))
         dI <- E*alpha - I*m - I*d2
         dR <- Q*(exp(heal_adj_coef[1] + heal_adj_coef[2]*t))
         dD <- Q*(exp(dead_adj_coef[1]+dead_adj_coef[2]*t)) + I*d2
         dN <- dS + dP + dE + dQ + dI + dR + dD
         res <- c(dS,dP,dE,dQ,dI,dR,dD,dN)
         list(res)
       })
}

## 拟合实际数据
start <- which(hubei_adj$time == '2020/1/23')
end <- which(hubei_adj$time == '2020/3/31')
time <- seq(0, end-start, 1)
R <- covid$R[start:end]
D <- covid$D[start:end]
Q <- covid$Q[start:end]

## 已知初始参数
para_init <- hubei_adj[start,]

## 构造似然函数估计参数
require(bbmle)
### likelihood function
sirLL <- function(llambda1,llambda2,lp1,lgamma,lbeta,ld2) {
  parms <- c(lambda1 = plogis(llambda1),
             lambda2 = plogis(llambda2),
             p1 = plogis(lp1),
             gamma = plogis(lgamma),
             beta = plogis(lbeta),
             alpha = 1/7,
             m = 1/3,
             d2 = plogis(ld2))
  x0 <- c(S = 59170000,
          P = 3000,
          E = para_init$E,
          Q = para_init$cur_confirm,
          I = para_init$I,
          R = para_init$cum_heal,
          D = para_init$cum_dead,
          N = 59170000)
  out <- ode(y=x0, time, c_seir, parms)
  colnames(out) <- c("time","S","P","E","Q","I","R","D","N")
  SD_R <- sqrt(sum( (R-out[,"R"])^2)/length(time) )
  SD_Q <- sqrt(sum( (Q-out[,"Q"])^2)/length(time) )
  SD_D <- sqrt(sum( (D-out[,"D"])^2)/length(time) )
  weight <- c(0, 1, 0)
  likehood <- -weight[1]*sum(dnorm(R, mean=out[,"R"], sd=SD_R, log=TRUE))+
    -weight[2]*sum(dnorm(Q, mean=out[,"Q"], sd=SD_Q, log=TRUE))+
    -weight[3]*sum(dnorm(D, mean=out[,"D"], sd=SD_D, log=TRUE))
  return(likehood)
}

### minimize negative-log-likelihood
fit <- mle2(sirLL, 
            start = list(llambda1=qlogis(1e-3),
                         llambda2=qlogis(1e-2),
                         lp1=qlogis(1e-1),
                         lgamma=qlogis(1e-2),
                         lbeta=qlogis(1e-1),
                         ld2=qlogis(1e-5)),
            method="Nelder-Mead",
            control=list(maxit=1E5,trace=2),
            trace=FALSE)

summary(fit)
# 参数估计结果
theta <- as.numeric(c(plogis(coef(fit)[1:6])))
theta_df1 <- data.frame('lambda1' = theta[1],
                       'lambda2' = theta[2],
                       'p1' = theta[3],
                       'gamma' = theta[4],
                       'beta' = theta[5],
                       'alpha' = 1/7,
                       'm' = 1/3,
                       'd2' = theta[6])
theta_df2 <- data.frame('lambda1' = theta[1],
                        'lambda2' = theta[2],
                        'p1' = theta[3],
                        'gamma' = theta[4],
                        'beta' = theta[5],
                        'alpha' = 1/7,
                        'm' = 1/3,
                        'd2' = theta[6])
theta_df3 <- data.frame('lambda1' = theta[1],
                        'lambda2' = theta[2],
                        'p1' = theta[3],
                        'gamma' = theta[4],
                        'beta' = theta[5],
                        'alpha' = 1/7,
                        'm' = 1/3,
                        'd2' = theta[6])
theta_df1
theta_df2
theta_df3
write.csv(theta_df3,"adjust_theta3.csv",row.names = F)
## 与实际对比
parms <- c(lambda1 = theta[1],
           lambda2 = theta[2],
           p1 = theta[3],
           gamma = theta[4],
           beta = theta[5],
           alpha = 1/7,
           m = 1/3,
           d2 = theta[6])
# times <- time
times <- seq(0,75,1)
x0 <- c(S = 59170000,
        P = 3000,
        E = para_init$E,
        Q = para_init$cur_confirm,
        I = para_init$I,
        R = para_init$cum_heal,
        D = para_init$cum_dead,
        N = 59170000)
stateMatrix1 <- ode(y=x0, times, c_seir, parms)
colnames(stateMatrix1) <- c("time","S","P","E","Q","I","R","D","N")
stateMatrix2 <- ode(y=x0, times, c_seir, parms)
colnames(stateMatrix2) <- c("time","S","P","E","Q","I","R","D","N")
stateMatrix3 <- ode(y=x0, times, c_seir, parms)
colnames(stateMatrix3) <- c("time","S","P","E","Q","I","R","D","N")

df_est1 <- as.data.frame(stateMatrix1)
df_est2 <- as.data.frame(stateMatrix2)
df_est3 <- as.data.frame(stateMatrix3)
df_est <- subset(df_est1, select = c(time, R))
df_est$Q <- df_est2$Q
df_est$D <- df_est3$D
df_real <- covid[start:(start+length(times)-1),]
df_est$day <- df_real$day

p <- ggplot() +
  geom_point(data=df_real,aes(as.Date(day),R,color="green")) +
  geom_line(data=df_est,aes(as.Date(day),R,linetype="dashed")) +
  scale_x_date(breaks=date_breaks("3 days"), labels=date_format("%m/%d")) +
  theme_classic() + 
  scale_color_manual(name = "",
                     values = c("green" = "green"), 
                     breaks = c("green"),
                     labels = c("cum_heal")) +
  scale_linetype_manual(name = "",
                        values = c("dashed" = "dashed"), 
                        breaks = c("dashed"),
                        labels = c("fit_cum_heal")) +
  labs(title = "Fit the Cumulative Heal", 
       x = "Date", y = "Population")
p

p <- ggplot() +
  geom_point(data=df_real,aes(as.Date(day),Q,color="red")) +
  geom_line(data=df_est,aes(as.Date(day),Q,linetype="twodash")) +
  scale_x_date(breaks=date_breaks("3 days"), labels=date_format("%m/%d")) +
  theme_classic() + 
  scale_color_manual(name = "",
                     values = c("red" = "red"), 
                     breaks = c("red"),
                     labels = c("cur_confirm")) +
  scale_linetype_manual(name = "",
                        values = c("twodash" = "twodash"), 
                        breaks = c("twodash"),
                        labels = c("fit_cur_confirm")) +
  labs(title = "Fit the Current Confirm", 
       x = "Date", y = "Population")
p

p <- ggplot() +
  geom_point(data=df_real,aes(as.Date(day),D,color="black")) +
  geom_line(data=df_est,aes(as.Date(day),D,linetype="longdash")) +
  scale_x_date(breaks=date_breaks("3 days"), labels=date_format("%m/%d")) +
  theme_classic() + 
  scale_color_manual(name = "",
                     values = c("black" = "black"), 
                     breaks = c("black"),
                     labels = c("cum_dead")) +
  scale_linetype_manual(name = "",
                        values = c("longdash" = "longdash"), 
                        breaks = c("longdash"),
                        labels = c("fit_cum_dead")) +
  labs(title = "Fit the Cumulative Dead", 
       x = "Date", y = "Population")
p

## 将三个图合并在一起
df_real$class <- "f"
df_real$class[which(df_real$day=="2020-04-01"):dim(df_real)[1]] <- "p"
df_real$class1 <- "f1"
df_real$class1[which(df_real$day=="2020-04-01"):dim(df_real)[1]] <- "p"
df_real$class2 <- "f2"
df_real$class2[which(df_real$day=="2020-04-01"):dim(df_real)[1]] <- "p"
p <- ggplot() +
  geom_point(data=df_real,aes(as.Date(day),R,color=class)) +
  geom_line(data=df_est,aes(as.Date(day),R,linetype="solid")) +
  geom_point(data=df_real,aes(as.Date(day),Q,color=class1)) +
  geom_line(data=df_est,aes(as.Date(day),Q,linetype="twodash")) +
  geom_point(data=df_real,aes(as.Date(day),D,color=class2)) +
  geom_line(data=df_est,aes(as.Date(day),D,linetype="longdash")) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  geom_text_repel(aes(as.Date(day),R-30000,label="predict"), data=df_real[df_real$day == '2020/03/31', ], hjust=-1) +
  geom_text_repel(aes(as.Date(day),max(Q),label=paste("PEAK",":",day,sep = " ")), data=df_real[which.max(df_real$Q),], hjust=2) +
  geom_text_repel(aes(as.Date(day),max(Q),label=paste("PEAK",":",day,sep = " ")), data=df_est[which.max(df_est$Q),], hjust=2) +
  theme_classic() + 
  geom_vline(xintercept = as.Date("2020-03-31"),linetype="dashed",size=1.5) +
  scale_color_manual(name = "",
                     values=c('#99CC99','#CC3300','#999999','#CCCC66'),
                     labels = c("cum_heal","cur_confirm","cum_dead","predict")) +
  scale_linetype_manual(name = "",
                        values = c("solid"="solid","twodash"="twodash","longdash"="longdash"),
                        labels = c("fit_cum_heal","fit_cur_confirm","fit_cum_dead")) +
  labs(title = "Fit the Covid-19 Trend", 
       x = "Date", y = "Population")
p

############################################################################
#--------------------------------PART SEVEN--------------------------------#
############################################################################

# 敏感性分析
## lambda1
df_Q <- data.frame(time = time)
for (i in 1:5) {
  parms <- c(lambda1 = theta_df2$lambda1*(1+0.05*(i-3)),
             lambda2 = theta_df2$lambda2,
             p1 = theta_df2$p1,
             gamma = theta_df2$gamma,
             beta = theta_df2$beta,
             alpha = 1/7,
             m = 1/3,
             d2 = theta_df2$d2)
  times <- time
  x0 <- c(S = 59170000,
          P = 3000,
          E = para_init$E,
          Q = para_init$cur_confirm,
          I = para_init$I,
          R = para_init$cum_heal,
          D = para_init$cum_dead,
          N = 59170000)
  stateMatrix <- ode(y=x0, times, c_seir, parms)
  colnames(stateMatrix) <- c("time","S","P","E","Q","I","R","D","N")
  df_Q[,i+1] <- stateMatrix[,"Q"]
}
df_Q$day <- covid$day[start:(start+length(times)-1)]
p <- ggplot() +
  geom_line(data=df_Q,aes(as.Date(day),V2,color="1")) + 
  geom_line(data=df_Q,aes(as.Date(day),V3,color="2")) +
  geom_line(data=df_Q,aes(as.Date(day),V4,color="3")) +
  geom_line(data=df_Q,aes(as.Date(day),V5,color="4")) +
  geom_line(data=df_Q,aes(as.Date(day),V6,color="5")) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  theme_classic() +
  scale_color_manual(name = "",
                     values = c("1"="#D9D9D9","2"="#BDBDBD","3"="black","4"="#969696","5"="#737373"), 
                     labels = c("-10%","-5%","initial","+5%","+10%")) +
  labs(title = "Sensitivity Analysis about Current Confirm (lambda1)", 
       x = "Date", y = "Population")
p             

## lambda2
df_Q <- data.frame(time = time)
for (i in 1:5) {
  parms <- c(lambda1 = theta_df2$lambda1,
             lambda2 = theta_df2$lambda2*(1+0.05*(i-3)),
             p1 = theta_df2$p1,
             gamma = theta_df2$gamma,
             beta = theta_df2$beta,
             alpha = 1/7,
             m = 1/3,
             d2 = theta_df2$d2)
  times <- time
  x0 <- c(S = 59170000,
          P = 3000,
          E = para_init$E,
          Q = para_init$cur_confirm,
          I = para_init$I,
          R = para_init$cum_heal,
          D = para_init$cum_dead,
          N = 59170000)
  stateMatrix <- ode(y=x0, times, c_seir, parms)
  colnames(stateMatrix) <- c("time","S","P","E","Q","I","R","D","N")
  df_Q[,i+1] <- stateMatrix[,"Q"]
}
df_Q$day <- covid$day[start:(start+length(times)-1)]
p <- ggplot() +
  geom_line(data=df_Q,aes(as.Date(day),V2,color="1")) + 
  geom_line(data=df_Q,aes(as.Date(day),V3,color="2")) +
  geom_line(data=df_Q,aes(as.Date(day),V4,color="3")) +
  geom_line(data=df_Q,aes(as.Date(day),V5,color="4")) +
  geom_line(data=df_Q,aes(as.Date(day),V6,color="5")) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  theme_classic() +
  scale_color_manual(name = "",
                     values = c("1"="#D9D9D9","2"="#BDBDBD","3"="black","4"="#969696","5"="#737373"), 
                     labels = c("-10%","-5%","initial","+5%","+10%")) +
  labs(title = "Sensitivity Analysis about Current Confirm (lambda2)", 
       x = "Date", y = "Population")
p             

## gamma
df_Q <- data.frame(time = time)
for (i in 1:5) {
  parms <- c(lambda1 = theta_df2$lambda1,
             lambda2 = theta_df2$lambda2,
             p1 = theta_df2$p1,
             gamma = theta_df2$gamma*(1+0.05*(i-3)),
             beta = theta_df2$beta,
             alpha = 1/7,
             m = 1/3,
             d2 = theta_df2$d2)
  times <- time
  x0 <- c(S = 59170000,
          P = 3000,
          E = para_init$E,
          Q = para_init$cur_confirm,
          I = para_init$I,
          R = para_init$cum_heal,
          D = para_init$cum_dead,
          N = 59170000)
  stateMatrix <- ode(y=x0, times, c_seir, parms)
  colnames(stateMatrix) <- c("time","S","P","E","Q","I","R","D","N")
  df_Q[,i+1] <- stateMatrix[,"Q"]
}
df_Q$day <- covid$day[start:(start+length(times)-1)]
p <- ggplot() +
  geom_line(data=df_Q,aes(as.Date(day),V2,color="1")) + 
  geom_line(data=df_Q,aes(as.Date(day),V3,color="2")) +
  geom_line(data=df_Q,aes(as.Date(day),V4,color="3")) +
  geom_line(data=df_Q,aes(as.Date(day),V5,color="4")) +
  geom_line(data=df_Q,aes(as.Date(day),V6,color="5")) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  theme_classic() +
  scale_color_manual(name = "",
                     values = c("1"="#D9D9D9","2"="#BDBDBD","3"="black","4"="#969696","5"="#737373"), 
                     labels = c("-10%","-5%","initial","+5%","+10%")) +
  labs(title = "Sensitivity Analysis about Current Confirm (gamma)", 
       x = "Date", y = "Population")
p 

## beta
df_Q <- data.frame(time = time)
for (i in 1:5) {
  parms <- c(lambda1 = theta_df2$lambda1,
             lambda2 = theta_df2$lambda2,
             p1 = theta_df2$p1,
             gamma = theta_df2$gamma,
             beta = theta_df2$beta*(1+0.05*(i-3)),
             alpha = 1/7,
             m = 1/3,
             d2 = theta_df2$d2)
  times <- time
  x0 <- c(S = 59170000,
          P = 3000,
          E = para_init$E,
          Q = para_init$cur_confirm,
          I = para_init$I,
          R = para_init$cum_heal,
          D = para_init$cum_dead,
          N = 59170000)
  stateMatrix <- ode(y=x0, times, c_seir, parms)
  colnames(stateMatrix) <- c("time","S","P","E","Q","I","R","D","N")
  df_Q[,i+1] <- stateMatrix[,"Q"]
}
df_Q$day <- covid$day[start:(start+length(times)-1)]
p <- ggplot() +
  geom_line(data=df_Q,aes(as.Date(day),V2,color="1")) + 
  geom_line(data=df_Q,aes(as.Date(day),V3,color="2")) +
  geom_line(data=df_Q,aes(as.Date(day),V4,color="3")) +
  geom_line(data=df_Q,aes(as.Date(day),V5,color="4")) +
  geom_line(data=df_Q,aes(as.Date(day),V6,color="5")) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  theme_classic() +
  scale_color_manual(name = "",
                     values = c("1"="#D9D9D9","2"="#BDBDBD","3"="black","4"="#969696","5"="#737373"), 
                     labels = c("-10%","-5%","initial","+5%","+10%")) +
  labs(title = "Sensitivity Analysis about Current Confirm (beta)", 
       x = "Date", y = "Population")
p 

############################################################################
#--------------------------------PART EIGHT--------------------------------#
############################################################################
# 预测拐点
require(deSolve)
hubei$t <- seq(0, (dim(hubei)[1]-1), 1)
covid <- hubei[,c("time","t","cur_confirm","cum_heal","cum_dead")]
names(covid) <- c("day","time","Q","R","D")

## 拟合实际数据
start <- which(hubei_adj$time == '2020/1/15')
end <- which(hubei_adj$time == '2020/2/15')
time <- seq(0, end-start, 1)
R <- covid$R[start:end]
D <- covid$D[start:end]
Q <- covid$Q[start:end]

## 已知初始参数
para_init <- hubei_adj[start,]

## 构造似然函数估计参数
require(bbmle)
### likelihood function
sirLL <- function(llambda1,llambda2,lp1,lgamma,lbeta,ld2) {
  parms <- c(lambda1 = plogis(llambda1),
             lambda2 = plogis(llambda2),
             p1 = plogis(lp1),
             gamma = plogis(lgamma),
             beta = plogis(lbeta),
             alpha = 1/7,
             m = 1/3,
             d2 = plogis(ld2))
  x0 <- c(S = 59170000,
          P = 3000,
          E = para_init$E,
          Q = para_init$cur_confirm,
          I = para_init$I,
          R = para_init$cum_heal,
          D = para_init$cum_dead,
          N = 59170000)
  out <- ode(y=x0, time, c_seir, parms)
  colnames(out) <- c("time","S","P","E","Q","I","R","D","N")
  SD_R <- sqrt(sum( (R-out[,"R"])^2)/length(time) )
  SD_Q <- sqrt(sum( (Q-out[,"Q"])^2)/length(time) )
  SD_D <- sqrt(sum( (D-out[,"D"])^2)/length(time) )
  weight <- c(1, 5, 1)
  likehood <- -weight[1]*sum(dnorm(R, mean=out[,"R"], sd=SD_R, log=TRUE))+
    -weight[2]*sum(dnorm(Q, mean=out[,"Q"], sd=SD_Q, log=TRUE))+
    -weight[3]*sum(dnorm(D, mean=out[,"D"], sd=SD_D, log=TRUE))
  return(likehood)
}

### minimize negative-log-likelihood
fit <- mle2(sirLL, 
            start = list(llambda1=qlogis(0.044021106),
                         llambda2=qlogis(0.001570524),
                         lp1=qlogis(0.009755703),
                         lgamma=qlogis(0.042349204),
                         lbeta=qlogis(0.107274578),
                         ld2=qlogis(0.000001226)),
            method="Nelder-Mead",
            control=list(maxit=1E5,trace=2),
            trace=FALSE)

summary(fit)
# 参数估计结果
theta <- as.numeric(c(plogis(coef(fit)[1:6])))
theta_df <- data.frame('lambda1' = theta[1],
                        'lambda2' = theta[2],
                        'p1' = theta[3],
                        'gamma' = theta[4],
                        'beta' = theta[5],
                        'alpha' = 1/7,
                        'm' = 1/3,
                        'd2' = theta[6])
theta_df
write.csv(theta_df,"predict_theta.csv",row.names = F)
## 与实际对比
parms <- c(lambda1 = theta[1],
           lambda2 = theta[2],
           p1 = theta[3],
           gamma = theta[4],
           beta = theta[5],
           alpha = 1/7,
           m = 1/3,
           d2 = theta[6])
# times <- time
times <- seq(0,50,1)
x0 <- c(S = 59170000,
        P = 3000,
        E = para_init$E,
        Q = para_init$cur_confirm,
        I = para_init$I,
        R = para_init$cum_heal,
        D = para_init$cum_dead,
        N = 59170000)
stateMatrix <- ode(y=x0, times, c_seir, parms)
colnames(stateMatrix) <- c("time","S","P","E","Q","I","R","D","N")

df_est <- as.data.frame(stateMatrix)
df_real <- covid[start:(start+length(times)-1),]
df_est$day <- df_real$day

## 将三个图合并在一起
df_real$class <- "f"
df_real$class[which(df_real$day=="2020-02-16"):dim(df_real)[1]] <- "p"
df_real$class1 <- "f1"
df_real$class1[which(df_real$day=="2020-02-16"):dim(df_real)[1]] <- "p"
df_real$class2 <- "f2"
df_real$class2[which(df_real$day=="2020-02-16"):dim(df_real)[1]] <- "p"
p <- ggplot() +
  geom_point(data=df_real,aes(as.Date(day),Q,color=class1)) +
  geom_line(data=df_est,aes(as.Date(day),Q,linetype="1")) +
  scale_x_date(breaks=date_breaks("5 days"), labels=date_format("%m/%d")) +
  geom_text_repel(aes(as.Date(day),R,label="predict"), data=df_real[df_real$day == '2020/02/15', ], hjust=-1) +
  geom_vline(xintercept = as.Date("2020-02-15"),linetype="dashed",size=1.5) +
  geom_text_repel(aes(as.Date(day),max(Q),label=paste("PEAK",":",day,sep = " ")), data=df_real[which.max(df_real$Q),], hjust=2) +
  geom_text_repel(aes(as.Date(day),max(Q),label=paste("PEAK",":",day,sep = " ")), data=df_est[which.max(df_est$Q),], hjust=2) +
  theme_classic() + 
  scale_color_manual(name = "",
                     values=c('#CC3300','#CCCC66'),
                     labels = c("cur_confirm","predict")) +
  scale_linetype_manual(name = "",
                        values = c("1"="solid"),
                        labels = c("fit_cur_confirm")) +
  labs(title = "Forecast the Inflection Point", 
       x = "Date", y = "Population")
p