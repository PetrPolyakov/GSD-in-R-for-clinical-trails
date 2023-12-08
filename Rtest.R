  library(gsDesign)
  library(BSDA)
alpha <- 0.05  # Уровень значимости
beta <- 0.20   # Ошибка II рода (мощность = 1 - beta)
gs_obj <- gsDesign(k=3, alpha=alpha, beta=beta, test.type=1)  
summary(gs_obj)
group1 <- c(1, 2, 3, 2, 1, 2, 3, 2, 1, 2, 3, 4, 3, 2, 4, 4, 2, 1, 2, 3, 4)
group2 <- c(3, 3, 5, 4, 3, 2, 3, 4, 4, 3, 2, 2, 3, 4, 3, 2, 3, 4, 4, 3)
t_test_result <- t.test(group1, group2, alternative = "two.sided")
z_score <- qt(1 - alpha / 2, t_test_result$parameter) 
gsProb_obj <- gsProbability(object=gs_obj, theta=z_score)
gsProb_obj <- gsProbability(k = 3,theta = z_score)
t_test_result <- t.test(group1, group2, alternative = "two.sided")

gsProb_obj <- gsProbability(gs_obj, theta=z_score)
p_value <- t_test_result$p.value
print(p_value)
cond_prob <- gsProbability()
if(!require('gsDesign')) {
  install.packages('gsDesign')
  library('gsDesign')
}
library(ggplot2)
#  symmetric, 2-sided design with O'Brien-Fleming-like boundaries
#  lower bound is non-binding (ignored in Type I error computation)
#  sample size is computed based on a fixed design requiring n=800
x <- gsDesign(k = 5, test.type = 2, n.fix = 800)

# note that "x" below is equivalent to print(x) and print.gsDesign(x)
x
plot(x)
plot(x, plottype = 2)

# Assuming after trial was designed actual analyses occurred after
# 300, 600, and 860 patients, reset bounds
y <- gsDesign(
  k = 3, test.type = 2, n.fix = 800, n.I = c(300, 600, 860),
  maxn.IPlan = x$n.I[x$k]
)
y

#  asymmetric design with user-specified spending that is non-binding
#  sample size is computed relative to a fixed design with n=1000
sfup <- c(.033333, .063367, .1)
sflp <- c(.25, .5, .75)
timing <- c(.1, .4, .7)
x <- gsDesign(
  k = 4, timing = timing, sfu = sfPoints, sfupar = sfup, sfl = sfPoints,
  sflpar = sflp, n.fix = 1000
)
x
plot(x)
plot(x, plottype = 2)

# same design, but with relative sample sizes
gsDesign(
  k = 4, timing = timing, sfu = sfPoints, sfupar = sfup, sfl = sfPoints,
  sflpar = sflp
)

sfup <- c(.033333, .063367, .1)
sflp <- c(.25, .5, .75)
timing <- c(0.1, 0.3, 0.5, 0.7, 0.8)
x <- gsDesign(
  k = 6, timing = timing, n.fix = 1000, alpha = 0.05, beta = 0.8, test.type = 2
)
x
plot(x)
point_x <- 800
point_y <- 2
points(point_x, point_y, col="red", pch=19)

x <- gsDesign(k = 4, test.type = 3, n.fix = 800 ,n.I = c(153,198,243,273))
x
plot(x)

library(gsDesign)
y <- gsDesign(
  k = 5, alpha = 0.025,beta = 0.1,test.type = 3, sfu="OF",  n.fix = 305, n.I = c(153, 198, 243,273,305))
y
plot(y)

x <- gsDesign(
  k = 5, alpha = 0.025,beta = 0.1,test.type = 3,  n.fix = 305, n.I = c(153, 198, 243,273,305))
x
plot(x)

z <- gsDesign(k=5, test.type=4,beta=0.1,   alpha=0.025,astar=0,delta=0, n.fix = 305, n.I = c(153, 198, 243,273,305), timing=1, sfu=sfHSD, sfupar=-4,sfl=sfHSD, sflpar=-2, tol=0.000001, r=18, maxn.IPlan = 0)
z
plot(z)

SA <- gsDesign(k=5, test.type=4,beta=0.1, alpha=0.025, n.fix = 269, n.I = c(153, 198, 243,273,305), sfu=sfLDOF, sfl=sfLDOF)
SA
plot(SA)

timing = c (0.5, 0.65, 0.8,0.9)
SA <- gsDesign(k=5, test.type=4,beta=0.1, alpha=0.025, n.fix = 269, delta = 1, timing = c (0.5, 0.65, 0.8,0.9), sfu=sfLDOF, sfl=sfLDOF)
SA
plot(SA)


install.packages('BSDA')
library(BSDA)


# Printing the results
x<-rnorm(78,mean=19,sd=2)
png(file = "rnormExample.png")
hist(x, breaks=50) 
dev.off()
shapiro.test(x)

# Чест сравнения двух частот
TPTR <- prop.test(x = c(490, 470), n = c(500, 500),p = NULL, alternative = "two.sided",correct = TRUE)
TPTR <- prop.test(x = c(70, 90), alternative = "two.sided", n = c(100, 100), correct = TRUE)
TPTR <- prop.test(x = c(70, 90), alternative = "less", n = c(100, 100), correct = TRUE)
TPTR <- prop.test(x = c(70, 90), alternative = "greater", n = c(100, 100), correct = TRUE)
TPTR
ZScore <- sqrt(TPTR$statistic)
PvalueOneSide <- TPTR$p.value/2
ZScore
PvalueOneSide

# SA для сравнения двух количественных данных
GSDTSMT <- gsDesign(k=5, beta=0.1,alpha=0.025, test.type=4,n.fix = 276,  timing = c (0.5, 0.65, 0.8,0.9), sfu=sfLDOF, sfl=sfLDOF)
GSDTSMT <- gsDesign(k=18, beta=0.1,alpha=0.025, test.type=4,   sfu=sfLDOF, sfl=sfLDOF)


ZE <- GSDTSMT$upper$bound
ZF <- GSDTSMT$lower$bound
PVE<-pnorm(q=ZE, lower.tail = FALSE)
PVF<-pnorm(q=ZF, lower.tail = FALSE)
NS<-GSDTSMT$n.I
NS
PVE
PVF
plot(GSDTSMT)
# Ваши выборки
sample1<-rnorm(78,mean=3,sd=2)
sample2<-rnorm(78,mean=2,sd=3) 
SD1<-sd(sample1)
SD1
SD2<-sd(sample2)
SD2



sample1 <- c(20,	19,	20,	20,	22,	19,	21,	18,	18,	18,	20,	20,	18,	19,	21,	18,	19,	18,	17,	18,	19,	21,	20,	21,	16,	18,	22,	19,	18,	16,	16,	20,	19,	18,	18,	18,	17,	15,	19,	19,	18,	20,	18,	17,	19,	19,	20,	17,	20,	18,	19,	21,	19,	17,	19,	18,	18,	21,	17,	18,	17,	19,	16,	16,	19,	21,	18,	20,	22,	18,	17,	18,	19,	19,	17,	21,	18,	18,	15,	19,	17,	21,	20,	18,	20,	20,	17,	17,	18,	18,	17,	19,	15,	20,	17,	20,	22,	16,	19,	17,	16,	17,	20,	18,	20,	20,	19,	20,	20,	21,	18,	22,	18,	19,	20,	21,	20,	18,	20,	20,	17,	19,	19,	17,	19,	18,	22,	18,	17,	17,	20,	18,	22,	19,	20,	22,	19,	21,	22,	16,	18,	20,	20,	20,	21,	22,	18,	23,	21,	21,	17,	18,	15,	20,	21,	18,	18
)
sample2 <- c(19,	15,	12,	19,	10,	16,	17,	24,	13,	21,	15,	15,	15,	19,	23,	17,	20,	14,	18,	19,	17,	17,	16,	20,	22,	21,	16,	19,	13,	18,	26,	19,	13,	25,	20,	19,	16,	17,	20,	19,	20,	14,	16,	12,	9,	20,	16,	18,	21,	25,	19,	23,	18,	15,	16,	15,	22,	20,	18,	20,	14,	22,	20,	15,	17,	18,	22,	16,	16,	17,	17,	20,	20,	18,	18,	20,	14,	19,	13,	18,	19,	17,	21,	19,	19,	18,	20,	17,	19,	15,	27,	18,	17,	20,	16,	19,	15,	13,	18,	20,	14,	22,	14,	13,	18,	15,	21,	13,	19,	22,	19,	23,	13,	20,	17,	17,	18,	15,	22,	17,	16,	21,	20,	18,	17,	14,	14,	19,	17,	19,	17,	13,	19,	17,	15,	17,	18,	18,	21,	17,	16,	19,	13,	17,	17,	17,	22,	24,	15,	23,	17,	16,	16,	16,	18,	18,	13)
        

ST_sample1 <- shapiro.test(sample1)
ST_sample2 <- shapiro.test(sample2)
ST_sample1$p.value
ST_sample2$p.value
# Нормализация
sample1_log = log(as.data.frame(sample1))
sample2_log = log(as.data.frame(sample2))
sample1_log <- shapiro.test(sample1_log$sample1)
sample2_log <- shapiro.test(sample2_log$sample2)
sample1_log$p.value
sample2_log$p.value 

hist(sample1, breaks=50) 
hist(sample2, breaks=50) 
hist(log_scale_sample1$sample1, breaks=50) 
hist(log_scale_sample2$sample2, breaks=50) 

sample2_log

SD1<-sd(sample1)
SD1
SD2<-sd(sample2)
SD2

SD1LOG<-sd(sample1_log$sample1)
SD1LOG
SD2LOG<-sd(sample2_log$sample2)
SD2LOG



sample1 <- c(20,	19,	20,	20,	22,	19,	21,	18,	18,	18,	20,	20,	18,	19,	21,	18,	19,	18,	17,	18,	19,	21,	20,	21,	16,	18,	22,	19,	18,	16,	16,	20,	19,	18,	18,	18,	17,	15,	19,	19,	18,	20,	18,	17,	19,	19,	20,	17,	20,	18,	19,	21,	19,	17,	19,	18,	18,	21,	17,	18,	17,	19,	16,	16,	19,	21,	18,	20,	22,	18,	17,	18,	19,	19,	17,	21,	18,	18,	15,	19,	17,	21,	20,	18,	20,	20,	17,	17,	18,	18,	17,	19,	15,	20,	17,	20,	22,	16,	19,	17,	16,	17,	20,	18,	20,	20,	19,	20,	20,	21,	18,	22,	18,	19,	20,	21,	20,	18,	20,	20,	17,	19,	19,	17,	19,	18,	22,	18,	17,	17,	20,	18,	22,	19,	20,	22,	19,	21,	22,	16,	18,	20,	20,	20,	21,	22,	18,	23,	21,	21,	17,	18,	15,	20,	21,	18,	18
)
sample2 <- c(19,	15,	12,	19,	10,	16,	17,	24,	13,	21,	15,	15,	15,	19,	23,	17,	20,	14,	18,	19,	17,	17,	16,	20,	22,	21,	16,	19,	13,	18,	26,	19,	13,	25,	20,	19,	16,	17,	20,	19,	20,	14,	16,	12,	9,	20,	16,	18,	21,	25,	19,	23,	18,	15,	16,	15,	22,	20,	18,	20,	14,	22,	20,	15,	17,	18,	22,	16,	16,	17,	17,	20,	20,	18,	18,	20,	14,	19,	13,	18,	19,	17,	21,	19,	19,	18,	20,	17,	19,	15,	27,	18,	17,	20,	16,	19,	15,	13,	18,	20,	14,	22,	14,	13,	18,	15,	21,	13,	19,	22,	19,	23,	13,	20,	17,	17,	18,	15,	22,	17,	16,	21,	20,	18,	17,	14,	14,	19,	17,	19,	17,	13,	19,	17,	15,	17,	18,	18,	21,	17,	16,	19,	13,	17,	17,	17,	22,	24,	15,	23,	17,	16,	16,	16,	18,	18,	13)
sample1_log = log(as.data.frame(sample1)) #лог-преобразование (нормализация) данных первой группы
sample2_log = log(as.data.frame(sample2)) #лог-преобразование (нормализация) данных второй группы
SD1LOG<-sd(sample1_log$sample1) #расчёт среднеквадратичного отклонения для данных первой группы
SD2LOG<-sd(sample2_log$sample2) #расчёт среднеквадратичного отклонения для данных второй группы
result <- z.test(x=sample1_log$sample1,y= sample2_log$sample2, alternative = "two.sided", mu = 0,sigma.x=SD1LOG, sigma.y=SD2LOG ) #применения Z-теста для сравнения первой и второй группы
result #вывод результатов Z-теста
PvalueOneSide <- result$p.value/2 #присвоение переменной PvalueOneSide уровня p-value (односторонний)
PvalueOneSide #вывод уровня p-value (односторонний)

#тест Мана Уитни
result <- wilcox.test(sample1, sample2, alternative = c("two.sided"), paired = FALSE) #применения U-критерий Манна-Уитни для сравнения первой и второй группы
PvalueOneSide <-result$p.value/2 #присвоение переменной PvalueOneSide уровня p-value (односторонний)
PvalueOneSide#вывод уровня p-value (односторонний)


# Выполнить Z-тест

result <- z.test(x=sample1,y= sample2,  alternative = "two.sided",mu = 0,sigma.x=SD1, sigma.y=SD2 )
result
result$p.value
PvalueOneSide <- result$p.value/2
PvalueOneSide

result <- z.test(x=sample1,y= sample2,  alternative = "greater",mu = 0,sigma.x=SD1, sigma.y=SD2 )
result
result$p.value

result <- z.test(x=sample1_log$sample1,y= sample2_log$sample2,  alternative = "two.sided",mu = 0,sigma.x=SD1LOG, sigma.y=SD2LOG )
result
result$p.value


# Выполнить Z-тест ОБРАТНЫЙ
result21 <- z.test(x=sample2,y= sample1, mu = 0,sigma.x=SD2, sigma.y=SD1 )
result21$p.value


resultLOG <- z.test(x=log_scale_sample1,y= log_scale_sample2, mu = 0,sigma.x=SD1LOG, sigma.y=SD2LOG )
resultLOG$p.value

SATSPT <- gsDesign(k=2, test.type=2,beta=0.1, alpha=0.025, n.fix = 242, n.I = c(92, 242), sfu=sfLDOF, sfl=sfLDOF)
SATSPT
plot(SATSPT)

# SA для сравнения двух частот
GSDTSPT <- gsDesign(k=3, beta=0.1, test.type=3,alpha=0.025, n.fix = 234,timing = c (0.5, 0.75, 1),  sfu=sfLDOF,  sfl=sfLDOF )
ZE <- GSDTSPT$upper$bound
ZF <- GSDTSPT$lower$bound
PVE<-pnorm(q=ZE, lower.tail = FALSE)
PVF<-pnorm(q=ZF, lower.tail = FALSE)
NS<-GSDTSPT$n.I
PVE
PVF
plot(GSDTSPT)
#  тест сравнения двух частот
res <- prop.test(x = c(90, 65), n = c(100, 100),p = NULL, alternative = "two.sided",correct = TRUE)
res$p.value
sqrt(res$statistic)

res2 <- prop.test(x = c(65, 90), n = c(100, 100),p = NULL, alternative = "two.sided",correct = TRUE)
res2$p.value
sqrt(res2$statistic)

SALRT <- gsDesign(k=2, test.type=2,beta=0.1, alpha=0.025, delta = -0.15, sfu=sfLDOF, sfl=sfLDOF)
SALRT
plot(SALRT)

ZU<-SA$upper$bound
ZL<-SA$lower$bound
ZU
ZL

pnorm(q=ZU, lower.tail = FALSE)
pnorm(q=ZL, lower.tail = FALSE)

data = c(1200,34567,3456,12,3456,0985,1211)
summary(data)
log_scale = log(as.data.frame(data))
log_scale


# ЭКСПЕРИМЕНТЫ

##Для примера общей таблицы с разными границами в статье
  GSDTSMT <- gsDesign(k=2, timing = c (0.7, 1), alpha=0.025, beta=0.1, test.type=4, sfu=sfLDOF, sfl=sfLDOF)
  GSDTSMT <- gsDesign(k=8, test.type=4,beta=0.1, alpha=0.025,    timing = c (0.2,0.3,0.4,0.5,0.6,0.7,0.8,  1), sfu=sfLDPocock, sfl=sfLDPocock)

  GSDTSMT <- gsDesign(k=8, test.type=4,beta=0.1, alpha=0.025,    timing = c (0.2,0.3,0.4,0.5,0.6,0.7,0.8,  1), sfu=sfLDPocock, sfl=sfLDPocock)
 
  GSDTSMT <- gsDesign(k=8, test.type=4,beta=0.1, alpha=0.025,    timing = c (0.2,0.3,0.4,0.5,0.6,0.7,0.8,  1), sfu=sfLDPocock, sfl=sfLDPocock)

  GSDTSMT <- gsDesign(k=8, test.type=4,beta=0.1, alpha=0.025,    timing = c (0.2,0.3,0.4,0.5,0.6,0.7,0.8,  1), sfu=sfLDPocock, sfl=sfLDPocock)

  GSDTSMT <- gsDesign(k=8, test.type=4,beta=0.1, alpha=0.025,    timing = c (0.2,0.3,0.4,0.5,0.6,0.7,0.8,  1), sfu=sfLDPocock, sfl=sfLDPocock)
 
  plot(GSDTSMT)
ZE <- GSDTSMT$upper$bound
ZF <- GSDTSMT$lower$bound
PVE<-pnorm(q=ZE, lower.tail = FALSE)
PVF<-pnorm(q=ZF, lower.tail = FALSE)
GSDTSMT
NS<-GSDTSMT$n.I
NS
ZE
ZF
PVE
PVF
plot(GSDTSMT)
##Для примера общей таблицы с разными границами в статье
sfup <- c(.033333, .063367, .1)
sflp <- c(.25, .5, .75)
timing <- c(.1, .4, .7)
x <- gsDesign(
  k = 3, test.type = 2, beta=0.2, alpha=0.025,   sfu=sfLDOF)
plot(x)
