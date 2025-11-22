setwd("/Users/lulu/desktop/实验室/徐化洁-新冠")
library(readxl)
covid<- read_excel("covid_match20250403.xlsx")
covid$secondary <- ifelse(covid$`events-AMI`== 1 | covid$`events-stroke` == 1 | covid$`event-death` == 1, 1, 0)
library(ggplot2)
km1<- survfit(Surv(time, events == 1) ~ group, data = covid)
ggsurvplot(km1,
           fun = "cumhaz",
           title = "",
           font.title = c(12, "bold"),
           font.x = c(12,"bold"),
           font.y = c(12,"bold"),
           xlab = "Months Since Randomization",
           ylab = "Cumulative Incidence (%)",
           xlim = c(0,12),
           ylim = c(0,0.15),
           break.x.by = 1,
           surv.scale = "percent",
           conf.int = FALSE,
           pval = F,
           censor = FALSE,
           pval.method = FALSE,
           palette = c("#2185db","#d6251f"),
           risk.table = TRUE,
           risk.table.y.text.col = FALSE,
           risk.table.height = 0.3,
           fontsize = 4,
           legend.title = "",
           legend = c(0.15,0.95),
           legend.labs = c("Non-COVID19","COVID19"))
sum(covid$events ==1 & covid$group == 0)/sum(covid$group == 0)
sum(covid$events ==1 & covid$group == 1)/sum(covid$group == 1)
sum(covid$secondary ==1 & covid$group == 0)/sum(covid$group == 0)
sum(covid$secondary ==1 & covid$group == 1)/sum(covid$group == 1)
cox_model <- coxph(Surv(time, events) ~ group + diabetes + smoke, data = covid)
summary(cox_model)
cox_model_ad <- coxph(Surv(time, events) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = covid)
summary(cox_model_ad)
cox_model_sec <- coxph(Surv(time, secondary) ~ group + diabetes + smoke, data = covid)
summary(cox_model_sec)
cox_model_sec_ad <- coxph(Surv(time, secondary) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = covid)
summary(cox_model_sec_ad)

# 单结局
sum(covid$`event-death` == 1 & covid$group == 0)/sum(covid$group == 0)
sum(covid$`event-death` == 1 & covid$group == 1)/sum(covid$group == 1)
sum(covid$`events-AMI` == 1 & covid$group == 0)/sum(covid$group == 0)
sum(covid$`events-AMI` == 1 & covid$group == 1)/sum(covid$group == 1)
sum(covid$`events-stroke` == 1 & covid$group == 0)/sum(covid$group == 0)
sum(covid$`events-stroke` == 1 & covid$group == 1)/sum(covid$group == 1)
sum(covid$`no-plan-PCI` == 1 & covid$group == 0)/sum(covid$group == 0)
sum(covid$`no-plan-PCI` == 1 & covid$group == 1)/sum(covid$group == 1)
sum(covid$`events-thrombosis` == 1 & covid$group == 0)/sum(covid$group == 0)
sum(covid$`events-thrombosis` == 1 & covid$group == 1)/sum(covid$group == 1)
cox_model <- coxph(Surv(time, `no-plan-PCI`) ~ group, data = covid)
summary(cox_model)
cox_model <- coxph(Surv(time, `events-thrombosis`) ~ group + diabetes + smoke, data = covid)
summary(cox_model)
cox_model_ad <- coxph(Surv(time, `events-thrombosis`) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = covid)
summary(cox_model_ad)

library("survival")
library("jskm")
library("remotes")
remotes::install_github("jinseob2kim/jskm")
fit1 <- survfit(Surv(time, events == 1) ~ group, data = covid)
jskm(fit1,
     mark = F,
     cut.landmark = 6,
     surv.scale = "percent",
     cumhaz=T,
     showpercent = T,
     linecols = c("#2185db","#d6251f"),
     font.title = c(12, "bold"),
     font.x = c(12,"bold"),
     font.y = c(12,"bold"),
     xlab = "Months Since Randomization",
     ylab = "Cumulative Incidence (%)",
     ystrataname = "",
     ystratalabs = c("Non-COVID19","COVID19"),
     legendposition = c(0.8,0.9),
     xlim = c(0,12),
     ylim = c(0,0.15),
     timeby = 3,
     table = T)

# 前3月cox回归
E_half1 <- covid
E_half1$time <- ifelse(E_half1$events == 0 & E_half1$time >= 3, 3, E_half1$time)
E_half1$events <- ifelse(E_half1$time >= 3, 0, E_half1$events)
E_half1$time <- ifelse(E_half1$time >= 3, 3, E_half1$time)
E_half1$`event-death` <- ifelse(E_half1$events == 0, 0, E_half1$`event-death`)
E_half1$`events-AMI` <- ifelse(E_half1$events == 0, 0, E_half1$`events-AMI`)
E_half1$`events-stroke` <- ifelse(E_half1$events == 0, 0, E_half1$`events-stroke`)
E_half1$`no-plan-PCI` <- ifelse(E_half1$events == 0, 0, E_half1$`no-plan-PCI`)
E_half1$`events-thrombosis` <- ifelse(E_half1$events == 0, 0, E_half1$`events-thrombosis`)
E_half1$secondary <- ifelse(E_half1$events == 0, 0, E_half1$secondary)
cox_model_1 <- coxph(Surv(time, events) ~ group + diabetes + smoke, data = E_half1)
summary(cox_model_1)
cox_model_1_ad <- coxph(Surv(time, events) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = E_half1)
summary(cox_model_1_ad)
cox_model_sec_1 <- coxph(Surv(time, secondary) ~ group + diabetes + smoke, data = E_half1)
summary(cox_model_sec_1)
cox_model_sec_1_ad <- coxph(Surv(time, secondary) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = E_half1)
summary(cox_model_sec_1_ad)
sum(E_half1$events ==1 & E_half1$group == 0)/sum(E_half1$group == 0)
sum(E_half1$events ==1 & E_half1$group == 1)/sum(E_half1$group == 1)
sum(E_half1$secondary ==1 & E_half1$group == 0)/sum(E_half1$group == 0)
sum(E_half1$secondary ==1 & E_half1$group == 1)/sum(E_half1$group == 1)

sum(E_half1$`event-death` == 1 & E_half1$group == 0)/sum(E_half1$group == 0)
sum(E_half1$`event-death` == 1 & E_half1$group == 1)/sum(E_half1$group == 1)
sum(E_half1$`events-AMI` == 1 & E_half1$group == 0)/sum(E_half1$group == 0)
sum(E_half1$`events-AMI` == 1 & E_half1$group == 1)/sum(E_half1$group == 1)
sum(E_half1$`events-stroke` == 1 & E_half1$group == 0)/sum(E_half1$group == 0)
sum(E_half1$`events-stroke` == 1 & E_half1$group == 1)/sum(E_half1$group == 1)
sum(E_half1$`no-plan-PCI` == 1 & E_half1$group == 0)/sum(E_half1$group == 0)
sum(E_half1$`no-plan-PCI` == 1 & E_half1$group == 1)/sum(E_half1$group == 1)
sum(E_half1$`events-thrombosis` == 1 & E_half1$group == 0)/sum(E_half1$group == 0)
sum(E_half1$`events-thrombosis` == 1 & E_half1$group == 1)/sum(E_half1$group == 1)

# 3-6月cox回归
E_half2 <- covid
E_half2 <- E_half2[!(E_half2$time < 3), ]
E_half2$time <- ifelse(E_half2$events == 0 & E_half2$time >= 6, 6, E_half2$time)
E_half2$events <- ifelse(E_half2$time >= 6, 0, E_half2$events)
E_half2$time <- ifelse(E_half2$time >= 6, 6, E_half2$time)
E_half2$secondary <- ifelse(E_half2$events == 0, 0, E_half2$secondary)
cox_model_2 <- coxph(Surv(time, events) ~ group, data = E_half2)
summary(cox_model_2)
cox_model_2_ad <- coxph(Surv(time, events) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = E_half2)
summary(cox_model_2_ad)
cox_model_sec_2 <- coxph(Surv(time, secondary) ~ group, data = E_half2)
summary(cox_model_sec_2)
cox_model_sec_2_ad <- coxph(Surv(time, secondary) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = E_half2)
summary(cox_model_sec_2_ad)
sum(E_half2$events ==1 & E_half2$group == 0)/sum(E_half2$group == 0)
sum(E_half2$events ==1 & E_half2$group == 1)/sum(E_half2$group == 1)
sum(E_half2$secondary ==1 & E_half2$group == 0)/sum(E_half2$group == 0)
sum(E_half2$secondary ==1 & E_half2$group == 1)/sum(E_half2$group == 1)

# 前6月cox回归
E_half3 <- covid
E_half3$time <- ifelse(E_half3$events == 0 & E_half3$time >= 6, 6, E_half3$time)
E_half3$events <- ifelse(E_half3$time >= 6, 0, E_half3$events)
E_half3$time <- ifelse(E_half3$time >= 6, 6, E_half3$time)
E_half3$`event-death` <- ifelse(E_half3$events == 0, 0, E_half3$`event-death`)
E_half3$`events-AMI` <- ifelse(E_half3$events == 0, 0, E_half3$`events-AMI`)
E_half3$`events-stroke` <- ifelse(E_half3$events == 0, 0, E_half3$`events-stroke`)
E_half3$`no-plan-PCI` <- ifelse(E_half3$events == 0, 0, E_half3$`no-plan-PCI`)
E_half3$`events-thrombosis` <- ifelse(E_half3$events == 0, 0, E_half3$`events-thrombosis`)
E_half3$secondary <- ifelse(E_half3$events == 0, 0, E_half3$secondary)
cox_model_3 <- coxph(Surv(time, events) ~ group + diabetes + smoke, data = E_half3)
summary(cox_model_3)
cox_model_3_ad <- coxph(Surv(time, events) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = E_half3)
summary(cox_model_3_ad)
cox_model_sec_3_ad <- coxph(Surv(time, secondary) ~ group + diabetes + smoke, data = E_half3)
summary(cox_model_sec_3)
cox_model_sec_3_ad <- coxph(Surv(time, secondary) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = E_half3)
summary(cox_model_sec_3_ad)
sum(E_half3$events ==1 & E_half3$group == 0)/sum(E_half3$group == 0)
sum(E_half3$events ==1 & E_half3$group == 1)/sum(E_half3$group == 1)
sum(E_half3$secondary ==1 & E_half3$group == 0)/sum(E_half3$group == 0)
sum(E_half3$secondary ==1 & E_half3$group == 1)/sum(E_half3$group == 1)

sum(E_half3$`event-death` == 1 & E_half3$group == 0)/sum(E_half3$group == 0)
sum(E_half3$`event-death` == 1 & E_half3$group == 1)/sum(E_half3$group == 1)
sum(E_half3$`events-AMI` == 1 & E_half3$group == 0)/sum(E_half3$group == 0)
sum(E_half3$`events-AMI` == 1 & E_half3$group == 1)/sum(E_half3$group == 1)
sum(E_half3$`events-stroke` == 1 & E_half3$group == 0)/sum(E_half3$group == 0)
sum(E_half3$`events-stroke` == 1 & E_half3$group == 1)/sum(E_half3$group == 1)
sum(E_half3$`no-plan-PCI` == 1 & E_half3$group == 0)/sum(E_half3$group == 0)
sum(E_half3$`no-plan-PCI` == 1 & E_half3$group == 1)/sum(E_half3$group == 1)
sum(E_half3$`events-thrombosis` == 1 & E_half3$group == 0)/sum(E_half3$group == 0)
sum(E_half3$`events-thrombosis` == 1 & E_half3$group == 1)/sum(E_half3$group == 1)

# 6月后cox回归
E_half4 <- covid
E_half4 <- E_half4[!(E_half4$time < 6), ]
cox_model_4 <- coxph(Surv(time, events) ~ group, data = E_half4)
summary(cox_model_4)
cox_model_4_ad <- coxph(Surv(time, events) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = E_half4)
summary(cox_model_4_ad)
cox_model_sec_4 <- coxph(Surv(time, secondary) ~ group, data = E_half4)
summary(cox_model_sec_4)
cox_model_sec_4_ad <- coxph(Surv(time, secondary) ~ group + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = E_half4)
summary(cox_model_sec_4_ad)
sum(E_half4$events ==1 & E_half4$group == 0)/sum(E_half4$group == 0)
sum(E_half4$events ==1 & E_half4$group == 1)/sum(E_half4$group == 1)
sum(E_half4$secondary ==1 & E_half4$group == 0)/sum(E_half4$group == 0)
sum(E_half4$secondary ==1 & E_half4$group == 1)/sum(E_half4$group == 1)

library(stats)
tapply(covid$logDD, covid$group, shapiro.test)

library(tableone)
library(survival)
myVars <- c("age","sex","BMI","smoke","HBP","diabetes","CKD","strokehist",
            "hyperlipid","AMI","PCI",
            "Hb","PLT","MPV","PCT",
            "P-LCR","PDW","HBA1c","D-dimer","Scr","TC",
            "TG","LDL","N-HDL","antiplatelet","statin/PCSK9i","mono","dual","aspirin","P2Y12I") # 所有需要描述的变量
catVars <- c("sex","smoke","HBP","diabetes","CKD","strokehist",
             "hyperlipid","AMI","PCI","antiplatelet", "statin/PCSK9i","mono","dual","aspirin","P2Y12I") # 分类变量
nonnormalVars <- c("age","BMI","Hb","PLT","MPV","PCT",
                   "P-LCR","PDW","HBA1c","D-dimer","Scr","TC",
                   "TG","LDL","N-HDL") # 非正态的连续型变量
table <- CreateTableOne(vars = myVars, strata = "group", data = covid, factorVars = catVars, addOverall = T)
print(table, nonnormal = nonnormalVars)

tab <- print(table, nonnormal = nonnormalVars, quote = F,
             noSpaces = T, printToggle = F, showAllLevels = T)
write.csv(tab, file = "baseline_covid.csv")

library(MatchIt)
ps <- matchit(group ~ antiplatelet + PCI + AMI + age + sex, data = covid,method = "nearest", distance = "logit", caliper = 0.05)
plot(ps)
# 进行PSM
matched_data <- match.data(ps)
covid <- matched_data
write.csv(matched_data, file = "covid_match.csv")
# 绘制匹配前的核密度图
ggplot(covid, aes(x = ps)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ `cov-group`) +
  labs(
    title = "Propensity Score Distribution Before Matching",
    x = "Propensity Score (PS)",
    y = "Density"
  )
# 绘制匹配后的核密度图
ggplot(matched_data, aes(x = ps)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Propensity Score Distribution After Matching",
    x = "Propensity Score (PS)",
    y = "Density"
  )

table <- CreateTableOne(vars = myVars, strata = "group", data = matched_data, factorVars = catVars, addOverall = T)
print(table, nonnormal = nonnormalVars)
tab <- print(table, nonnormal = nonnormalVars, quote = F,
             noSpaces = T, printToggle = F, showAllLevels = T)
write.csv(tab, file = "baseline_covid_match.csv")

cox_model <- coxph(Surv(time, events) ~ `cov-group`, data = covid)
summary(cox_model)
cox_model_ad <- coxph(Surv(time, events) ~ `cov-group` + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = covid)
summary(cox_model_ad)
cox_model_ad1 <- coxph(Surv(time, events) ~ `cov-group` + age + sex + BMI + smoke + AMI + PCI + HBP + diabetes + hyperlipid + `D-dimer`, data = matched_data)
summary(cox_model_ad1)

x1 <- sum(covid$events == 1 & covid$group == 1)  # 目标组样本事件次数
n1 <- sum(covid$group == 1) # 目标组样本总次数
x2 <- sum(covid$events == 1 & covid$group == 0) # 对照组样本事件次数
n2 <- sum(covid$group == 0) # 对照组样本总次数

result <- prop.test(x = c(x1, x2), n = c(n1, n2), correct = FALSE)
result
risk_diff <- result$estimate[1] - result$estimate[2]
lower_ci <- result$conf.int[1]
upper_ci <- result$conf.int[2]

cuminc.fit <- cuminc(ftime = covid$time, fstatus = covid$events, group = covid$group)
cuminc.fit

covid_merge <- read_excel("covid_match.xlsx")
drug <- data.frame(covid$subject, covid$aspirin, covid$P2Y12I, covid$xaban, covid$statin, covid$bbc, covid$aceiarb, covid$ppi)
colnames(drug) <- c("subject","aspirin", "P2Y12I", "xaban", "statin", "bbc", "aceiarb", "ppi")
merge <- merge(covid_merge, drug, all.x = T, by = "subject")
write.csv(merge, file = "covid_match.csv")

drug <- read_excel("基线表-药物.xlsx")
drug <- data.frame(drug$subject,drug$indobufen,drug$西洛他唑)
colname(drug) <- c("subject","indobufen","西洛他唑")

merge <- merge(matched_data, drug, by.x = "subject", by.y = "drug.subject", all.x = T)
write.csv(merge, file = "covid_match20250403.csv")


#计算SMD
library(scitb)
matched_data <- read_excel("covid_match20250403.xlsx")
out <- scitb1(vars = myVars, fvars = catVars, strata = "group", data = covid, statistic = T, smd = T)
out_match <- scitb1(vars = myVars, fvars = catVars, strata = "group", data = matched_data, statistic = T, smd = T)
write.csv(out, file = "baseline_SMD.csv")
write.csv(out_match, file = "baseline_match_SMD.csv")

#亚组分析
covid<- read_excel("covid20241014.xlsx")
covid<- read_excel("covid_match.xlsx")
covid$subgroup <- ifelse(covid$hyperlipid == 1, 1, 0)
sum(covid$events ==1 & covid$group == 1 & covid$subgroup == 1)/sum(covid$group == 1 & covid$subgroup == 1)
sum(covid$events ==1 & covid$group == 0 & covid$subgroup == 1)/sum(covid$group == 0 & covid$subgroup == 1)
cox_sub <- coxph(Surv(time, events) ~ group * subgroup, data = covid)
summary(cox_sub)
cox_sub1 <- coxph(Surv(time, events) ~ group, data = covid[covid$subgroup == 1, ])
summary(cox_sub1)
cox_sub0 <- coxph(Surv(time, events) ~ group, data = covid[covid$subgroup == 0, ])
summary(cox_sub0)

## IPTW逆概率加权
# 生成初始基线表
library(tableone)
covid$D_dimer <- covid$`D-dimer`
covid$statin_PCSK9i <- covid$`statin/PCSK9i`
covid$P_LCR <- covid$`P-LCR`
covid$N_HDL <- covid$`N-HDL`
covid$logDD <- ifelse(covid$D_dimer == 0, 0, log(covid$D_dimer * 100))
myVars <- c("age","sex","BMI","smoke","HBP","diabetes","CKD","hyperlipid",
            "strokehist","AMI","PCI",
            "Hb","PLT","MPV","PCT",
            "P_LCR","PDW","HBA1c","logDD","Scr","TC",
            "TG","LDL","N_HDL","antiplatelet","mono","dual","statin_PCSK9i") # 所有需要描述的变量
catVars <- c("sex","smoke","HBP","diabetes","CKD","hyperlipid","strokehist",
             "AMI","PCI","antiplatelet","mono","dual", "statin_PCSK9i") # 分类变量
nonnormalVars <- c("age","BMI","Hb","PLT","MPV","PCT",
                   "P_LCR","PDW","HBA1c","logDD","Scr","TC",
                   "TG","LDL","N_HDL") # 非正态的连续型变量
table <- CreateTableOne(vars = myVars, strata = "group", data = covid, test = T, factorVars = catVars, addOverall = T)
print(table, showAllLevels = TRUE, smd = TRUE)
tab <- print(table, nonnormal = nonnormalVars, smd = TRUE)
write.csv(tab, file = "baseline_beforeIPTW.csv")
# 计算权重
library(ipw)
# 拟合倾向得分模型
psModel <- glm(group ~ age+sex+BMI+smoke+HBP+diabetes+CKD+strokehist+hyperlipid
               +AMI+PCI+Hb+PLT+MPV+PCT+P_LCR+PDW+HBA1c+logDD+Scr+TC+TG+LDL
               +N_HDL+antiplatelet+mono+dual+statin_PCSK9i,
               family = binomial(link = "logit"),
               data = covid)
# 计算倾向得分（PS）
covid$ps <- predict(psModel, type = "response")
# 计算治疗组和对照组的逆概率
# 假设group=1是治疗组，group=0是对照组
covid$wt1 <- 1 / covid$ps          # 治疗组的权重
covid$wt0 <- 1 / (1 - covid$ps)    # 对照组的权重
# 根据患者分组分配权重
# 如果患者为治疗组（group=1）用wt1加权，否则用wt0加权
covid$w <- ifelse(covid$group == 1, covid$wt1, covid$wt0)
# 检查权重分布
summary(covid$w)
hist(covid$w, breaks = 50, main = "IP权重分布")
# 修剪极端权重（可选）
# 通常修剪在1%-99%或5%-95%分位数
lower_bound <- quantile(covid$w, 0.01)
upper_bound <- quantile(covid$w, 0.99)
covid$w_trimmed <- ifelse(covid$w < lower_bound, lower_bound,
                          ifelse(covid$w > upper_bound, upper_bound, covid$w))
# 检查修剪后的权重
summary(covid$w_trimmed)
hist(covid$w_trimmed, breaks = 50, main = "修剪后的IP权重分布")

#提取加权后的数据
library(survey)
covid_iptw <- svydesign(ids = ~1, weights = ~w_trimmed, data = covid)

## 生成IPTW后的基线表
tab_iptw <- svyCreateTableOne(vars = myVars,
                              strata = "group", data = covid_iptw, test = T,
                              factorVars = catVars, addOverall = T) 
print(tab_iptw, showAllLevels = TRUE, smd = TRUE) # 展示SMD
tab_ipw <- print(tab_iptw, nonnormal = nonnormalVars, smd = TRUE)
write.csv(tab_ipw, file = "baseline_IPTW.csv")

weighted_cox <- svycoxph(Surv(time, events) ~ group, 
                        design = covid_iptw)
summary(weighted_cox)

weighted_cox_sec <- svycoxph(Surv(time, secondary) ~ group, 
                         design = covid_iptw)
summary(weighted_cox_sec)
# 前3月cox回归
E_half1 <- covid
E_half1$time <- ifelse(E_half1$events == 0 & E_half1$time >= 3, 3, E_half1$time)
E_half1$events <- ifelse(E_half1$time >= 3, 0, E_half1$events)
E_half1$time <- ifelse(E_half1$time >= 3, 3, E_half1$time)
E_half1$secondary <- ifelse(E_half1$events == 0, 0, E_half1$secondary)
E_half1_iptw <- svydesign(ids = ~1, weights = ~w_trimmed, data = E_half1)
weighted_cox_1 <- svycoxph(Surv(time, events) ~ group, design = E_half1_iptw)
summary(weighted_cox_1)
weighted_cox_sec_1 <- svycoxph(Surv(time, secondary) ~ group, design = E_half1_iptw)
summary(weighted_cox_sec_1)
# 前6月cox回归
E_half3 <- covid
E_half3$time <- ifelse(E_half3$events == 0 & E_half3$time >= 6, 6, E_half3$time)
E_half3$events <- ifelse(E_half3$time >= 6, 0, E_half3$events)
E_half3$time <- ifelse(E_half3$time >= 6, 6, E_half3$time)
E_half3$secondary <- ifelse(E_half3$events == 0, 0, E_half3$secondary)
E_half3_iptw <- svydesign(ids = ~1, weights = ~w_trimmed, data = E_half3)
weighted_cox_3 <- svycoxph(Surv(time, events) ~ group, design = E_half3_iptw)
summary(weighted_cox_3)
weighted_cox_sec_3 <- svycoxph(Surv(time, secondary) ~ group, design = E_half3_iptw)
summary(weighted_cox_sec_3)

## 计算E-velue
library(EValue)
# 提取Cox回归结果
cox_coef <- coef(cox_model_sec_ad)
cox_ci <- confint(cox_model_sec_ad)
hr <- exp(cox_coef)[1]
ci_lower <- exp(cox_ci)[1, 1]
ci_upper <- exp(cox_ci)[1, 2]
# 计算E-value
evalue_results <- evalues.HR(est = hr, lo = ci_lower, hi = ci_upper, rare = TRUE)
print(evalue_results)
