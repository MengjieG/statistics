
install.packages("data.table")
install.packages("mice")
install.packages("DMwR")
install.packages("caret")
install.packages("rlm")
library(caret)
library(data.table)
library(tidyverse)
library(tidyr)
library(mice)
library(DMwR)
library(rlm)
# 导入数据 ----------------------------------------------------------------

pediatric <- fread('E:/个人勿动/儿科手术质量文献/pediatric.csv')
# 1、数据清理 ------------------------------------------------------------------
colnames(pediatric)<-c("ID","sex","age","age_d","urgent","office","birth_weight","Hos_weight",
                       "diagnose","illness","operation","oper_ID","Anesthesia_surgery",
                       "surgery_type","surgery_time","surgery_time_min","time_score",
                       "Anesthesia","Anesthesia_score","incision","incision_score","NNIS",
                       "outcome","ICU","ventilator","WBC")
pediatric_1<-pediatric[,c("sex","age_d","urgent","office","birth_weight","diagnose",
                               "Anesthesia_surgery","surgery_type","surgery_time","surgery_time_min","time_score",
                               "Anesthesia","Anesthesia_score","incision","incision_score","NNIS",
                               "outcome","ICU","ventilator","WBC")]


# 1.2 描述性分析 ---------------------------------------------------------------

# 1.3 缺失值分析 ---------------------------------------------------------------
pediatric_md<-md.pattern(pediatric_1,plot = TRUE)
write.csv(pediatric_md,"E:/个人勿动/儿科手术质量文献/pediatric_md.csv")

# 1.4 缺失值填补 ---------------------------------------------------------------

# 1.4.1 出生体重填补 -----------------------------------------------------------
# 根据新生儿性别、是否早产进行匹配填补缺失值。
ped_weight<-pediatric_1%>%
  group_by(sex,urgent)%>%
  summarize(
    mean=mean(birth_weight,na.rm = TRUE),
    sd=sd(birth_weight,na.rm = TRUE)
  )
pediatric_1$birth_w<-as.numeric(pediatric_1$birth_weight)

pediatric_1[is.na(birth_weight) & sex=="男" & urgent=="否",birth_weight]

set.seed(100)
length(pediatric[is.na(birth_weight) & sex=="男" & urgent=="否"]$birth_weight)
pediatric_1[is.na(birth_weight) & sex=="男" & urgent=="否"]$birth_w<-rnorm(914,mean = 3369.286,490.0422)

length(pediatric_1[is.na(birth_weight) & sex=="男" & urgent=="是"]$birth_weight)
pediatric_1[is.na(birth_w) & sex=="男" & urgent=="是"]$birth_w<-1933.333

length(pediatric_1[is.na(birth_w) & sex=="女" & urgent=="否"]$birth_w)
pediatric_1[is.na(birth_w) & sex=="女" & urgent=="否"]$birth_w<-rnorm(498,3201.190,715.1152)


length(pediatric_1[is.na(birth_w) & sex=="女" & urgent=="是"]$birth_w)
pediatric_1[is.na(birth_w) & sex=="女" & urgent=="是"]$birth_w<-1912.222

# 1.4.2 术前白细胞计数的填补 --------------------------------------------------------
# 根据新生儿性别、是否早产以及出院诊断进行匹配填补缺失值。

ped_wbc<-pediatric_1%>%
  group_by(sex,urgent,diagnose)%>%
  summarize(
    mean=mean(WBC,na.rm = TRUE),
    sd=sd(WBC,na.rm = TRUE)
  )


# 1.4.3ICU和呼吸机时长的填补 -------------------------------------------------------

# 有缺失是没用

pediatric_1[is.na(ICU)]$ICU<-0
pediatric_1[is.na(ventilator)]$ventilator<-0


# 填补后的数据清洗 ----------------------------------------------------------------

pediatric_2<-pediatric_1[complete.cases(pediatric_1[,c("surgery_time_min","time_score","WBC")])]

pediatric_2[Anesthesia=="0"|is.na(Anesthesia)]$Anesthesia<-"I"
write.csv(pediatric_2,"E:/兼职统计/康维金桥/儿科手术质量文献/建模及数据/pediatric_final.csv")

pediatric_final<-fread("E:/兼职统计/康维金桥/儿科手术质量文献/建模及数据/pediatric_final.csv")

pediatric_final$outcome_1<-ifelse(pediatric_final$outcome=="死亡",1,0)

complete.cases(pediatric_1[,c("surgery_time_min","time_score")])
# 1.5 数据重新编码 --------------------------------------------------------------



# 2、建立数学模型 ----------------------------------------------------------------


# 2.1参数设置 -----------------------------------------------------------------

set.seed(100)
ctrl_1<-trainControl(method = "LGOCV",summaryFunction = twoClassSummary,
                   classProbs = TRUE)

ctrl_2<-trainControl(method = "boot",summaryFunction = twoClassSummary,
                     classProbs = TRUE)

ctrl<-trainControl(method = "cv",number = 10,summaryFunction = twoClassSummary,
                   classProbs = TRUE)


# 2.2 模型建立---------------------------------------------------------------------

pediatric_fit<-train(x =   ,
                     y =  ,preProcess = ,trControl = ctrl,method = "glm",
                     metric = "ROC")


pediatric_fit$finalModel


