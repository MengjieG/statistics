
install.packages("data.table")
install.packages("mice")
library(data.table)
library(tidyverse)
library(tidyr)
library(mice)
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

# 1.4.2 术前白细胞计数的填补 --------------------------------------------------------
# 根据新生儿性别、是否早产以及出院诊断进行匹配填补缺失值。


# 1.5 数据重新编码 --------------------------------------------------------------



# 2、建立数学模型 ----------------------------------------------------------------


