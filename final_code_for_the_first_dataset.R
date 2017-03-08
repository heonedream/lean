
#读入数据
setwd("E:\\Lean\\20170214\\结果汇总20170308")
origdata <- read.csv("data_Interven.csv")
dim(origdata)

o_data_1 <- origdata[,c(which(names(origdata)=="study_id"),
which(names(origdata)=="redcap_event_name"),which(names(origdata)=="Interven"),
which(names(origdata)=="gender_care"),
which(names(origdata)=="gender"),
which(names(origdata)=="marriage"),
which(names(origdata)=="marriage_status"),
which(names(origdata)=="date_of_birth"),
which(names(origdata)=="date_of_birth_care"),
which(names(origdata)=="career_care"),
which(names(origdata)=="years_of_education_care"),
which(names(origdata)=="income_care"),
which(names(origdata)=="income_family_total"),
which(names(origdata)=="living_status_care"),
which(names(origdata)=="year_of_incidence"),
which(names(origdata)=="overall_cgi1"),
which(names(origdata)=="overall_cgi2"),
which(names(origdata)=="sms_usage"),
which(names(origdata)=="sms_usage_lay"),
which(names(origdata)=="phone_ownership"),
which(names(origdata)=="phone_ownership_lay"),
which(names(origdata)=="employment"),
which(names(origdata)=="education_care"),
which(names(origdata)=="date_of_diagnosis"),
which(names(origdata)=="supervision_taking_medicine"),
which(names(origdata)=="years_at_school")

)]

head(o_data_1)


#care model 3.3 中education_care转化为在校年数
o_data_1$education_care_transformed[o_data_1$education_care==1]=0
o_data_1$education_care_transformed[o_data_1$education_care==2]=7
o_data_1$education_care_transformed[o_data_1$education_care==3]=10
o_data_1$education_care_transformed[o_data_1$education_care==4]=13
o_data_1$education_care_transformed[o_data_1$education_care==5]=16
o_data_1$education_care_transformed[o_data_1$education_care==6]=17





#whodas的marriage 转成跟care_model 3.3一样的格式（多一个同居分类出来）
#1-2；2-1；3,4-4；5-3；6-5
#1已婚；2未婚；3丧偶；4离异或分居；5同居

o_data_1$marriage_transformed[o_data_1$marriage=="1"]=2
o_data_1$marriage_transformed[o_data_1$marriage=="2"]=1
o_data_1$marriage_transformed[o_data_1$marriage=="3"]=4
o_data_1$marriage_transformed[o_data_1$marriage=="4"]=4
o_data_1$marriage_transformed[o_data_1$marriage=="5"]=3
o_data_1$marriage_transformed[o_data_1$marriage=="6"]=5


WHODAS <- origdata[,c(which(names(origdata)=="study_id"),
which(names(origdata)=="redcap_event_name"),
which(names(origdata)=="Interven"),
which(names(origdata)=="code_interviewer"):which(names(origdata)=="whodas12patientlcs_complete"))]
head(WHODAS)
str(WHODAS)
#量表赋值
WHODAS$score_1[WHODAS$whodas_s1==1] <- 0
WHODAS$score_1[WHODAS$whodas_s1==2] <- 1
WHODAS$score_1[WHODAS$whodas_s1==3] <- 2
WHODAS$score_1[WHODAS$whodas_s1==4] <- 3
WHODAS$score_1[WHODAS$whodas_s1==5] <- 4
WHODAS$score_2[WHODAS$whodas_s2==1] <- 0
WHODAS$score_2[WHODAS$whodas_s2==2] <- 1
WHODAS$score_2[WHODAS$whodas_s2==3] <- 2
WHODAS$score_2[WHODAS$whodas_s2==4] <- 3
WHODAS$score_2[WHODAS$whodas_s2==5] <- 4
WHODAS$score_3[WHODAS$whodas_s3==1] <- 0
WHODAS$score_3[WHODAS$whodas_s3==2] <- 1
WHODAS$score_3[WHODAS$whodas_s3==3] <- 2
WHODAS$score_3[WHODAS$whodas_s3==4] <- 3
WHODAS$score_3[WHODAS$whodas_s3==5] <- 4
WHODAS$score_4[WHODAS$whodas_s4==1] <- 0
WHODAS$score_4[WHODAS$whodas_s4==2] <- 1
WHODAS$score_4[WHODAS$whodas_s4==3] <- 2
WHODAS$score_4[WHODAS$whodas_s4==4] <- 3
WHODAS$score_4[WHODAS$whodas_s4==5] <- 4
WHODAS$score_5[WHODAS$whodas_s5==1] <- 0
WHODAS$score_5[WHODAS$whodas_s5==2] <- 1
WHODAS$score_5[WHODAS$whodas_s5==3] <- 2
WHODAS$score_5[WHODAS$whodas_s5==4] <- 3
WHODAS$score_5[WHODAS$whodas_s5==5] <- 4
WHODAS$score_6[WHODAS$whodas_s6==1] <- 0
WHODAS$score_6[WHODAS$whodas_s6==2] <- 1
WHODAS$score_6[WHODAS$whodas_s6==3] <- 2
WHODAS$score_6[WHODAS$whodas_s6==4] <- 3
WHODAS$score_6[WHODAS$whodas_s6==5] <- 4
WHODAS$score_7[WHODAS$whodas_s7==1] <- 0
WHODAS$score_7[WHODAS$whodas_s7==2] <- 1
WHODAS$score_7[WHODAS$whodas_s7==3] <- 2
WHODAS$score_7[WHODAS$whodas_s7==4] <- 3
WHODAS$score_7[WHODAS$whodas_s7==5] <- 4
WHODAS$score_8[WHODAS$whodas_s8==1] <- 0
WHODAS$score_8[WHODAS$whodas_s8==2] <- 1
WHODAS$score_8[WHODAS$whodas_s8==3] <- 2
WHODAS$score_8[WHODAS$whodas_s8==4] <- 3
WHODAS$score_8[WHODAS$whodas_s8==5] <- 4
WHODAS$score_9[WHODAS$whodas_s9==1] <- 0
WHODAS$score_9[WHODAS$whodas_s9==2] <- 1
WHODAS$score_9[WHODAS$whodas_s9==3] <- 2
WHODAS$score_9[WHODAS$whodas_s9==4] <- 3
WHODAS$score_9[WHODAS$whodas_s9==5] <- 4
WHODAS$score_10[WHODAS$whodas_s10==1] <- 0
WHODAS$score_10[WHODAS$whodas_s10==2] <- 1
WHODAS$score_10[WHODAS$whodas_s10==3] <- 2
WHODAS$score_10[WHODAS$whodas_s10==4] <- 3
WHODAS$score_10[WHODAS$whodas_s10==5] <- 4
WHODAS$score_11[WHODAS$whodas_s11==1] <- 0
WHODAS$score_11[WHODAS$whodas_s11==2] <- 1
WHODAS$score_11[WHODAS$whodas_s11==3] <- 2
WHODAS$score_11[WHODAS$whodas_s11==4] <- 3
WHODAS$score_11[WHODAS$whodas_s11==5] <- 4
WHODAS$score_12[WHODAS$whodas_s12==1] <- 0
WHODAS$score_12[WHODAS$whodas_s12==2] <- 1
WHODAS$score_12[WHODAS$whodas_s12==3] <- 2
WHODAS$score_12[WHODAS$whodas_s12==4] <- 3
WHODAS$score_12[WHODAS$whodas_s12==5] <- 4
#计算评分
WHODAS$WHODAS_score <- (WHODAS$score_1+WHODAS$score_2+WHODAS$score_3+WHODAS$score_4+
  WHODAS$score_5+WHODAS$score_6+WHODAS$score_7+WHODAS$score_8+WHODAS$score_9+WHODAS$score_10+WHODAS$score_11+WHODAS$score_12)/48
dim(WHODAS)

o_data_1$WHODAS <- WHODAS$WHODAS_score

head(o_data_1)










Dai_data <- origdata[,c(which(names(origdata)=="study_id"),
which(names(origdata)=="redcap_event_name"),
which(names(origdata)=="Interven"),
which(names(origdata)=="patient_or_lcs_dai"):which(names(origdata)=="drug_attitude_inventory10_complete"))]
head(Dai_data)

#按照评分标准赋值
Dai_data$score_1[Dai_data$dai_1==1] <- 1
Dai_data$score_1[Dai_data$dai_1==2] <- -1
Dai_data$score_2[Dai_data$dai_2==1] <- -1
Dai_data$score_2[Dai_data$dai_2==2] <- 1
Dai_data$score_3[Dai_data$dai_3==1] <- -1
Dai_data$score_3[Dai_data$dai_3==2] <- 1
Dai_data$score_4[Dai_data$dai_4==1] <- 1
Dai_data$score_4[Dai_data$dai_4==2] <- -1
Dai_data$score_5[Dai_data$dai_5==1] <- -1
Dai_data$score_5[Dai_data$dai_5==2] <- 1
Dai_data$score_6[Dai_data$dai_6==1] <- -1
Dai_data$score_6[Dai_data$dai_6==2] <- 1
Dai_data$score_7[Dai_data$dai_7==1] <- 1
Dai_data$score_7[Dai_data$dai_7==2] <- -1
Dai_data$score_8[Dai_data$dai_8==1] <- -1
Dai_data$score_8[Dai_data$dai_8==2] <- 1
Dai_data$score_9[Dai_data$dai_9==1] <- 1
Dai_data$score_9[Dai_data$dai_9==2] <- -1
Dai_data$score_10[Dai_data$dai_10==1] <- 1
Dai_data$score_10[Dai_data$dai_10==2] <- -1
#DAI得分为各分值之和
Dai_data$DAI_score <- Dai_data$score_1+Dai_data$score_2+Dai_data$score_3+Dai_data$score_4+
  Dai_data$score_5+Dai_data$score_6+Dai_data$score_7+Dai_data$score_8+
  Dai_data$score_9+Dai_data$score_10
dim(Dai_data)
o_data_1$DAI <- Dai_data$DAI_score
head(o_data_1)
dim(o_data_1)




#计算BAR
bar_data <- origdata[,c(2,3,143:249,276:287)]
head(bar_data)
#第二题赋值，26.5=30-（0+7）/2   20=30-(7+13)/2
bar_data$factor_2[bar_data$frequency_of_drugs_taking==1] <- 26.5
bar_data$factor_2[bar_data$frequency_of_drugs_taking==2] <- 20
bar_data$factor_2[bar_data$frequency_of_drugs_taking==3] <- 13
bar_data$factor_2[bar_data$frequency_of_drugs_taking==4] <- 5
#第三题赋值
bar_data$factor_3[bar_data$frequency_of_not_taking_drugs==1] <- 0.56
#0.56=1*(1-(1+0.76)/2)+0.5*(1+0.76)/2
bar_data$factor_3[bar_data$frequency_of_not_taking_drugs==2] <- 0.685
#0.685=1*(1-(0.75+0.51)/2)+0.5*(0.75+0.51)/2
bar_data$factor_3[bar_data$frequency_of_not_taking_drugs==3] <- 0.81
#0.81=1*(1-(0.5+0.26)/2)+0.5*(0.5+0.26)/2
bar_data$factor_3[bar_data$frequency_of_not_taking_drugs==4] <- 0.9375
#0.9375=1*(1-(0+0.25)/2)+0.5*(0+0.25)/2
bar_data$BAR <- (bar_data$factor_2*bar_data$factor_3)/30
o_data_1$BAR <- bar_data$BAR
head(o_data_1)
dim(o_data_1)

#计算pill_count1例的Bar条目
pill_count1_bar=origdata[,c(2,3,which(names(origdata)=="days_inadherence"),which(names(origdata)=="days_cut"))]
#第二题赋值，26.5=30-（0+7）/2   20=30-(7+13)/2
pill_count1_bar$factor_2[pill_count1_bar$days_inadherence==1] <- 26.5
pill_count1_bar$factor_2[pill_count1_bar$days_inadherence==2] <- 20
pill_count1_bar$factor_2[pill_count1_bar$days_inadherence==3] <- 13
pill_count1_bar$factor_2[pill_count1_bar$days_inadherence==4] <- 5
#第三题赋值
pill_count1_bar$factor_3[pill_count1_bar$days_cut==1] <- 0.56
#0.56=1*(1-(1+0.76)/2)+0.5*(1+0.76)/2
pill_count1_bar$factor_3[pill_count1_bar$days_cut==2] <- 0.685
#0.685=1*(1-(0.75+0.51)/2)+0.5*(0.75+0.51)/2
pill_count1_bar$factor_3[pill_count1_bar$days_cut==3] <- 0.81
#0.81=1*(1-(0.5+0.26)/2)+0.5*(0.5+0.26)/2
pill_count1_bar$factor_3[pill_count1_bar$days_cut==4] <- 0.9375
#0.9375=1*(1-(0+0.25)/2)+0.5*(0+0.25)/2
pill_count1_bar$BAR_pill_count1 <- (pill_count1_bar$factor_2*pill_count1_bar$factor_3)/30
o_data_1$BAR_pill_count1 <- pill_count1_bar$BAR_pill_count1



#计算morisky
#提取量表
Morisky <- origdata[,c(which(names(origdata)=="study_id"),
which(names(origdata)=="redcap_event_name"),
which(names(origdata)=="Interven"),
which(names(origdata)=="patient_or_lcs_morisky"):which(names(origdata)=="morisky8_items_complete"))]
head(Morisky)
#量表赋值
Morisky$score_1[Morisky$morisky01==1] <- 0
Morisky$score_1[Morisky$morisky01==2] <- 1
Morisky$score_2[Morisky$morisky02==1] <- 0
Morisky$score_2[Morisky$morisky02==2] <- 1
Morisky$score_3[Morisky$morisky03==1] <- 0
Morisky$score_3[Morisky$morisky03==2] <- 1
Morisky$score_4[Morisky$morisky04==1] <- 0
Morisky$score_4[Morisky$morisky04==2] <- 1
Morisky$score_5[Morisky$morisky05==1] <- 1
Morisky$score_5[Morisky$morisky05==2] <- 0
Morisky$score_6[Morisky$morisky06==1] <- 0
Morisky$score_6[Morisky$morisky06==2] <- 1
Morisky$score_7[Morisky$morisky07==1] <- 0
Morisky$score_7[Morisky$morisky07==2] <- 1
Morisky$score_8[Morisky$morisky08==1] <- 1
Morisky$score_8[Morisky$morisky08==2] <- 0.75
Morisky$score_8[Morisky$morisky08==3] <- 0.5
Morisky$score_8[Morisky$morisky08==4] <- 0.25
Morisky$score_8[Morisky$morisky08==5] <- 0
#计算评分
Morisky$Morisky8_score <- Morisky$score_1+Morisky$score_2+Morisky$score_3+Morisky$score_4+
  Morisky$score_5+Morisky$score_6+Morisky$score_7+Morisky$score_8
o_data_1$Morisky <- Morisky$Morisky8_score 
head(o_data_1)
dim(o_data_1)
#GASS计算
#提取量表
GASS <- origdata[,c(which(names(origdata)=="study_id"),
                       which(names(origdata)=="redcap_event_name"),
                       which(names(origdata)=="Interven"),
                       which(names(origdata)=="side_effects1"):which(names(origdata)=="glasgow_antipsychotic_side_effects_scale_complete"))]
head(GASS)
GASS[GASS$redcap_event_name=="second_visit_nov_arm_1",]
#量表赋值
GASS$score_1[GASS$side_effects1==1] <- 0
GASS$score_1[GASS$side_effects1==2] <- 1
GASS$score_1[GASS$side_effects1==3] <- 2
GASS$score_1[GASS$side_effects1==4] <- 3

GASS$score_2[GASS$side_effects2==1] <- 0
GASS$score_2[GASS$side_effects2==2] <- 1
GASS$score_2[GASS$side_effects2==3] <- 2
GASS$score_2[GASS$side_effects2==4] <- 3

GASS$score_3[GASS$side_effects3==1] <- 0
GASS$score_3[GASS$side_effects3==2] <- 1
GASS$score_3[GASS$side_effects3==3] <- 2
GASS$score_3[GASS$side_effects3==4] <- 3

GASS$score_4[GASS$side_effects4==1] <- 0
GASS$score_4[GASS$side_effects4==2] <- 1
GASS$score_4[GASS$side_effects4==3] <- 2
GASS$score_4[GASS$side_effects4==4] <- 3

GASS$score_5[GASS$side_effects5==1] <- 0
GASS$score_5[GASS$side_effects5==2] <- 1
GASS$score_5[GASS$side_effects5==3] <- 2
GASS$score_5[GASS$side_effects5==4] <- 3

GASS$score_6[GASS$side_effects6==1] <- 0
GASS$score_6[GASS$side_effects6==2] <- 1
GASS$score_6[GASS$side_effects6==3] <- 2
GASS$score_6[GASS$side_effects6==4] <- 3

GASS$score_7[GASS$side_effects7==1] <- 0
GASS$score_7[GASS$side_effects7==2] <- 1
GASS$score_7[GASS$side_effects7==3] <- 2
GASS$score_7[GASS$side_effects7==4] <- 3

GASS$score_8[GASS$side_effects8==1] <- 0
GASS$score_8[GASS$side_effects8==2] <- 1
GASS$score_8[GASS$side_effects8==3] <- 2
GASS$score_8[GASS$side_effects8==4] <- 3

GASS$score_9[GASS$side_effects9==1] <- 0
GASS$score_9[GASS$side_effects9==2] <- 1
GASS$score_9[GASS$side_effects9==3] <- 2
GASS$score_9[GASS$side_effects9==4] <- 3

GASS$score_10[GASS$side_effects10==1] <- 0
GASS$score_10[GASS$side_effects10==2] <- 1
GASS$score_10[GASS$side_effects10==3] <- 2
GASS$score_10[GASS$side_effects10==4] <- 3

GASS$score_11[GASS$side_effects11==1] <- 0
GASS$score_11[GASS$side_effects11==2] <- 1
GASS$score_11[GASS$side_effects11==3] <- 2
GASS$score_11[GASS$side_effects11==4] <- 3

GASS$score_12[GASS$side_effects12==1] <- 0
GASS$score_12[GASS$side_effects12==2] <- 1
GASS$score_12[GASS$side_effects12==3] <- 2
GASS$score_12[GASS$side_effects12==4] <- 3

GASS$score_13[GASS$side_effects13==1] <- 0
GASS$score_13[GASS$side_effects13==2] <- 1
GASS$score_13[GASS$side_effects13==3] <- 2
GASS$score_13[GASS$side_effects13==4] <- 3

GASS$score_14[GASS$side_effects14==1] <- 0
GASS$score_14[GASS$side_effects14==2] <- 1
GASS$score_14[GASS$side_effects14==3] <- 2
GASS$score_14[GASS$side_effects14==4] <- 3

GASS$score_15[GASS$side_effects15==1] <- 0
GASS$score_15[GASS$side_effects15==2] <- 1
GASS$score_15[GASS$side_effects15==3] <- 2
GASS$score_15[GASS$side_effects15==4] <- 3

GASS$score_16[GASS$side_effects16==1] <- 0
GASS$score_16[GASS$side_effects16==2] <- 1
GASS$score_16[GASS$side_effects16==3] <- 2
GASS$score_16[GASS$side_effects16==4] <- 3

GASS$score_17[GASS$side_effects17==1] <- 0
GASS$score_17[GASS$side_effects17==2] <- 1
GASS$score_17[GASS$side_effects17==3] <- 2
GASS$score_17[GASS$side_effects17==4] <- 3

GASS$score_18[GASS$side_effects18==1] <- 0
GASS$score_18[GASS$side_effects18==2] <- 1
GASS$score_18[GASS$side_effects18==3] <- 2
GASS$score_18[GASS$side_effects18==4] <- 3

GASS$score_19[GASS$side_effects19==1] <- 0
GASS$score_19[GASS$side_effects19==2] <- 1
GASS$score_19[GASS$side_effects19==3] <- 2
GASS$score_19[GASS$side_effects19==4] <- 3

GASS$score_20[GASS$side_effects20==1] <- 0
GASS$score_20[GASS$side_effects20==2] <- 1
GASS$score_20[GASS$side_effects20==3] <- 2
GASS$score_20[GASS$side_effects20==4] <- 3

GASS$score_21[GASS$period_change==1] <- 3
GASS$score_21[GASS$period_change==0]<- 0

GASS$score_22[GASS$weight_gain==1] <- 3
GASS$score_22[GASS$weight_gain==0] <- 0

GASS$toltal_score=GASS$score_1+GASS$score_2+GASS$score_3+GASS$score_4+GASS$score_5+GASS$score_6+
  GASS$score_7+GASS$score_8+GASS$score_9+GASS$score_10+GASS$score_11+GASS$score_12+
  GASS$score_13+GASS$score_14+GASS$score_15+GASS$score_16+GASS$score_17+GASS$score_18+
  GASS$score_19+GASS$score_20+GASS$score_21+GASS$score_22

o_data_1$GASS=GASS$toltal_score#GASS全部缺失，因为没有不缺失的案例

#o_data_1$illness_severity <- origdata$overall_cgi1
#o_data_1$lack_of_insight <- o_data_1$DAI

#o_data_1$medication_supervised <- origdata$supervision_taking_medicine
#pill count?
head(o_data_1)
dim(o_data_1)



###################################################################################

#计算pharmacy record
phar_data <- read.csv("2014-2016年患者领药记录.csv")
head(phar_data)
phar_data$pharmacy_record <- (as.numeric(as.matrix(phar_data$X2015年11月))+as.numeric(as.matrix(phar_data$X2016年1月))
                              +as.numeric(as.matrix(phar_data$X2016年3月))+as.numeric(as.matrix(phar_data$X2016年5月)))/4
phar_data=phar_data[,c("ID","pharmacy_record")]
names(phar_data)[1]="study_id"
#library(reshape)
#使ID名统一，便于合并
#phar_data <- rename(phar_data,c(ID="study_id"))
#phar_added_data <- merge(origdata,phar_data,by="study_id")
#head(phar_added_data)

#o_data_1$pharmacy_record <- (as.numeric(as.matrix(phar_added_data$X2015年11月))+as.numeric(as.matrix(phar_added_data$X2016年1月))
 #                            +as.numeric(as.matrix(phar_added_data$X2016年3月))+as.numeric(as.matrix(phar_added_data$X2016年5月)))/4


#o_data_1[1:20,]
##########################################################################################
o_data_1w <- reshape(o_data_1, v.names =c(names(o_data_1)[3:dim(o_data_1)[2]]),
idvar = "study_id", timevar="redcap_event_name", direction="wide")
head(o_data_1w[,1:10])
names(o_data_1w)
dim(o_data_1w)

str(o_data_1w)
#####################################baseline########################
#sex.baseline
#用的care_model_3.3的数据
o_data_1w$sex_n.baseline <- o_data_1w$gender_care.endline_visit_may_arm_1
o_data_1w$sex_n_indicate.baseline <- rep(0, times=length(o_data_1w$sex_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$sex_n_note.baseline <- rep("用的care_model_3.3的数据", times=length(o_data_1w$sex_n.baseline))
#缺失的用case record p686中的补
for (i in 1:length(o_data_1w$sex_n.baseline)) {
if (is.na(o_data_1w$sex_n.baseline[i])&!is.na(o_data_1w$gender.enrollmentmarch_arm_1[i])) {
o_data_1w$sex_n.baseline[i]=o_data_1w$gender.enrollmentmarch_arm_1[i];
o_data_1w$sex_n_indicate.baseline;
o_data_1w$sex_n_note.baseline[i] <- "用case record p686中的补"
}
}



#marriage
#用的WHODAS基线的数据
o_data_1w$marriage_n.baseline <- o_data_1w$marriage_transformed.baseline_vist_may_arm_1
o_data_1w$marriage_n_indicate.baseline <- rep(0, times=length(o_data_1w$marriage_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$marriage_n_note.baseline <- rep("用的WHODAS基线的数据", times=length(o_data_1w$marriage_n.baseline))
#缺失的用care_model_3.3中的补
#for (i in 1:length(o_data_1w$marriage_n.baseline)) {
#if (is.na(o_data_1w$marriage_n.baseline[i])&!is.na(o_data_1w$marriage_status.endline_visit_may_arm_1[i])) {
#o_data_1w$marriage_n.baseline[i]=o_data_1w$marriage_status.endline_visit_may_arm_1[i];
#o_data_1w$marriage_n_indicate.baseline[i]=1;
#o_data_1w$marriage_n_note.baseline[i] <- "用care_model_3.3中的补"
#}
#}


#year of birth  baseline
#用的care_model_3.3的数据
o_data_1w$year_of_birth_n.baseline <- o_data_1w$date_of_birth_care.endline_visit_may_arm_1
o_data_1w$year_of_birth_n_indicate.baseline <- rep(0, times=length(o_data_1w$year_of_birth_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$year_of_birth_n_note.baseline <- rep("用的care_model_3.3的数据", times=length(o_data_1w$year_of_birth_n.baseline))
#缺失的用case record p686中的补
# for (i in 1:length(o_data_1w$year_of_birth_n.baseline)) {
#   if (is.na(o_data_1w$year_of_birth_n.baseline[i])&!is.na(as.numeric(substring(o_data_1w$date_of_birth.enrollmentmarch_arm_1[i],1,4)))) {
#     o_data_1w$year_of_birth_n.baseline[i]=as.numeric(substring(o_data_1w$date_of_birth.enrollmentmarch_arm_1[i],1,4));
#     o_data_1w$year_of_birth_n_indicate.baseline[i]=1;
#     o_data_1w$year_of_birth_n_note.baseline[i] <- "用case record p686中的补"
#   }
# }

#age
o_data_1w$age_n.baseline=2016-o_data_1w$year_of_birth_n.baseline


#occupation   baseline
#用的care_model_3.3的数据
o_data_1w$occupation_n.baseline <- o_data_1w$career_care.endline_visit_may_arm_1
o_data_1w$occupation_n_indicate.baseline <- rep(0, times=length(o_data_1w$occupation_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$occupation_n_note.baseline <- rep("用的care_model_3.3的数据", times=length(o_data_1w$occupation_n.baseline))
#attention：WHODAS的occupation 类别不同

#将WHODAS中职业的变量重新赋值，按照care_model_3.3的赋值规则
o_data_1w$employment.baseline_vist_may_arm_1[o_data_1w$employment.baseline_vist_may_arm_1==1] <- 1  #有偿工作
o_data_1w$employment.baseline_vist_may_arm_1[o_data_1w$employment.baseline_vist_may_arm_1==2] <- 2  #自营企业
o_data_1w$employment.baseline_vist_may_arm_1[o_data_1w$employment.baseline_vist_may_arm_1==3] <- 4  #无偿工作
o_data_1w$employment.baseline_vist_may_arm_1[o_data_1w$employment.baseline_vist_may_arm_1==4] <- 5  #学生
o_data_1w$employment.baseline_vist_may_arm_1[o_data_1w$employment.baseline_vist_may_arm_1==5] <- 6  #照顾家庭
o_data_1w$employment.baseline_vist_may_arm_1[o_data_1w$employment.baseline_vist_may_arm_1==6] <- 7  #退休
o_data_1w$employment.baseline_vist_may_arm_1[o_data_1w$employment.baseline_vist_may_arm_1==7] <- 8  #无业（由于健康原因）
o_data_1w$employment.baseline_vist_may_arm_1[o_data_1w$employment.baseline_vist_may_arm_1==8] <- 9  #无业（由于其他原因）
o_data_1w$employment.baseline_vist_may_arm_1[o_data_1w$employment.baseline_vist_may_arm_1==9] <- 10 #其他

# #缺失的用WHODAS中的补
# for (i in 1:length(o_data_1w$occupation_n.baseline)) {
#   if (is.na(o_data_1w$occupation_n.baseline[i])&!is.na(o_data_1w$employment.baseline_vist_may_arm_1[i])) {
#     o_data_1w$occupation_n.baseline[i]=o_data_1w$employment.baseline_vist_may_arm_1[i];
#     o_data_1w$occupation_n_indicate.baseline[i]=1;
#     o_data_1w$occupation_n_note.baseline[i] <- "用WHODAS中的补"
#   }
# }

#education  baseline
#用WHODAS的baseline 数据
o_data_1w$education.baseline <- o_data_1w$years_at_school.baseline_vist_may_arm_1
o_data_1w$education_indicate.baseline <- rep(0, times=length(o_data_1w$education.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$education_note.baseline <- rep("用WHODAS的baseline 数据", times=length(o_data_1w$education.baseline))
# #缺失的用WHODAS的third visit补
# for (i in 1:length(o_data_1w$education.baseline)) {
#   if (is.na(o_data_1w$education.baseline[i])) {
#     o_data_1w$education.baseline[i]=o_data_1w$years_at_school.third_visit_mar_arm_1[i];
#     o_data_1w$education_indicate.baseline[i]=1;
#     o_data_1w$education_note.baseline[i] <- "用WHODAS的third visit补"
#   }
# }
#缺失的用WHODAS的endline的补
# for (i in 1:length(o_data_1w$education.baseline)) {
#   if (is.na(o_data_1w$education.baseline[i])) {
#     o_data_1w$education.baseline[i]=o_data_1w$years_at_school.endline_visit_2jun_arm_1[i];
#     o_data_1w$education_indicate.baseline[i]=1;
#     o_data_1w$education_note.baseline[i] <- "用WHODAS的endline的补"
#   }
# }
#缺失的用care_model_3.3的数据的years_of_education_care补
# for (i in 1:length(o_data_1w$education.baseline)) {
#   if (is.na(o_data_1w$education.baseline[i])) {
#     o_data_1w$education.baseline[i]=o_data_1w$years_of_education_care.endline_visit_may_arm_1[i];
#     o_data_1w$education_indicate.baseline[i]=1;
#     o_data_1w$education_note.baseline[i] <- "用care_model_3.3的数据的years_of_education_care补"
#   }
# }

# #缺失的用care_model_3.3的数据的education_care补
# for (i in 1:length(o_data_1w$education.baseline)) {
#   if (is.na(o_data_1w$education.baseline[i])) {
#     o_data_1w$education.baseline[i]=o_data_1w$education_care_transformed.endline_visit_may_arm_1[i];
#     o_data_1w$education_indicate.baseline[i]=1;
#     o_data_1w$education_note.baseline[i] <- "care_model_3.3的数据的education_care补"
#   }
# }

#income
#用care model 3.3 endline的数据
o_data_1w$income_care_n.baseline=o_data_1w$income_care.endline_visit_may_arm_1
o_data_1w$income_care_n_indicate.baseline <- rep(0, times=length(o_data_1w$income_care_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$income_care_n_note.baseline <- rep("用的care_model_3.3 endline的数据", times=length(o_data_1w$income_care_n.baseline))
#family income
#用care model 3.3 endline的数据
o_data_1w$income_family_total_n.baseline=o_data_1w$income_family_total.endline_visit_may_arm_1
o_data_1w$income_family_total_n_indicate.baseline <- rep(0, times=length(o_data_1w$income_family_total_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$income_family_total_n_note.baseline <- rep("用的care_model_3.3 endline的数据", times=length(o_data_1w$income_family_total_n.baseline))
#living_status
#用care model 3.3 endline的数据
o_data_1w$living_status_care_n.baseline=o_data_1w$living_status_care.endline_visit_may_arm_1
o_data_1w$living_status_care_n_indicate.baseline <- rep(0, times=length(o_data_1w$living_status_care_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$living_status_care_n_note.baseline <- rep("用的care_model_3.3 endline的数据", times=length(o_data_1w$living_status_care_n.baseline))



#duration_of_illness  baseline
#用Care_model_3.3 (demographic)量表year_of_incidence变量
o_data_1w$duration_of_illness_n.baseline <- 2016-o_data_1w$year_of_incidence.endline_visit_may_arm_1
o_data_1w$duration_of_illness_n_indicate.baseline <- rep(0, times=length(o_data_1w$duration_of_illness_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$duration_of_illness_n_note.baseline <- rep("用Care_model_3.3 (demographic)量表year_of_incidence变量", times=length(o_data_1w$duration_of_illness_n.baseline))
#第73个为离群值，考虑为输入错误
o_data_1w$duration_of_illness_n.baseline[73]=NA
#缺失用Case Record p686量表date of diagnosis变量
# for (i in 1:length(o_data_1w$duration_of_illness_n.baseline)) {
#   if (is.na(o_data_1w$duration_of_illness_n.baseline[i])&!is.na((2016-as.numeric(format(as.Date(o_data_1w$date_of_diagnosis.enrollmentmarch_arm_1),format="%Y")))[i])) {
#     o_data_1w$duration_of_illness_n.baseline[i]=(2016-as.numeric(format(as.Date(o_data_1w$date_of_diagnosis.enrollmentmarch_arm_1),format="%Y")))[i];
#     o_data_1w$duration_of_illness_n_indicate.baseline[i]=1;
#     o_data_1w$duration_of_illness_n_note.baseline[i] <- "用Case Record p686量表date of diagnosis变量补"
#   }
# }


#cgi1  baseline
#整体严重程度，用基线的overall_cgi1
o_data_1w$cgi1_n.baseline <- o_data_1w$overall_cgi1.baseline_vist_may_arm_1
o_data_1w$cgi1_n_indicate.baseline <- rep(0, times=length(o_data_1w$cgi1_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$cgi1_n_note.baseline <- rep("用基线的overall_cgi1", times=length(o_data_1w$cgi1_n.baseline))

# #缺失用Second visit （Nov.）期的CGI替补
# for (i in 1:length(o_data_1w$cgi1_n.baseline)) {
#   if (is.na(o_data_1w$cgi1_n.baseline[i])&!is.na(o_data_1w$overall_cgi1.second_visit_nov_arm_1[i])) {
#     o_data_1w$cgi1_n.baseline[i]=o_data_1w$overall_cgi1.second_visit_nov_arm_1[i];
#     o_data_1w$cgi1_n_indicate.baseline[i]=1;
#     o_data_1w$cgi1_n_note.baseline[i] <- "用Second visit （Nov.）期的CGI替补"
#   }
# }

# #两次都没有数据，用third_visit_mar_arm_1的CGI填补
# for (i in 1:length(o_data_1w$cgi1_n.baseline)) {
#   if (is.na(o_data_1w$cgi1_n.baseline[i])&!is.na(o_data_1w$overall_cgi1.third_visit_mar_arm_1[i])) {
#     o_data_1w$cgi1_n.baseline[i]=o_data_1w$overall_cgi1.third_visit_mar_arm_1[i];
#     o_data_1w$cgi1_n_indicate.baseline[i]=2;
#     o_data_1w$cgi1_n_note.baseline[i] <- "用third_visit_mar_arm_1的CGI填补"
#   }
# }

# #还没有数据，用endline_visit_2jun_arm_1的填补
# for (i in 1:length(o_data_1w$cgi1_n.baseline)) {
#   if (is.na(o_data_1w$cgi1_n.baseline[i])&!is.na(o_data_1w$overall_cgi1.endline_visit_2jun_arm_1[i])) {
#     o_data_1w$cgi1_n.baseline[i]=o_data_1w$overall_cgi1.endline_visit_2jun_arm_1[i];
#     o_data_1w$cgi1_n_indicate.baseline[i]=2;
#     o_data_1w$cgi1_n_note.baseline[i] <- "用endline_visit_2jun_arm_1的填补"
#   }
# }



#cgi2  baseline
#整体严重程度，用基线的overall_cgi2
o_data_1w$cgi2_n.baseline <- o_data_1w$overall_cgi2.baseline_vist_may_arm_1
o_data_1w$cgi2_n_indicate.baseline <- rep(0, times=length(o_data_1w$cgi2_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$cgi2_n_note.baseline <- rep("用基线的overall_cgi2", times=length(o_data_1w$cgi2_n.baseline))

# #缺失用Second visit （Nov.）期的CGI替补
# for (i in 1:length(o_data_1w$cgi2_n.baseline)) {
#   if (is.na(o_data_1w$cgi2_n.baseline[i])&!is.na(o_data_1w$overall_cgi2.second_visit_nov_arm_1[i])) {
#     o_data_1w$cgi2_n.baseline[i]=o_data_1w$overall_cgi2.second_visit_nov_arm_1[i];
#     o_data_1w$cgi2_n_indicate.baseline[i]=1;
#     o_data_1w$cgi2_n_note.baseline[i] <- "用Second visit （Nov.）期的CGI替补"
#   }
# }

# #两次都没有数据，用third_visit_mar_arm_1的CGI填补
# for (i in 1:length(o_data_1w$cgi2_n.baseline)) {
#   if (is.na(o_data_1w$cgi2_n.baseline[i])&!is.na(o_data_1w$overall_cgi2.third_visit_mar_arm_1[i])) {
#     o_data_1w$cgi2_n.baseline[i]=o_data_1w$overall_cgi2.third_visit_mar_arm_1[i];
#     o_data_1w$cgi2_n_indicate.baseline[i]=2;
#     o_data_1w$cgi2_n_note.baseline[i] <- "用third_visit_mar_arm_1的CGI填补"
#   }
# }

# #还没有数据，用endline_visit_2jun_arm_1的填补
# for (i in 1:length(o_data_1w$cgi2_n.baseline)) {
#   if (is.na(o_data_1w$cgi2_n.baseline[i])&!is.na(o_data_1w$overall_cgi2.endline_visit_2jun_arm_1[i])) {
#     o_data_1w$cgi2_n.baseline[i]=o_data_1w$overall_cgi2.endline_visit_2jun_arm_1[i];
#     o_data_1w$cgi2_n_indicate.baseline[i]=2;
#     o_data_1w$cgi2_n_note.baseline[i] <- "用endline_visit_2jun_arm_1的填补"
#   }
# }


#WHODAS baseline
#用基线的WHODAS数据
o_data_1w$WHODAS_n.baseline <- o_data_1w$WHODAS.baseline_vist_may_arm_1
o_data_1w$WHODAS_n_indicate.baseline <- rep(0, times=length(o_data_1w$WHODAS_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$WHODAS_n_note.baseline <- rep("用基线的WHODAS数据", times=length(o_data_1w$WHODAS_n.baseline))
#填补还没有做





#sms_usage
#用Registration enrollment的数据
o_data_1w$sms_usage_n.baseline=o_data_1w$sms_usage.enrollmentmarch_arm_1
o_data_1w$sms_usage_n_indicate.baseline <- rep(0, times=length(o_data_1w$sms_usage_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$sms_usage_n_note.baseline <- rep("Registration enrollment的数据", times=length(o_data_1w$sms_usage_n.baseline))

#sms_usage_lay
#用Registration enrollment的数据
o_data_1w$sms_usage_lay_n.baseline=o_data_1w$sms_usage_lay.enrollmentmarch_arm_1
o_data_1w$sms_usage_lay_n_indicate.baseline <- rep(0, times=length(o_data_1w$sms_usage_lay_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$sms_usage_lay_n_note.baseline <- rep("Registration enrollment的数据", times=length(o_data_1w$sms_usage_lay_n.baseline))

#phone_ownership
#用Registration enrollment的数据
o_data_1w$phone_ownership_n.baseline=o_data_1w$phone_ownership.enrollmentmarch_arm_1
o_data_1w$phone_ownership_n_indicate.baseline <- rep(0, times=length(o_data_1w$phone_ownership_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$phone_ownership_n_note.baseline <- rep("Registration enrollment的数据", times=length(o_data_1w$phone_ownership_n.baseline))


#phone_ownership_lay
#用Registration enrollment的数据
o_data_1w$phone_ownership_lay_n.baseline=o_data_1w$phone_ownership_lay.enrollmentmarch_arm_1
o_data_1w$phone_ownership_lay_n_indicate.baseline <- rep(0, times=length(o_data_1w$phone_ownership_lay_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$phone_ownership_lay_n_note.baseline <- rep("Registration enrollment的数据", times=length(o_data_1w$phone_ownership_lay_n.baseline))







#DAI
#用DAI baseline的数据
o_data_1w$DAI_n.baseline=o_data_1w$DAI.baseline_vist_may_arm_1
o_data_1w$DAI_n_indicate.baseline <- rep(0, times=length(o_data_1w$DAI_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$DAI_n_note.baseline <- rep("用DAI baseline的数据", times=length(o_data_1w$DAI_n.baseline))

# #两次都没有数据，用third_visit_mar_arm_1的CGI填补
# for (i in 1:length(o_data_1w$DAI_n.baseline[i])) {
#   if (is.na(o_data_1w$DAI_n.baseline[i])&!is.na(o_data_1w$DAI.third_visit_mar_arm_1[i])) {
#     o_data_1w$DAI_n.baseline[i]=o_data_1w$DAI.third_visit_mar_arm_1[i];
#     o_data_1w$DAI_n_indicate.baseline[i]=2;
#     o_data_1w$DAI_n_note.baseline[i] <- "用third_visit_mar_arm_1的DAI填补"
#   }
# }

# #还没有数据，用endline_visit_2jun_arm_1的填补
# for (i in 1:length(o_data_1w$DAI_n.baseline[i])) {
#   if (is.na(o_data_1w$DAI_n.baseline[i])&!is.na(o_data_1w$overall_cgi2.endline_visit_2jun_arm_1[i])) {
#     o_data_1w$DAI_n.baseline[i]=o_data_1w$overall_cgi2.endline_visit_2jun_arm_1[i];
#     o_data_1w$DAI_n_indicate.baseline[i]=2;
#     o_data_1w$DAI_n_note.baseline[i] <- "用endline_visit_2jun_arm_1的DAI填补"
#   }
# }










#Bars
#用Bars baseline的数据
o_data_1w$BAR_n.baseline=o_data_1w$BAR.baseline_vist_may_arm_1
o_data_1w$BAR_n_indicate.baseline <- rep(0, times=length(o_data_1w$BAR_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$BAR_n_note.baseline <- rep("用BARs baseline的数据", times=length(o_data_1w$BAR_n.baseline))


# #用pill count1 baseline的bars值来填补
# for (i in 1:length(o_data_1w$BAR_n.baseline)) {
#   if (is.na(o_data_1w$BAR_n.baseline[i])&!is.na(o_data_1w$BAR_pill_count1.baseline_vist_may_arm_1[i])) {
#     o_data_1w$BAR_n.baseline[i]=o_data_1w$BAR_pill_count1.baseline_vist_may_arm_1[i];
#     o_data_1w$BAR_n_indicate.baseline[i]=1;
#     o_data_1w$BAR_n_note.baseline[i] <- "用pill count1 baseline的bars值来填补"
#   }
# }

# 
# #用pill count1 enrollment的bars值来填补
# for (i in 1:length(o_data_1w$BAR_n.baseline)) {
#   if (is.na(o_data_1w$BAR_n.baseline[i])&!is.na(o_data_1w$BAR_pill_count1.enrollmentmarch_arm_1[i])) {
#     o_data_1w$BAR_n.baseline[i]=o_data_1w$BAR_pill_count1.enrollmentmarch_arm_1[i];
#     o_data_1w$BAR_n_indicate.baseline[i]=1;
#     o_data_1w$BAR_n_note.baseline[i] <- "用pill count1 enrollment的bars值来填补"
#   }
# }



#morisky
#用morisky baseline的数据
o_data_1w$Morisky_n.baseline=o_data_1w$Morisky.baseline_vist_may_arm_1
o_data_1w$Morisky_n_indicate.baseline <- rep(0, times=length(o_data_1w$Morisky_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$Morisky_n_note.baseline <- rep("用morisky baseline的数据", times=length(o_data_1w$Morisky_n.baseline))


#GASS
#用GASS second visit的数据
o_data_1w$GASS_n.baseline=o_data_1w$GASS.second_visit_nov_arm_1
o_data_1w$GASS_classfied_n.baseline[o_data_1w$GASS_n.baseline<=21]=1
o_data_1w$GASS_classfied_n.baseline[o_data_1w$GASS_n.baseline>=22&o_data_1w$GASS_n.baseline<=42]=2
o_data_1w$GASS_classfied_n.baseline[o_data_1w$GASS_n.baseline>=43]=3
o_data_1w$GASS_n_indicate.baseline <- rep(0, times=length(o_data_1w$GASS_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$GASS_n_note.baseline <- rep("用GASS second visit的数据", times=length(o_data_1w$GASS_n.baseline))




#medication supervised
#用Feasibility_of_LEAN_1 second visit的数据
o_data_1w$medication_supervised_n.baseline=o_data_1w$supervision_taking_medicine.second_visit_nov_arm_1
o_data_1w$medication_supervised_n_indicate.baseline <- rep(0, times=length(o_data_1w$medication_supervised_n.baseline)) #0代表原始值，1代表R，2代表I
o_data_1w$medication_supervised_n_note.baseline <- rep("用Feasibility_of_LEAN_1 second visit的数据", times=length(o_data_1w$medication_supervised_n.baseline))



##########################################endline#########################################
#Bars
#用Bars endline的数据
o_data_1w$BAR_n.endline=o_data_1w$BAR.endline_visit_2jun_arm_1
o_data_1w$BAR_n_indicate.endline <- rep(0, times=length(o_data_1w$BAR_n.endline)) #0代表原始值，1代表R，2代表I
o_data_1w$BAR_n_note.endline <- rep("用BARs endline的数据", times=length(o_data_1w$BAR_n.endline))
#其他期都是缺失的,无法插补

#pharmacy record
o_data_1w=merge(o_data_1w,phar_data,by=names(phar_data)[1])
names(o_data_1w)[which(names(o_data_1w)=="pharmacy_record")]="pharmacy_record_n.endline"
o_data_1w$pharmacy_record_n_indicate.endline <- rep(0, times=length(o_data_1w$pharmacy_record_n.endline)) #0代表原始值，1代表R，2代表I
o_data_1w$pharmacy_record_n_note.endline <- rep("额外表格的数据", times=length(o_data_1w$pharmacy_record_n.endline))


#Dai
#用DAI endline的数据
o_data_1w$DAI_n.endline=o_data_1w$DAI.endline_visit_2jun_arm_1
o_data_1w$DAI_n_indicate.endline <- rep(0, times=length(o_data_1w$DAI_n.endline)) #0代表原始值，1代表R，2代表I
o_data_1w$DAI_n_note.endline <- rep("用DAI endline的数据", times=length(o_data_1w$DAI_n.endline))

#缺失用third_visit_mar_arm_1的DAI填补
# for (i in 1:length(o_data_1w$DAI_n.endline)) {
#   if (is.na(o_data_1w$DAI_n.endline[i])&!is.na(o_data_1w$DAI.third_visit_mar_arm_1[i])) {
#     o_data_1w$DAI_n.endline[i]=o_data_1w$DAI.third_visit_mar_arm_1[i];
#     o_data_1w$DAI_n_indicate.endline[i]=2;
#     o_data_1w$DAI_n_note.endline[i] <- "用third_visit_mar_arm_1的DAI填补"
#   }
# }

#缺失用baseline_vist_may_arm_1的DAI填补
# for (i in 1:length(o_data_1w$DAI_n.endline)) {
#   if (is.na(o_data_1w$DAI_n.endline[i])&!is.na(o_data_1w$DAI.baseline_vist_may_arm_1[i])) {
#     o_data_1w$DAI_n.endline[i]=o_data_1w$DAI.baseline_vist_may_arm_1[i];
#     o_data_1w$DAI_n_indicate.endline[i]=2;
#     o_data_1w$DAI_n_note.endline[i] <- "用baseline_vist_may_arm_1的DAI填补"
#   }
# }


#Morisky
#用Morisky endline的数据
o_data_1w$Morisky_n.endline=o_data_1w$Morisky.endline_visit_2jun_arm_1
o_data_1w$Morisky_n_indicate.endline <- rep(0, times=length(o_data_1w$Morisky_n.endline)) #0代表原始值，1代表R，2代表I
o_data_1w$Morisky_n_note.endline <- rep("用Morisky endline的数据", times=length(o_data_1w$Morisky_n.endline))

#缺失用third_visit_mar_arm_1的morisky填补
# for (i in 1:length(o_data_1w$Morisky_n.endline)) {
#   if (is.na(o_data_1w$Morisky_n.endline[i])&!is.na(o_data_1w$Morisky.third_visit_mar_arm_1[i])) {
#     o_data_1w$Morisky_n.endline[i]=o_data_1w$Morisky.third_visit_mar_arm_1[i];
#     o_data_1w$Morisky_n_indicate.endline[i]=2;
#     o_data_1w$Morisky_n_note.endline[i] <- "third_visit_mar_arm_1的morisky填补"
#   }
# }

#缺失用baseline_vist_may_arm_1的morisky填补
# for (i in 1:length(o_data_1w$Morisky_n.endline)) {
#   if (is.na(o_data_1w$Morisky_n.endline[i])&!is.na(o_data_1w$Morisky.baseline_vist_may_arm_1[i])) {
#     o_data_1w$Morisky_n.endline[i]=o_data_1w$Morisky.baseline_vist_may_arm_1[i];
#     o_data_1w$Morisky_n_indicate.endline[i]=2;
#     o_data_1w$Morisky_n_note.endline[i] <- "用baseline_vist_may_arm_1的morisky填补"
#   }
# }





#cgi1 endline
##整体严重程度，用endline_visit_2jun_arm_1的overall_cgi1
o_data_1w$cgi1_n.endline <- o_data_1w$overall_cgi1.endline_visit_2jun_arm_1
o_data_1w$cgi1_n_indicate.endline <- rep(0, times=length(o_data_1w$cgi1_n.endline)) #0代表原始值，1代表R，2代表I
o_data_1w$cgi1_n_note.endline <- rep("用endline_visit_2jun_arm_1的overall_cgi1", times=length(o_data_1w$cgi1_n.endline))
#缺失用third_visit_mar_arm_1的CGI填补
# for (i in 1:length(o_data_1w$cgi1_n.endline)) {
#   if (is.na(o_data_1w$cgi1_n.endline[i])&!is.na(o_data_1w$overall_cgi1.third_visit_mar_arm_1[i])) {
#     o_data_1w$cgi1_n.endline[i]=o_data_1w$overall_cgi1.third_visit_mar_arm_1[i];
#     o_data_1w$cgi1_n_indicate.endline[i]=2;
#     o_data_1w$cgi1_n_note.endline[i] <- "用third_visit_mar_arm_1的CGI填补"
#   }
# }
# #再缺失用second_visit_nov_arm_1的CGI填补
# for (i in 1:length(o_data_1w$cgi1_n.endline)) {
#   if (is.na(o_data_1w$cgi1_n.endline[i])&!is.na(o_data_1w$overall_cgi1.second_visit_nov_arm_1[i])) {
#     o_data_1w$cgi1_n.endline[i]=o_data_1w$overall_cgi1.second_visit_nov_arm_1[i];
#     o_data_1w$cgi1_n_indicate.endline[i]=2;
#     o_data_1w$cgi1_n_note.endline[i] <- "用second_visit_nov_arm_1的CGI填补"
#   }
# }
# #再缺失用baseline_vist_may_arm_1的CGI填补
# for (i in 1:length(o_data_1w$cgi1_n.endline)) {
#   if (is.na(o_data_1w$cgi1_n.endline[i])&!is.na(o_data_1w$overall_cgi1.baseline_vist_may_arm_1[i])) {
#     o_data_1w$cgi1_n.endline[i]=o_data_1w$overall_cgi1.baseline_vist_may_arm_1[i];
#     o_data_1w$cgi1_n_indicate.endline[i]=2;
#     o_data_1w$cgi1_n_note.endline[i] <- "用baseline_vist_may_arm_1的CGI填补"
#   }
# }


#cgi2  endline
##整体严重程度，用endline_visit_2jun_arm_1的overall_cgi2
o_data_1w$cgi2_n.endline <- o_data_1w$overall_cgi2.endline_visit_2jun_arm_1
o_data_1w$cgi2_n_indicate.endline <- rep(0, times=length(o_data_1w$cgi2_n.endline)) #0代表原始值，1代表R，2代表I
o_data_1w$cgi2_n_note.endline <- rep("用endline_visit_2jun_arm_1的overall_cgi2", times=length(o_data_1w$cgi2_n.endline))
#缺失用third_visit_mar_arm_1的CGI填补
# for (i in 1:length(o_data_1w$cgi2_n.endline)) {
#   if (is.na(o_data_1w$cgi2_n.endline[i])&!is.na(o_data_1w$overall_cgi2.third_visit_mar_arm_1[i])) {
#     o_data_1w$cgi2_n.endline[i]=o_data_1w$overall_cgi2.third_visit_mar_arm_1[i];
#     o_data_1w$cgi2_n_indicate.endline[i]=2;
#     o_data_1w$cgi2_n_note.endline[i] <- "用third_visit_mar_arm_1的CGI填补"
#   }
# }
# #再缺失用second_visit_nov_arm_1的CGI填补
# for (i in 1:length(o_data_1w$cgi2_n.endline)) {
#   if (is.na(o_data_1w$cgi2_n.endline[i])&!is.na(o_data_1w$overall_cgi2.second_visit_nov_arm_1[i])) {
#     o_data_1w$cgi2_n.endline[i]=o_data_1w$overall_cgi2.second_visit_nov_arm_1[i];
#     o_data_1w$cgi2_n_indicate.endline[i]=2;
#     o_data_1w$cgi2_n_note.endline[i] <- "用second_visit_nov_arm_1的CGI填补"
#   }
# }
# #再缺失用baseline_vist_may_arm_1的CGI填补
# for (i in 1:length(o_data_1w$cgi2_n.endline)) {
#   if (is.na(o_data_1w$cgi2_n.endline[i])&!is.na(o_data_1w$overall_cgi2.baseline_vist_may_arm_1[i])) {
#     o_data_1w$cgi2_n.endline[i]=o_data_1w$overall_cgi2.baseline_vist_may_arm_1[i];
#     o_data_1w$cgi2_n_indicate.endline[i]=2;
#     o_data_1w$cgi2_n_note.endline[i] <- "用baseline_vist_may_arm_1的CGI填补"
#   }
# }



#WHODAS endline
#用endline_visit_2jun_arm_1的数据
o_data_1w$WHODAS_n.endline <- o_data_1w$WHODAS.endline_visit_2jun_arm_1
o_data_1w$WHODAS_n_indicate.endline <- rep(0, times=length(o_data_1w$WHODAS_n.endline)) #0代表原始值，1代表R，2代表I
o_data_1w$WHODAS_n_note.endline <- rep("用endline_visit_2jun_arm_1的数据", times=length(o_data_1w$WHODAS_n.endline))
#缺失用third_visit_mar_arm_1的数据填补
# for (i in 1:length(o_data_1w$WHODAS_n.endline)) {
#   if (is.na(o_data_1w$WHODAS_n.endline[i])&!is.na(o_data_1w$WHODAS.third_visit_mar_arm_1[i])) {
#     o_data_1w$WHODAS_n.endline[i]=o_data_1w$WHODAS.third_visit_mar_arm_1[i];
#     o_data_1w$WHODAS_n_indicate.endline[i]=2;
#     o_data_1w$WHODAS_n_note.endline[i] <- "用third_visit_mar_arm_1的数据填补"
#   }
# }
#再缺失用baseline_vist_may_arm_1的数据填补
# for (i in 1:length(o_data_1w$WHODAS_n.endline)) {
#   if (is.na(o_data_1w$WHODAS_n.endline[i])&!is.na(o_data_1w$WHODAS.baseline_vist_may_arm_1[i])) {
#     o_data_1w$WHODAS_n.endline[i]=o_data_1w$WHODAS.baseline_vist_may_arm_1[i];
#     o_data_1w$WHODAS_n_indicate.endline[i]=2;
#     o_data_1w$WHODAS_n_note.endline[i] <- "用baseline_vist_may_arm_1的数据填补"
#   }
# }






data.final=o_data_1w[,-c(which(names(o_data_1w)=="gender_care.enrollmentmarch_arm_1"):which(names(o_data_1w)=="GASS.baseline_vist_may_arm_1"))]

write.csv(data.final,file = "dataset1.csv")

################################################################################################

#datal <- reshape(o_data_1w,direction = "long",v.names =c(names(o_data_1w)[3:dim(o_data_1)[2]]),
#sep = ".")


#data.test=data.final[,c("sex_n.baseline","marriage_n.baseline","year_of_birth_n.baseline","occupation_n.baseline","education.baseline","income_care_n.baseline","income_family_total_n.baseline",
 #                       "living_status_care_n.baseline","duration_of_illness_n.baseline","medication_supervised_n.baseline")]
#im(na.omit(data.test))