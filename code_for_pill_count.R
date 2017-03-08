#¶ÁÈëÊı¾İ
setwd("E:\\Lean\\20170214")
origdata <- read.csv("data_Interven.csv")
dim(origdata)

pill_data_fenzi <- origdata[,c(which(names(origdata)=="study_id"),which(names(origdata)=="redcap_event_name"),which(names(origdata)=="Interven"),
                               which(names(origdata)=="drug_code_name"),which(names(origdata)=="number_of_pills"),which(names(origdata)=="number_of_pills_6"),which(names(origdata)=="pills_new"),
                               which(names(origdata)=="drug_code_name2_ceb"),which(names(origdata)=="number_of_pills2_d1d"),which(names(origdata)=="number_of_pills_62_000"),which(names(origdata)=="pills_new2_828"),
                               which(names(origdata)=="drug_code_name2_957"),which(names(origdata)=="number_of_pills2_b20"),which(names(origdata)=="number_of_pills_62_2fc"),which(names(origdata)=="pills_new2_61f"),
                               which(names(origdata)=="drug_code_name2_fe6"),which(names(origdata)=="number_of_pills2_dbc"),which(names(origdata)=="number_of_pills_62_1fb"),which(names(origdata)=="pills_new2_6d4"),
                               which(names(origdata)=="drug_code_name2_569"),which(names(origdata)=="number_of_pills2_d2d"),which(names(origdata)=="number_of_pills_62_623"),which(names(origdata)=="pills_new2_7ac"),
                               which(names(origdata)=="drug_code_name2_e96"),which(names(origdata)=="number_of_pills2_11f"),which(names(origdata)=="number_of_pills_62_724"),which(names(origdata)=="pills_new2_3cf"),
                               which(names(origdata)=="drug_code_name2_3ed"),which(names(origdata)=="number_of_pills2_f27"),which(names(origdata)=="number_of_pills_62_864"),which(names(origdata)=="pills_new2_3e9"),
                               which(names(origdata)=="drug_code_name2_0d2"),which(names(origdata)=="number_of_pills2_4f0"),which(names(origdata)=="number_of_pills_62_e2a"),which(names(origdata)=="pills_new2_ad9"))]

write.csv(pill_data_fenzi,file = "pill_data_fenzi.csv")

pill_data_fenmu=origdata[origdata$redcap_event_name=="third_visit_mar_arm_1",c(which(names(origdata)=="study_id"),which(names(origdata)=="redcap_event_name"),which(names(origdata)=="Interven"),
                            which(names(origdata)=="name_drug_1"),which(names(origdata)=="amount_drug"),which(names(origdata)=="weight_of_drugs"),
                            which(names(origdata)=="name_drug_12_03b"),which(names(origdata)=="amount_drug2_932"),which(names(origdata)=="weight_of_drugs2_71e"),
                            which(names(origdata)=="name_drug_12_abd"),which(names(origdata)=="amount_drug2_6dc"),which(names(origdata)=="weight_of_drugs2_32c"),
                            which(names(origdata)=="name_drug_12_0bd"),which(names(origdata)=="amount_drug2_451"),which(names(origdata)=="weight_of_drugs2_df6"),
                            which(names(origdata)=="name_drug_12_47e"),which(names(origdata)=="amount_drug2_5fe "),which(names(origdata)=="weight_of_drugs2_bda"))]

write.csv(pill_data_fenmu,file = "pill_data_fenmu.csv")
