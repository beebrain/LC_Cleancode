#first Script Evaluate_map_new4_optimize.R-> Evaluate_Fcore_OpenMax_LC_4.R -> Calculate_AUC_OpenMax.R -> EvaluatefindMaxFscore_cutoff.R
#update Add Alexnet Mesument for TP TN FN TP
# Update 6/10/2019
# this Script update for filter incorrect remove out
# Not use openset Data 
# change Parameter If select only correct dataset
rm(list=ls())  
library(data.table)
library(xlsx)
library(ggplot2)

library("argparser")

parser <- arg_parser(description='Process commandline arguments')
parser <- add_argument(parser, arg="--samplingNum", type="integer", help = "sampling Number")
parser <- add_argument(parser, arg="--modeMethod", type="integer", help = "1 is openmax , 2 is eak, 3 is pow3ak")
#
args = parse_args(parser)
samplingNumber =  args$samplingNum
method_select =  args$modeMethod

samplingNumber =  300   ### fixCode remove when you need to run
method_select =  5     ### fixcode remove when you need to run

rep = 98

min1000Matrix = function(data){ return (min(data,1000))}


calculate_PR_and_AP_ObjectDetection = function(All_data_PR,rep,samplingNumber){
  total_data = nrow(All_data_PR)
  
  Data_cal = All_data_PR[,c("GT_Index","Obj_predict","Prob_Of_Obj")]
  
  
  
  #order by ...
  RecallData = Data_cal[,"Prob_Of_Obj"]
  RecallIndex_order = order(-RecallData)
  
  #Sort range 
  Data_cal.sort = Data_cal[RecallIndex_order,]
  head(Data_cal)
  head(Data_cal.sort)
  
  
  
  cutoff_index = Data_cal.sort[,"Prob_Of_Obj"]
  
  
  TP_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1)
  FP_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1)
  FN_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1)
  TN_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1) 
  
  
  
  
  
  for (indexCutoff in seq(1:length(cutoff_index))){
    if (indexCutoff%%1000 == 0){
      print(paste(indexCutoff,"of ",length(cutoff_index)))
    }
    cutoff  = cutoff_index[indexCutoff]
    Data_cal.sort["Predict"] = Data_cal.sort$Obj_predict   ### Predict following by openMaxPrecit
    
    
    
    id_cut_to_unseenPredict = which(Data_cal.sort[,"Prob_Of_Obj"]< cutoff)
    Data_cal.sort[id_cut_to_unseenPredict,"Predict"]  = 1
    
    
    paticipate_data_Presicion = Data_cal.sort
    
    paticipate_data_Presicion["TP"] = 0
    paticipate_data_Presicion["FP"] = 0
    paticipate_data_Presicion["FN"] = 0
    paticipate_data_Presicion["TN"] = 0
    
    
    ########################### Find TP FP FN  1 is positive that is unseen image 0 is seen that is nagative###########
    paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]== 1 & paticipate_data_Presicion[,"GT_Index"] == 1),"TP"] =  1
    
    paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==1 
                                    & paticipate_data_Presicion[,"GT_Index"]!= 1),"FP"] = 1
    
    paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]== 0 
                                    & paticipate_data_Presicion[,"GT_Index"]!= 0) ,"FN"] = 1
    
    paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]== 0 
                                    & paticipate_data_Presicion[,"GT_Index"]== 0) ,"TN"] = 1
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    TP_Kclass_cutoff = sum(paticipate_data_Presicion$TP)
    FP_Kclass_cutoff = sum(paticipate_data_Presicion$FP)
    FN_Kclass_cutoff = sum(paticipate_data_Presicion$FN)
    TN_Kclass_cutoff = sum(paticipate_data_Presicion$TN)
    
    
    
    
    
    
    
    TP_ALL[indexCutoff] = TP_Kclass_cutoff
    FP_ALL[indexCutoff] = FP_Kclass_cutoff
    FN_ALL[indexCutoff] = FN_Kclass_cutoff
    TN_ALL[indexCutoff] = TN_Kclass_cutoff
    
  }
  
  
  Data_TP_ALL  =   data.frame(TP_ALL)
  Data_TP_ALL = cbind(cutoff_index,Data_TP_ALL)
  Data_FP_ALL  =   data.frame(FP_ALL)
  Data_FP_ALL = cbind(cutoff_index,Data_FP_ALL)
  Data_FN_ALL  =   data.frame(FN_ALL)
  Data_FN_ALL = cbind(cutoff_index,Data_FN_ALL)
  Data_TN_ALL  =   data.frame(TN_ALL)
  Data_TN_ALL = cbind(cutoff_index,Data_TN_ALL)
  
  write.csv(Data_TP_ALL,paste(toString(rep),"/",samplingNumber,"_TP_Binary_Obj.csv",sep = ""))
  write.csv(Data_FP_ALL,paste(toString(rep),"/",samplingNumber,"_FP_Binary_Obj.csv",sep = ""))
  write.csv(Data_FN_ALL,paste(toString(rep),"/",samplingNumber,"_FN_Binary_Obj.csv",sep = ""))
  write.csv(Data_TN_ALL,paste(toString(rep),"/",samplingNumber,"_TN_Binary_Obj.csv",sep = ""))
  
}

######################## Find AP each class ###############################
calculate_PR_and_AP = function(All_data_PR,method = 'eak',rep,samplingNumber){
  total_data = nrow(All_data_PR)
  
  Data_cal = All_data_PR[,c("GT_Index","SoftMaxPredict",method)]
  
  
  #order by ...
  RecallData = Data_cal[,method]
  RecallIndex_order = order(-RecallData)
  
  #Sort range 
  Data_cal.sort = Data_cal[RecallIndex_order,]
  head(Data_cal)
  head(Data_cal.sort)
  
  
  
  cutoff_index = Data_cal.sort[,method]
  
  
  TP_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1)
  FP_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1)
  FN_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1)
  TN_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1) 
  
  

  
  
  for (indexCutoff in seq(1:length(cutoff_index))){
    if (indexCutoff%%1000 == 0){
      print(paste(indexCutoff,"of ",length(cutoff_index)))
    }
    
    cutoff  = cutoff_index[indexCutoff]
    Data_cal.sort["Predict"] = Data_cal.sort$SoftMaxPredict
    id_cut_to_unseenPredict = which(Data_cal.sort[,method]< cutoff)
    Data_cal.sort[id_cut_to_unseenPredict,"Predict"]  = 1
    
    ### relabel GT_Index_C for fooing
    #Data_cal.sort$GT_Index_C = apply(as.matrix(Data_cal.sort[,"GT_Index"]), 1, min1000Matrix)
    
   
        paticipate_data_Presicion = Data_cal.sort
        
        paticipate_data_Presicion["TP"] = 0
        paticipate_data_Presicion["FP"] = 0
        paticipate_data_Presicion["FN"] = 0
        paticipate_data_Presicion["TN"] = 0

        
        ########################### Find TP FP FN  1 is positive that is unseen image 0 is seen that is nagative###########
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]== 1 & paticipate_data_Presicion[,"GT_Index"] == 1),"TP"] =  1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==1 
                                        & paticipate_data_Presicion[,"GT_Index"]!= 1),"FP"] = 1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]== 0 
                                        & paticipate_data_Presicion[,"GT_Index"]!= 0) ,"FN"] = 1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]== 0 
                                        & paticipate_data_Presicion[,"GT_Index"]== 0) ,"TN"] = 1
        
        TP_Kclass_cutoff = sum(paticipate_data_Presicion$TP)
        FP_Kclass_cutoff = sum(paticipate_data_Presicion$FP)
        FN_Kclass_cutoff = sum(paticipate_data_Presicion$FN)
        TN_Kclass_cutoff = sum(paticipate_data_Presicion$TN)
        
        TP_ALL[indexCutoff] = TP_Kclass_cutoff
        FP_ALL[indexCutoff] = FP_Kclass_cutoff
        FN_ALL[indexCutoff] = FN_Kclass_cutoff
        TN_ALL[indexCutoff] = TN_Kclass_cutoff
        
    
    
  }
  Data_TP_ALL  =   data.frame(TP_ALL)
  Data_TP_ALL = cbind(cutoff_index,Data_TP_ALL)
  Data_FP_ALL  =   data.frame(FP_ALL)
  Data_FP_ALL = cbind(cutoff_index,Data_FP_ALL)
  Data_FN_ALL  =   data.frame(FN_ALL)
  Data_FN_ALL = cbind(cutoff_index,Data_FN_ALL)
  Data_TN_ALL  =   data.frame(TN_ALL)
  Data_TN_ALL = cbind(cutoff_index,Data_TN_ALL)
  
  write.csv(Data_TP_ALL,paste(toString(rep),"/",samplingNumber,"_TP_Binary_",method,".csv",sep = ""))
  write.csv(Data_FP_ALL,paste(toString(rep),"/",samplingNumber,"_FP_Binary_",method,".csv",sep = ""))
  write.csv(Data_FN_ALL,paste(toString(rep),"/",samplingNumber,"_FN_Binary_",method,".csv",sep = ""))
  write.csv(Data_TN_ALL,paste(toString(rep),"/",samplingNumber,"_TN_Binary_",method,".csv",sep = ""))
}

#### prepare Openmax Data before import to the function
calculate_PR_and_AP_Openmax = function(All_data_PR,rep,samplingNumber){
  total_data = nrow(All_data_PR)
  Data_cal = All_data_PR[,c("GT_Index","OpenMaxPredict","Prob_Of_Openmax")]

  
  #order by ...
  RecallData = Data_cal[,"Prob_Of_Openmax"]
  RecallIndex_order = order(-RecallData)
  
  #Sort range 
  Data_cal.sort = Data_cal[RecallIndex_order,]
  head(Data_cal)
  head(Data_cal.sort)
  
  
  cutoff_index = Data_cal.sort[,"Prob_Of_Openmax"]
  
  
  TP_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1)
  FP_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1)
  FN_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1)
  TN_ALL = matrix(rep(0,length(cutoff_index)*1), nrow = length(cutoff_index), ncol = 1) 
  

  for (indexCutoff in seq(1:length(cutoff_index))){
    if (indexCutoff%%1000 == 0){
      print(paste(indexCutoff,"of ",length(cutoff_index)))
    }
    cutoff  = cutoff_index[indexCutoff]
    Data_cal.sort["Predict"] = Data_cal.sort$OpenMaxPredict   ### Predict following by openMaxPrecit
    
    id_cut_to_unseenPredict = which(Data_cal.sort[,"Prob_Of_Openmax"]< cutoff)
    Data_cal.sort[id_cut_to_unseenPredict,"Predict"]  = 1
    
    
    paticipate_data_Presicion = Data_cal.sort
    
    paticipate_data_Presicion["TP"] = 0
    paticipate_data_Presicion["FP"] = 0
    paticipate_data_Presicion["FN"] = 0
    paticipate_data_Presicion["TN"] = 0
    
    
    ########################### Find TP FP FN  1 is positive that is unseen image 0 is seen that is nagative###########
    paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]== 1 & paticipate_data_Presicion[,"GT_Index"] == 1),"TP"] =  1
    
    paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==1 
                                    & paticipate_data_Presicion[,"GT_Index"]!= 1),"FP"] = 1
    
    paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]== 0 
                                    & paticipate_data_Presicion[,"GT_Index"]!= 0) ,"FN"] = 1
    
    paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]== 0 
                                    & paticipate_data_Presicion[,"GT_Index"]== 0) ,"TN"] = 1
    
    TP_Kclass_cutoff = sum(paticipate_data_Presicion$TP)
    FP_Kclass_cutoff = sum(paticipate_data_Presicion$FP)
    FN_Kclass_cutoff = sum(paticipate_data_Presicion$FN)
    TN_Kclass_cutoff = sum(paticipate_data_Presicion$TN)
    
    TP_ALL[indexCutoff] = TP_Kclass_cutoff
    FP_ALL[indexCutoff] = FP_Kclass_cutoff
    FN_ALL[indexCutoff] = FN_Kclass_cutoff
    TN_ALL[indexCutoff] = TN_Kclass_cutoff
    
  }
  
  
  Data_TP_ALL  =   data.frame(TP_ALL)
  Data_TP_ALL = cbind(cutoff_index,Data_TP_ALL)
  Data_FP_ALL  =   data.frame(FP_ALL)
  Data_FP_ALL = cbind(cutoff_index,Data_FP_ALL)
  Data_FN_ALL  =   data.frame(FN_ALL)
  Data_FN_ALL = cbind(cutoff_index,Data_FN_ALL)
  Data_TN_ALL  =   data.frame(TN_ALL)
  Data_TN_ALL = cbind(cutoff_index,Data_TN_ALL)
  
  write.csv(Data_TP_ALL,paste(toString(rep),"/",samplingNumber,"_TP_Binary_Openmax.csv",sep = ""))
  write.csv(Data_FP_ALL,paste(toString(rep),"/",samplingNumber,"_FP_Binary_Openmax.csv",sep = ""))
  write.csv(Data_FN_ALL,paste(toString(rep),"/",samplingNumber,"_FN_Binary_Openmax.csv",sep = ""))
  write.csv(Data_TN_ALL,paste(toString(rep),"/",samplingNumber,"_TN_Binary_Openmax.csv",sep = ""))
  
}



if (!dir.exists(toString(rep))){
  dir.create(toString(rep), showWarnings = TRUE, recursive = FALSE, mode = "0777")
} else {
  print("Dir already exists!")
}
# 
# 
# sample_index = sample(301,samplingNumber)
# List_Openset = list()
# for (i in seq(1:360)){
#   List_Openset = c(List_Openset, list(sample_index+((i-1)*301)))
# }
# 
# 
# #### please copy List Sampling from Evaluate Folder
# if (!file.exists(paste("./",samplingNumber,"_Listsampling.txt",sep = ""))){
#   sample_index = sample(301,samplingNumber)
#   List_Openset = list()
#   for (i in seq(1:360)){
#     List_Openset = c(List_Openset, list(sample_index+((i-1)*301)))
#   }
#   
#   SamplingOpenSet = array(as.numeric(unlist(List_Openset)))
#   print("Create Fix File")
#   write.csv(SamplingOpenSet,paste("./",samplingNumber,"_Listsampling.txt",sep = ""))
# }else{
#   SamplingOpenSet_e <- read.csv(paste("./",samplingNumber,"_Listsampling.txt",sep = ""),header = TRUE)
#   SamplingOpenSet = array(as.numeric(SamplingOpenSet_e$x))
#   print("Load Fix list sampling")
#   #print(SamplingOpenSet)
# }

#start load raw openMax Cdata 
openMax_Data_CloseSet <- read.csv("../ResultObjectMap/OpenMax_ResultCloseset_fill20Class.txt",header = TRUE)
openMax_Data_HackImage_raw <- read.csv("../ResultObjectMap/OpenMax_ResultOpenset_fill20Class.txt",header = TRUE)

###############  Addition Remove Incorrect predict in Result Closeset #####################
 #IndexCorrect_op = which(openMax_Data_CloseSet$SoftMaxPredict == openMax_Data_CloseSet$GT_Index)
 #openMax_Data_CloseSet = openMax_Data_CloseSet[IndexCorrect_op,]


############## ReIndex to binary if 0 is seen and 1 is unseen 
IndexCloseset_Pre_gt = which(openMax_Data_CloseSet$OpenMaxPredict < 20)
IndexHackImage_Pre_gt = which(openMax_Data_HackImage_raw$OpenMaxPredict < 20)
openMax_Data_CloseSet[IndexCloseset_Pre_gt,"OpenMaxPredict"] = 0
openMax_Data_HackImage_raw[IndexHackImage_Pre_gt,"OpenMaxPredict"] = 0

IndexCloseset_Pre_op = which(openMax_Data_CloseSet$OpenMaxPredict == 20)
IndexHackImage_Pre_op = which(openMax_Data_HackImage_raw$OpenMaxPredict == 20)
openMax_Data_CloseSet[IndexCloseset_Pre_op,"OpenMaxPredict"] = 1
openMax_Data_HackImage_raw[IndexHackImage_Pre_op,"OpenMaxPredict"] = 1


openMax_Data_CloseSet$GT_Index = 0
openMax_Data_HackImage_raw$GT_Index = 1
##################################################################



############## invert Prob of open Max that predict class 1000 th
openMax_Data_CloseSet[IndexCloseset_Pre_op,]$Prob_Of_Openmax = 1-openMax_Data_CloseSet[IndexCloseset_Pre_op,]$Prob_Of_Openmax
openMax_Data_HackImage_raw[IndexHackImage_Pre_op,]$Prob_Of_Openmax = 1- openMax_Data_HackImage_raw[IndexHackImage_Pre_op,]$Prob_Of_Openmax


################# load Data LC ################

LC_Data_CloseSet <- read.csv("../ResultObjectMap/Lecog_ResultCloseset_fill20Class.txt",header = TRUE)
LC_Data_HackImage_raw <- read.csv("../ResultObjectMap/Lecog_ResultOpenset_fill20Class.txt",header = TRUE)

###############  Addition Remove Incorrect predict in Result Closeset #####################
 #IndexCorrect_op = which(LC_Data_CloseSet$SoftMaxPredict == LC_Data_CloseSet$GT_Index)
 #LC_Data_CloseSet = LC_Data_CloseSet[IndexCorrect_op,]



############## ReIndex to binary if 0 is seen and 1 is unseen 
IndexCloseset_Pre_gt = which(LC_Data_CloseSet$SoftMaxPredict < 20)
IndexHackImage_Pre_gt = which(LC_Data_HackImage_raw$SoftMaxPredict < 20)
LC_Data_CloseSet[IndexCloseset_Pre_gt,"SoftMaxPredict"] = 0
LC_Data_HackImage_raw[IndexHackImage_Pre_gt,"SoftMaxPredict"] = 0

IndexCloseset_Pre_op = which(LC_Data_CloseSet$SoftMaxPredict == 20)
IndexHackImage_Pre_op = which(LC_Data_HackImage_raw$SoftMaxPredict == 20)
LC_Data_CloseSet[IndexCloseset_Pre_op,"SoftMaxPredict"] = 1
LC_Data_HackImage_raw[IndexHackImage_Pre_op,"SoftMaxPredict"] = 1


LC_Data_CloseSet$GT_Index = 0
LC_Data_HackImage_raw$GT_Index = 1
##################################################################


################# load Data Objectdeteciton ################

Obj_Data_CloseSet <- read.csv("../ResultObjectMap/Obj_resultCloseset_fill20Class.txt",header = TRUE)
Obj_Data_HackImage_raw <- read.csv("../ResultObjectMap/Obj_resultOpenset_fill20Class.txt",header = TRUE)

###############  Addition Remove Incorrect predict in Result Closeset #####################
#IndexCorrect_op = which(LC_Data_CloseSet$SoftMaxPredict == LC_Data_CloseSet$GT_Index)
#LC_Data_CloseSet = LC_Data_CloseSet[IndexCorrect_op,]



############## ReIndex to binary if 0 is seen and 1 is unseen 
IndexCloseset_Pre_gt = which(Obj_Data_CloseSet$SoftMaxPredict < 20)
IndexHackImage_Pre_gt = which(Obj_Data_HackImage_raw$SoftMaxPredict < 20)
Obj_Data_CloseSet[IndexCloseset_Pre_gt,"SoftMaxPredict"] = 0
Obj_Data_HackImage_raw[IndexHackImage_Pre_gt,"SoftMaxPredict"] = 0

IndexCloseset_Pre_op = which(Obj_Data_CloseSet$SoftMaxPredict == 20)
IndexHackImage_Pre_op = which(Obj_Data_HackImage_raw$SoftMaxPredict == 20)
Obj_Data_CloseSet[IndexCloseset_Pre_op,"SoftMaxPredict"] = 1
Obj_Data_HackImage_raw[IndexHackImage_Pre_op,"SoftMaxPredict"] = 1


Obj_Data_CloseSet$GT_Index = 0
Obj_Data_HackImage_raw$GT_Index = 1
##################################################################


########################## Relabel Grouth HackImage TO 1000 only labe in Adversarial case ####################
openMax_Data_HackImage_raw$GT_Index = 1
LC_Data_HackImage_raw$GT_Index = 1
Obj_Data_HackImage_raw$GT_Index = 1


#######################################################


if (method_select == 1){
  ### Bind All Dataset
  All_data_openmax  = data.frame()
  All_data_openmax = rbind(openMax_Data_CloseSet,openMax_Data_HackImage_raw)
  
  ##### calculate AUC Openmax
  print("CalCulate TP FP FN from openmax")
  calculate_PR_and_AP_Openmax(All_data_openmax,rep,samplingNumber)
  
}else if(method_select ==2){
  
  All_data_LC  = data.frame()
  All_data_LC = rbind(LC_Data_CloseSet,LC_Data_HackImage_raw)
  
  print("CalCulate TP FP FN from eak")
  calculate_PR_and_AP(All_data_LC,method = "eak",rep,samplingNumber)
}else if(method_select ==3){
  
  
  All_data_LC  = data.frame()
  All_data_LC = rbind(LC_Data_CloseSet,LC_Data_HackImage_raw)
  
  
  print("CalCulate TP FP FN from 3ak")
  calculate_PR_and_AP(All_data_LC,method = "pow3Ak",rep,samplingNumber)
  
}else if (method_select == 4){
  
  All_data_LC  = data.frame()
  All_data_LC = rbind(LC_Data_CloseSet,LC_Data_HackImage_raw)
  
  
  print("CalCulate TP FP FN from Alexnet")
  calculate_PR_and_AP_Alexnet(All_data_LC,method = "Alexnet",rep,samplingNumber)
}else if (method_select == 5){
  
  All_data_LC  = data.frame()
  All_data_LC = rbind(Obj_Data_CloseSet,Obj_Data_HackImage_raw)
  
  
  print("CalCulate TP FP FN from Object")
  calculate_PR_and_AP_ObjectDetection(All_data_LC,rep,samplingNumber)
  
}




####################################### Start Evaluate AUC with LC ################







