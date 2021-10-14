#first Script Evaluate_map_new4_optimize.R-> Evaluate_Fcore_OpenMax_LC_4.R -> Calculate_AUC_OpenMax.R -> EvaluatefindMaxFscore_cutoff.R
#update Add Alexnet Mesument for TP TN FN TP
# Update 27/09/2019
# this Script update for filter incorrect remove out
rm(list=ls())  
library(data.table)
library(xlsx)
library(ggplot2)

library("argparser")


#samplingNumber =  42   ### fixCode remove when you need to run
#method_select =  1     ### fixcode remove when you need to run

rep = 98

min1000Matrix = function(data){ return (min(data,1000))}



if (!dir.exists(toString(rep))){
  dir.create(toString(rep), showWarnings = TRUE, recursive = FALSE, mode = "0777")
} else {
  print("Dir already exists!")
}


sample_index = sample(301,samplingNumber)
List_Openset = list()
for (i in seq(1:360)){
  List_Openset = c(List_Openset, list(sample_index+((i-1)*301)))
}


#### please copy List Sampling from Evaluate Folder
if (!file.exists(paste("./",samplingNumber,"_Listsampling.txt",sep = ""))){
  sample_index = sample(301,samplingNumber)
  List_Openset = list()
  for (i in seq(1:360)){
    List_Openset = c(List_Openset, list(sample_index+((i-1)*301)))
  }
  
  SamplingOpenSet = array(as.numeric(unlist(List_Openset)))
  print("Create Fix File")
  write.csv(SamplingOpenSet,paste("./",samplingNumber,"_Listsampling.txt",sep = ""))
}else{
  SamplingOpenSet_e <- read.csv(paste("./",samplingNumber,"_Listsampling.txt",sep = ""),header = TRUE)
  SamplingOpenSet = array(as.numeric(SamplingOpenSet_e$x))
  print("Load Fix list sampling")
  #print(SamplingOpenSet)
}

#start load raw openMax Cdata 
openMax_Data_CloseSet <- read.csv("../ResultCloseset.txt",header = TRUE)
openMax_Data_OpenSet_raw <- read.csv("../ResultOpenset.txt",header = TRUE)
openMax_Data_HackImage_raw <- read.csv("../ResultHackset.txt",header = TRUE)

###############  Addition Remove Incorrect predict in Result Closeset #####################
IndexCorrect_op = which(openMax_Data_CloseSet$SoftMaxPredict == openMax_Data_CloseSet$GT_Index)
openMax_Data_CloseSet = openMax_Data_CloseSet[IndexCorrect_op,]


IndexCloseset_Pre_op = which(openMax_Data_CloseSet$OpenMaxPredict == 1000)
IndexOpenSet_Pre_op = which(openMax_Data_OpenSet_raw$OpenMaxPredict == 1000)
IndexHackImage_Pre_op = which(openMax_Data_HackImage_raw$OpenMaxPredict == 1000)

############## invert Prob of open Max that predict class 1000 th
openMax_Data_CloseSet[IndexCloseset_Pre_op,]$Prob_Of_Openmax = 1-openMax_Data_CloseSet[IndexCloseset_Pre_op,]$Prob_Of_Openmax
openMax_Data_OpenSet_raw[IndexOpenSet_Pre_op,]$Prob_Of_Openmax = 1-openMax_Data_OpenSet_raw[IndexOpenSet_Pre_op,]$Prob_Of_Openmax
openMax_Data_HackImage_raw[IndexHackImage_Pre_op,]$Prob_Of_Openmax = 1- openMax_Data_HackImage_raw[IndexHackImage_Pre_op,]$Prob_Of_Openmax


################# load Data LC ################

LC_Data_CloseSet <- read.csv("../Lecog_ResultCloseset_1.txt",header = TRUE)
LC_Data_OpenSet_raw <- read.csv("../Lecog_ResultOpenset_1.txt",header = TRUE)
LC_Data_HackImage_raw <- read.csv("../Lecog_ResultHackeset_1.txt",header = TRUE)

###############  Addition Remove Incorrect predict in Result Closeset #####################
IndexCorrect_op = which(LC_Data_CloseSet$SoftMaxPredict == LC_Data_CloseSet$GT_Index)
LC_Data_CloseSet = LC_Data_CloseSet[IndexCorrect_op,]

## select only sampling openset
openMax_Data_OpenSet_raw = openMax_Data_OpenSet_raw[SamplingOpenSet,]

#select sampling openset sampling same openMax sampling
LC_Data_OpenSet_raw = LC_Data_OpenSet_raw[SamplingOpenSet,]


############################ Recheck Openset is Random under condition #############
print("Recheck Dataset is randomed under condition")
data_dict <- data.frame(matrix(ncol = 2, nrow = 0))
name_col <- c("className", "count")
colnames(data_dict) <- name_col

AllClass = LC_Data_OpenSet_raw[1]$ImageName
each_name = AllClass[1]
for( each_name in AllClass){
  name = strsplit(each_name[[1]],"\\\\")
  class_name = strsplit(name[[1]][3],'_')
  class_name = class_name[[1]][1]
  indexClass = which(data_dict$className == class_name)
  
  if (!is.null(indexClass) & length(indexClass) > 0){
    data_dict[indexClass,"count"] =1+data_dict[indexClass,"count"]
    
  }else{
    df=data.frame(class_name,1)
    colnames(df)=name_col
    data_dict =rbind(data_dict, df)
  }
}

print(data_dict)
########################## Relabel Grouth HackImage TO 1001 ####################
openMax_Data_HackImage_raw$GT_Index = 1001
LC_Data_HackImage_raw$GT_Index = 1001


#######################################################


if (method_select == 1){
  ### Bind All Dataset
  All_data_openmax  = data.frame()
  All_data_openmax = rbind(openMax_Data_CloseSet,openMax_Data_OpenSet_raw,openMax_Data_HackImage_raw)
  
  ##### calculate AUC Openmax
  print("CalCulate TP FP FN from openmax")
  calculate_PR_and_AP_Openmax(All_data_openmax,rep,samplingNumber)
  
}else if(method_select ==2){
  
  All_data_LC  = data.frame()
  All_data_LC = rbind(LC_Data_CloseSet,LC_Data_OpenSet_raw,LC_Data_HackImage_raw)
  
  print("CalCulate TP FP FN from eak")
  calculate_PR_and_AP(All_data_LC,method = "eak",rep,samplingNumber)
}else if(method_select ==3){
  
  
  All_data_LC  = data.frame()
  All_data_LC = rbind(LC_Data_CloseSet,LC_Data_OpenSet_raw,LC_Data_HackImage_raw)
  
  
  print("CalCulate TP FP FN from 3ak")
  calculate_PR_and_AP(All_data_LC,method = "pow3Ak",rep,samplingNumber)
  
}else if (method_select == 4){
  
  All_data_LC  = data.frame()
  All_data_LC = rbind(LC_Data_CloseSet,LC_Data_OpenSet_raw,LC_Data_HackImage_raw)
  
  
  print("CalCulate TP FP FN from Alexnet")
  calculate_PR_and_AP_Alexnet(All_data_LC,method = "Alexnet",rep,samplingNumber)
}




####################################### Start Evaluate AUC with LC ################







