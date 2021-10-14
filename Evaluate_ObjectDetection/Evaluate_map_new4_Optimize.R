#first Script Evaluate_map_new4_optimize.R-> Evaluate_Fcore_OpenMax_LC_4.R -> Calculate_AUC_OpenMax.R -> EvaluatefindMaxFscore_cutoff.R
#update Add Alexnet Mesument for TP TN FN TP
rm(list=ls())  
library(data.table)
library(xlsx)
library(ggplot2)
library(dplyr)
library("argparser")

parser <- arg_parser(description='Process commandline arguments')
parser <- add_argument(parser, arg="--samplingNum", type="integer", help = "sampling Number")
parser <- add_argument(parser, arg="--modeMethod", type="integer", help = "1 is openmax , 2 is eak, 3 is pow3ak")

args = parse_args(parser)
samplingNumber =  args$samplingNum
method_select =  args$modeMethod

samplingNumber = 42
method_select = 1
rep = 98

min1000Matrix = function(data){ return (min(data,1000))}

##################### Find AP each Class For Alexnet only #############
calculate_PR_and_AP_Alexnet = function(All_data_PR,method = 'Alexnet',rep,samplingNumber){
  total_data = nrow(All_data_PR)
  
  ## add Fix Alexnet predict
  if(method == "Alexnet"){
    method = "Prob_Of_Softmax"
    
  }
  Data_cal = All_data_PR[,c("GT_Index","SoftMaxPredict",method)]
  
  
  ### relabel GT_Index_C for fooing
  Data_cal$GT_Index_C = apply(as.matrix(Data_cal[,"GT_Index"]), 1, min1000Matrix)
  
  #order by ...
  RecallData = Data_cal[,method]
  RecallIndex_order = order(-RecallData)
  
  #Sort range 
  Data_cal.sort = Data_cal[RecallIndex_order,]
  head(Data_cal)
  head(Data_cal.sort)
  
  
  
  cutoff_index = Data_cal.sort[,method]
  
  
  TP_ALL = matrix(rep(0,length(cutoff_index)*1002), nrow = length(cutoff_index), ncol = 1002)
  FP_ALL = matrix(rep(0,length(cutoff_index)*1002), nrow = length(cutoff_index), ncol = 1002)
  FN_ALL = matrix(rep(0,length(cutoff_index)*1002), nrow = length(cutoff_index), ncol = 1002)
  
  
  for (indexCutoff in seq(1:length(cutoff_index))){
    if (indexCutoff%%1000 == 0){
      print(paste(indexCutoff,"of ",length(cutoff_index)))
    }
    
    cutoff  = cutoff_index[indexCutoff]
    Data_cal.sort["Predict"] = Data_cal.sort$SoftMaxPredict
    
    ## not cutoff for unseen  Alexnet don't predict unseen ########
    #id_cut_to_unseenPredict = which(Data_cal.sort[,method]< cutoff)
    #Data_cal.sort[id_cut_to_unseenPredict,"Predict"]  = 1000
    
    ### relabel GT_Index_C for fooing
    #Data_cal.sort$GT_Index_C = apply(as.matrix(Data_cal.sort[,"GT_Index"]), 1, min1000Matrix)
    
    if (indexCutoff == 1){
      for (index_class in seq(1:1002)){
        selected_class = index_class-1
        
        paticipate_data_Presicion = Data_cal.sort[which(Data_cal.sort$GT_Index == selected_class | Data_cal.sort$Predict == selected_class),]
        
        paticipate_data_Presicion["TP"] = 0
        paticipate_data_Presicion["FP"] = 0
        paticipate_data_Presicion["FN"] = 0
        
        ########################### Reduce lable fooling hack to 1000 when compare unseen using min(1000,i)
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==  paticipate_data_Presicion[,"GT_Index_C"]
                                        & paticipate_data_Presicion[,"GT_Index"] == selected_class),"TP"] =  1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FP"] = 1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"GT_Index"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FN"] = 1
        
        
        
        
        TP_Kclass_cutoff = sum(paticipate_data_Presicion$TP)
        FP_Kclass_cutoff = sum(paticipate_data_Presicion$FP)
        FN_Kclass_cutoff = sum(paticipate_data_Presicion$FN)
        
        TP_ALL[indexCutoff,index_class] = TP_Kclass_cutoff
        FP_ALL[indexCutoff,index_class] = FP_Kclass_cutoff
        FN_ALL[indexCutoff,index_class] = FN_Kclass_cutoff
      }
      GT_Old_class = Data_cal.sort[1,"GT_Index"]
      Predict_Old_class = Data_cal.sort[1,"Predict"]
    }
    else{
      ############# calculate only class change   
      GT_New_class = Data_cal.sort[indexCutoff,"GT_Index"]
      Predict_New_class = Data_cal.sort[indexCutoff,"Predict"]
      
      TP_ALL[indexCutoff,] = TP_ALL[indexCutoff-1,]
      FP_ALL[indexCutoff,] = FP_ALL[indexCutoff-1,]
      FN_ALL[indexCutoff,] = FN_ALL[indexCutoff-1,]
      for(index_class in c(GT_Old_class+1,Predict_Old_class+1,GT_New_class+1,Predict_New_class+1,1001,1002)){
        selected_class = index_class-1
        paticipate_data_Presicion = Data_cal.sort[which(Data_cal.sort$GT_Index == selected_class | 
                                                          Data_cal.sort$Predict == selected_class),]
        
        paticipate_data_Presicion["TP"] = 0
        paticipate_data_Presicion["FP"] = 0
        paticipate_data_Presicion["FN"] = 0
        
        ########################### Reduce lable fooling hack to 1000 when compare unseen using min(1000,i)
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==  paticipate_data_Presicion[,"GT_Index_C"]
                                        & paticipate_data_Presicion[,"GT_Index"] == selected_class),"TP"] =  1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FP"] = 1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"GT_Index"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FN"] = 1
        
        
        
        
        TP_Kclass_cutoff = sum(paticipate_data_Presicion$TP)
        FP_Kclass_cutoff = sum(paticipate_data_Presicion$FP)
        FN_Kclass_cutoff = sum(paticipate_data_Presicion$FN)
        
        TP_ALL[indexCutoff,index_class] = TP_Kclass_cutoff
        FP_ALL[indexCutoff,index_class] = FP_Kclass_cutoff
        FN_ALL[indexCutoff,index_class] = FN_Kclass_cutoff
      }
      
      ## Go Next step Class
      GT_Old_class = GT_New_class
      Predict_Old_class = Predict_New_class
    }
    
  }
  Data_TP_ALL  =   data.frame(TP_ALL)
  Data_TP_ALL = cbind(cutoff_index,Data_TP_ALL)
  Data_FP_ALL  =   data.frame(FP_ALL)
  Data_FP_ALL = cbind(cutoff_index,Data_FP_ALL)
  Data_FN_ALL  =   data.frame(FN_ALL)
  Data_FN_ALL = cbind(cutoff_index,Data_FN_ALL)
  
  write.csv(Data_TP_ALL,paste(toString(rep),"/",samplingNumber,"_TP_",method,".csv",sep = ""))
  write.csv(Data_FP_ALL,paste(toString(rep),"/",samplingNumber,"_FP_",method,".csv",sep = ""))
  write.csv(Data_FN_ALL,paste(toString(rep),"/",samplingNumber,"_FN_",method,".csv",sep = ""))
}

######################## Find AP each class ###############################
calculate_PR_and_AP = function(All_data_PR,method = 'eak',rep,samplingNumber){
  total_data = nrow(All_data_PR)
  
  ## add Fix Alexnet predict
  if(method == "Alexnet"){
    method = "Prob_Of_Softmax"

  }
  Data_cal = All_data_PR[,c("GT_Index","SoftMaxPredict",method)]
  
  
   ### relabel GT_Index_C for fooing
   Data_cal$GT_Index_C = apply(as.matrix(Data_cal[,"GT_Index"]), 1, min1000Matrix)
	
  #order by ...
  RecallData = Data_cal[,method]
  RecallIndex_order = order(-RecallData)
  
  #Sort range 
  Data_cal.sort = Data_cal[RecallIndex_order,]
  head(Data_cal)
  head(Data_cal.sort)
  
  
  
  cutoff_index = Data_cal.sort[,method]
  
  
  TP_ALL = matrix(rep(0,length(cutoff_index)*1002), nrow = length(cutoff_index), ncol = 1002)
  FP_ALL = matrix(rep(0,length(cutoff_index)*1002), nrow = length(cutoff_index), ncol = 1002)
  FN_ALL = matrix(rep(0,length(cutoff_index)*1002), nrow = length(cutoff_index), ncol = 1002)
  
  
  for (indexCutoff in seq(1:length(cutoff_index))){
    if (indexCutoff%%1000 == 0){
      print(paste(indexCutoff,"of ",length(cutoff_index)))
    }
    
    cutoff  = cutoff_index[indexCutoff]
    Data_cal.sort["Predict"] = Data_cal.sort$SoftMaxPredict
    id_cut_to_unseenPredict = which(Data_cal.sort[,method]< cutoff)
    Data_cal.sort[id_cut_to_unseenPredict,"Predict"]  = 1000
    
    ### relabel GT_Index_C for fooing
    #Data_cal.sort$GT_Index_C = apply(as.matrix(Data_cal.sort[,"GT_Index"]), 1, min1000Matrix)
    
    if (indexCutoff == 1){
      for (index_class in seq(1:1002)){
        selected_class = index_class-1
        
        paticipate_data_Presicion = Data_cal.sort[which(Data_cal.sort$GT_Index == selected_class | Data_cal.sort$Predict == selected_class),]
        
        paticipate_data_Presicion["TP"] = 0
        paticipate_data_Presicion["FP"] = 0
        paticipate_data_Presicion["FN"] = 0
        
        ########################### Reduce lable fooling hack to 1000 when compare unseen using min(1000,i)
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==  paticipate_data_Presicion[,"GT_Index_C"]
                                        & paticipate_data_Presicion[,"GT_Index"] == selected_class),"TP"] =  1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FP"] = 1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"GT_Index"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FN"] = 1
        
        
        
        
        TP_Kclass_cutoff = sum(paticipate_data_Presicion$TP)
        FP_Kclass_cutoff = sum(paticipate_data_Presicion$FP)
        FN_Kclass_cutoff = sum(paticipate_data_Presicion$FN)
        
        TP_ALL[indexCutoff,index_class] = TP_Kclass_cutoff
        FP_ALL[indexCutoff,index_class] = FP_Kclass_cutoff
        FN_ALL[indexCutoff,index_class] = FN_Kclass_cutoff
      }
      GT_Old_class = Data_cal.sort[1,"GT_Index"]
      Predict_Old_class = Data_cal.sort[1,"Predict"]
    }
    else{
      ############# calculate only class change   
      GT_New_class = Data_cal.sort[indexCutoff,"GT_Index"]
      Predict_New_class = Data_cal.sort[indexCutoff,"Predict"]
      
      TP_ALL[indexCutoff,] = TP_ALL[indexCutoff-1,]
      FP_ALL[indexCutoff,] = FP_ALL[indexCutoff-1,]
      FN_ALL[indexCutoff,] = FN_ALL[indexCutoff-1,]
      for(index_class in c(GT_Old_class+1,Predict_Old_class+1,GT_New_class+1,Predict_New_class+1,1001,1002)){
        selected_class = index_class-1
        paticipate_data_Presicion = Data_cal.sort[which(Data_cal.sort$GT_Index == selected_class | 
                                                          Data_cal.sort$Predict == selected_class),]
        
        paticipate_data_Presicion["TP"] = 0
        paticipate_data_Presicion["FP"] = 0
        paticipate_data_Presicion["FN"] = 0
        
        ########################### Reduce lable fooling hack to 1000 when compare unseen using min(1000,i)
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==  paticipate_data_Presicion[,"GT_Index_C"]
                                        & paticipate_data_Presicion[,"GT_Index"] == selected_class),"TP"] =  1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FP"] = 1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"GT_Index"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FN"] = 1
        
        
        
        
        TP_Kclass_cutoff = sum(paticipate_data_Presicion$TP)
        FP_Kclass_cutoff = sum(paticipate_data_Presicion$FP)
        FN_Kclass_cutoff = sum(paticipate_data_Presicion$FN)
        
        TP_ALL[indexCutoff,index_class] = TP_Kclass_cutoff
        FP_ALL[indexCutoff,index_class] = FP_Kclass_cutoff
        FN_ALL[indexCutoff,index_class] = FN_Kclass_cutoff
      }
      
      ## Go Next step Class
      GT_Old_class = GT_New_class
      Predict_Old_class = Predict_New_class
    }
    
  }
  Data_TP_ALL  =   data.frame(TP_ALL)
  Data_TP_ALL = cbind(cutoff_index,Data_TP_ALL)
  Data_FP_ALL  =   data.frame(FP_ALL)
  Data_FP_ALL = cbind(cutoff_index,Data_FP_ALL)
  Data_FN_ALL  =   data.frame(FN_ALL)
  Data_FN_ALL = cbind(cutoff_index,Data_FN_ALL)
  
  write.csv(Data_TP_ALL,paste(toString(rep),"/",samplingNumber,"_TP_",method,".csv",sep = ""))
  write.csv(Data_FP_ALL,paste(toString(rep),"/",samplingNumber,"_FP_",method,".csv",sep = ""))
  write.csv(Data_FN_ALL,paste(toString(rep),"/",samplingNumber,"_FN_",method,".csv",sep = ""))
}


#### prepare Openmax Data before import to the function
calculate_PR_and_AP_Openmax = function(All_data_PR,rep,samplingNumber){
  total_data = nrow(All_data_PR)
  Data_cal = All_data_PR[,c("GT_Index","OpenMaxPredict","Prob_Of_Openmax")]
     ### relabel GT_Index_C for fooing
  Data_cal$GT_Index_C = apply(as.matrix(Data_cal[,"GT_Index"]), 1, min1000Matrix)
  #order by ...
  RecallData = Data_cal[,"Prob_Of_Openmax"]
  RecallIndex_order = order(-RecallData)
  
  #Sort range 
  Data_cal.sort = Data_cal[RecallIndex_order,]
  head(Data_cal)
  head(Data_cal.sort)
  
  
  cutoff_index = Data_cal.sort[,"Prob_Of_Openmax"]
  
  
  TP_ALL = matrix(rep(0,length(cutoff_index)*1002), nrow = length(cutoff_index), ncol = 1002)
  FP_ALL = matrix(rep(0,length(cutoff_index)*1002), nrow = length(cutoff_index), ncol = 1002)
  FN_ALL = matrix(rep(0,length(cutoff_index)*1002), nrow = length(cutoff_index), ncol = 1002)
  
  GT_Old_class = 0
  Predict_Old_class =0
  GT_New_class = 0
  Predict_New_class = 0
  for (indexCutoff in seq(1:length(cutoff_index))){
    #print(paste(indexCutoff,"of ",length(cutoff_index)))
    if (indexCutoff%%1000 == 0){
      print(paste(indexCutoff,"of ",length(cutoff_index)))
      #break;
    }
    cutoff  = cutoff_index[indexCutoff]
    Data_cal.sort["Predict"] = Data_cal.sort$OpenMaxPredict
    
    id_cut_to_unseenPredict = which(Data_cal.sort[,"Prob_Of_Openmax"]< cutoff)
    Data_cal.sort[id_cut_to_unseenPredict,"Predict"]  = 1000
    
    ### relabel GT_Index_C for fooing
    #Data_cal.sort$GT_Index_C = apply(as.matrix(Data_cal.sort[,"GT_Index"]), 1, min1000Matrix)
    
    if (indexCutoff == 1){
      sequence_class = as.numeric(Data_cal.sort$GT_Index)
      sequence_class = unique(sequence_class)
      sequence_class = sequence_class[order(sequence_class)]
      for (selected_class in sequence_class){
        #selected_class = index_class-1
        index_class = selected_class+1
        paticipate_data_Presicion = Data_cal.sort[which(Data_cal.sort$GT_Index == selected_class | 
                                                          Data_cal.sort$Predict == selected_class),]
        
        paticipate_data_Presicion["TP"] = 0
        paticipate_data_Presicion["FP"] = 0
        paticipate_data_Presicion["FN"] = 0
        
        ########################### Reduce lable fooling hack to 1000 when compare unseen using min(1000,i)
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==  paticipate_data_Presicion[,"GT_Index_C"]
                                        & paticipate_data_Presicion[,"GT_Index"] == selected_class),"TP"] =  1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FP"] = 1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"GT_Index"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FN"] = 1
        
        
        
        TP_Kclass_cutoff = sum(paticipate_data_Presicion$TP)
        FP_Kclass_cutoff = sum(paticipate_data_Presicion$FP)
        FN_Kclass_cutoff = sum(paticipate_data_Presicion$FN)
        
        TP_ALL[indexCutoff,index_class] = sum(paticipate_data_Presicion$TP)
        FP_ALL[indexCutoff,index_class] = sum(paticipate_data_Presicion$FP)
        FN_ALL[indexCutoff,index_class] = sum(paticipate_data_Presicion$FN)
      }
      GT_Old_class = Data_cal.sort[1,"GT_Index"]
      Predict_Old_class = Data_cal.sort[1,"Predict"]
    }else{
      ############# calculate only class change   
      GT_New_class = Data_cal.sort[indexCutoff,"GT_Index"]
      Predict_New_class = Data_cal.sort[indexCutoff,"Predict"]
      
      TP_ALL[indexCutoff,] = TP_ALL[indexCutoff-1,]
      FP_ALL[indexCutoff,] = FP_ALL[indexCutoff-1,]
      FN_ALL[indexCutoff,] = FN_ALL[indexCutoff-1,]
      
      
      ############## loop only cut off is not calculate
      for(index_class in c(GT_Old_class+1,Predict_Old_class+1,GT_New_class+1,Predict_New_class+1,1001,1002)){
        selected_class = index_class-1
        paticipate_data_Presicion = Data_cal.sort[which(Data_cal.sort$GT_Index == selected_class | 
                                                          Data_cal.sort$Predict == selected_class),]
        
        paticipate_data_Presicion["TP"] = 0
        paticipate_data_Presicion["FP"] = 0
        paticipate_data_Presicion["FN"] = 0
        
        ########################### Reduce lable fooling hack to 1000 when compare unseen using min(1000,i)
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==  paticipate_data_Presicion[,"GT_Index_C"]
                                        & paticipate_data_Presicion[,"GT_Index"] == selected_class),"TP"] =  1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"Predict"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FP"] = 1
        
        paticipate_data_Presicion[which(paticipate_data_Presicion[,"GT_Index"]==selected_class 
                                        & paticipate_data_Presicion[,"Predict"]!= paticipate_data_Presicion[,"GT_Index_C"]),"FN"] = 1
        
        
        
        
        TP_Kclass_cutoff = sum(paticipate_data_Presicion$TP)
        FP_Kclass_cutoff = sum(paticipate_data_Presicion$FP)
        FN_Kclass_cutoff = sum(paticipate_data_Presicion$FN)
        
        TP_ALL[indexCutoff,index_class] = TP_Kclass_cutoff
        FP_ALL[indexCutoff,index_class] = FP_Kclass_cutoff
        FN_ALL[indexCutoff,index_class] = FN_Kclass_cutoff
      }
      
      ## Go Next step Class
      GT_Old_class = GT_New_class
      Predict_Old_class = Predict_New_class
    }
  }
  
  
  Data_TP_ALL  =   data.frame(TP_ALL)
  Data_TP_ALL = cbind(cutoff_index,Data_TP_ALL)
  Data_FP_ALL  =   data.frame(FP_ALL)
  Data_FP_ALL = cbind(cutoff_index,Data_FP_ALL)
  Data_FN_ALL  =   data.frame(FN_ALL)
  Data_FN_ALL = cbind(cutoff_index,Data_FN_ALL)
  
  write.csv(Data_TP_ALL,paste(toString(rep),"/",samplingNumber,"_TP_Openmax.csv",sep = ""))
  write.csv(Data_FP_ALL,paste(toString(rep),"/",samplingNumber,"_FP_Openmax.csv",sep = ""))
  write.csv(Data_FN_ALL,paste(toString(rep),"/",samplingNumber,"_FN_Openmax.csv",sep = ""))
  
}



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
  print(SamplingOpenSet)
}

#start load raw openMax Cdata
openMax_Data_CloseSet <- read.csv("./ResultCloseset.txt",header = TRUE)
openMax_Data_OpenSet_raw <- read.csv("./ResultOpenset.txt",header = TRUE)
openMax_Data_HackImage_raw <- read.csv("./ResultHackset.txt",header = TRUE)

IndexCloseset_Pre_op = which(openMax_Data_CloseSet$OpenMaxPredict == 1000)
IndexOpenSet_Pre_op = which(openMax_Data_OpenSet_raw$OpenMaxPredict == 1000)
IndexHackImage_Pre_op = which(openMax_Data_HackImage_raw$OpenMaxPredict == 1000)

############## invert Prob of open Max that predict class 1000 th
openMax_Data_CloseSet[IndexCloseset_Pre_op,]$Prob_Of_Openmax = 1-openMax_Data_CloseSet[IndexCloseset_Pre_op,]$Prob_Of_Openmax
openMax_Data_OpenSet_raw[IndexOpenSet_Pre_op,]$Prob_Of_Openmax = 1-openMax_Data_OpenSet_raw[IndexOpenSet_Pre_op,]$Prob_Of_Openmax
openMax_Data_HackImage_raw[IndexHackImage_Pre_op,]$Prob_Of_Openmax = 1- openMax_Data_HackImage_raw[IndexHackImage_Pre_op,]$Prob_Of_Openmax


################# load Data LC ################

LC_Data_CloseSet <- read.csv("./Lecog_ResultCloseset_1.txt",header = TRUE)
LC_Data_OpenSet_raw <- read.csv("./Lecog_ResultOpenset_1.txt",header = TRUE)
LC_Data_HackImage_raw <- read.csv("./Lecog_ResultHackeset_1.txt",header = TRUE)



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







