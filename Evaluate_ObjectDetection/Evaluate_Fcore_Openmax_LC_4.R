rm(list=ls())  
library(data.table)
library(xlsx)
library(ggplot2)

gc()
memory.limit(9999999999)

Evaluate_path = "./"
index_sampling = c(300)
method_array  = c("Openmax","eak","pow3Ak","Prob_Of_Softmax","Objdetect")
evaluate_AUC =   data.frame(matrix(ncol = 5, nrow = 0))
epsion = 1e-4
rep = 98
samplingNumber = 1
method = "Prob_Of_Softmax"
#for (rep in seq(1:5)){
for (method in method_array){
  index_sam = 1
  for (index_sam in seq(1:1)){
    samplingNumber = index_sampling[index_sam]
    print("ReadData TP FP FN")
    data_presision_TP= read.csv(paste(Evaluate_path,toString(rep),"/",samplingNumber,"_TP_Correct_",method,".csv",sep = ""))
    data_presision_FP= read.csv(paste(Evaluate_path,toString(rep),"/",samplingNumber,"_FP_Correct_",method,".csv",sep = ""))
    data_presision_FN= read.csv(paste(Evaluate_path,toString(rep),"/",samplingNumber,"_FN_Correct_",method,".csv",sep = ""))
    
	print("Finished Read")
	print(samplingNumber)
	print(method)
    cutoffIndex = data_presision_FN[,2]
    
    ############# data TP FP FN of Seen ################
    data_presision_TP_S = data_presision_TP[,3:22]
    data_presision_FP_S = data_presision_FP[,3:22]
    data_presision_FN_S = data_presision_FN[,3:22]
    
    ############# data TP FP FN of UnSeen ################
    data_presision_TP_u = data_presision_TP$X21
    data_presision_FP_u = data_presision_FP$X21
    data_presision_FN_u = data_presision_FN$X21
    
    ############# data TP FP FN of HackImage ################
    # = data_presision_TP$X1002
    #data_presision_FP_f = data_presision_FP$X1002
    #data_presision_FN_f = data_presision_FN$X1002
    
    
    
    
    
    data_presision_S = data_presision_TP_S/(data_presision_TP_S+data_presision_FP_S+epsion)
    data_recall_S = data_presision_TP_S/(data_presision_TP_S+data_presision_FN_S)
    
    
    ######### cal presision unseen
    #data_presision_U = (data_presision_TP_u+data_presision_TP_f)/
    #  (data_presision_TP_u+data_presision_TP_f+data_presision_FP_u+epsion)
    data_presision_U = (data_presision_TP_u)/
      (data_presision_TP_u+data_presision_FP_u+epsion)
    
    ######### cal recall unseen
    #data_recall_U = (data_presision_TP_u+data_presision_TP_f)/
    #  (data_presision_TP_u+data_presision_TP_f+data_presision_FN_u+data_presision_FN_f)
    data_recall_U = (data_presision_TP_u)/
      (data_presision_TP_u+data_presision_FN_u)
    
    
    data_presision_All = cbind(data_presision_S,data_presision_U)
    data_recall_All = cbind(data_recall_S,data_recall_U)
    Fscore = 2*(data_presision_All*data_recall_All)/(data_presision_All+data_recall_All+epsion)
    
    gc()
    
    path_write = paste(Evaluate_path,toString(rep),"/",samplingNumber,"_Fscore_Correct_",method,".csv",sep = "")
    path_write_Presision = paste(Evaluate_path,toString(rep),"/",samplingNumber,"_Presision_Correct_",method,".csv",sep = "")
    path_write_recall = paste(Evaluate_path,toString(rep),"/",samplingNumber,"_Recall_Correct_",method,".csv",sep = "")
    #### merge cutoff index #######
    
    #data_presision_All = cbind(cutoffIndex,data_presision_All) #uncomment if you need to see the detail
    #data_recall_All = cbind(cutoffIndex,data_recall_All)       #uncomment if you need to see the detail
    #write.csv(data_presision_All,path_write_Presision)
    #write.csv(data_recall_All,path_write_recall)
    
    Fscore = cbind(cutoffIndex,Fscore)
    write.csv(Fscore,path_write)
    
  }
  }
#}
