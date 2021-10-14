rm(list=ls())  
library(data.table)
library(xlsx)
library(ggplot2)


Evaluate_path = "./"
index_sampling = c(300)
method_array  = c("Openmax","eak","pow3Ak","Obj")
evaluate_AUC =   data.frame(matrix(ncol = 5, nrow = 0))
epsion = 1e-4
rep = 98
samplingNumber = 10
method = "Openmax"
index_sam = 1
#for (rep in seq(1:5)){
for (method in method_array){
  for (index_sam in seq(1:length(index_sampling))){
    samplingNumber = index_sampling[index_sam]
    print("ReadData TP FP FN")
    data_presision_TP= read.csv(paste(Evaluate_path,toString(rep),"/",samplingNumber,"_TP_Binary_",method,".csv",sep = ""))
    data_presision_FP= read.csv(paste(Evaluate_path,toString(rep),"/",samplingNumber,"_FP_Binary_",method,".csv",sep = ""))
    data_presision_FN= read.csv(paste(Evaluate_path,toString(rep),"/",samplingNumber,"_FN_Binary_",method,".csv",sep = ""))
    
	print("Finished Read")
	print(samplingNumber)
	print(method)
    cutoffIndex = data_presision_FN[,2]
    
    ############# data TP FP FN of Seen ################
    data_presision_TP = data_presision_TP[,3]
    data_presision_FP = data_presision_FP[,3]
    data_presision_FN = data_presision_FN[,3]
    

    
    
    
    
    
    
    data_presision_S = data_presision_TP/(data_presision_TP+data_presision_FP+epsion)
    data_recall_S = data_presision_TP/(data_presision_TP+data_presision_FN)
    
    
    
    data_presision_All = data_presision_S
    data_recall_All = data_recall_S
    Fscore = 2*(data_presision_All*data_recall_All)/(data_presision_All+data_recall_All+epsion)
    
    
    
    path_write = paste(Evaluate_path,toString(rep),"/",samplingNumber,"_Fscore_Binary_",method,".csv",sep = "")
    path_write_Presision = paste(Evaluate_path,toString(rep),"/",samplingNumber,"_Presision_Binary_",method,".csv",sep = "")
    path_write_recall = paste(Evaluate_path,toString(rep),"/",samplingNumber,"_Recall_Binary_",method,".csv",sep = "")
    #### merge cutoff index #######
    Fscore = cbind(cutoffIndex,Fscore)
    data_presision_All = cbind(cutoffIndex,data_presision_All)
    data_recall_All = cbind(cutoffIndex,data_recall_All)
    
    
    write.csv(Fscore,path_write)
    write.csv(data_presision_All,path_write_Presision)
    write.csv(data_recall_All,path_write_recall)

  }
  }
#}
