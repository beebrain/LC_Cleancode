############## Run Evaluate_Fcore_Opnemax_LC

rm(list=ls())  
library(data.table)
library(xlsx)
library(ggplot2)

calcutFscore_index = function(dataset,Evaluate_path,method,samplingNumber){
  cutoff = dataset[,1]
  data_fscore = dataset[,2:22]
  ########### find F score equation 1 ###########

  #print(rowMeans(data_fscore[1,1:20]))
  F_s =  rowMeans(data_fscore[,1:20])
  #print(F_s[1])
  F_u = data_fscore[,21]
  F1 = (F_s+F_u)/2
  
  
  ########### find F score equation 2 ###########
  F2 = sqrt(F_s*F_u)
  
  
  cutoffF1 = which.max(F1)
  cutoffF2 = which.max(F2)
  maxcutoff_f1 = cutoff[cutoffF1]
  maxcutoff_f2 = cutoff[cutoffF2]
  print(paste("F1",max(F1),maxcutoff_f1))
  print(paste("F2",max(F2),maxcutoff_f2))
  write.csv(F1,paste(Evaluate_path,"/","_Fscore_P1_Correct",samplingNumber,"_",method,".csv",sep = ""))
  write.csv(F2,paste(Evaluate_path,"/","_Fscore_P2_Correct",samplingNumber,"_",method,".csv",sep = ""))
  return(c(max(F1),cutoff[cutoffF1],max(F2),cutoff[cutoffF2]))
}



Evaluate_path = "./"
index_sampling = c(300)
method_array  = c("Openmax","eak","pow3Ak","Prob_Of_Softmax","Objdetect")
evaluate_AUC =   data.frame(matrix(ncol = 5, nrow = 0))
epsion = 1e-4
samplingNumber =300
method  = "Openmax"
rep = 98

for (method in method_array){
  print(paste("Method :",method))
    data_Fmax = data.frame()
    index_sam =1
    for (index_sam in seq(1:1)){
      samplingNumber = index_sampling[index_sam]

      print(paste("sampling =",samplingNumber))
      path_read_Fscore = paste(Evaluate_path,toString(rep),"/",samplingNumber,"_Fscore_Correct_",method,".csv",sep = "")
      path_read_Presision = paste(Evaluate_path,toString(rep),"/",samplingNumber,"_Presision_Correct_",method,".csv",sep = "")
      path_read_Recall = paste(Evaluate_path,toString(rep),"/",samplingNumber,"_Recall_Correct_",method,".csv",sep = "")
      data_FScore= read.csv(path_read_Fscore)
      #data_Presision= read.csv(path_read_Presision)
      #data_Recall = read.csv(path_read_Recall)
      
      data_FScore = data_FScore[2:23]
      #data_Presision = data_Presision[2:1003]
      #data_Recall = data_Recall[2:1003]
      Fmax_data = calcutFscore_index(data_FScore,paste(Evaluate_path,toString(rep),sep=""),method,samplingNumber)
      data_Fmax = rbind(data_Fmax,Fmax_data)
      colnames(data_Fmax) = c("F1","F1MaxcutIndex","F2","F2MaxcutIndex")
    }
  write.csv(data_Fmax,paste(Evaluate_path,toString(rep),"/","_Fscore_Correct_Average_",method,".csv",sep = ""))
}
