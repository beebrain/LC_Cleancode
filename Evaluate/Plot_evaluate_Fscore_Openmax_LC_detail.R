#### note  
#### LC1 is LC : exp
#### LC2 is LC : cubic
## update  2/10/2019

rm(list=ls())  
library(data.table)
library(xlsx)
library(ggplot2)
library(cowplot)


plotformat = function(dataframtoplot,title_name="",xlabel="",ylabel=""){
  dataframtoplot$method <- factor(dataframtoplot$method, levels=c("OpenMax", "LC : Exp", "LC : Cubic","Alexnet"))
  
  dummy_plot = ggplot(data = dataframtoplot , aes(x = cutoff,group = method)) +
    geom_line(aes( y = Pscore,linetype = method,color=method),size=1)+
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size=32),
          axis.text=element_text(size=24),
          #axis.text = element_blank(), 
          axis.ticks.length = unit(0, "mm"),
          
          legend.text=element_text(size=26),
          legend.title=element_blank(),
          legend.position = c(0.6, 0.75),
          legend.justification = c(0,0),
          legend.background = element_rect(fill=alpha('white', 0.0))
    )+
    labs(title = title_name ,x = NULL,y=NULL,fill="method")+
    scale_linetype_manual(values=c( "twodash","solid","dotted","dotdash"))+
    scale_y_continuous(limits = c(0,1))
  
  
  return (dummy_plot)
  
}





Evaluate_path = "I:/MyOpenMax/Evaluate/TPami_Evaluate/"
index_sampling = c(42,97,236,300)
rep = "98"

sampling = "236"
for (sampling in index_sampling){

path_P1_openmax = paste(Evaluate_path,rep,"/_Fscore_","P1_Correct",sampling,"_OpenMax.csv",sep = "")
path_P1_eak = paste(Evaluate_path,rep,"/_Fscore_","P1_Correct",sampling,"_eak.csv",sep = "")
path_P1_ak3 = paste(Evaluate_path,rep,"/_Fscore_","P1_Correct",sampling,"_pow3Ak.csv",sep = "")
path_P1_Alexnet = paste(Evaluate_path,rep,"/_Fscore_","P1_Correct",sampling,"_Prob_Of_Softmax.csv",sep = "")

data_P1_openmax= read.csv(path_P1_openmax)
data_P1_eak= read.csv(path_P1_eak)
data_P1_ak3= read.csv(path_P1_ak3)
data_P1_Alexnet= read.csv(path_P1_Alexnet)

path_P2_openmax = paste(Evaluate_path,rep,"/_Fscore_","P2_Correct",sampling,"_OpenMax.csv",sep = "")
path_P2_eak = paste(Evaluate_path,rep,"/_Fscore_","P2_Correct",sampling,"_eak.csv",sep = "")
path_P2_ak3 = paste(Evaluate_path,rep,"/_Fscore_","P2_Correct",sampling,"_pow3Ak.csv",sep = "")
path_P2_Alexnet = paste(Evaluate_path,rep,"/_Fscore_","P2_Correct",sampling,"_Prob_Of_Softmax.csv",sep = "")

data_P2_openmax= read.csv(path_P2_openmax)
data_P2_eak= read.csv(path_P2_eak)
data_P2_ak3= read.csv(path_P2_ak3)
data_P2_Alexnet= read.csv(path_P2_Alexnet)


totalnumberdata = nrow(data_P1_ak3)

colnames(data_P1_openmax) = c("cutoff","Pscore")
colnames(data_P1_eak) = c("cutoff","Pscore")
colnames(data_P1_ak3) = c("cutoff","Pscore")
colnames(data_P1_Alexnet) = c("cutoff","Pscore")

colnames(data_P2_openmax) = c("cutoff","Pscore")
colnames(data_P2_eak) = c("cutoff","Pscore")
colnames(data_P2_ak3) = c("cutoff","Pscore")
colnames(data_P2_Alexnet) = c("cutoff","Pscore")


data_P1_openmax$cutoff = (data_P1_openmax$cutoff-1)/(totalnumberdata-1)
data_P1_eak$cutoff = (data_P1_eak$cutoff-1)/(totalnumberdata-1)
data_P1_ak3$cutoff = (data_P1_ak3$cutoff-1)/(totalnumberdata-1)
data_P1_Alexnet$cutoff = (data_P1_Alexnet$cutoff-1)/(totalnumberdata-1)

data_P2_openmax$cutoff = (data_P2_openmax$cutoff-1)/(totalnumberdata-1)
data_P2_eak$cutoff = (data_P2_eak$cutoff-1)/(totalnumberdata-1)
data_P2_ak3$cutoff = (data_P2_ak3$cutoff-1)/(totalnumberdata-1)
data_P2_Alexnet$cutoff = (data_P2_Alexnet$cutoff-1)/(totalnumberdata-1)


data_P1_openmax["method"] = "OpenMax"
data_P1_eak["method"] = "LC : Exp"
data_P1_ak3["method"] = "LC : Cubic"
data_P1_Alexnet["method"] = "Alexnet"


data_P2_openmax["method"] = "OpenMax"
data_P2_eak["method"] = "LC : Exp"
data_P2_ak3["method"] = "LC : Cubic"
data_P2_Alexnet["method"] = "Alexnet"



data_source_p1 = rbind(data_P1_ak3,data_P1_eak,data_P1_openmax,data_P1_Alexnet)
data_source_p2 = rbind(data_P2_ak3,data_P2_eak,data_P2_openmax,data_P2_Alexnet)


# 
dummpyplot_p1 = plotformat(data_source_p1,"Q1")
dummpyplot_p2 = plotformat(data_source_p2,"Q2")
dummpyplot_p1
savePath = paste("./Evaluate_","P1_Correct_addAlexnet",sampling,".png",sep = "")
savePath_eps = paste("./Evaluate_","P1_Correct_addAlexnet",sampling,".eps",sep = "")
save_plot(savePath, dummpyplot_p1, base_asp = 1,base_height = 7)
save_plot(savePath_eps, dummpyplot_p1, base_asp = 1,base_height = 7)

dummpyplot_p2
savePath = paste("./Evaluate_","P2_Correct_addAlexnet",sampling,".png",sep = "")
savePath_eps = paste("./Evaluate_","P2_Correct_addAlexnet",sampling,".eps",sep = "")
save_plot(savePath, dummpyplot_p2, base_asp = 1,base_height = 7)
save_plot(savePath_eps, dummpyplot_p2, base_asp = 1,base_height = 7)

}

