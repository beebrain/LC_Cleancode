### process 
# Evaluate_map_new3.R -> Evaluate_Fscore_Openmax_LC.R -> CalCulate_AUC_OpenMax.R -> Plot_evaluateFscoreOpenmax_LC.R

rm(list=ls())  
library(data.table)
library(xlsx)
library(ggplot2)
library(cowplot)
#################
# Find F1 and F2 From
# ########### find F score equation 1 ###########
# 
# print(rowMeans(data_fscore[1,1:1000]))
# F_s =  rowMeans(data_fscore[,1:1000])
# print(F_s[1])
# F_u = data_fscore[,1001]
# F1 = (F_s+F_u)/2
# 
# 
# ########### find F score equation 2 ###########
# F2 = sqrt(F_s*F_u)


plotformat = function(dataframtoplot,title_name="",xlabel="",ylabel=""){
  dataframtoplot$method <- factor(dataframtoplot$method, levels=c("OpenMax", "LC : Exp", "LC : Cubic","Alexnet"))
  
  dummy_plot = ggplot(data = dataframtoplot , aes(x = factor(indexsampling),group = method)) +
    geom_line(aes( y = value,linetype = method,color=method),size=1)+
    geom_point(aes( y = value,shape=method,color=method))+
    
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size=32),
          axis.text=element_text(size=24),
          #axis.text = element_blank(), 
          axis.ticks.length = unit(0, "mm"),
          
          legend.text=element_text(size=26),
          legend.title=element_blank(),
          legend.position = c(0.6, 0.7),
          legend.justification = c(0,0),
          legend.background = element_rect(fill=alpha('white', 0.0))
    )+
    labs(title = title_name ,x = xlabel,y=ylabel,fill="")+
    scale_linetype_manual(values=c( "twodash","solid","dotted","dotdash"))+
    scale_x_discrete( labels = c('0.625',"0.500","0.333","0.288"))+
    scale_y_continuous(limits = c(0.0,1))
  
  return (dummy_plot)
  
}
rep = 98
Evaluate_path = "./" ## fix code path in Local file
index_sampling = c(42,97,236,300)
method_array  = c("Openmax","eak","pow3Ak","Prob_Of_Softmax")
method_name = list("Openmax" = "OpenMax","eak" = "LC : Exp","pow3Ak" = "LC : Cubic","Prob_Of_Softmax"="Alexnet")
epsion = 1e-4
data_FScore_all = data.frame()
for (method in method_array){
  print(paste("Method :",method))

  path_Fscore_Average = paste(Evaluate_path,toString(rep),"/","_Fscore_Average_",method,".csv",sep = "")
  data_FScore= read.csv(path_Fscore_Average)
  data_FScore["method"] = method_name[method]
  colnames(data_FScore) = c("indexsampling","F1","F1MaxcutIndex","F2","F2MaxcutIndex","method")
  data_FScore_all = rbind(data_FScore_all,data_FScore)
  
  
}
####### F1
dataplot = melt(data=as.data.table(data_FScore_all),id.vars = c("indexsampling","method"))
dataplot = dataplot[which(dataplot$variable %in% c("F1")),]


dummy_plot  = plotformat(dataplot,"Q1",xlabel="Comfort ratio",ylabel="Performance index (Q1)")
dummy_plot

save_plot("Evaluate_F1score_OpenmaxWithLCAnd_Alexnet.png", dummy_plot, base_asp = 1,base_height = 7)
save_plot("Evaluate_F1score_OpenmaxWithLCAnd_Alexnet.eps", dummy_plot, base_asp = 1,base_height = 7)


########### F2
dataplot = melt(data=as.data.table(data_FScore_all),id.vars = c("indexsampling","method"))
dataplot = dataplot[which(dataplot$variable %in% c("F2")),]

dummy_plot  = plotformat(dataplot,"Q2",xlabel="Comfort ratio",ylabel="Performance index (Q2)")
dummy_plot
save_plot("Evaluate_F2score_OpenmaxWithLCAnd_Alexnet.png", dummy_plot, base_asp = 1,base_height = 7)
save_plot("Evaluate_F2score_OpenmaxWithLCAnd_Alexnet.eps", dummy_plot, base_asp = 1,base_height = 7)


##########################   plot P1 and P2 Each Sample

