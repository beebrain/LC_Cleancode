##Instruction 
1. Change directory to Evalate
2. Run batch RunIncludeIncorrrect.bat. It takes about 3-4 days that depend on your memory and computation speed.
	2.1 For RemoveIncorrect Run RunIncorrrectfilter.bat
3. Run RScript Evaluate_Fcore_OpenMax_LC_4.R. For calculate True positive, True negative ,False positive, and True negative.
4. Calculate area Under cuve with Rscipt Calculate_AUC_OpenMax.R 
5. Calculate Fscore with cutoff thresould with EvaluatefindMaxFscore_cutoff.R
6. Plot the result Graph with Plot_evaluate_Fscore_Openmax_LC.R and Plot_evaluate_Fscore_Openmax_LC_detail.R