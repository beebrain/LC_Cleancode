import pandas as pd
import os

def searchNametoIndexCoco(dictItem, search_item):
    res = dict((v,k) for k,v in dictItem.items())
    return res[search_item]

def getImageNetClass(dataFrame,serach_Item):
    if len(dataFrame.loc[dataFrame["Coco"]==serach_Item,"GT_New"]) == 0:
        return ""
    return (dataFrame.loc[dataFrame["Coco"]==serach_Item,"GT_New"].iloc[0])

def getImageNetNewIndex(dataFrame,serach_Item):
    if len(dataFrame.loc[dataFrame["GT_Index"]==serach_Item,"GT_New"]) == 0:
        return ""
    return (dataFrame.loc[dataFrame["GT_Index"]==serach_Item,"GT_New"].iloc[0])


######### load COCO Class and Map To Index
dataframe = pd.read_table("cocoLabel.txt",header=None)
dataframe = dataframe.T
coco_dict = dataframe.to_dict('records')[0]

print(searchNametoIndexCoco(coco_dict,'person'))

cocoMapImagenet = pd.read_csv("cocoMapImagenet.csv",encoding='UTF-8')

###### list os to file
data_LC_closeset = pd.read_csv("./ResultObjectMap/Lecog_ResultCloseset_fill20Class.txt")

OBject_result = pd.DataFrame()
for _,row_LC in data_LC_closeset.iterrows():
    fileName = row_LC["lowLevelName"]
    raw_data = pd.read_csv("./ResultObjectMap/Closeset/{}.csv".format(fileName))
    print(fileName)
    selectedclass = False
    for _,row_obj in raw_data.iterrows():
        classCocoPredict = int(row_obj["detection_classes"])
        ImagenetClass = getImageNetClass(cocoMapImagenet,coco_dict[classCocoPredict])
        if not ImagenetClass == "":
            dataPredict = {"ImageName":"{}.csv".format(fileName),
            "GT_Index":int(row_LC["GT_Index"]),
            "Prob_Of_Obj":row_obj["detection_scores"],
            "Obj_predict":ImagenetClass}
            selectedclass = True
            break
    
    if not selectedclass :
        dataPredict = {"ImageName":"{}.csv".format(fileName),
            "GT_Index":int(row_LC["GT_Index"]),
            "Prob_Of_Obj":0.9999,
            "Obj_predict":-1}               ## Predict Out Class
    df = pd.Series(dataPredict)
    OBject_result = OBject_result.append(df.T,ignore_index=True)
OBject_result = OBject_result.astype({"GT_Index": int, "Obj_predict": int})
OBject_result.to_csv("./ResultObjectMap/Obj_resultCloseset_fill20Class.txt")