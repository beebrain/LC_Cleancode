import pandas as pd
import os
import shutil

def searchNametoIndexCoco(dictItem, search_item):
    res = dict((v,k) for k,v in dictItem.items())
    return res[search_item]


def getImageNetClass(dataFrame,serach_Item):
    if len(dataFrame.loc[dataFrame["Coco"]==serach_Item,"GT_New"]) == 0:
        return ""
    return (dataFrame.loc[dataFrame["Coco"]==serach_Item,"GT_New"].iloc[0])

def getImageNetNewIndex(dataFrame,serach_Item):
    if len(dataFrame.loc[dataFrame["Imagenet"]==serach_Item,"GT_New"]) == 0:
        return ""
    return (dataFrame.loc[dataFrame["Imagenet"]==serach_Item,"GT_New"].iloc[0])

def getImageNetCodeToNewIndex(dataFrame,serach_Item):
    if serach_Item == 1000:
        return 20
    if len(dataFrame.loc[dataFrame["GT_Index"]==serach_Item,"GT_New"]) == 0:
        return -1
    return (dataFrame.loc[dataFrame["GT_Index"]==serach_Item,"GT_New"].iloc[0])

######### load COCO Class and Map To Index
dataframe = pd.read_table("cocoLabel.txt",header=None)
dataframe = dataframe.T
coco_dict = dataframe.to_dict('records')[0]

print(searchNametoIndexCoco(coco_dict,'person'))

cocoMapImagenet = pd.read_csv("cocoMapImagenet.csv",encoding='UTF-8')


######### Load interesting class of Imagenet  to Ourfolder ######
if not os.path.exists("ResultObjectMap/Closeset"):
    os.makedirs("ResultObjectMap/Closeset")

## load closet data of object detection
close_Imagenet_class = pd.read_csv("ClosetFillterForObjdetection.txt",header=None)
close_Openmax_Data = pd.read_csv("ResultCloseset.txt")
close_LC_Data = pd.read_csv("Lecog_ResultCloseset_1.txt")


## extract class and file name from imagefile column in dataframe
SplitImageName = close_Openmax_Data["ImageName"].str.split("\\", expand = True)
className = SplitImageName[1]
lowLevelName = SplitImageName[2].str.split("_",n=2,expand=True)[2].str.split(".",n=1,expand=True)[0]
### add new column to close_openmax
close_Openmax_Data["className"]= className
close_Openmax_Data["lowLevelName"]= lowLevelName

## extract class and file name from imagefile column in dataframe fro LC file
SplitImageName =  close_LC_Data["ImageName"].str.split("\\", expand = True)
className = SplitImageName[1]
lowLevelName = SplitImageName[2].str.split(".",n=1,expand=True)[0]

### add new column to close_openmax
close_LC_Data["className"]= className
close_LC_Data["lowLevelName"]= lowLevelName
## delete unnameof LC
close_LC_Data.pop("Unnamed: 6")




################ Fillter class Interestion ####################
select_class = close_Imagenet_class[1].tolist()   ## select 20 Classes from  ClosetFillterForObjdetection
print(select_class)
selectedClassOfLC = close_LC_Data[close_LC_Data.className.isin(select_class)]
selectedClassOfOpenMax = close_Openmax_Data[close_Openmax_Data.className.isin(select_class)]



for select_indexClass in select_class:
  selectedClassOfLC.loc[selectedClassOfLC.className == select_indexClass,"GT_Index"] = getImageNetNewIndex(cocoMapImagenet,select_indexClass)

for i in range(0,1001):
  selectedClassOfLC.loc[selectedClassOfLC[' SoftMaxPredict']	 == i,"SoftMaxPredict_re"] = getImageNetCodeToNewIndex(cocoMapImagenet,i)

selectedClassOfLC.pop(" SoftMaxPredict")
selectedClassOfLC = selectedClassOfLC.rename(columns={'SoftMaxPredict_re': 'SoftMaxPredict'}, inplace=False)


for select_indexClass in select_class:
  selectedClassOfOpenMax.loc[selectedClassOfOpenMax.className == select_indexClass,"GT_Index"] = getImageNetNewIndex(cocoMapImagenet,select_indexClass)
for i in range(0,1001):
  selectedClassOfOpenMax.loc[selectedClassOfOpenMax.SoftMaxPredict == i,"SoftMaxPredict_re"] = getImageNetCodeToNewIndex(cocoMapImagenet,i)
  selectedClassOfOpenMax.loc[selectedClassOfOpenMax.OpenMaxPredict == i,"OpenMaxPredict_re"] = getImageNetCodeToNewIndex(cocoMapImagenet,i)

selectedClassOfOpenMax.pop("SoftMaxPredict")
selectedClassOfOpenMax.pop("OpenMaxPredict")
selectedClassOfOpenMax = selectedClassOfOpenMax.rename(columns={'SoftMaxPredict_re': 'SoftMaxPredict','OpenMaxPredict_re':"OpenMaxPredict"}, inplace=False)


#save fillted 20 classes result LC and Openmax
selectedClassOfLC.astype({"SoftMaxPredict": int})
selectedClassOfLC.to_csv("./ResultObjectMap/Lecog_ResultCloseset_fill20Class.txt",index= False)
selectedClassOfOpenMax.astype({"SoftMaxPredict": int, "OpenMaxPredict": int})
selectedClassOfOpenMax.to_csv("./ResultObjectMap/OpenMax_ResultCloseset_fill20Class.txt",index = False)



### copy closeset detection result file to our directory list file name from LC and Openmax lowlevelname column
resultOBD_Path = "../results/validation/"
resultOBDFill_Path = "./ResultObjectMap/Closeset/"
for _, row in selectedClassOfOpenMax.iterrows():
  fileName = row.lowLevelName
  fullPath = "{}/{}.csv".format(resultOBD_Path,fileName)
  destPath = "{}/{}.csv".format(resultOBDFill_Path,fileName)
  shutil.copyfile(fullPath,destPath )
  print(fileName)

# closetMap = close_Imagenet_class.copy(deep=True)
# closetMap["GT_Index"] = ""
# for _,row in close_Imagenet_class.iterrows():
#   closetMap.loc[closetMap[1]==row[1],"GT_Index"] = selectedClassOfLC.loc[selectedClassOfLC['className'] ==row[1],"GT_Index"].iloc[0]
#   print(row[1])

# closetMap = closetMap.rename(columns={0: "Coco", 1: "Imagenet"})
# closetMap.to_csv("cocoMapImagenet.csv",index=False)

####################################### END Section CloseSet###################



############################ Start Section Openset #########################

######### Load interesting class of Imagenet  to Ourfolder ######
if not os.path.exists("ResultObjectMap/Openset"):
    os.makedirs("ResultObjectMap/Openset")

## load Open data of object detection
Open_Imagenet_class = pd.read_csv("OpensetFillterForObjdetection.txt",header=None)
Open_Openmax_Data = pd.read_csv("ResultOpenset.txt")
Open_LC_Data = pd.read_csv("Lecog_ResultOpenset_1.txt")



## extract class and file name from imagefile column in dataframe
SplitImageName = Open_Openmax_Data["ImageName"].str.split("\\", expand = True)
className = SplitImageName[2].str.split("_",expand=True)[2]
lowLevelName = SplitImageName[2].str.split("_",n=2,expand=True)[2].str.split(".",n=1,expand=True)[0]
### add new column to open_openmax
Open_Openmax_Data["className"]= className
Open_Openmax_Data["lowLevelName"]= lowLevelName



## extract class and file name from imagefile column in dataframe fro LC file
SplitImageName = Open_LC_Data["ImageName"].str.split("\\", expand = True)
className = SplitImageName[2].str.split("_",expand=True)[0]
lowLevelName = SplitImageName[2].str.split(".",n=1,expand=True)[0]


### add new column to close_openmax
Open_LC_Data["className"]= className
Open_LC_Data["lowLevelName"]= lowLevelName
## delete unnameof LC
Open_LC_Data.pop("Unnamed: 6")




################ Fillter class Interestion ####################
select_class = Open_Imagenet_class[0].tolist()   ## select 20 Classes from  ClosetFillterForObjdetection
print(select_class)

selectedClassOfLC = Open_LC_Data[Open_LC_Data.className.isin(select_class)]
selectedClassOfLC.GT_Index = 20



selectedClassOfOpenMax = Open_Openmax_Data[Open_Openmax_Data.className.isin(select_class)]
selectedClassOfOpenMax.GT_Index=20


for i in range(0,1001):
  selectedClassOfLC.loc[selectedClassOfLC[' SoftMaxPredict']	 == i,"SoftMaxPredict_re"] = getImageNetCodeToNewIndex(cocoMapImagenet,i)

selectedClassOfLC.pop(" SoftMaxPredict")
selectedClassOfLC = selectedClassOfLC.rename(columns={'SoftMaxPredict_re': 'SoftMaxPredict'}, inplace=False)

for i in range(0,1001):
  selectedClassOfOpenMax.loc[selectedClassOfOpenMax.SoftMaxPredict == i,"SoftMaxPredict_re"] = getImageNetCodeToNewIndex(cocoMapImagenet,i)
  selectedClassOfOpenMax.loc[selectedClassOfOpenMax.OpenMaxPredict == i,"OpenMaxPredict_re"] = getImageNetCodeToNewIndex(cocoMapImagenet,i)

selectedClassOfOpenMax.pop("SoftMaxPredict")
selectedClassOfOpenMax.pop("OpenMaxPredict")
selectedClassOfOpenMax = selectedClassOfOpenMax.rename(columns={'SoftMaxPredict_re': 'SoftMaxPredict','OpenMaxPredict_re':"OpenMaxPredict"}, inplace=False)

#save fillted 20 classes result LC and Openmax
selectedClassOfLC.astype({"GT_Index": int, "SoftMaxPredict": int})
selectedClassOfLC.to_csv("./ResultObjectMap/Lecog_ResultOpenset_fill20Class.txt",index= False)
selectedClassOfOpenMax.astype({"SoftMaxPredict": int, "OpenMaxPredict": int})
selectedClassOfOpenMax.to_csv("./ResultObjectMap/OpenMax_ResultOpenset_fill20Class.txt",index = False)

### copy Openset detection result file to our directory list file name from LC and Openmax lowlevelname column
resultOBD_Path = "../results/1001/"
resultOBDFill_Path = "./ResultObjectMap/Openset/"
for _, row in selectedClassOfOpenMax.iterrows():
  fileName = row.lowLevelName
  fullPath = "{}/{}.csv".format(resultOBD_Path,fileName)
  destPath = "{}/{}.csv".format(resultOBDFill_Path,fileName)
  shutil.copyfile(fullPath,destPath )
  print(fileName)