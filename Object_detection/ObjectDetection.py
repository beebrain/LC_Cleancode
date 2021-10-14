## This script for run object dection with tensorflow
# please provide the path image folder testing. The script will create the outputs description into 
# folder results.
# default Folder
# PATH_TO_TEST_IMAGES_DIR = "TestImage"
# Type_Image  = ".jpeg"
# model_name = "ssd_mobilenet_v1_coco_2017_11_17"
# PATH_TO_LABELS = './mscoco_label_map.pbtxt'   -> please don't change this file
# TO change Model please visit 
# https://github.com/tensorflow/models/blob/master/research/object_detection/g3doc/tf1_detection_zoo.md
# right click and copy link addresss. select only name of the model.
# http://download.tensorflow.org/models/object_detection/faster_rcnn_resnet101_coco_2018_01_28.tar.gz
TEST_IMAGES_DIR = "TestImages"
TYPE_IMAGE = "jpg"
MODEL_NAME = "faster_rcnn_resnet101_coco_2018_01_28"

import numpy as np
import os
import six.moves.urllib as urllib
import sys
import tarfile
import tensorflow as tf
import zipfile
import os
import pathlib
import cv2
import pandas as pd

from collections import defaultdict
from io import StringIO
from matplotlib import pyplot as plt
from PIL import Image
from IPython.display import display

from object_detection.utils import ops as utils_ops
from object_detection.utils import label_map_util
from object_detection.utils import visualization_utils as vis_util

# patch tf1 into `utils.ops`
utils_ops.tf = tf.compat.v1

# Patch the location of gfile
tf.gfile = tf.io.gfile

def load_model(model_name="faster_rcnn_resnet50_v1_640x640_coco17_tpu-8"):
  #"http://download.tensorflow.org/models/object_detection/tf2/20200711/faster_rcnn_resnet50_v1_640x640_coco17_tpu-8.tar.gz"
  base_url = 'http://download.tensorflow.org/models/object_detection/'
  model_file = model_name + '.tar.gz'
  model_dir = tf.keras.utils.get_file(
    fname=model_name, 
    origin=base_url + model_file,
    untar=True)
  print(model_dir)
  model_dir = pathlib.Path(model_dir)/"saved_model"

  model = tf.saved_model.load(str(model_dir))

  return model


# List of the strings that is used to add correct label for each box.
PATH_TO_LABELS = './mscoco_label_map.pbtxt'
category_index = label_map_util.create_category_index_from_labelmap(PATH_TO_LABELS, use_display_name=True)


# If you want to test the code with your images, just add path to the images to the TEST_IMAGE_PATHS.
PATH_TO_TEST_IMAGES_DIR = pathlib.Path('./{}'.format(TEST_IMAGES_DIR))
TEST_IMAGE_PATHS = sorted(list(PATH_TO_TEST_IMAGES_DIR.glob("*.{}".format(TYPE_IMAGE))))
print("Total Images = {}".format(len(TEST_IMAGE_PATHS)))
model_name = '{}'.format(MODEL_NAME)
detection_model = load_model(model_name)

# print(detection_model.signatures['serving_default'].inputs)

def run_inference_for_single_image(model, image):
  image = np.asarray(image)
  # The input needs to be a tensor, convert it using `tf.convert_to_tensor`.
  input_tensor = tf.convert_to_tensor(image)
  # The model expects a batch of images, so add an axis with `tf.newaxis`.
  input_tensor = input_tensor[tf.newaxis,...]

  # Run inference
  model_fn = model.signatures['serving_default']
  output_dict = model_fn(input_tensor)

  # All outputs are batches tensors.
  # Convert to numpy arrays, and take index [0] to remove the batch dimension.
  # We're only interested in the first num_detections.
  num_detections = int(output_dict.pop('num_detections'))
  output_dict = {key:value[0, :num_detections].numpy() 
                 for key,value in output_dict.items()}
  output_dict['num_detections'] = num_detections

  # detection_classes should be ints.
  output_dict['detection_classes'] = output_dict['detection_classes'].astype(np.int64)
   
  # Handle models with masks:
  if 'detection_masks' in output_dict:
    # Reframe the the bbox mask to the image size.
    detection_masks_reframed = utils_ops.reframe_box_masks_to_image_masks(
              output_dict['detection_masks'], output_dict['detection_boxes'],
               image.shape[0], image.shape[1])      
    detection_masks_reframed = tf.cast(detection_masks_reframed > 0.5,
                                       tf.uint8)
    output_dict['detection_masks_reframed'] = detection_masks_reframed.numpy()
    
  return output_dict

def show_inference(model, image_path):
  # the array based representation of the image will be used later in order to prepare the
  # result image with boxes and labels on it.
  image_np = np.array(Image.open(image_path))
  # Actual detection.
  output_dict = run_inference_for_single_image(model, image_np)
  # Visualization of the results of a detection.
  vis_util.visualize_boxes_and_labels_on_image_array(
      image_np,
      output_dict['detection_boxes'],
      output_dict['detection_classes'],
      output_dict['detection_scores'],
      category_index,
      min_score_thresh=.0,
      instance_masks=output_dict.get('detection_masks_reframed', None),
      use_normalized_coordinates=True,
      line_thickness=8)
  new_dict = {}

  for k in output_dict.keys():
    if 'detection_classes' in k:
        new_dict[k] = output_dict[k]
    if 'detection_scores' in k:
        new_dict[k] = output_dict[k]

  # cv2.imshow("inspect",image_np)
  # cv2.waitKey(0)
  return new_dict

if not os.path.exists('results/{}/'.format(PATH_TO_TEST_IMAGES_DIR)):
    os.makedirs('results/{}/'.format(PATH_TO_TEST_IMAGES_DIR))
for image_path in TEST_IMAGE_PATHS:
    output_dict = show_inference(detection_model, image_path)
    filename,_=os.path.splitext(image_path)
    filename = os.path.split(filename)[1]
    df = pd.DataFrame.from_dict(output_dict)
    df.to_csv("./results/{}/{}.csv".format(PATH_TO_TEST_IMAGES_DIR,filename),index=False)
    