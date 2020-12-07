#!/usr/bin/env bash

# Final project, Stats506 F20
# This script downloads the 2012 CBECS metadata and codebook 
# from the EIA website. 
# Author: Yanyu Long, longyyu@umich.edu
# Updated: Nov 28, 2020
# 79: -------------------------------------------------------------------------
data_lib="./data"
if [ ! -d $data_lib ]; then
  mkdir $data_lib && echo "Created directory ${data_lib}."
fi
cd $data_lib

url_head="https://www.eia.gov/consumption/commercial/data/2012/xls"

for file in 2012_public_use_data_aug2016.csv 2012microdata_codebook.xlsx; do
  if [ ! -f "$file" ]; then
    url="${url_head}/${file}"
    wget $url
  else
    echo "${data_lib}/${file} already exists."
  fi
done
# 79: -------------------------------------------------------------------------

