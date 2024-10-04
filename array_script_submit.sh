#!/bin/bash

SUBMIT_SCRIPT=$1
FOLDER_NAME=$2

NO_FILES=$(ls -1 "$FOLDER_NAME"| wc -l)

#NO_FILES=2

sbatch --array=1-$NO_FILES "$SUBMIT_SCRIPT"



#Submit with following command (customize...)
#sbatch array_script_submit.sh submit_gamlss_fits_ABCD.sh /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_v2
#sbatch array_script_submit.sh submit_gamlss_fits_ABCD.sh /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_allPhen