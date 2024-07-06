#!/bin/bash
#SBATCH --job-name=ABCD_family_selection_test
#SBATCH --output=%x_%A_%a_output.txt
#SBATCH --time=01:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=4G

# Directory containing the model files
MODEL_DIR="~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_models/"

# Get the list of model files
model_files=($(ls $MODEL_DIR))

# Get the Nth file based on the array index
MODEL_NAME=${model_files[$SLURM_ARRAY_TASK_ID - 1]}

echo "Model name: $MODEL_NAME"

OUTPUT_DIR="~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/out_messages/"
ERROR_DIR="~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/err_messages/"

# Specify the output and error file names based on the model name
#SBATCH --output=${OUTPUT_DIR}${MODEL_NAME}_output.txt
#SBATCH --error=${ERROR_DIR}${MODEL_NAME}_error.txt
  
DATA_FILENAME="~/Documents/Grad_School/BGDLab/ABCD_data/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-07-04.csv"
OUT_FOLDER="~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/"
OUTPUT_FILENAME="~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/gamlss_fits_stats.csv"

# Run the R script with the specified model
Rscript abcd_gamlss_model_fit.R $MODEL_NAME $DATA_FILENAME $OUT_FOLDER $OUTPUT_FILENAME