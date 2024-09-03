#!/bin/bash
#SBATCH --job-name=ABCD_family_selection_test
#SBATCH --output=/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_CG/out_messages/%x_%A_%a_output.txt
#SBATCH --time=04:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=4G

# Directory containing the model files
MODEL_DIR="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_v2/"
DATA_FILENAME="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-07-15.csv"
OUT_FOLDER="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_CG/"
OUTPUT_FILENAME="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_CG/gamlss_family_fits_stats_CG.csv"

module load R/4.2.3

# Run the R script with the specified model
Rscript abcd_gamlss_model_fit_CG.R $SLURM_ARRAY_TASK_ID $MODEL_DIR $DATA_FILENAME $OUT_FOLDER $OUTPUT_FILENAME