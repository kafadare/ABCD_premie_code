#!/bin/bash
#SBATCH --job-name=ABCD_family_selection_test
#SBATCH --output=/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen/out_messages/%x_%A_%a_output.txt
#SBATCH --time=04:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=4G

# Directory containing the model files
MODEL_DIR="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_allPhen/"
DATA_FILENAME="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd_baseline_matchedTrain_selectVars_famfilter2024-10-04.csv"
OUT_FOLDER="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen/"
OUTPUT_FILENAME="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen/gamlss_family_fits_stats_allPhen.csv"

#Create a temp directory for the current job
TMPDIR=$(mktemp -d /mnt/isilon/bgdlab-processing/Eren/job_${SLURM_ARRAY_TASK_ID}_XXXXXX)
export TMPDIR
cd $TMPDIR

module load R/4.2.3

# Run the R script with the specified model
Rscript /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/ABCD_premie_code/abcd_gamlss_model_fit.R $SLURM_ARRAY_TASK_ID $MODEL_DIR $DATA_FILENAME $OUT_FOLDER $OUTPUT_FILENAME

trap "rm -rf $TMPDIR" EXIT