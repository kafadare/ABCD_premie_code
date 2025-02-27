#!/bin/bash
#SBATCH -t 0-8:00

SUBMIT_SCRIPT=$1
FOLDER_NAME=$2
MODEL_DIR=$3
DATA_FILENAME=$4
OUT_FOLDER=$5
OUTPUT_FILENAME=$6
#SHELL_OUTPUT_FOLDER=$7
#JOB_NAME=$8

NO_FILES=$(ls -1 "$FOLDER_NAME"| wc -l)

#NO_FILES=10



job_array_id=$(sbatch --array=1-$NO_FILES "$SUBMIT_SCRIPT" "$MODEL_DIR" "$DATA_FILENAME" "$OUT_FOLDER" "$OUTPUT_FILENAME" | awk '{print $NF}')
#job_array_id=$(sbatch --array=1-$NO_FILES --export=ALL "$SUBMIT_SCRIPT" | awk '{print $NF}')
echo "Job Array ID: $job_array_id"

sleep 1

#Waiting for all jobs to finish to concatanate output files
while squeue --job "$job_array_id" | grep -q "R\| PD"; do
  echo "Waiting for job array $job_array_id to finish."
  sleep 30
done

echo "All jobs in array have completed. Proceeding with concatanating the output files."

head -n 1 "${OUTPUT_FILENAME}_1.csv" > "${OUTPUT_FILENAME}_all.csv"
echo "Added header to output file."

for file in "${OUTPUT_FILENAME}_"*[0-9]*.csv; do
  tail -n +2 "$file" >> "${OUTPUT_FILENAME}_all.csv"
  echo "Added new line to output file from $file."
done

rm "${OUTPUT_FILENAME}_"*[0-9]*.csv

#Submit with following command (customize...)

#Global fits GG, RS, model selection split 2
#sbatch array_script_submit.sh submit_gamlss_fits_ABCD.sh /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_GG_global "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_GG_global/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd_baseline_matchedTrainTwo_selectVars_famfilter2024-11-26.csv" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_GG_global_TWO/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_GG_global_TWO/gamlss_fits_stats"

#Global fits GG, RS, model selection split 1
#sbatch array_script_submit.sh submit_gamlss_fits_ABCD.sh /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_GG_global "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_GG_global/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd_baseline_matchedTrainOne_selectVars_famfilter2024-11-26.csv" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_GG_global_ONE/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_GG_global_ONE/gamlss_fits_stats"

#Regional fits with RS
#sbatch array_script_submit.sh submit_gamlss_fits_ABCD.sh /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_allPhen "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_allPhen/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd_baseline_matchedTrain_selectVars_famfilter2024-10-04.csv" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen_RS/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen_RS/gamlss_fits_stats_RS"

#Linear Family Fits
#sbatch array_script_submit.sh submit_gamlss_fits_ABCD.sh /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_linear "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_linear/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-09-20.csv" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_linear/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_linear/gamlss_fits_stats"

#Linear Family Fits - Site as Fixed
#sbatch array_script_submit.sh submit_gamlss_fits_ABCD.sh /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_linear_fixedsite "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_linear_fixedsite/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-09-20.csv" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_linear_fixedsite/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_linear_fixedsite/gamlss_fits_stats"

#Skew Family Fits
#sbatch array_script_submit.sh submit_gamlss_fits_ABCD.sh /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_skew "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_skew/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-09-20.csv" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_skew/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_skew/gamlss_fits_stats"

#Linear Family Fits with Outliers Removed
#sbatch array_script_submit.sh submit_gamlss_fits_ABCD.sh /mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_linear "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family_linear/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter_outliersRM2024-10-28.csv" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_linear_outRM/" "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_linear_outRM/gamlss_fits_stats"

# MODEL_DIR="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_allPhen/"
# DATA_FILENAME="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd_baseline_matchedTrain_selectVars_famfilter2024-10-04.csv"
# OUT_FOLDER="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen_RS/"
# OUTPUT_FILENAME="/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen_RS/gamlss_fits_stats_RS.csv"
