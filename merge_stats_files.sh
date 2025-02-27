#!/bin/bash

OUTPUT_FILENAME=$1

head -n 1 "${OUTPUT_FILENAME}_1.csv" > "${OUTPUT_FILENAME}_all.csv"
echo "Added header to output file."

for file in "${OUTPUT_FILENAME}_"*[0-9]*.csv; do
  tail -n +2 "$file" >> "${OUTPUT_FILENAME}_all.csv"
  echo "Added new line to output file from $file."
done

rm "${OUTPUT_FILENAME}_"*[0-9]*.csv


#rm "gamlss_fits_stats_"*[0-9]*.csv