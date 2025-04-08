from neuromaps import datasets, images, nulls, resampling
from nilearn.datasets import fetch_atlas_surf_destrieux
import abagen
import pandas as pd

dsk = abagen.fetch_desikan_killiany(surface = True)
print(sorted(dsk))
print(dsk['info'])
info_df = pd.read_csv(dsk['info'])
#left and right hemispheres have identical label values. There is a column that denotes hemisphere
print(info_df.columns)
print(info_df.head(100))
cortical_right = info_df[(info_df['structure'] == 'cortical') & (info_df['hemisphere'] == 'R')]
cortical_left = info_df[(info_df['structure'] == 'cortical') & (info_df['hemisphere'] == 'L')]
#read in the tables with bw and bw-pgs values
bw_df = pd.read_csv("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/Surface_Maps/dsk_birthweight_all.csv")
print(bw_df.columns)
print(bw_df.head)
bw_pgs_df = pd.read_csv("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/Surface_Maps/dsk_birthweightPGS_all.csv")
print(bw_pgs_df.columns)
print(bw_pgs_df.head)


