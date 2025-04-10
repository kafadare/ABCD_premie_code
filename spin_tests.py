from neuromaps import  nulls, stats
from nilearn.datasets import fetch_atlas_surf_destrieux
import abagen
import pandas as pd
import nibabel as nib

atlas_files = abagen.fetch_desikan_killiany(surface = True)
atlas_lh = nib.load(atlas_files['image'][0])
atlas_rh = nib.load(atlas_files['image'][1])
labels_lh = atlas_lh.labeltable.get_labels_as_dict()
labels_rh = atlas_rh.labeltable.get_labels_as_dict()

#read in the bw betas
bw_betas = pd.read_csv("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/Surface_Maps/dsk_birthweight_all.csv")
bw_pgs_betas = pd.read_csv("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/Surface_Maps/dsk_birthweightPGS_all.csv")
ga_betas = pd.read_csv("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/Surface_Maps/dsk_gestAge_all.csv")
genP_betas = pd.read_csv("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/Surface_Maps/dsk_genP_all.csv")

#Split by hemisphere, which also removes non-cortical. Should be 34 for each hemisphere
bw_left = bw_betas[bw_betas['hemisphere'] == 'L'].copy()
bw_right = bw_betas[bw_betas['hemisphere'] == 'R'].copy()
pgs_left = bw_pgs_betas[bw_pgs_betas['hemisphere'] == 'L'].copy()
pgs_right = bw_pgs_betas[bw_pgs_betas['hemisphere'] == 'R'].copy()
ga_left = ga_betas[ga_betas['hemisphere'] == 'L'].copy()
ga_right = ga_betas[ga_betas['hemisphere'] == 'R'].copy()
genP_left = genP_betas[genP_betas['hemisphere'] == 'L'].copy()
genP_right = genP_betas[genP_betas['hemisphere'] == 'R'].copy()

# Convert label dicts into DataFrames
df_labels_lh = pd.DataFrame(list(labels_lh.items()), columns=['Key', 'label'])
df_labels_rh = pd.DataFrame(list(labels_rh.items()), columns=['Key', 'label'])

# Merge label-Key mapping with betas
bw_left = pd.merge(bw_left, df_labels_lh, on='label')
bw_right = pd.merge(bw_right, df_labels_rh, on='label')
pgs_left = pd.merge(pgs_left, df_labels_lh, on='label')
pgs_right = pd.merge(pgs_right, df_labels_rh, on='label')
ga_left = pd.merge(ga_left, df_labels_lh, on='label')
ga_right = pd.merge(ga_right, df_labels_rh, on='label')
genP_left = pd.merge(genP_left, df_labels_lh, on='label')
genP_right = pd.merge(genP_right, df_labels_rh, on='label')

# Sort by Key
bw_left = bw_left.sort_values(by='Key')
bw_right = bw_right.sort_values(by='Key')
pgs_left = pgs_left.sort_values(by='Key')
pgs_right = pgs_right.sort_values(by='Key')
ga_left = ga_left.sort_values(by='Key')
ga_right = ga_right.sort_values(by='Key')
genP_left = genP_left.sort_values(by='Key')
genP_right = genP_right.sort_values(by='Key')

# Combine hemispheres
bw_betas_ordered = pd.concat([bw_left, bw_right])
pgs_betas_ordered = pd.concat([pgs_left, pgs_right])
ga_betas_ordered = pd.concat([ga_left, ga_right])
genP_betas_ordered = pd.concat([genP_left, genP_right])

# Get 1D vectors of beta values
bw_beta_vector = bw_betas_ordered['bw_beta'].values
pgs_beta_vector = pgs_betas_ordered['bw_pgs_beta'].values
ga_beta_vector = ga_betas_ordered['ga_beta'].values
genP_beta_vector = genP_betas_ordered['gp_beta'].values

#Generating indices of null spins instead of rotating data, so can apply indices to the map being rotated
nulls_idx = nulls.alexander_bloch(data=None, atlas='fsaverage', density='10k', parcellation=[atlas_lh, atlas_rh], n_perm=10000, seed=1234)

#Using the indices to rotate the maps
rotated_bw = bw_beta_vector[nulls_idx]

#Test bw-bwpgs
pgs_corr, pgs_pval = stats.compare_images(bw_beta_vector, pgs_beta_vector, nulls=rotated_bw, metric='spearmanr')
#Test bw-gestAge
ga_corr, ga_pval = stats.compare_images(bw_beta_vector, ga_beta_vector, nulls=rotated_bw, metric='spearmanr')
#Test bw-genP
gp_corr, gp_pval = stats.compare_images(bw_beta_vector, genP_beta_vector, nulls=rotated_bw, metric='spearmanr')