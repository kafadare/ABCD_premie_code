library(magrittr)
library(dplyr)
library(gifti)
library(R.matlab)
library(RNifti)
library(ciftiTools)
ciftiTools.setOption('wb_path', '~/Applications/workbench')
library(ggplot2)

#Load desikan killiany atlas
#Atlases downloaded from abagen fetch.atlases - they have lh and rh consecutively labeled
atlas_lh <- read_gifti('~/Documents/Grad_School/BGDLab/ABCD_data/atlas-desikankilliany-lh.label.gii')
atlas_rh <- read_gifti('~/Documents/Grad_School/BGDLab/ABCD_data/atlas-desikankilliany-rh.label.gii')

#Load the CSV files with beta weights
bw_betas <- read.csv('~/Documents/Grad_School/BGDLab/ABCD_data/dsk_birthweight_all.csv')
bw_betas_left <- bw_betas %>% filter(hemisphere == "L")
bw_betas_right <- bw_betas %>% filter(hemisphere == "R")
bw_pgs_betas <- read.csv('~/Documents/Grad_School/BGDLab/ABCD_data/dsk_birthweightPGS_all.csv')
bw_pgs_betas_left <- bw_pgs_betas %>% filter(hemisphere == "L")
bw_pgs_betas_right <- bw_pgs_betas %>% filter(hemisphere == "R")



#Add a column to BW and BW-pgs beta csv files to correspond to the numerical labels
left_labels <- as.data.frame(atlas_lh$label) %>% mutate(label = rownames(.))
right_labels <- as.data.frame(atlas_rh$label) %>% mutate(label = rownames(.))
bw_betas_left <- merge(bw_betas_left, left_labels[,c("label", "Key")], by = "label")
bw_betas_right <- merge(bw_betas_right, right_labels[,c("label", "Key")], by = "label")

bw_pgs_betas_left <- merge(bw_pgs_betas_left, left_labels[,c("label", "Key")], by = "label")
bw_pgs_betas_right <- merge(bw_pgs_betas_right, right_labels[,c("label", "Key")], by = "label")

#Order the betas tables by the key order
bw_betas_left <-  bw_betas_left[order(as.numeric(bw_betas_left$Key)), ]
bw_betas_right <- bw_betas_right[order(as.numeric(bw_betas_right$Key)), ]

bw_pgs_betas_left <- bw_pgs_betas_left[order(as.numeric(bw_pgs_betas_left$Key)), ]
bw_pgs_betas_right <- bw_pgs_betas_right[order(as.numeric(bw_pgs_betas_right$Key)), ]

#combine csvs to have a single one including both hemispheres (no subcortical structures)
bw_betas_save <- rbind(bw_betas_left, bw_betas_right)
bw_pgs_betas_save <- rbind(bw_pgs_betas_left, bw_pgs_betas_right)
#save the ordered csvs
write.csv(bw_betas_save, "~/Documents/Grad_School/BGDLab/ABCD_data/bw_betas_ordered.csv")
write.csv(bw_pgs_betas_save, "~/Documents/Grad_School/BGDLab/ABCD_data/bw_pgs_betas_ordered.csv")
#save single text file of the ordered beta weights
write.table(bw_betas_save$bw_beta, file = "~/Documents/Grad_School/BGDLab/ABCD_data/bw_betas.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(bw_pgs_betas_save$bw_pgs_beta, file = "~/Documents/Grad_School/BGDLab/ABCD_data/bw_pgs_betas.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)


#From Kevin Sun's code
#define BW and BW-pgs maps using dsk atlas for lh and rh
BW_map_lh <- atlas_lh
BW_map_rh <- atlas_rh

BWpgs_map_lh <- atlas_lh
BWpgs_map_rh <- atlas_rh

#assign beta weights to BW
Weight_map$data$cortex_left <- vertex_weights_lh
Weight_map$data$cortex_right <- vertex_weights_rh

#assign beta weights to BW-pgs 


#save out cifti files in A or B dir
#write_cifti(Weight_map_med,outfile)
}