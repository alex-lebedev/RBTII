#==============================#
#=== RBTII_BIDSconvert.tcsh ===#
#==============================#
#!tcsh
# The script reads out raw DICOM data from REBOOT-II, converts it to NIFTI
# and reorganizes the folders according to BIDS (longitudinal) format

# CD to working directory:
cd /home/ALDRECENTRUM/alexander.lebedev/Desktop/RBTII_data

# Create output directory for NIFTI images:
mkdir NIFTI

# Set subjects list:
set subjs = `ls -d DCM/*/V2/*`

# Start the loop:
foreach i ($subjs)
	# the two lines below cut the strings read by 'ls' to generate proper subject IDs from them:
	set snam = `echo $i | cut -d'/' -f4-`
	set snam = `echo $snam | cut -d'_' -f1`
  
	# Create BIDS folder structure for each subject
	# Visit 1
	mkdir NIFTI/sub-$snam
	mkdir NIFTI/sub-$snam/ses-pretrain
	mkdir NIFTI/sub-$snam/ses-pretrain/anat
	mkdir NIFTI/sub-$snam/ses-pretrain/func
	mkdir NIFTI/sub-$snam/ses-pretrain/dwi
	mkdir NIFTI/sub-$snam/ses-pretrain/fmap

	# Visit 2
	mkdir NIFTI/sub-$snam
	mkdir NIFTI/sub-$snam/ses-posttrain
	mkdir NIFTI/sub-$snam/ses-posttrain/anat
	mkdir NIFTI/sub-$snam/ses-posttrain/func
	mkdir NIFTI/sub-$snam/ses-posttrain/dwi
	mkdir NIFTI/sub-$snam/ses-posttrain/fmap

############
# PreTrain #
############
	# T1pretrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-pretrain/anat DCM/*/V1/$snam*/*Forskning_Ki_Reboot_*/MPRAGE_sag_ADNI*/IM-*-0001*
	rm NIFTI/sub-$snam/ses-pretrain/anat/co* NIFTI/sub-$snam/ses-pretrain/anat/o*
	mv NIFTI/sub-$snam/ses-pretrain/anat/*MPRAGE*.nii.gz NIFTI/sub-$snam/ses-pretrain/anat/sub-$snam\_\ses-pretrain_T1w.nii.gz
	# T2pretrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-pretrain/anat DCM/*/V1/$snam*/*Forskning_Ki_Reboot_*/t2_space_sag_p2_iso*/IM-*-0001*
	rm NIFTI/sub-$snam/ses-pretrain/anat/o*
	mv NIFTI/sub-$snam/ses-pretrain/anat/*isos*.nii.gz NIFTI/sub-$snam/ses-pretrain/anat/sub-$snam\_\ses-pretrain_T2w.nii.gz

	# RESTpretrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-pretrain/func DCM/*/V1/$snam*/*Forskning_Ki_Reboot_*/BOLD_resting_state*/IM-*-0001*
	mv NIFTI/sub-$snam/ses-pretrain/func/*BOLDresting*.nii.gz NIFTI/sub-$snam/ses-pretrain/func/sub-$snam\_\ses-pretrain_task-rest_bold.nii.gz
	cp  info_rest.json NIFTI/sub-$snam/ses-pretrain/func/sub-$snam\_\ses-pretrain_task-rest_bold.json
	# NBACKpretrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-pretrain/func DCM/*/V1/$snam*/*Forskning_Ki_Reboot_*/BOLD_task*/IM-*-0001*
	mv NIFTI/sub-$snam/ses-pretrain/func/*BOLDtasks*.nii.gz NIFTI/sub-$snam/ses-pretrain/func/sub-$snam\_\ses-pretrain_task-nback_bold.nii.gz
	cp  info_nback.json NIFTI/sub-$snam/ses-pretrain/func/sub-$snam\_\ses-pretrain_task-nback_bold.json

	# DTIpretrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-pretrain/dwi DCM/*/V1/$snam*/*Forskning_Ki_Reboot_*/ep2d_diff_mddw_64_p2_DFC_MIX*/IM-*-0001*
	mv NIFTI/sub-$snam/ses-pretrain/dwi/*ddiff*.nii.gz NIFTI/sub-$snam/ses-pretrain/dwi/sub-$snam\_\ses-pretrain_dwi.nii.gz
	mv NIFTI/sub-$snam/ses-pretrain/dwi/*.bvec NIFTI/sub-$snam/ses-pretrain/dwi/sub-$snam\_\ses-pretrain_dwi.bvec
	mv NIFTI/sub-$snam/ses-pretrain/dwi/*.bval NIFTI/sub-$snam/ses-pretrain/dwi/sub-$snam\_\ses-pretrain_dwi.bval
	
	#GREpretrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-pretrain/fmap DCM/*/V1/$snam*/*Forskning_Ki_Reboot_*/gre_field_mapping*/IM-*-0001*
	mv NIFTI/sub-$snam/ses-pretrain/fmap/*grefieldmapping*.nii.gz NIFTI/sub-$snam/ses-pretrain/fmap/sub-$snam\_\ses-pretrain_phasediff.nii.gz

#############
# PostTrain #
#############
	# T1posttrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-posttrain/anat DCM/*/V2/$snam*/*Forskning_Ki_Reboot_*/MPRAGE_sag_ADNI*/IM-*-0001*
	rm NIFTI/sub-$snam/ses-posttrain/anat/co* NIFTI/sub-$snam/ses-posttrain/anat/o*
	mv NIFTI/sub-$snam/ses-posttrain/anat/*MPRAGE*.nii.gz NIFTI/sub-$snam/ses-posttrain/anat/sub-$snam\_\ses-posttrain_T1w.nii.gz
	
	# T2posttrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-posttrain/anat DCM/*/V2/$snam*/*Forskning_Ki_Reboot_*/t2_space_sag_p2_iso*/IM-*-0001*
	rm NIFTI/sub-$snam/ses-posttrain/anat/o*
	mv NIFTI/sub-$snam/ses-posttrain/anat/*isos*.nii.gz NIFTI/sub-$snam/ses-posttrain/anat/sub-$snam\_\ses-posttrain_T2w.nii.gz

	# RESTposttrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-posttrain/func DCM/*/V2/$snam*/*Forskning_Ki_Reboot_*/BOLD_resting_state*/IM-*-0001*
	mv NIFTI/sub-$snam/ses-posttrain/func/*BOLDresting*.nii.gz NIFTI/sub-$snam/ses-posttrain/func/sub-$snam\_\ses-posttrain_task-rest_bold.nii.gz
	cp  info_rest.json NIFTI/sub-$snam/ses-posttrain/func/sub-$snam\_\ses-posttrain_task-rest_bold.json
	
	# NBACKposttrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-posttrain/func DCM/*/V2/$snam*/*Forskning_Ki_Reboot_*/BOLD_task*/IM-*-0001*
	mv NIFTI/sub-$snam/ses-posttrain/func/*BOLDtasks*.nii.gz NIFTI/sub-$snam/ses-posttrain/func/sub-$snam\_\ses-posttrain_task-nback_bold.nii.gz
	cp  info_nback.json NIFTI/sub-$snam/ses-posttrain/func/sub-$snam\_\ses-posttrain_task-nback_bold.json

	# DTIposttrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-posttrain/dwi DCM/*/V2/$snam*/*Forskning_Ki_Reboot_*/ep2d_diff_mddw_64_p2_DFC_MIX*/IM-*-0001*
	mv NIFTI/sub-$snam/ses-posttrain/dwi/*ddiff*.nii.gz NIFTI/sub-$snam/ses-posttrain/dwi/sub-$snam\_\ses-posttrain_dwi.nii.gz
	mv NIFTI/sub-$snam/ses-posttrain/dwi/*.bvec NIFTI/sub-$snam/ses-posttrain/dwi/sub-$snam\_\ses-posttrain_dwi.bvec
	mv NIFTI/sub-$snam/ses-posttrain/dwi/*.bval NIFTI/sub-$snam/ses-posttrain/dwi/sub-$snam\_\ses-posttrain_dwi.bval
	
	# GREposttrain
	/usr/local/mricron/dcm2nii -o NIFTI/sub-$snam/ses-posttrain/fmap DCM/*/V2/$snam*/*Forskning_Ki_Reboot_*/gre_field_mapping*/IM-*-0001*
	mv NIFTI/sub-$snam/ses-posttrain/fmap/*grefieldmapping*.nii.gz NIFTI/sub-$snam/ses-posttrain/fmap/sub-$snam\_\ses-posttrain_phasediff.nii.gz
end


# End of the script
