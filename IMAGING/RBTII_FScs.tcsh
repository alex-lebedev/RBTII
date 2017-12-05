######################################################################
### Freesurfer cross-sectional pipeline for REBOOTII T1 and T2 data ##
######################################################################

#! tcsh
cd /home/ALDRECENTRUM/alexander.lebedev/Desktop/RBTII_data/NIFTI


# Set subject list and necessary paths:
set subjs = `ls -d *`
setenv FREESURFER_HOME /usr/local/freesurfer
source $FREESURFER_HOME/SetUpFreeSurfer.csh
setenv SUBJECTS_DIR /home/ALDRECENTRUM/alexander.lebedev/Desktop/RBTII_data/FSDIR
cd $SUBJECTS_DIR

# Run the script:
foreach i ($subjs)
recon-all -subjid $i\_1  -i /home/ALDRECENTRUM/alexander.lebedev/Desktop/RBTII_data/NIFTI/$i/ses-pretrain/anat/*T1w.nii -T2 /home/ALDRECENTRUM/alexander.lebedev/Desktop/RBTII_data/NIFTI/$i/ses-pretrain/anat/*T2w.nii -all
recon-all -subjid $i\_2  -i /home/ALDRECENTRUM/alexander.lebedev/Desktop/RBTII_data/NIFTI/$i/ses-posttrain/anat/*T1w.nii -T2 /home/ALDRECENTRUM/alexander.lebedev/Desktop/RBTII_data/NIFTI/$i/ses-posttrain/anat/*T2w.nii -all
end
