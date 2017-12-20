####################################################################
### Freesurfer longitudinal pipeline for REBOOTII T1 and T2 data ###
####################################################################

#! tcsh
cd /home/ALDRECENTRUM/alexander.lebedev/Desktop/RBTII_data

# Set subject list and necessary paths:
set subjs = `ls -d NIFTI/*`
setenv FREESURFER_HOME /usr/local/freesurfer
source $FREESURFER_HOME/SetUpFreeSurfer.csh
setenv SUBJECTS_DIR /home/ALDRECENTRUM/alexander.lebedev/Desktop/RBTII_data/FSDIR
cd $SUBJECTS_DIR

# Run the script:
foreach i ($subjs)
	recon-all -base $i\_base -tp $i\_1 -tp $i\_2 -all
	recon-all -long $i\_1 $i\_base -all
	recon-all -long $i\_2 $i\_base -all
end
