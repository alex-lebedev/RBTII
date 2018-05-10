% The script performs fMRI design/contrast specification and estimation for
% "REBOOT: N-back" task
% NOTE: "/Volumes/REBOOTII/RBTII/IMAGING_PROCESSED/fMRI/DPABIDIR_nback/spm/nBack" should already
% contain empty subject-specific folders
slist = dir('/Volumes/REBOOTII/RBTII/ANALYSIS/Nback_fmriprep/V1');
slist = slist(3:numel(slist)) % subject list


spm('defaults','FMRI')

for i = 1:size(slist,1)
% Generate multiple motion regressors (6-DOF + FD-Power):
    mot = readtable(strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', slist(i,:).name, '_ses-pretrain_task-nback_bold_confounds.tsv'), 'Filetype', 'text');
    R = table2array(mot(:,[{'CSF'} {'X'} {'Y'} {'Z'} {'RotX'} {'RotY'} {'RotZ'}]));
    save(strcat('/Volumes/REBOOTII/RBTII/ANALYSIS/Nback_fmriprep/V1/',slist(i,:).name,'/mot_nback.mat'), 'R');
    
    matlabbatch{1}.spm.stats.fmri_spec.dir = {strcat('/Volumes/REBOOTII/RBTII/ANALYSIS/Nback_fmriprep/V1/',slist(i,:).name)};
    matlabbatch{1}.spm.stats.fmri_spec.timing.units = 'scans';
    matlabbatch{1}.spm.stats.fmri_spec.timing.RT = 2;
    matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t = 16;
    matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t0 = 8;

    matlabbatch{1}.spm.stats.fmri_spec.sess.scans = {
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,1')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,2')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,3')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,4')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,5')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,6')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,7')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,8')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,9')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,10')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,11')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,12')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,13')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,14')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,15')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,16')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,17')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,18')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,19')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,20')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,21')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,22')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,23')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,24')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,25')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,26')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,27')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,28')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,29')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,30')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,31')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,32')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,33')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,34')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,35')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,36')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,37')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,38')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,39')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,40')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,41')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,42')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,43')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,44')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,45')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,46')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,47')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,48')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,49')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,50')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,51')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,52')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,53')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,54')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,55')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,56')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,57')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,58')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,59')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,60')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,61')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,62')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,63')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,64')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,65')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,66')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,67')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,68')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,69')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,70')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,71')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,72')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,73')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,74')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,75')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,76')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,77')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,78')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,79')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,80')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,81')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,82')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,83')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,84')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,85')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,86')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,87')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,88')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,89')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,90')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,91')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,92')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,93')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,94')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,95')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,96')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,97')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,98')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,99')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,100')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,101')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,102')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,103')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,104')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,105')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,106')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,107')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,108')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,109')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,110')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,111')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,112')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,113')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,114')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,115')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,116')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,117')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,118')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,119')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,120')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,121')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,122')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,123')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,124')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,125')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,126')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,127')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,128')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,129')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,130')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,131')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,132')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,133')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,134')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,135')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,136')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,137')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,138')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,139')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,140')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,141')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,142')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,143')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,144')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,145')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,146')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,147')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,148')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,149')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,150')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,151')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,152')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,153')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,154')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,155')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,156')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,157')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,158')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,159')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,160')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,161')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,162')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,163')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,164')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,165')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,166')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,167')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,168')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,169')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,170')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,171')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,172')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,173')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,174')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,175')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,176')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,177')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,178')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,179')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,180')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,181')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,182')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,183')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,184')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,185')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,186')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,187')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,188')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,189')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,190')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,191')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,192')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,193')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,194')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,195')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,196')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,197')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,198')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,199')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,200')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,201')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,202')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,203')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,204')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,205')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,206')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,207')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,208')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,209')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,210')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,211')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,212')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,213')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,214')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,215')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,216')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,217')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,218')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,219')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,220')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,221')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,222')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,223')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,224')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,225')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,226')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,227')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,228')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,229')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,230')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,231')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,232')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,233')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,234')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,235')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,236')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,237')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,238')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,239')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,240')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,241')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,242')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,243')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,244')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,245')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,246')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,247')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,248')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,249')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,250')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,251')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,252')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,253')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,254')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,255')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,256')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,257')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,258')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,259')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,260')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,261')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,262')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,263')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,264')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,265')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,266')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,267')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,268')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,269')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,270')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,271')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,272')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,273')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,274')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,275')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,276')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,277')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,278')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,279')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,280')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,281')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,282')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,283')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,284')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,285')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,286')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,287')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,288')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,289')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,290')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,291')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,292')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,293')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,294')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,295')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,296')
                                                     strcat('/Volumes/REBOOTII/RBTII/PROCESSED/fmriprep/fmriprep/',slist(i,:).name,'/ses-pretrain/func/', 's',slist(i,:).name, '_ses-pretrain_task-nback_bold_space-MNI152NLin2009cAsym_preproc.nii,297')
                                                     };
    %%
    % Onsets/Duration for block "N1":
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(1).name = 'N1';
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(1).onset = [3
                                                             102
                                                             201];
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(1).duration = 30;
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(1).tmod = 0;
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(1).pmod = struct('name', {}, 'param', {}, 'poly', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(1).orth = 1;
    
    % Onsets/Duration for block "N2":
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(2).name = 'N2';
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(2).onset = [36
                                                             135
                                                             234];
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(2).duration = 30;
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(2).tmod = 0;
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(2).pmod = struct('name', {}, 'param', {}, 'poly', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(2).orth = 1;
    
    % Onsets/Duration for block "N3":
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(3).name = 'N3';
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(3).onset = [69
                                                             168
                                                             267];
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(3).duration = 30;
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(3).tmod = 0;
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(3).pmod = struct('name', {}, 'param', {}, 'poly', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(3).orth = 1;
    
    % Onsets/Duration for fixation dot:
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(4).name = 'fix';
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(4).onset = [0
                                                             33
                                                             66
                                                             99
                                                             132
                                                             165
                                                             198
                                                             231
                                                             264];
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(4).duration = 3;
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(4).tmod = 0;
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(4).pmod = struct('name', {}, 'param', {}, 'poly', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess.cond(4).orth = 1;
    matlabbatch{1}.spm.stats.fmri_spec.sess.multi = {''};
    matlabbatch{1}.spm.stats.fmri_spec.sess.regress = struct('name', {}, 'val', {});
    matlabbatch{1}.spm.stats.fmri_spec.sess.multi_reg = {strcat('/Volumes/REBOOTII/RBTII/ANALYSIS/Nback_fmriprep/V1/',slist(i,:).name,'/mot_nback.mat')};
    matlabbatch{1}.spm.stats.fmri_spec.sess.hpf = 190; % Setting larger high-pass filter due to long block duration
    matlabbatch{1}.spm.stats.fmri_spec.fact = struct('name', {}, 'levels', {});
    matlabbatch{1}.spm.stats.fmri_spec.bases.hrf.derivs = [0 0];
    matlabbatch{1}.spm.stats.fmri_spec.volt = 1;
    matlabbatch{1}.spm.stats.fmri_spec.global = 'None';
    matlabbatch{1}.spm.stats.fmri_spec.mthresh = 0.8;
    matlabbatch{1}.spm.stats.fmri_spec.mask = {''};
    matlabbatch{1}.spm.stats.fmri_spec.cvi = 'AR(1)';
    matlabbatch{2}.spm.stats.fmri_est.spmmat(1) = cfg_dep('fMRI model specification: SPM.mat File', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','spmmat'));
    matlabbatch{2}.spm.stats.fmri_est.write_residuals = 0;
    matlabbatch{2}.spm.stats.fmri_est.method.Classical = 1;
    matlabbatch{3}.spm.stats.con.spmmat(1) = cfg_dep('Model estimation: SPM.mat File', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','spmmat'));

    % Specifying contrasts of interest:
    matlabbatch{3}.spm.stats.con.consess{1}.tcon.name = 'n2>n1';
    matlabbatch{3}.spm.stats.con.consess{1}.tcon.weights = [-1 1];
    matlabbatch{3}.spm.stats.con.consess{1}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{2}.tcon.name = 'n3>n1';
    matlabbatch{3}.spm.stats.con.consess{2}.tcon.weights = [-1 0 1];
    matlabbatch{3}.spm.stats.con.consess{2}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{3}.tcon.name = 'n3>n2';
    matlabbatch{3}.spm.stats.con.consess{3}.tcon.weights = [0 -1 1];
    matlabbatch{3}.spm.stats.con.consess{3}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{4}.tcon.name = 'n1>BL';
    matlabbatch{3}.spm.stats.con.consess{4}.tcon.weights = [1 0 0 -1];
    matlabbatch{3}.spm.stats.con.consess{4}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{5}.tcon.name = 'n2>BL';
    matlabbatch{3}.spm.stats.con.consess{5}.tcon.weights = [0 1 0 -1];
    matlabbatch{3}.spm.stats.con.consess{5}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{6}.tcon.name = 'n3>BL';
    matlabbatch{3}.spm.stats.con.consess{6}.tcon.weights = [0 0 1 -1];
    matlabbatch{3}.spm.stats.con.consess{6}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.delete = 0;
  
    % Running the batch for each subject: 
    spm_jobman('run',matlabbatch)
end