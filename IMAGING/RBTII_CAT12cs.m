%-----------------------------------------------------------------------
% This is a template job file for processing structural T1 images in the CRESCENT
% Job generated on 15-Nov-2017 11:21:21 by Alexander Lebedev
% requires SPM12 (used version 6685) and CAT12 installed in the toolbox folder
%-----------------------------------------------------------------------

matlabbatch{1}.spm.tools.cat.estwrite.data = {
                                              'NIFTI/sub-1001/ses-posttrain/anat/sub-1001_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1001/ses-pretrain/anat/sub-1001_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1017/ses-posttrain/anat/sub-1017_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1017/ses-pretrain/anat/sub-1017_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1033/ses-posttrain/anat/sub-1033_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1033/ses-pretrain/anat/sub-1033_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1049/ses-posttrain/anat/sub-1049_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1049/ses-pretrain/anat/sub-1049_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1065/ses-posttrain/anat/sub-1065_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1065/ses-pretrain/anat/sub-1065_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1081/ses-posttrain/anat/sub-1081_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1081/ses-pretrain/anat/sub-1081_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1097/ses-posttrain/anat/sub-1097_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1097/ses-pretrain/anat/sub-1097_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1113/ses-posttrain/anat/sub-1113_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1113/ses-pretrain/anat/sub-1113_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1129/ses-posttrain/anat/sub-1129_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1129/ses-pretrain/anat/sub-1129_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1145/ses-posttrain/anat/sub-1145_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1145/ses-pretrain/anat/sub-1145_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-1161/ses-posttrain/anat/sub-1161_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-1161/ses-pretrain/anat/sub-1161_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2002/ses-posttrain/anat/sub-2002_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2002/ses-pretrain/anat/sub-2002_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2034/ses-posttrain/anat/sub-2034_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2034/ses-pretrain/anat/sub-2034_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2050/ses-posttrain/anat/sub-2050_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2050/ses-pretrain/anat/sub-2050_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2066/ses-posttrain/anat/sub-2066_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2066/ses-pretrain/anat/sub-2066_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2098/ses-posttrain/anat/sub-2098_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2098/ses-pretrain/anat/sub-2098_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2114/ses-posttrain/anat/sub-2114_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2114/ses-pretrain/anat/sub-2114_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2130/ses-posttrain/anat/sub-2130_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2130/ses-pretrain/anat/sub-2130_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2178/ses-posttrain/anat/sub-2178_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2178/ses-pretrain/anat/sub-2178_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2194/ses-posttrain/anat/sub-2194_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2194/ses-pretrain/anat/sub-2194_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2210/ses-posttrain/anat/sub-2210_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2210/ses-pretrain/anat/sub-2210_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-2226/ses-posttrain/anat/sub-2226_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-2226/ses-pretrain/anat/sub-2226_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3003/ses-posttrain/anat/sub-3003_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3003/ses-pretrain/anat/sub-3003_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3019/ses-posttrain/anat/sub-3019_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3019/ses-pretrain/anat/sub-3019_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3051/ses-posttrain/anat/sub-3051_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3051/ses-pretrain/anat/sub-3051_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3067/ses-posttrain/anat/sub-3067_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3067/ses-pretrain/anat/sub-3067_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3083/ses-posttrain/anat/sub-3083_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3083/ses-pretrain/anat/sub-3083_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3099/ses-posttrain/anat/sub-3099_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3099/ses-pretrain/anat/sub-3099_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3115/ses-posttrain/anat/sub-3115_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3115/ses-pretrain/anat/sub-3115_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3131/ses-posttrain/anat/sub-3131_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3131/ses-pretrain/anat/sub-3131_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3147/ses-posttrain/anat/sub-3147_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3147/ses-pretrain/anat/sub-3147_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-3163/ses-posttrain/anat/sub-3163_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-3163/ses-pretrain/anat/sub-3163_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4004/ses-posttrain/anat/sub-4004_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4004/ses-pretrain/anat/sub-4004_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4036/ses-posttrain/anat/sub-4036_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4036/ses-pretrain/anat/sub-4036_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4052/ses-posttrain/anat/sub-4052_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4052/ses-pretrain/anat/sub-4052_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4068/ses-posttrain/anat/sub-4068_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4068/ses-pretrain/anat/sub-4068_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4084/ses-posttrain/anat/sub-4084_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4084/ses-pretrain/anat/sub-4084_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4100/ses-posttrain/anat/sub-4100_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4100/ses-pretrain/anat/sub-4100_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4116/ses-posttrain/anat/sub-4116_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4116/ses-pretrain/anat/sub-4116_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4132/ses-posttrain/anat/sub-4132_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4132/ses-pretrain/anat/sub-4132_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4148/ses-posttrain/anat/sub-4148_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4148/ses-pretrain/anat/sub-4148_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4164/ses-posttrain/anat/sub-4164_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4164/ses-pretrain/anat/sub-4164_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4180/ses-posttrain/anat/sub-4180_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4180/ses-pretrain/anat/sub-4180_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-4228/ses-posttrain/anat/sub-4228_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-4228/ses-pretrain/anat/sub-4228_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5005/ses-posttrain/anat/sub-5005_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5005/ses-pretrain/anat/sub-5005_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5053/ses-posttrain/anat/sub-5053_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5053/ses-pretrain/anat/sub-5053_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5069/ses-posttrain/anat/sub-5069_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5069/ses-pretrain/anat/sub-5069_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5085/ses-posttrain/anat/sub-5085_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5085/ses-pretrain/anat/sub-5085_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5101/ses-posttrain/anat/sub-5101_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5101/ses-pretrain/anat/sub-5101_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5117/ses-posttrain/anat/sub-5117_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5117/ses-pretrain/anat/sub-5117_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5133/ses-posttrain/anat/sub-5133_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5133/ses-pretrain/anat/sub-5133_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5149/ses-posttrain/anat/sub-5149_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5149/ses-pretrain/anat/sub-5149_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5165/ses-posttrain/anat/sub-5165_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5165/ses-pretrain/anat/sub-5165_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5181/ses-posttrain/anat/sub-5181_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5181/ses-pretrain/anat/sub-5181_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5197/ses-posttrain/anat/sub-5197_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5197/ses-pretrain/anat/sub-5197_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5213/ses-posttrain/anat/sub-5213_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5213/ses-pretrain/anat/sub-5213_ses-pretrain_T1w.nii'
                                              'NIFTI/sub-5229/ses-posttrain/anat/sub-5229_ses-posttrain_T1w.nii'
                                              'NIFTI/sub-5229/ses-pretrain/anat/sub-5229_ses-pretrain_T1w.nii'
                                              };
matlabbatch{1}.spm.tools.cat.estwrite.nproc = 5;

% --- Reference to TPMs --- %
matlabbatch{1}.spm.tools.cat.estwrite.opts.tpm = {'/home/ALDRECENTRUM/alexander.lebedev/Desktop/tbx/spm12/tpm/TPM.nii'};
% ----------- END --------- %

matlabbatch{1}.spm.tools.cat.estwrite.opts.affreg = 'mni';
matlabbatch{1}.spm.tools.cat.estwrite.opts.biasstr = 0.75;
matlabbatch{1}.spm.tools.cat.estwrite.extopts.APP = 2;
matlabbatch{1}.spm.tools.cat.estwrite.extopts.LASstr = 0.75;
matlabbatch{1}.spm.tools.cat.estwrite.extopts.gcutstr = 0.5;
matlabbatch{1}.spm.tools.cat.estwrite.extopts.cleanupstr = 0.5;

% --- References to the templates --- %
matlabbatch{1}.spm.tools.cat.estwrite.extopts.registration.darteltpm = {'/home/ALDRECENTRUM/alexander.lebedev/Desktop/tbx/spm12/toolbox/cat12/templates_1.50mm/Template_1_IXI555_MNI152.nii'};
matlabbatch{1}.spm.tools.cat.estwrite.extopts.registration.shootingtpm = {'/home/ALDRECENTRUM/alexander.lebedev/Desktop/tbx/spm12/toolbox/cat12/templates_1.50mm/Template_0_IXI555_MNI152_GS.nii'};
% --------------- END --------------- %

matlabbatch{1}.spm.tools.cat.estwrite.extopts.registration.regstr = 4;
matlabbatch{1}.spm.tools.cat.estwrite.extopts.vox = 1.5;
matlabbatch{1}.spm.tools.cat.estwrite.output.surface = 1;
matlabbatch{1}.spm.tools.cat.estwrite.output.ROI = 1;
matlabbatch{1}.spm.tools.cat.estwrite.output.GM.native = 1;
matlabbatch{1}.spm.tools.cat.estwrite.output.GM.mod = 1;
matlabbatch{1}.spm.tools.cat.estwrite.output.GM.dartel = 2;
matlabbatch{1}.spm.tools.cat.estwrite.output.WM.native = 0;
matlabbatch{1}.spm.tools.cat.estwrite.output.WM.mod = 1;
matlabbatch{1}.spm.tools.cat.estwrite.output.WM.dartel = 0;
matlabbatch{1}.spm.tools.cat.estwrite.output.bias.warped = 1;
matlabbatch{1}.spm.tools.cat.estwrite.output.jacobian.warped = 0;
matlabbatch{1}.spm.tools.cat.estwrite.output.warps = [1 0];
