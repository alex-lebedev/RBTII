% List of open inputs
nrun = X; % enter the number of runs here
jobfile = {'/Users/alebedev/GitHub/RBTII/IMAGING/RBTII_fmriprep_smooth_8mm_job.m'};
jobs = repmat(jobfile, 1, nrun);
inputs = cell(0, nrun);
for crun = 1:nrun
end
spm('defaults', 'FMRI');
spm_jobman('run', jobs, inputs{:});
