% Network-based statistics:

clear all

thlds = [0.01];


wdir = '/Volumes/REBOOTII/RBTII/PROCESSED/DPABI_rest/fmriprep_Glob/';
cd(wdir)


ind = [1:200];


% 1. Generate *mat- files (adjacency matrices)
foldRS1 = strcat(wdir, 'Results/ROISignals_FunImgPWglobalCSDF'); % set your folder name here
foldRS2 = strcat(wdir, 'S2_Results/S2_ROISignals_FunImgPWglobalCSDF');



mfliesRS1 = getAllFiles(foldRS1, 'ROISignals*.mat',1);
mfliesRS2 = getAllFiles(foldRS2, 'ROISignals*.mat',1);



t=1;
thld=thlds(t);
    % Weighted
    for i = 1:length(mfliesRS1)
      load(mfliesRS1{i})
      adjMrs1(:,:,i)= corrcoef(ROISignals(:,ind));
      %adjMrs1(adjMrs1<thld)=0;
      str1(i,:) = strengths_und(adjMrs1(:,:,i));
    [Ppos1(i,:) Pneg1(i,:)] = participation_coef_sign(adjMrs1(:,:,i),CI);     
    clear ROISignals
      %%%%%%%%%%%%%%%
      load(mfliesRS2{i})
      adjMrs2(:,:,i)= corrcoef(ROISignals(:,ind)); 
      %adjMrs2(adjMrs2<thld)=0;
      str2(i,:) = strengths_und(adjMrs2(:,:,i));
      [Ppos2(i,:) Pneg2(i,:)] = participation_coef_sign(adjMrs2(:,:,i),CI);
      clear ROISignals
      %%%%%%%%%%%%%%%
    end
    
adjM(:,:,:,1)=adjMrs1;
adjM(:,:,:,2)=adjMrs2;
adjM = mean(mean(adjM,4),3);


[CI Q]=community_louvain(adjM,[],[], 'negative_sym')   % then re-run the loop above

save('/Volumes/REBOOTII/RBTII/PROCESSED/NBS/str_rest_fmriprepGlobal.mat', 'str1', 'str2', 'Ppos1','Ppos2')

adjMrs1(adjMrs1<thld)=0;
adjMrs2(adjMrs2<thld)=0;
M(:,:,1:size(adjMrs1,3))=adjMrs1;
M(:,:,(size(adjMrs1,3)+1):(size(adjMrs2,3)*2))=adjMrs2;
save('/Volumes/REBOOTII/RBTII/PROCESSED/NBS/M_rest_fmriprep.mat', 'M')


gr = [1 1 1 0 0 0 1 0 0 1 1 1 1 0 0 1 0 0 0 1 1 1 0 0 0 1 1 1 1 0 0 1 ...
        0 0 1 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 0 0 0 0 1 1 0];

design = [[transpose(gr); repmat(0,size(gr,2),1)] [repmat(0,size(gr,2),1);transpose(gr)]...
 [transpose(1-gr);repmat(0,size(gr,2),1)] [repmat(0,size(gr,2),1);transpose(1-gr)]...
];


designWS = repmat(0,size(gr,2)*2,size(gr,2));
g = gr;
for i = 1:size(gr,2) 
    designWS([i,(size(gr,2)+i)],i)=1;
    g(:,i)=-1;
    g(:,(size(gr,2)+i))=1;
end


designWS = [designWS transpose(g) transpose(g.*[gr gr])];
design = [design transpose(g) transpose(g.*[gr gr])];



save('/Volumes/REBOOTII/RBTII/PROCESSED/NBS/design_rest_BetweenGroup.mat', 'design')
save('/Volumes/REBOOTII/RBTII/PROCESSED/NBS/design_rest_WithinSubject.mat', 'designWS')


% Design: [transpose(repmat(0,4,1)), 0,1]
% DesignSW: [transpose(repmat(0,57,1)), 0,1]
        