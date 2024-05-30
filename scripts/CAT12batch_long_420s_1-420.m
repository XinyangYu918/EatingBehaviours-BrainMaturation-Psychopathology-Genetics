%-----------------------------------------------------------------------
% Job saved on 03-Jan-2022 13:18:48 by cfg_util (rev $Rev: 7345 $)
% spm SPM - SPM12 (7771)
% cfg_basicio BasicIO - Unknown
%-----------------------------------------------------------------------
%%
matlabbatch{1}.spm.tools.cat.long.datalong.subjects = {
                                                       {
'/FU2_FU3/original/000000112288_fu2.nii,1'
'/FU2_FU3/original/000000112288_fu3.nii.1'
}
...

                                                       }';
%%
matlabbatch{1}.spm.tools.cat.long.longmodel = 2;
matlabbatch{1}.spm.tools.cat.long.enablepriors = 1;
matlabbatch{1}.spm.tools.cat.long.bstr = 0;
matlabbatch{1}.spm.tools.cat.long.nproc = 30;
matlabbatch{1}.spm.tools.cat.long.opts.tpm = {'/softwares/spm521/tpm/TPM.nii'};
matlabbatch{1}.spm.tools.cat.long.opts.affreg = 'mni';
matlabbatch{1}.spm.tools.cat.long.opts.biasstr = 0.5;
matlabbatch{1}.spm.tools.cat.long.opts.accstr = 0.5;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.restypes.optimal = [1 0.3];
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.setCOM = 1;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.APP = 1070;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.affmod = 0;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.NCstr = -Inf;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.spm_kamap = 0;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.LASstr = 0.5;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.LASmyostr = 0;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.gcutstr = 2;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.cleanupstr = 0.5;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.BVCstr = 0.5;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.WMHC = 2;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.SLC = 0;
matlabbatch{1}.spm.tools.cat.long.extopts.segmentation.mrf = 1;
matlabbatch{1}.spm.tools.cat.long.extopts.registration.regmethod.shooting.shootingtpm = {'/softwares/spm521/toolbox/cat12/templates_MNI152NLin2009cAsym/Template_0_GS.nii'};
matlabbatch{1}.spm.tools.cat.long.extopts.registration.regmethod.shooting.regstr = 0.5;
matlabbatch{1}.spm.tools.cat.long.extopts.registration.vox = 1.5;
matlabbatch{1}.spm.tools.cat.long.extopts.registration.bb = 12;
matlabbatch{1}.spm.tools.cat.long.extopts.surface.pbtres = 0.5;
matlabbatch{1}.spm.tools.cat.long.extopts.surface.pbtmethod = 'pbt2x';
matlabbatch{1}.spm.tools.cat.long.extopts.surface.SRP = 22;
matlabbatch{1}.spm.tools.cat.long.extopts.surface.reduce_mesh = 1;
matlabbatch{1}.spm.tools.cat.long.extopts.surface.vdist = 2;
matlabbatch{1}.spm.tools.cat.long.extopts.surface.scale_cortex = 0.7;
matlabbatch{1}.spm.tools.cat.long.extopts.surface.add_parahipp = 0.1;
matlabbatch{1}.spm.tools.cat.long.extopts.surface.close_parahipp = 1;
matlabbatch{1}.spm.tools.cat.long.extopts.admin.experimental = 0;
matlabbatch{1}.spm.tools.cat.long.extopts.admin.new_release = 0;
matlabbatch{1}.spm.tools.cat.long.extopts.admin.lazy = 0;
matlabbatch{1}.spm.tools.cat.long.extopts.admin.ignoreErrors = 1;
matlabbatch{1}.spm.tools.cat.long.extopts.admin.verb = 2;
matlabbatch{1}.spm.tools.cat.long.extopts.admin.print = 2;
matlabbatch{1}.spm.tools.cat.long.output.BIDS.BIDSno = 1;
matlabbatch{1}.spm.tools.cat.long.output.surface = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.neuromorphometrics = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.lpba40 = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.cobra = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.hammers = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.thalamus = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.ibsr = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.aal3 = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.mori = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.anatomy3 = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.julichbrain = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.Schaefer2018_100Parcels_17Networks_order = 0;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.Schaefer2018_200Parcels_17Networks_order = 0;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.Schaefer2018_400Parcels_17Networks_order = 1;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.Schaefer2018_600Parcels_17Networks_order = 0;
matlabbatch{1}.spm.tools.cat.long.ROImenu.atlases.ownatlas = {''};
matlabbatch{1}.spm.tools.cat.long.longTPM = 1;
matlabbatch{1}.spm.tools.cat.long.modulate = 1;
matlabbatch{1}.spm.tools.cat.long.dartel = 0;
matlabbatch{1}.spm.tools.cat.long.delete_temp = 1;
