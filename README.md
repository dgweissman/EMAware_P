# EMAware_P
Data and code for Weissman et al. (Under Review) Low Emotional Awareness as a Transdiagnostic Mechanism Underlying Psychopathology in Adolescence. https://doi.org/10.31234/osf.io/49a6h
Filenames with "ft" refer to Study 1.
Filenames with "MT" or refer to Study 2.

Study1_Prep_psychopathology_data.R: 
R Script used to select higher of the parent or child psychopthology measures and recode symptom counts into decile scores for study 1

ftp10.csv:
Psychopathology decile scores used by Mplus for study 1

ft_pfactor_pt.inp, ft_pfactor_pt.out:
MPlus input and output files of bifactor model, including PTSD symptoms for study 1

ft_pfactor_nopt.inp, ft_pfactor_nopt.out:
MPlus input and output files of bifactor model, excluding PTSD symptoms for study 1

ft_2factor_pt.inp, ft_2factor_pt.out:
MPlus input and output files of correlated factors model, including PTSD symptoms for study 1

ft_2factor_nopt.inp, ft_2factor_nopt.out:
MPlus input and output files of correlated factors model, excluding PTSD symptoms for study 1

ftp_nopt.txt:
Factor scores from ft_pfactor_nopt

Study1_analyses.R:
Study 1 analysis script

MT_P_bi_com_noTHD.out:
MPlus output file of bifactor model for study 2

MT_P_corr_com_noTHD.out:
MPlus output file of correlated factors model for study 2

Study2_analyses.R:
Study 2 analysis script
