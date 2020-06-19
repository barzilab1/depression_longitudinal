
#mapping
bblid_datasetid_mapping <- read_csv("Data/CNB_ITEMWISE/go_bblid_datasetid_mapping_n9492.csv")
#get relevant ids
bblid_datasetid_mapping = merge(bblid_datasetid_mapping, Y_bucket[,"bblid",drop = F])

#AGE DIFFERENTIATION 
adt36ai <- read_csv("Data/CNB_ITEMWISE/adt36ai_9492_sample.csv")
adt36bi <- read_csv("Data/CNB_ITEMWISE/adt36bi_9492_sample.csv")

adt36ai = merge(bblid_datasetid_mapping, adt36ai[grep(pattern = "V/*",adt36ai[,"VC"][[1]], ignore.case = T),])
adt36bi = merge(bblid_datasetid_mapping, adt36bi)


#FACE MEMORY
CPF_A_I <- read_csv("Data/CNB_ITEMWISE/CPF_A_I.9492.csv")
CPF_B_I <- read_csv("Data/CNB_ITEMWISE/CPF_B_I.9492.csv")
CPFD_A_I <- read_csv("Data/CNB_ITEMWISE/CPFD_A_I.9492.csv")

CPF_A_I = merge(bblid_datasetid_mapping, CPF_A_I[grep(pattern = "V/*",CPF_A_I[,"VC"][[1]], ignore.case = T),])
CPF_B_I = merge(bblid_datasetid_mapping, CPF_B_I[grep(pattern = "V/*",CPF_B_I[,"VC"][[1]], ignore.case = T),])
CPFD_A_I = merge(bblid_datasetid_mapping, CPFD_A_I)

#EMOTION RECOGNITION
ER40_I <- read_csv("Data/CNB_ITEMWISE/ER40_I.9492.csv")
ER40C_I <- read_csv("Data/CNB_ITEMWISE/ER40C_I.9492.csv")
ER40D_I <- read_csv("Data/CNB_ITEMWISE/ER40D_I.9492.csv")
ER40E_I <- read_csv("Data/CNB_ITEMWISE/ER40E_I.9492.csv")

ER40_I  = merge(bblid_datasetid_mapping, ER40_I[grep(pattern = "V/*",ER40_I[,"VC"][[1]], ignore.case = T),]) 
ER40C_I = merge(bblid_datasetid_mapping, ER40C_I)  
ER40D_I = merge(bblid_datasetid_mapping, ER40D_I)  
ER40E_I = merge(bblid_datasetid_mapping, ER40E_I[grep(pattern = "V/*",ER40E_I[,"VC"][[1]], ignore.case = T),])  

#ABSTRACTION & MENTAL FLEXIBILITY
pcet_spcet_itemwise <- read_csv("Data/CNB_ITEMWISE/go_9492_cnb_pcet_spcet_itemwise_20140603.csv")
pcet_itemwise <- read_csv("Data/CNB_ITEMWISE/GO_spcet_pcet_itemwise_n9492.csv")

pcet_spcet_itemwise = merge(bblid_datasetid_mapping, pcet_spcet_itemwise, by.x = "bblid", by.y = "test_sessions.bblid" ) 
pcet_itemwise = merge(bblid_datasetid_mapping, pcet_itemwise, by.x = "bblid", by.y = "test_sessions.bblid" ) 
#all.equal(pcet_spcet_itemwise,pcet_itemwise) #not equal

#LANGUAGE REASONING 
kspvrt_cd <- read_csv("Data/CNB_ITEMWISE/kspvrt_cd_i_9492.csv")

kspvrt_cd = merge(bblid_datasetid_mapping, kspvrt_cd, by.x = "bblid", by.y = "test_sessions.bblid",  ) 

#EMOTION DISCRIMINATION
medf36bi <- read_csv("Data/CNB_ITEMWISE/medf36bi_9492_sample.csv")
medf60i <- read_csv("Data/CNB_ITEMWISE/medf60i_9492_sample.csv")

medf36bi = merge(bblid_datasetid_mapping, medf36bi[grep(pattern = "V/*",medf36bi[,"VC"][[1]], ignore.case = T),])
medf60i = merge(bblid_datasetid_mapping, medf60i)

#Motor Praxis 
MPRACT_I <- read_csv("Data/CNB_ITEMWISE/MPRACT_I.9492.csv")

MPRACT_I = merge(bblid_datasetid_mapping, MPRACT_I)

#NONVERBAL REASONING
pmat18ai <- read_csv("Data/CNB_ITEMWISE/pmat18ai_9492_sample.csv")
pmat18bi <- read_csv("Data/CNB_ITEMWISE/pmat18bi_9492_sample.csv")
pmat24ai <- read_csv("Data/CNB_ITEMWISE/pmat24ai_9492_sample.csv")
pmat24bi <- read_csv("Data/CNB_ITEMWISE/pmat24bi_9492_sample.csv")

pmat18ai = merge(bblid_datasetid_mapping, pmat18ai)
pmat18bi = merge(bblid_datasetid_mapping, pmat18bi)
pmat24ai = merge(bblid_datasetid_mapping, pmat24ai)
pmat24bi = merge(bblid_datasetid_mapping, pmat24bi[grep(pattern = "V/*",pmat24bi[,"VC"][[1]], ignore.case = T),])


#WORKING MEMORY
SLNB2_I <- read_csv("Data/CNB_ITEMWISE/SLNB2_I.9492.csv")

SLNB2_I = merge(bblid_datasetid_mapping, SLNB2_I[grep(pattern = "V/*",SLNB2_I[,"VC"][[1]], ignore.case = T),])

#SPATIAL MEMORY
svolt_i <- read_csv("Data/CNB_ITEMWISE/svolt_i_9492.csv")

svolt_i = merge(bblid_datasetid_mapping, svolt_i, by.x = "bblid", by.y = "test_sessions.bblid")

#SPATIAL ABILITY
v_s_plot_i <- read_csv("Data/CNB_ITEMWISE/v_s_plot_i_9492.csv")

v_s_plot_i = merge(bblid_datasetid_mapping, v_s_plot_i, by.x = "bblid", by.y = "test_sessions.bblid")

#ATTENTION
SPCPTNL_i <- read_csv("Data/CNB_ITEMWISE/SPCPTNL_i.9492.csv")

SPCPTNL_i = merge(bblid_datasetid_mapping, SPCPTNL_i[grep(pattern = "V/*",SPCPTNL_i[,"VC"][[1]], ignore.case = T),])

#Verbal Memory
cpw_and_cpwd_i <- read_csv("Data/CNB_ITEMWISE/cpw_and_cpwd_i_9492.csv")

cpw_and_cpwd_i = merge(bblid_datasetid_mapping, cpw_and_cpwd_i, by.x = "bblid", by.y = "test_sessions.bblid")



