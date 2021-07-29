#====================================================================================================#

if (!require(c("ape", "phytools","tidyverse","tidytree","data.table"))) 
  install.packages(c("ape", "phytools","tidyverse","tidytree","data.table"))

if (!requireNamespace("BiocManager", quietly = TRUE))+
  install.packages("BiocManager"); BiocManager::install("ggtree")

library(ape);library(phytools); library(tidyverse); library(tidytree) 
  library(data.table); library(ggtree)

#====================================================================================================#

#For RStudio only
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#====================================================================================================#


#=================#
# READING DATASET #
#=================#

mimosa_tree_all<- read.tree("mimosa_clean-VASCONCELOS2020.txt")
mimosa_tree_selected <- read.table("mimosa_spp_phd.txt", stringsAsFactors = FALSE, header=T)

#---Pruning outgroups

mimosa_pruned.tree <- drop.tip(mimosa_tree_all, 
                               mimosa_tree_all$tip.label[mimosa_tree_all$tip.label 
                                                     %in% c('Anadenanthera_colubrina',
'Microlobius_foetidus',
'Parapiptadenia_excelsa',
'Piptadenia_adiantoides',
'Piptadenia_buchtienii',
'Piptadenia_gonoacantha',
'Pityrocarpa_moniliformis',
'Pityrocarpa_obliqua',
'Piptadenia_stipulacea',
'Piptadenia_trisperma',
'Stryphnodendron_adstringens',
'Stryphnodendron_obovatum')])


#--- Just checking

plotTree(mimosa_pruned.tree)

ggtree(mimosa_pruned.tree, branch.length='none', layout = 'circular')

#==========================#
# Marking clades according #
#   to Simon et al. 2011   #
#==========================#

#--- Marking clades according to Simon et al. 2011

MRCA(mimosa_pruned.tree, grep("Mimosa_revoluta", mimosa_pruned.tree$tip.label),
     grep("Mimosa_guilandinae", mimosa_pruned.tree$tip.label)) # 748; clade A

MRCA(mimosa_pruned.tree, grep("Mimosa_torresiae", mimosa_pruned.tree$tip.label),
     grep("Mimosa_benthami", mimosa_pruned.tree$tip.label)) # 696; clade B 

MRCA(mimosa_pruned.tree, grep("Mimosa_detinens", mimosa_pruned.tree$tip.label),
     grep("Mimosa_montana_montana",mimosa_pruned.tree$tip.label)) #739; clade C

MRCA(mimosa_pruned.tree, grep("Mimosa_ceratonia_ceratonia", mimosa_pruned.tree$tip.label),
     grep("Mimosa_volubilis", mimosa_pruned.tree$tip.label)) #665; clade D

MRCA(mimosa_pruned.tree, grep("Mimosa_busseana", mimosa_pruned.tree$tip.label),
     grep("Mimosa_volubilis", mimosa_pruned.tree$tip.label))  # 666; clade E 

MRCA(mimosa_pruned.tree, grep("Mimosa_antioquensis_isthmensis", mimosa_pruned.tree$tip.label),
     grep("Mimosa_invisa_invisa", mimosa_pruned.tree$tip.label)) # 675; clade F

MRCA(mimosa_pruned.tree, grep("Mimosa_candollei", mimosa_pruned.tree$tip.label),
     grep("Mimosa_sinaloensis", mimosa_pruned.tree$tip.label)) #657; clade G

MRCA(mimosa_pruned.tree, grep("Mimosa_diplotricha_diplotricha", mimosa_pruned.tree$tip.label),
     grep("Mimosa_crumenarioides", mimosa_pruned.tree$tip.label)) #646; clade H

MRCA(mimosa_pruned.tree, grep("Mimosa_minarum", mimosa_pruned.tree$tip.label),
     grep("Mimosa_cordistipula", mimosa_pruned.tree$tip.label)) #636; clade I 

MRCA(mimosa_pruned.tree, grep("Mimosa_dormiens", mimosa_pruned.tree$tip.label),
     grep("Mimosa_weddelliana", mimosa_pruned.tree$tip.label)) #471; clade J

MRCA(mimosa_pruned.tree, grep("Mimosa_lepidota", mimosa_pruned.tree$tip.label),
     grep("Mimosa_uraguensis", mimosa_pruned.tree$tip.label)) #458; clade K 

MRCA(mimosa_pruned.tree, grep("Mimosa_adenocarpa", mimosa_pruned.tree$tip.label),
     grep("Mimosa_somnians_lasiocarpa", mimosa_pruned.tree$tip.label)) # 501; clade L

MRCA(mimosa_pruned.tree, grep("Mimosa_orthocarpa", mimosa_pruned.tree$tip.label),
     grep("Mimosa_josephina", mimosa_pruned.tree$tip.label)) #503; clade M

MRCA(mimosa_pruned.tree, grep("Mimosa_lewisii", mimosa_pruned.tree$tip.label),
     grep("Mimosa_sericantha", mimosa_pruned.tree$tip.label)) #481; clade N

MRCA(mimosa_pruned.tree, grep("Mimosa_speciosissima", mimosa_pruned.tree$tip.label),
     grep("Mimosa_ulei_grallator", mimosa_pruned.tree$tip.label)) #390; clade O 

MRCA(mimosa_pruned.tree, grep("Mimosa_polydactyla", mimosa_pruned.tree$tip.label),
     grep("Mimosa_affinis", mimosa_pruned.tree$tip.label)) #610; clade P

MRCA(mimosa_pruned.tree, grep("Mimosa_ursina", mimosa_pruned.tree$tip.label),
     grep("Mimosa_honesta", mimosa_pruned.tree$tip.label)) #634; clade Q

MRCA(mimosa_pruned.tree, grep("Mimosa_vestita",mimosa_pruned.tree$tip.label),
     grep("Mimosa_virgula", mimosa_pruned.tree$tip.label)) #615; clade R


MRCA(mimosa_pruned.tree, grep("Mimosa_verecunda", mimosa_pruned.tree$tip.label),
     grep("Mimosa_jacobita", mimosa_pruned.tree$tip.label)) #604; clade S

MRCA(mimosa_pruned.tree, grep("Mimosa_sensitiva", mimosa_pruned.tree$tip.label),
     grep("Mimosa_nuda", mimosa_pruned.tree$tip.label)) #597; clade T 

MRCA(mimosa_pruned.tree, grep("Mimosa_flagellaris", mimosa_pruned.tree$tip.label),
     grep("Mimosa_polycarpa_subandina", mimosa_pruned.tree$tip.label)) # 535; clade U

MRCA(mimosa_pruned.tree, grep("Mimosa_lactiflua", mimosa_pruned.tree$tip.label),
     grep("Mimosa_psilocarpa", mimosa_pruned.tree$tip.label)) #526; clade V

MRCA(mimosa_pruned.tree, grep("Mimosa_woodii", mimosa_pruned.tree$tip.label),
     grep("Mimosa_lamolina", mimosa_pruned.tree$tip.label)) #537; clade W

MRCA(mimosa_pruned.tree, grep("Mimosa_incana", mimosa_pruned.tree$tip.label),
     grep("Mimosa_schleidenii", mimosa_pruned.tree$tip.label)) #521; clade X

tidy_tree <- as_tibble(mimosa_pruned.tree)

tidy_tree$node.labels <- NA
tidy_tree$node.labels[tidy_tree$node == 748] <- "A"
tidy_tree$node.labels[tidy_tree$node == 696] <- "B"
tidy_tree$node.labels[tidy_tree$node == 739] <- "C"
tidy_tree$node.labels[tidy_tree$node == 665] <- "D"
tidy_tree$node.labels[tidy_tree$node == 666] <- "E"
tidy_tree$node.labels[tidy_tree$node == 662] <- "F"
tidy_tree$node.labels[tidy_tree$node == 657] <- "G"
tidy_tree$node.labels[tidy_tree$node == 646] <- "H"
tidy_tree$node.labels[tidy_tree$node == 636] <- "I"
tidy_tree$node.labels[tidy_tree$node == 471] <- "J"
tidy_tree$node.labels[tidy_tree$node == 458] <- "K"
tidy_tree$node.labels[tidy_tree$node == 501] <- "L"
tidy_tree$node.labels[tidy_tree$node == 503] <- "M"
tidy_tree$node.labels[tidy_tree$node == 481] <- "N"
tidy_tree$node.labels[tidy_tree$node == 390] <- "O"
tidy_tree$node.labels[tidy_tree$node == 610] <- "P"
tidy_tree$node.labels[tidy_tree$node == 634] <- "Q"
tidy_tree$node.labels[tidy_tree$node == 617] <- "R"
tidy_tree$node.labels[tidy_tree$node == 604] <- "S"
tidy_tree$node.labels[tidy_tree$node == 597] <- "T"
tidy_tree$node.labels[tidy_tree$node == 535] <- "U"
tidy_tree$node.labels[tidy_tree$node == 526] <- "V"
tidy_tree$node.labels[tidy_tree$node == 537] <- "W"
tidy_tree$node.labels[tidy_tree$node == 521] <- "X"
tree_data <- as.treedata(tidy_tree)

#===============#
# PLOTTING TREE #
#===============#

p<- ggtree(tree_data, layout = "circular", branch.length = "none") + 
  geom_nodepoint(aes(subset = node %in% c(748, 696, 739, 665, 666,  662, 657, 
                                          646, 636, 471,458, 501, 503, 481, 390,  610,
                                          634, 617, 604, 597, 535, 526, 537, 521)), 
  color="black", size=5) +
  geom_text(aes(label = node.labels), hjust = 0.5, vjust = 0.5, color = "white",
            size = 3) 

#--- Coloring selected tips (from my PhD project)

mimosa_tree_selected <- mimosa_tree_selected[ , 1]
mimosa_df <- as.data.frame(matrix(ncol = 1, nrow = length(mimosa_tree_all$tip.label)))
rownames(mimosa_df) <- mimosa_tree_all$tip.label
colnames(mimosa_df) <- "type"
for(i in 1:nrow(mimosa_df)){
  if(rownames(mimosa_df)[i] %in% mimosa_tree_selected){
    mimosa_df$type[i] <- "Selected"
  } else{
    mimosa_df$type[i] <- "Not selected"
  }
}

p <- ggtree(tree_data, layout = "circular", branch.length = "none") 
cols <- c("Selected" = "#2E8B57", "Not selected" = "#D3D3D399") 
gheatmap(p, mimosa_df, offset = -0.7, width = 0.2, 
         colnames = FALSE) +
  scale_fill_manual(values = cols, breaks = c("selected",
                                              "not.selected"), 
                    name = NULL)+
  geom_nodepoint(aes(subset = node %in% c(748, 696, 739, 665, 666,  662, 657, 
                                          646, 636, 471,458, 501, 503, 481, 390,  610,
                                          634, 617, 604, 597, 535, 526, 537, 521)),  
                 color="black", size=5)+
  geom_text(aes(label = node.labels), hjust = 0.5, vjust = 0.5, color = "white",
            size = 3) 
