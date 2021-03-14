## ---------------------------
##
## Script name: Script to create DFs with Community Membership Information
##
## Purpose of script:
##
## Author: Simon Krukowski
##
## Date Created: 2021-03-14
##
## Copyright (c) Simon Krukowski, 2021
## Email: simon.krukowski@stud.uni-due.de
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC

setwd("/Users/simonkrukowski/Documents/ANS/data")      # Simon's working directory (mac)
#setwd("C:/Users/tim/Google Drive/")    #  working directory (PC)

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## load up the packages we will need:  

require(igraph)
require(linkcomm)
require(dplyr)

## ---------------------------

## load up our required functions into memory

#Functions needed for import of Networks and julia-generated DFs
readGraphs <- function(path){
  g <- graph_from_edgelist(as.matrix(read.csv(paste0("./networks/",path))),directed = F)
  return(g)
}
vertAttr <- function(index){
  graphs[[index]] <- set_vertex_attr(graphs[[index]],name = "name",value = V(graphs[[index]]))
}
getNames <- function(index){
  return(as.character(V(graphs[[index]])))
}
importNetworkDfs <- function(filename){
  networkdf <- read.csv(paste0("./dfs/",filename))
}

#Functions needed for generation of ego-networks and deletion of egos
createEgoGraphs <- function(index){
  return(
    ego_graphs <- make_ego_graph(graphs[[index]],order = 1)
  )
}
deleteEgos_inside <- function(listobject){
  for(i in 1:1000){
    listobject[[i]] <- delete_vertices(listobject[[i]],names[[i]])
  }
  return(listobject)
}

#Functions needed to create Louvain Communities
calculateModularitys_inside <- function(object){
  out <- tryCatch(
    {
      message("Es wird versucht..")
      suppressWarnings(cluster_louvain(object))
    },
    error=function(cond) {
      message("Dieses Netzwerk ist zu klein")
      return(NA)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={
    }
  )    
  return(out)
}
calculateModularitys <- function(list){
  list <- lapply(list,calculateModularitys_inside)
}

#Functions needed to extract number of communities out of community objects
getNumClustersModularity_inside <- function(listobject){
  if(any(is.na(listobject))){
    return(NA)
  }else{
    clusters <- nrow(as.data.frame(sizes(listobject)) %>%
                       filter(Freq > 1))
    return(clusters)
  }
  
}
getNumClustersModularity <- function(listobject){
  list <- lapply(listobject,getNumClustersModularity_inside)
}
getVectors <- function(listobject){
  return(as.vector(unlist(listobject)))
}
addNames <- function(listobject){
  return(
    cbind(names,listobject)
  )
}
convertToDFs <- function(listobject){
  return(
    as.data.frame(listobject)
  )
}
convertType <- function(listobject){
  listobject$node_name <- as.numeric(listobject$node_name)
  listobject$communities_louvain <- as.numeric(listobject$communities_louvain)
  return(listobject)
}

#Merge julia-generated DFs with community sizes
mergeDFs <- function(index){
  data <- left_join(network_dfs[[index]],vectors[[index]],by="node_name",copy=T)
  return(data)
}

#Export DFs
exportDFs <- function(index){
  write.csv(dfs_final[[index]],paste0("./dfs_with_modularity/network_",index,".csv"),row.names = F)
}


names(dfs_final[1])
## The magic starts here---------------------------

#Create filelists for import
graphs_list <- list.files("./networks/")
dfileslist <- list.files(paste0("./dfs/"))

#Import networks and julia-generated dfs
graphs <- lapply(graphs_list,readGraphs)
network_dfs <- lapply(dfileslist,importNetworkDfs)

#Make sure the networks are undirected
lapply(1:8,function(index){return(is.directed(graphs[[index]]))})

#Add name attribute to networks
graphs <- lapply(1:8,vertAttr)

#Save names in vector for later access (its simple, all nodes are named incrementally from 1:1000)    
names <- c(as.character(1:1000))

#Create ego-graphs
ego_graphs <- lapply(1:8,createEgoGraphs)

#Delete egos
ego_graphs_no_ego <- lapply(ego_graphs,deleteEgos_inside)

#Calcualte modularities (results in list of lists of iGraph community objects)
modularities <- lapply(ego_graphs_no_ego,calculateModularitys)

#Extract number of communities out of iGraph community objects (note: trivial communities (size smaller or equal 1) will be dismissed)
numClustersModularity <- lapply(modularities,getNumClustersModularity) #this results in community sizes, but names are still missing

#Make numClustersModularity to vectors
vectors <- lapply(numClustersModularity,getVectors)

#Add node names as separate column, taken from the names vector
vectors <- lapply(vectors,addNames)

#Convert to DFs
vectors <- lapply(vectors,convertToDFs)

#Change column names 
vectors <- lapply(vectors,setNames, nm = c("node_name","communities_louvain"))

#Convert type of columns, for later merging
vectors <- lapply(vectors,convertType)

#Merge julia-generated DFs with community sizes 
dfs_final <- lapply(1:8,mergeDFs)

#Name list objects for export later on
names(dfs_final) <- dfileslist

#Export new DFs
lapply(1:8,exportDFs)
