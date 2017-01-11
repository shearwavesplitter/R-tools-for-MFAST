closeAllConnections()
rm(list=ls())

library(circular)

library(datasets)
#library(animation)
library(plot3D)
library(movMF)
#library(cclust)


#Disable these to use server version
#library(viridis)
library(RColorBrewer)
library(fields)
library(ggplot2)
library(ggmap)

#And enable these
#library(png)
#library(rgl)
#
source("~/R_MFAST/fn_read.R")
source("~/R_MFAST/fn_readlocal.R")
source("~/R_MFAST/fn_readverylocal.R")
source("~/R_MFAST/fn_stde.R")
source("~/R_MFAST/fn_getev.R")
source("~/R_MFAST/fn_cleansumm.R")
source("~/R_MFAST/fn_projmap.R")
source("~/R_MFAST/fn_readraw.R")
source("~/R_MFAST/fn_pathclus.R")
source("~/R_MFAST/reload.R")
source("~/R_MFAST/fn_plot.R")
source("~/R_MFAST/fn_createTESSA.R")