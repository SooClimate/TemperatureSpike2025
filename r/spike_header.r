library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)
library(data.table)
# library(matrixStats)
library(ggthemes)
library(reshape2)
library(tseries)
library(urca)
library(forecast)
library(pracma)
# library(ggpubr)
library(cowplot)
library(ggnewscale)
library(xtable)

avoid_color_deficient_vision <- F

source("./r/rdef.r")
source("../2018-cmip5/src/adjust.r")

dat_dir <- "./data/"
scm_dt <- 6

nsample <- 1000

h_name <- Sys.info()["sysname"][[1]]
if (grepl("Linux", h_name, fixed=TRUE)) {
	work_dir <- "/S/data00/G6008/d0997/"
	scm_dir <- paste0(work_dir, "model/AD-DICE2013R/scm4opt_v33/")
	source(paste0(scm_dir,"trunk/rshr/rdef.r"))
	source(paste0(scm_dir,"trunk/r/ems_interp_def.r"))
    period_temp_dir <- paste0(work_dir,"contrib/sum_period/")
	slow_dat <- paste0(work_dir,"contrib/data-spike/")
	final_dat <- paste0(work_dir,"contrib/data-final/")
	exp_sum <- paste0(work_dir,"contrib/sum_period/")
	par_cores_no <- 60
    nthread <- 30
}else if(grepl("Darwin", h_name, fixed=TRUE)){
	work_dir <- "~/work/"
	scm_dir <- paste0(work_dir, "model/AD-DICE2013R/scm4opt_v33/")
	source(paste0(scm_dir,"trunk/rshr/rdef.r"))
	source(paste0(scm_dir,"trunk/r/ems_interp_def.r"))
    # period_temp_dir <- "/Volumes/Data/contrib/sum_period/"
	# slow_dat <- "/Volumes/Data/contrib/data-spike/"
	# exp_sum <- "/Volumes/Data/contrib/sum_period/"
    period_temp_dir <- paste0(work_dir,"contrib/sum_period/")
	slow_dat <- paste0("./data-spike/")
	final_dat <- paste0("./data-final/")
	exp_sum <- paste0("./contrib/sum_period/")
	par_cores_no <- 4
    nthread <- 6
}

source("./r/shr_cmm_def.r")
source("./r/shr_cal_decade.r")
source("./r/shr_get_period.r")
source("./r/shr_get_record.r")
# source("./r/shr_get_cmip6.r")

source("./r/shr_cmm_cl_def.r")

options(device=png)
options(width = 200)

fig_dir <- "./figs-spike/"