library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)
library(data.table)
library(reshape2)

# define the global variables for the file paths
agage_dir <- "./data/AGAGE/Agage_gcmd_gcms.data.2025_02_01"
agage_sav <- "./data/AGAGE/Agage2004"
agage_avg <- "./data/AGAGE/Agage2004_mean"
agage_csv <- "Agage_gcmd_gcms.data.2025_02_01.csv"
scm_dir <- "../../../../model/AD-DICE2013R/scm4opt_v33/"

# summary the AGAGE data
exp_agage <- function(){
	ems_mtx <- NULL

	txt_all <- list.files(agage_dir, pattern="*.txt", full.names=T, all.files = T, recursive = T)
	txt_all <- txt_all[grep("monthly_mean", txt_all, fixed=T)]

	# iterate over all the files
	for(fl in txt_all){
		con <- file(fl,open="r")

		# extract the file header
		lines <- readLines(con,n=50)
		close(con)

		skp <- grep("6th column", lines, fixed=T) + 1

		# extract the file information
		stat_ong <- gsub("# station_long_name: ","",lines[grep("station_long_name:", lines, fixed=T)], fixed=T)
		elev_masl <- gsub("# inlet_base_elevation_masl: ","",lines[grep("inlet_base_elevation_masl:", lines, fixed=T)], fixed=T)
		inlet_lati <- gsub("# inlet_latitude: ","",lines[grep("inlet_latitude:", lines, fixed=T)], fixed=T)
		inlet_longi <- gsub("# inlet_longitude: ","",lines[grep("inlet_longitude:", lines, fixed=T)], fixed=T)
		inlet_date <- gsub("# inlet_date: ","",lines[grep("inlet_date:", lines, fixed=T)], fixed=T)
		specs_unit <- gsub("# units: ","",lines[grep("units:", lines, fixed=T)], fixed=T)

		# print(fl)
		# print(stat_ong)
		# print(elev_masl)
		# print(as.numeric(elev_masl))
		# print(as.numeric(inlet_lati))
		# print(as.numeric(inlet_longi))
		# print(inlet_date)
		# print(specs_unit)

		if(length(inlet_date)==0){inlet_date<-NA}
		if(length(specs_unit)==0){specs_unit<-NA}

		# read the data
		fl_mtx <- data.frame(fread(fl, header = F, sep = " ", check.names = FALSE, skip=skp), check.names = FALSE)
		fl_mtx <- na.omit(fl_mtx)
		if(nrow(fl_mtx)==0){next}

		fl_name <- basename(fl)
		fl_ns <- str_split(fl_name, "_")[[1]]
		meas <- fl_ns[1]
		stat <- fl_ns[2]
		if(grepl("h2_pdd", fl_name, fixed=T)){
			emis <- "h2_pdd"
		}else{
			emis <- fl_ns[3]
		}
		
		emis_tag <- c(gsub("-","",emis,fixed=T))

		if(emis_tag=="h1211"){
			emis_tag <- "halon1211"
		}else if(emis_tag=="h1301"){
			emis_tag <- "halon1301"
		}else if(emis_tag=="h2402"){
			emis_tag <- "halon2402"
		}else if(emis_tag=="pfc116"){
			emis_tag <- "c2f6"
		}else if(emis_tag=="pfc218"){
			emis_tag <- "c3f8"
		}else if(emis_tag=="pfc318"){
			emis_tag <- "cc4f8"
		}
		# else if(emis_tag=="pfc14"){
		# 	emis_tag <- "cf4"
		# }

		# create the data frame
		colnames(fl_mtx) <- c("time","year","month","mean","std_dev","numb")

		fl_mtx <- data.frame(inlet_base_elevation_masl=c(as.numeric(gsub("#", "",elev_masl,fixed=T))), 
							inlet_latitude=c(as.numeric(gsub("#", "",inlet_lati,fixed=T))),
							inlet_longitude=c(as.numeric(gsub("#", "",inlet_longi,fixed=T))),
							inlet_date=c(gsub("#", "",inlet_date,fixed=T)),
							units=c(gsub("#", "",specs_unit,fixed=T)),
							measurement=c(gsub("#", "",meas,fixed=T)), 
							station=c(gsub("#", "",stat,fixed=T)), 
							station_long_name=gsub("#", "",stat_ong,fixed=T),
							emission=c(gsub("#", "",emis,fixed=T)),
							emission_tag=c(gsub("#", "",emis_tag,fixed=T)),
							fl_mtx)
		
		# combine the data frame
		if(is.null(ems_mtx)){
			ems_mtx <- fl_mtx
		}else{
			ems_mtx <- rbind(ems_mtx, fl_mtx)
		}
	}	

	# save the data frame
	sav_csv <- paste0(agage_dir, "/",agage_csv)
	write.csv(ems_mtx, file = sav_csv, row.names=F)
	print(paste0(sav_csv, " saved."))   
}

# calculate the monthly and bimonthly mean of the AGAGE data
exp_monthly_mean <- function(){
	# read the data
	fl <- paste0(agage_dir, "/",agage_csv)
	fl_mtx <- data.frame(fread(fl, header = T, sep = ",", check.names = FALSE), check.names = FALSE)
	var_all <- unique(fl_mtx$emission_tag)
	for(var in var_all){
		var_mtx <- subset(fl_mtx, emission_tag==var)
		mea_all <- unique(var_mtx$measurement)

		meas_mean <- NULL
		for(mea in mea_all){
			mea_mtx <- subset(var_mtx, measurement==mea)
			cst_mtx <- reshape2::dcast(mea_mtx, year + month ~ station, value.var = "mean")
			ncl <- ncol(cst_mtx)
			if(ncl>3){
				cst_mtx$mean <- rowMeans(cst_mtx[,c(3:ncl)],na.rm = T)
			}else{
				cst_mtx$mean <- cst_mtx[,c(3)]
			}
			
			# save the data separately
			sav_csv <- paste0(agage_sav, "/", var, "_", mea, ".csv")
			write.csv(cst_mtx, file = sav_csv, row.names=F)
			print(paste0(sav_csv, " saved."))   

			mea_vt <- cst_mtx[,c("year","month","mean")]
			mea_vt <- na.omit(mea_vt)
			colnames(mea_vt)[3] <- mea

			if(is.null(meas_mean)){
				meas_mean <- mea_vt
			}else{
				meas_mean <- merge(meas_mean, mea_vt, by = c("year","month"), all = T)
			}	
		}

		ncl <- ncol(meas_mean)
		var_mean <- data.frame(year=meas_mean$year,month=meas_mean$month)

		if(ncl>3){
			var_mean$value <- rowMeans(meas_mean[,c(3:ncl)],na.rm = T)
		}else{
			var_mean$value <- meas_mean[,c(3)]
		}

		# save the monthly mean data
		sav_csv <- paste0(agage_avg, "/", var, ".csv")
		write.csv(var_mean, file = sav_csv, row.names=F)

		print(">>>>>>>>>>>>>>>>>>>")
		print(paste0(sav_csv, " saved."))   

		# calculate the bimonthly mean
        bim_mtx <- data.frame(year=var_mean$year+round(((var_mean$month-1)%/%2)/scm_dt,3), value=var_mean$value) 
		dst_mtx <- aggregate(bim_mtx$value, list(bim_mtx$year), FUN=mean)
		colnames(dst_mtx) <- c("year","value")
		dst_mtx$year <- as.character(dst_mtx$year)

		# save the bimonthly mean data
		sav_csv <- paste0(scm_dir, "trunk/dat/AGAGE/",var, "_origin.csv")
		write.table(dst_mtx, file = sav_csv, row.names=F, col.names=F, sep=",")

		print(">>>>>>>>>>>>>>>>>>>")
		print(paste0(sav_csv, " saved."))   

		# project the data
		nrw <- nrow(dst_mtx)
		last_month <- as.numeric(dst_mtx$year[nrw])

		# project the data based on the last 5 years
		# although the data is projected to 2026, the data by 2024 is used for the analysis
		if(last_month>=2023){
			fit_mtx <- subset(dst_mtx,year>=last_month-5)
			fit_mtx$year <- as.numeric(as.character(fit_mtx$year))
			fit <- lm(value ~ year,data=fit_mtx)
			pred_mtx <- as.data.frame(x=fit_mtx$year+3)
			colnames(pred_mtx) <- c("year")
# print(predict(fit, pred_mtx))
			proj_mtx <- data.frame(year=fit_mtx$year+3, value=predict(fit, pred_mtx))

			proj_mtx <- subset(proj_mtx, year>last_month&year<=2026)
			dst_mtx <- rbind(dst_mtx,proj_mtx)
		}
		
		# save the bimonthly mean data, used in the SCM4OPT model
		sav_csv <- paste0(scm_dir, "trunk/dat/AGAGE/",var, ".csv")
		write.table(dst_mtx, file = sav_csv, row.names=F, col.names=F, sep=",")

		print(">>>>>>>>>>>>>>>>>>>")
		print(paste0(sav_csv, " saved."))   
	}
}

# summary the AGAGE data
exp_agage()

# calculate the monthly and bimonthly mean
exp_monthly_mean()


