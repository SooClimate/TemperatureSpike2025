library(stringr)
library(grid)
library(data.table)

source("../2018-cmip5/src/adjust.r")

# define the global variables
scm_dt <- 6
scm_dir <- "../../../../model/AD-DICE2013R/scm4opt_v33/"

# summarize the GHG concentration to be used in the SCM4OPT model
exp_ghg_mm_conc <- function(src_csv, dst_file, var, skp){
    dst_mtx <- NULL

    if(file.exists(src_csv)){
		# load data frame from csv file
        csv_mtx <- data.frame(fread(src_csv, header = TRUE, sep = ",", check.names = FALSE, skip=skp,blank.lines.skip=TRUE), check.names = FALSE)

		# calculate the bimonthly average
        csv_mtx <- data.frame(year=csv_mtx$year+round(((csv_mtx$month-1)%/%2)/scm_dt,3), value=csv_mtx[,c(var)])  
		# csv_mtx <- csv_mtx[,c("year",var)]
		# colnames(csv_mtx) <- c("year", "value")
		# print(csv_mtx)
		dst_mtx <- aggregate(csv_mtx$value, list(csv_mtx$year), FUN=mean)
		colnames(dst_mtx) <- c("year", "value")

		# project the data based on the last 5 years
		# although the data is projected to 2026, the data by 2024 is used for the analysis
		nrw <- nrow(dst_mtx)
		last_month <- as.numeric(dst_mtx$year[nrw])
		if(last_month>=2023){
			fit_mtx <- subset(dst_mtx,year>=last_month-5)
			fit_mtx$year <- as.numeric(as.character(fit_mtx$year))
			fit <- lm(value ~ year,data=fit_mtx)
			pred_mtx <- as.data.frame(x=fit_mtx$year+3)
			colnames(pred_mtx) <- c("year")

			proj_mtx <- data.frame(year=fit_mtx$year+3, value=predict(fit, pred_mtx))

			proj_mtx <- subset(proj_mtx, year>last_month&year<=2026)
			dst_mtx <- rbind(dst_mtx,proj_mtx)

		}


		# save the bimonthly mean data, used in the SCM4OPT model
		dst_mtx$year <- as.character(dst_mtx$year)
		sav_csv <- paste0(scm_dir, dst_file)
		write.table(dst_mtx, file = sav_csv, row.names=F, col.names=F, sep=",")
		print(paste0(sav_csv, " saved."))   
    }
}

# summarize the CO2 concentration to be used in the SCM4OPT model
exp_ghg_mm_conc("./data/sum/CO2/co2_mm_mlo.csv", "trunk/dat/NOAA/noaa_co2_mm_mlo.csv", "deseasonalized", 40)

# summarize the CH4 concentrations to be used in the SCM4OPT model
exp_ghg_mm_conc("./data/sum/CH4/ch4_mm_gl.csv", "trunk/dat/NOAA/noaa_ch4_mm_gl.csv", "average", 45)

# summarize the N2O concentrations to be used in the SCM4OPT model
exp_ghg_mm_conc("./data/sum/N2O/n2o_mm_gl.csv", "trunk/dat/NOAA/noaa_n2o_mm_gl.csv", "average", 45)

# summarize the SF6 concentrations to be used in the SCM4OPT model
exp_ghg_mm_conc("./data/sum/SF6/sf6_mm_gl.csv", "trunk/dat/NOAA/noaa_sf6_mm_gl.csv", "average", 45)