library(ncdf4)
library(ncdf4.helpers)

# extract the cti data from nc file and save it as csv file
# src: the source nc file
# save the csv file in the same folder as the nc file
sav_cti <- function(src){
    if(file.exists(src)){
        cti_nc <- src

		# open the nc file
        ncin <- nc_open(cti_nc)

		# extract the data
        sst <- data.frame(ncvar_get(ncin,"sst"))

		# extract the time series
        nc_date <- nc.get.time.series(f = ncin, time.dim.name = "time")

        dst_mtx <- data.frame(year=as.character(nc_date),sst)
        rownames(dst_mtx) <- NULL
        colnames(dst_mtx) <- c("year","value")        

		# save the csv file
        sav_csv <- gsub(".nc",".csv",src,fixed=T)
        write.csv(dst_mtx, file = sav_csv, row.names=F)
        print(paste0(sav_csv, " saved."))   
    }
}

# extract the cti data from nc file and save it as csv file
sav_cti("./data/CTI/cti1854.nc")