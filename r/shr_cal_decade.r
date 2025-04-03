
# calculate the decadal trend of a matrix
# src_mtx: source matrix
# start_yr: start year
# end_yr: end year
# return: decadal trend
get_decadal_inc <- function(src_mtx, start_yr, end_yr){
    # extract the sub matrix
    sub_mtx <- subset(src_mtx, year>=start_yr&year<(end_yr+1))

    nlen <- nrow(sub_mtx)

    # perform linear regression
    lin_fit <- lm(value ~ year, data=sub_mtx)
    decadal_inc <- coef(lin_fit)[["year"]] * 10

    # return the result
    return(decadal_inc)
}

# calculate the rolling trend of a vector (annual), used in parallel processing
# kk need to be defined as a global variable
# x: source vector
get_vt_decadal_roll <- function(x){
    dst_mtx <- NULL

    max_yr <- length(x)
    # define the data matrix
    src_mtx <- data.frame(year=c(1:max_yr),value=x)
    for(start_yr in c(1:max_yr)){
        end_yr <-  start_yr + kk - 1
        if(end_yr > max_yr){
            break
        }

        # extract the sub matrix
        sub_mtx <- subset(src_mtx, year>=start_yr&year<(end_yr+1))

        # perform linear regression
        nlen <- nrow(sub_mtx)
        lin_fit <- lm(value ~ year, data=sub_mtx)
        inc <- coef(lin_fit)[["year"]] * 10
        
        inc_vt <- c(start_yr+kk%/%2,inc)

        # append the result
        if(is.null(dst_mtx)){
            dst_mtx <- inc_vt
        }else{
            dst_mtx <- rbind(dst_mtx,inc_vt)
        }
        
    }

    # define the result
    dst_mtx <- data.frame(dst_mtx)
    colnames(dst_mtx) <- c("year", "value")

    # return the result
    return(dst_mtx$value)
}

# calculate the rolling trend of a vector (monthly), used in parallel processing
# kk need to be defined as a global variable
# x: source vector
get_vt_decadal_roll_monthly <- function(x){
    dst_mtx <- NULL

    months_in_year <- 12
    max_yr <- length(x)

    # define the data matrix
    src_mtx <- data.frame(year=c(1:max_yr),value=x)
    for(start_yr in c(1:max_yr)){
        end_yr <-  start_yr + kk * months_in_year - 1
        if(end_yr > max_yr){
            break
        }
        
        # extract the sub matrix
        sub_mtx <- subset(src_mtx, year>=start_yr&year<(end_yr+1))
# print(start_yr)
# print(sub_mtx)

        # perform linear regression
        nlen <- nrow(sub_mtx)
        lin_fit <- lm(value ~ year, data=sub_mtx)
        inc <- coef(lin_fit)[["year"]] * 10 * months_in_year
        
        # append the result
        inc_vt <- c(start_yr+(kk*months_in_year)%/%2,inc)
        if(is.null(dst_mtx)){
            dst_mtx <- inc_vt
        }else{
            dst_mtx <- rbind(dst_mtx,inc_vt)
        }
        
    }

    # define the result
    dst_mtx <- data.frame(dst_mtx)
    colnames(dst_mtx) <- c("year", "value")

    # return the result
    return(dst_mtx$value)
}

# get the rolling trend of a matrix
# var_mtx: source matrix
# start_yr: start year
# k: rolling window size
get_dec_par_roll <- function(var_mtx, start_yr=1855,k=11){
    yr <- as.numeric(as.character(colnames(var_mtx)))

    yr <- yr[yr>=start_yr&yr<2024+1]
    var_mtx <- subset(var_mtx, select=c(as.character(yr)))

    # use parallel processing to calculate the rolling trend
    clust <- makeCluster(par_cores_no)
    kk <- k
    clusterExport(clust, "kk",envir = environment())
    dec_mtx <- parApply(clust, var_mtx, 1, get_vt_decadal_roll_monthly)
    stopCluster(clust)

    # define the result year
    yr <- as.numeric(as.character(colnames(var_mtx)))
    yr <- yr[-c(1:trunc(k/2))]
    nrw <- nrow(dec_mtx)
    yr <- yr[1:nrw]

    dst_mtx <- t(dec_mtx)
    colnames(dst_mtx) <- yr

    # get the mean and sd of the result, using parallel processing
    qnt_mtx <- get_qnt_par_mtx(dst_mtx, par_cores_no)

    # return the result
    return(qnt_mtx)
}

# get the rolling trend of a matrix (annual)
# var_mtx: source matrix
# start_yr: start year
# k: rolling window size
get_dec_par_annual_roll <- function(var_mtx, start_yr=1855,k=11){
    yr <- as.numeric(as.character(colnames(var_mtx)))

    yr <- yr[yr>=start_yr&yr<2024+1]
    var_mtx <- subset(var_mtx, select=c(as.character(yr)))

    # use parallel processing to calculate the rolling trend
    clust <- makeCluster(par_cores_no)
    kk <- k
    clusterExport(clust, "kk",envir = environment())
    dec_mtx <- parApply(clust, var_mtx, 1, get_vt_decadal_roll)
    stopCluster(clust)

    # define the result year
    yr <- as.numeric(as.character(colnames(var_mtx)))
    yr <- yr[-c(1:trunc(k/2))]
    nrw <- nrow(dec_mtx)
    yr <- yr[1:nrw]

    dst_mtx <- t(dec_mtx)
    colnames(dst_mtx) <- yr

    # get the mean and sd of the result, using parallel processing
    qnt_mtx <- get_qnt_par_mtx(dst_mtx, par_cores_no)

    # return the result
    return(qnt_mtx)
}

# get the 11-year rolling trend of a variable
# fol: folder name
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src: variable source
# obs_src: observation source
# k: rolling window size
# start_yr: start year
# exp_tag: the regression tag
get_dec_roll <- function(fol, reg, hsrc, psrc, sce, tag, src, obs_src="HadCRUTv5.0.2",k=11, start_yr, exp_tag="AntVolSolMeiAmo"){

    # define the csv file name
    src_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src, "_", exp_tag,"_roll_", k, ".csv")
    
    dst_mtx <- NULL
    # print(src_csv)
    if(file.exists(src_csv)){
        # print(src_csv)
        # load data frame from csv file
        dst_mtx <- data.frame(read.table(src_csv, header = TRUE, check.names=F, sep=","), check.names=F)
        
    }    
    # print(head(qnt_mtx))

    # return the result
    return(dst_mtx)
}

# get the k-year rolling trend of a variable from a matrix
# src_mtx: source matrix
# k: rolling window size
# return: rolling trend matrix
get_decadal_roll <- function(src_mtx, k){
    dst_mtx <- NULL
    # print(head(src_mtx))
    max_yr <- max(src_mtx$year)
    for(start_yr in src_mtx$year){
        end_yr <-  start_yr + k - 1
        if(end_yr > max_yr){
            break
        }
        # calculate the decadal trend of a matrix
        inc <- get_decadal_inc(src_mtx, start_yr, end_yr)
        inc_vt <- c(start_yr+k%/%2,inc)

        # append the result
        if(is.null(dst_mtx)){
            dst_mtx <- inc_vt
        }else{
            dst_mtx <- rbind(dst_mtx,inc_vt)
        }
        
    }
    colnames(dst_mtx) <- c("year", "value")

    # return the result
    return(data.frame(dst_mtx))
}

# get the decadal trend of a variable from a matrix with ensemble members
# src_mtx: source matrix
# start_yr: start year
# end_yr: end year
# return: decadal trend
get_decadal_inc_ens <- function(src_mtx, start_yr, end_yr){
    ncl <- ncol(src_mtx)
    # print(head(ncl))
    inc_vt <- vector()
    for(i in c(2:ncl)){
        sub_mtx <- subset(src_mtx, select=c(1,i))
        colnames(sub_mtx) <- c("year", "value")

        # calculate the decadal trend of a matrix
        decadal_inc <- get_decadal_inc(sub_mtx,start_yr,end_yr)

        # append the result
        inc_vt <- c(inc_vt, decadal_inc)
    }

    # return the result
    return(as.vector(inc_vt))
}

# calculate the decadal trend of a vector, used in parallel processing
# x: source vector
# year_index: year index, defined as a global variable
get_decadal_inc_par <- function(x){
    lin_fit <- lm(x ~ year_index)
    decadal_inc <- coef(lin_fit)[["year_index"]] * 10
    return(decadal_inc)
}

# calculate the decadal trend of a matrix with ensemble members, use parallel processing
# src_mtx: source matrix
# start_yr: start year
# end_yr: end year
# return: decadal trend
get_decadal_inc_par_ens <- function(src_mtx, start_yr, end_yr){
    clust <- makeCluster(par_cores_no)
    sub_mtx <- subset(src_mtx, year>=start_yr&year<(end_yr+1))
    year_index <- sub_mtx$year
    ncl <- ncol(sub_mtx)
    # define the global variable
    clusterExport(clust, "year_index",envir = environment())

    # use parallel processing to calculate the decadal trend
    inc_vt <- parApply(clust, sub_mtx[,c(2:ncl)], 2, get_decadal_inc_par) 
    stopCluster(clust)

    # return the result
    return(unname(unlist(inc_vt)))
}

# calculate the linear increase of a matrix
# src_mtx: source matrix
# start_yr: start year
# end_yr: end year
# return: the linear increase of a matrix
get_linear_inc <- function(src_mtx, start_yr, end_yr){
    sub_mtx <- subset(src_mtx, year>=start_yr&year<(end_yr+1))
    # print(sub_mtx)
    nlen <- nrow(sub_mtx)

    # calculate the linear trend
    lin_fit <- lm(value ~ year, data=sub_mtx)
# print(data.frame(x=sub_mtx$year))
    # create the data frame for the prediction
    lin_mtx <- data.frame(year=sub_mtx$year, value=predict(lin_fit, x=sub_mtx$year))

    # return the result
    return(lin_mtx[lin_mtx$year == end_yr,]$value-lin_mtx[lin_mtx$year == start_yr,]$value)
}

# get the rolling trend of a variable
# var_mtx: source matrix
# start_yr: start year
# k: rolling window size
# return: rolling trend matrix
get_roll_trend <- function(var_mtx, start_yr=1850, k=11){

	src_all <- unique(var_mtx$source)
	dec_mtx <- NULL

    # loop over all variables
	for (src in src_all){
		ems_mtx <- subset(var_mtx, source==src)
		ems_mtx <- subset(ems_mtx, select=c("year","value"))
		ems_mtx <- ems_mtx[order(ems_mtx$year),]

		src_mtx <- t(ems_mtx)
		colnames(src_mtx) <- ems_mtx$year
		src_mtx <- data.frame(t(src_mtx[-1,]), check.names = FALSE)

        # calculate the rolling trend
		dec_vt <- get_dec_par_annual_roll(src_mtx, start_yr=1850,k=5)
		dec_vt <- data.frame(variable = c(src), dec_vt)

        # append the result
		if(is.null(dec_mtx)){
			dec_mtx <- dec_vt
		}else{
			dec_mtx <- rbind(dec_mtx, dec_vt)
		}
	}

    # return the result
	return(dec_mtx)
}