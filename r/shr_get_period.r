# get the resulting climate variable matrix after the regression (monthly)
# fol: folder name
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src: variable source
# obs_src: observation source
# exp_tag: the regression tag
# return: the resulting climate variable after the regression
get_stat_ens_mtx <- function(fol, reg, hsrc, psrc, sce, tag, src, obs_src="HadCRUTv5.0.2", exp_tag="AntVolSolMeiAmo"){
    tem_mtx <- NULL
    
    # get the regression coefficient
    sub_dir <- paste0(obs_src,"_stat_rfc/")
    fit_csv <- paste0(slow_dat,sub_dir, "est_fit_", obs_src,"_", exp_tag, ".csv")   

    # load the coefficient matrix from csv file
    fit_mtx <- data.frame(fread(fit_csv, header = TRUE, check.names=F, sep=","), check.names=F)

    max_col <- 2100 #by 2024.917
    if(!is.null(fit_mtx)){
        if(src%in%c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC", "rfc_LCC", "rfc_BCsnow", 
                    "rfc_O3s", "rfc_O3t", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_cloud", "rfc_H2Os", 
                    "r_ghg", "r_cm6ghg", "r_othanthro", "r_ofgs", "r_fgs", "r_o3", "r_h2o", "r_abd", "r_aer", "r_cld", "r_aer_all",
                    all_fgs_lst, "r_anthro")){
            coef_vt <- fit_mtx$r_anthro

            # get the attributed variable matrix
            rfc_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, src, obs_src)
            if(!is.null(rfc_mtx)){
                rfc_mtx <- rfc_mtx[,c(1:max_col)] 

                # multiply the coefficient vector and the attributed variable matrix
                dst_mtx <- apply(rfc_mtx, 2, FUN=`*`, coef_vt)
                tem_mtx <- dst_mtx
            }
            # print(dim(tem_mtx))

        }else if(src%in%c("r_nat")){
            # get the attributed variable matrix
            sol_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, "rfc_solar", obs_src)[,c(1:max_col)]  
            vol_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, "rfc_volc", obs_src)[,c(1:max_col)] 

            sol_vt <- fit_mtx$rfc_solar
            vol_vt <- fit_mtx$rfc_volc

            # multiply the coefficient vector and the attributed variable matrix
            sol_mtx <- apply(sol_mtx, 2, FUN=`*`, sol_vt)
            vol_mtx <- apply(vol_mtx, 2, FUN=`*`, vol_vt)

            # adding up the results
            tem_mtx <- sol_mtx + vol_mtx
        }else if(src%in%c("rfc_solar")){
            # get the attributed variable matrix
            sol_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, "rfc_solar", obs_src)[,c(1:max_col)]  
            sol_vt <- fit_mtx$rfc_solar

            # multiply the coefficient vector and the attributed variable matrix
            sol_mtx <- apply(sol_mtx, 2, FUN=`*`, sol_vt)
            tem_mtx <- sol_mtx
            
        }else if(src%in%c("rfc_volc")){
            # get the attributed variable matrix
            vol_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, "rfc_volc", obs_src)[,c(1:max_col)] 
            vol_vt <- fit_mtx$rfc_volc

            # multiply the coefficient vector and the attributed variable matrix
            vol_mtx <- apply(vol_mtx, 2, FUN=`*`, vol_vt)

            tem_mtx <- vol_mtx            
        }else if(src%in%c("vari")){
            # get the climate variability vector
            mei_mtx <- read_IV("cti1854",4)
            amo_mtx <- read_IV("ersst.v5.amo.dat.txt",0)
            mei_vt <- fit_mtx$mei
            amo_vt <- fit_mtx$amo

            nrw <- length(mei_vt)

            # create a matrix with the same size as the climate variability vector
            mei_df <- t(data.frame(value=mei_mtx$value))
            colnames(mei_df) <- mei_mtx$year
            rownames(mei_df) <- NULL
            mei_df <- as.data.frame(lapply(mei_df, rep, nrw))
            colnames(mei_df) <- mei_mtx$year

            # create a matrix with the same size as the climate variability vector
            amo_df <- t(data.frame(value=amo_mtx$value))
            colnames(amo_df) <- amo_mtx$year
            rownames(amo_df) <- NULL
            amo_df <- as.data.frame(lapply(amo_df, rep, nrw))
            colnames(amo_df) <- amo_mtx$year

            # multiply the coefficient vector and the climate variability matrix
            mei_df <- apply(mei_df, 2, FUN=`*`, mei_vt)
            amo_df <- apply(amo_df, 2, FUN=`*`, amo_vt)

            # print(dim(mei_mtx))
            # adding up the results
            tem_mtx <- mei_df + amo_df
        }else if(src%in%c("mei")){
            # get the climate variability vector
            mei_mtx <- read_IV("cti1854",4)
            mei_vt <- fit_mtx$mei
            nrw <- length(mei_vt)

            # create a matrix with the same size as the climate variability vector
            mei_df <- t(data.frame(value=mei_mtx$value))
            colnames(mei_df) <- mei_mtx$year
            rownames(mei_df) <- NULL
            mei_df <- as.data.frame(lapply(mei_df, rep, nrw))
            colnames(mei_df) <- mei_mtx$year

            # multiply the coefficient vector and the climate variability matrix
            mei_df <- apply(mei_df, 2, FUN=`*`, mei_vt)
            # print(dim(mei_mtx))
            tem_mtx <- mei_df
        }else if(src%in%c("amo")){
            # get the climate variability vector
            amo_mtx <- read_IV("ersst.v5.amo.dat.txt",0)
            amo_vt <- fit_mtx$amo

            # create a matrix with the same size as the climate variability vector
            nrw <- length(amo_vt)
            amo_df <- t(data.frame(value=amo_mtx$value))
            colnames(amo_df) <- amo_mtx$year
            rownames(amo_df) <- NULL
            amo_df <- as.data.frame(lapply(amo_df, rep, nrw))
            colnames(amo_df) <- amo_mtx$year

            # multiply the coefficient vector and the climate variability matrix
            amo_df <- apply(amo_df, 2, FUN=`*`, amo_vt)

            # print(dim(mei_mtx))
            tem_mtx <- amo_df
        }else if(src%in%c("est")){
            # define the csv file name
            sub_dir <- paste0(obs_src,"_stat_rfc/")
            fit_csv <- paste0(slow_dat,sub_dir, "est_tem_", obs_src,"_", exp_tag, ".csv")   

            # load the result matrix from the csv file
            fit_mtx <- data.frame(fread(fit_csv, header = TRUE, check.names=F, sep=","), check.names=F)

            tem_mtx <- data.frame(t(fit_mtx[,c(-1)]))
            colnames(tem_mtx) <- fit_mtx[,c(1)]
            rownames(tem_mtx) <- NULL
        }else if(src%in%c("obs")){
            # define the csv file name
            sub_dir <- paste0(obs_src,"_stat_rfc/")
            fit_csv <- paste0(slow_dat,sub_dir, "est_obs_", obs_src,"_", exp_tag, ".csv") 

            # load the result matrix from the csv file  
            fit_mtx <- data.frame(fread(fit_csv, header = TRUE, check.names=F, sep=","), check.names=F)

            tem_mtx <- data.frame(t(fit_mtx[,c(-1)]))
            colnames(tem_mtx) <- fit_mtx[,c(1)]
            rownames(tem_mtx) <- NULL
        }else if(src%in%c("resi")){
            # define the csv file name
            sub_dir <- paste0(obs_src,"_stat_rfc/")
            fit_csv <- paste0(slow_dat,sub_dir, "est_res_", obs_src,"_", exp_tag, ".csv")   

            # load the result matrix from the csv file
            fit_mtx <- data.frame(fread(fit_csv, header = TRUE, check.names=F, sep=","), check.names=F)

            tem_mtx <- data.frame(t(fit_mtx[,c(-1)]))
            colnames(tem_mtx) <- fit_mtx[,c(1)]
            rownames(tem_mtx) <- NULL
        }
    }
    # return the result
    return(tem_mtx)

}

# get the resulting climate variable matrix after the regression (annual)
# fol: folder name
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src: variable source
# obs_src: observation source
# exp_tag: the regression tag
# return  the resulting climate variable matrix
get_stat_ens_annual_mtx <- function(fol, reg, hsrc, psrc, sce, tag, src, obs_src="HadCRUTv5.0.2", exp_tag="AntVolSolMeiAmo"){
    tem_mtx <- NULL
    
    # define the csv file name
    sub_dir <- paste0(obs_src,"_stat_rfc/")
    fit_csv <- paste0(slow_dat,sub_dir, "est_fit_", obs_src,"_", exp_tag, ".csv")   

    # load the coefficient matrix from csv file
    fit_mtx <- data.frame(fread(fit_csv, header = TRUE, check.names=F, sep=","), check.names=F)

    # max_col <- 2100 #by 2024.917
    if(!is.null(fit_mtx)){
        if(src%in%c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC", "rfc_LCC", "rfc_BCsnow", 
                    "rfc_O3s", "rfc_O3t", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_cloud", "rfc_H2Os", 
                    "r_ghg", "r_cm6ghg", "r_othanthro", "r_ofgs", "r_fgs", "r_o3", "r_h2o", "r_abd", "r_aer", "r_cld", "r_aer_all",
                    all_fgs_lst, "r_anthro")){
            coef_vt <- fit_mtx$r_anthro

            # get the attributed variable matrix
            rfc_mtx <- get_rfc_annual_mtx(fol, reg, hsrc, psrc, sce, tag, src, obs_src)
            # print((rfc_mtx))
            if(!is.null(rfc_mtx)){
                # rfc_mtx <- rfc_mtx[,c(1:max_col)] 
                # multiply the coefficient vector and the attributed variable matrix
                dst_mtx <- apply(rfc_mtx, 2, FUN=`*`, coef_vt)
                tem_mtx <- dst_mtx
            }
            # print(dim(tem_mtx))

        }else if(src%in%c("r_nat")){
            # get the attributed variable matrix
            sol_mtx <- get_rfc_annual_mtx(fol, reg, hsrc, psrc, sce, tag, "rfc_solar", obs_src) 
            vol_mtx <- get_rfc_annual_mtx(fol, reg, hsrc, psrc, sce, tag, "rfc_volc", obs_src)

            sol_vt <- fit_mtx$rfc_solar
            vol_vt <- fit_mtx$rfc_volc

            # multiply the coefficient vector and the attributed variable matrix
            sol_mtx <- apply(sol_mtx, 2, FUN=`*`, sol_vt)
            vol_mtx <- apply(vol_mtx, 2, FUN=`*`, vol_vt)

            tem_mtx <- sol_mtx + vol_mtx
        }else if(src%in%c("rfc_solar")){
            # get the attributed variable matrix
            sol_mtx <- get_rfc_annual_mtx(fol, reg, hsrc, psrc, sce, tag, "rfc_solar", obs_src)  
            sol_vt <- fit_mtx$rfc_solar

            # multiply the coefficient vector and the attributed variable matrix
            sol_mtx <- apply(sol_mtx, 2, FUN=`*`, sol_vt)
            tem_mtx <- sol_mtx
            
        }else if(src%in%c("rfc_volc")){
            # get the attributed variable matrix
            vol_mtx <- get_rfc_annual_mtx(fol, reg, hsrc, psrc, sce, tag, "rfc_volc", obs_src)
            vol_vt <- fit_mtx$rfc_volc

            # multiply the coefficient vector and the attributed variable matrix
            vol_mtx <- apply(vol_mtx, 2, FUN=`*`, vol_vt)

            tem_mtx <- vol_mtx            
        }else if(src%in%c("vari")){
           # get the climate variability vector 
            mei_mtx <- read_IV("cti1854",4)
            amo_mtx <- read_IV("ersst.v5.amo.dat.txt",0)
            mei_vt <- fit_mtx$mei
            amo_vt <- fit_mtx$amo

            # calculate the annual mean of the climate variability vector
            mei_mtx$year <- trunc(mei_mtx$year)
            mei_mtx <- aggregate(.~year, data=mei_mtx, FUN=mean)

            # calculate the annual mean of the climate variability vector
            amo_mtx$year <- trunc(amo_mtx$year)
            amo_mtx <- aggregate(.~year, data=amo_mtx, FUN=mean)

            nrw <- length(mei_vt)

            # create a matrix with the same size as the climate variability vector
            mei_df <- t(data.frame(value=mei_mtx$value))
            colnames(mei_df) <- mei_mtx$year
            rownames(mei_df) <- NULL
            mei_df <- as.data.frame(lapply(mei_df, rep, nrw))
            colnames(mei_df) <- mei_mtx$year

            # create a matrix with the same size as the climate variability vector
            amo_df <- t(data.frame(value=amo_mtx$value))
            colnames(amo_df) <- amo_mtx$year
            rownames(amo_df) <- NULL
            amo_df <- as.data.frame(lapply(amo_df, rep, nrw))
            colnames(amo_df) <- amo_mtx$year

            # multiply the coefficient vector and the climate variability vector
            mei_df <- apply(mei_df, 2, FUN=`*`, mei_vt)
            amo_df <- apply(amo_df, 2, FUN=`*`, amo_vt)

            # print(dim(mei_mtx))
            # add the two matrices together
            tem_mtx <- mei_df + amo_df
        }else if(src%in%c("mei")){
            # get the climate variability vector
            mei_mtx <- read_IV("cti1854",4)
            mei_vt <- fit_mtx$mei
            nrw <- length(mei_vt)

            # calculate the annual mean of the climate variability vector
            mei_mtx$year <- trunc(mei_mtx$year)
            mei_mtx <- aggregate(.~year, data=mei_mtx, FUN=mean)

            # create a matrix with the same size as the climate variability vector
            mei_df <- t(data.frame(value=mei_mtx$value))
            colnames(mei_df) <- mei_mtx$year
            rownames(mei_df) <- NULL
            mei_df <- as.data.frame(lapply(mei_df, rep, nrw))
            colnames(mei_df) <- mei_mtx$year

            # multiply the coefficient vector and the climate variability vector
            mei_df <- apply(mei_df, 2, FUN=`*`, mei_vt)
            # print(dim(mei_mtx))
            tem_mtx <- mei_df
        }else if(src%in%c("amo")){
            # get the climate variability vector
            amo_mtx <- read_IV("ersst.v5.amo.dat.txt",0)
            amo_vt <- fit_mtx$amo

            # calculate the annual mean of the climate variability vector
            amo_mtx$year <- trunc(amo_mtx$year)
            amo_mtx <- aggregate(.~year, data=amo_mtx, FUN=mean)

            # create a matrix with the same size as the climate variability vector
            nrw <- length(amo_vt)
            amo_df <- t(data.frame(value=amo_mtx$value))
            colnames(amo_df) <- amo_mtx$year
            rownames(amo_df) <- NULL
            amo_df <- as.data.frame(lapply(amo_df, rep, nrw))
            colnames(amo_df) <- amo_mtx$year

            # multiply the coefficient vector and the climate variability vector
            amo_df <- apply(amo_df, 2, FUN=`*`, amo_vt)

            # print(dim(mei_mtx))
            tem_mtx <- amo_df
        }else if(src%in%c("est")){
            # define the csv file name
            sub_dir <- paste0(obs_src,"_stat_rfc/")
            fit_csv <- paste0(slow_dat,sub_dir, "est_tem_", obs_src,"_", exp_tag, ".csv")   

            # load the result matrix from the csv file
            fit_mtx <- data.frame(fread(fit_csv, header = TRUE, check.names=F, sep=","), check.names=F)
# print(head(fit_mtx[c(1:10),c(1:10)]))
            
            # calculate the annual mean of the result matrix
            ncl <- ncol(fit_mtx)
            colnames(fit_mtx) <- c("year",c(1:(ncl-1)))
            fit_mtx$year <- trunc(fit_mtx$year)
            fit_mtx <- aggregate(.~year, data=fit_mtx, FUN=mean)
# print(head(fit_mtx[c(1:10),c(1:10)]))
            tem_mtx <- data.frame(t(fit_mtx[,c(-1)]))
            colnames(tem_mtx) <- fit_mtx[,c(1)]
            rownames(tem_mtx) <- NULL
        }else if(src%in%c("obs")){
            # define the csv file name
            sub_dir <- paste0(obs_src,"_stat_rfc/")
            fit_csv <- paste0(slow_dat,sub_dir, "est_obs_", obs_src,"_", exp_tag, ".csv")  

            # load the result matrix from the csv file 
            fit_mtx <- data.frame(fread(fit_csv, header = TRUE, check.names=F, sep=","), check.names=F)

            # calculate the annual mean of the result matrix
            ncl <- ncol(fit_mtx)
            colnames(fit_mtx) <- c("year",c(1:(ncl-1)))
            fit_mtx$year <- trunc(fit_mtx$year)
            fit_mtx <- aggregate(.~year, data=fit_mtx, FUN=mean)

            tem_mtx <- data.frame(t(fit_mtx[,c(-1)]))
            colnames(tem_mtx) <- fit_mtx[,c(1)]
            rownames(tem_mtx) <- NULL
        }else if(src%in%c("resi")){
            # define the csv file name
            sub_dir <- paste0(obs_src,"_stat_rfc/")
            fit_csv <- paste0(slow_dat,sub_dir, "est_res_", obs_src,"_", exp_tag, ".csv")  

            # load the result matrix from the csv file 
            fit_mtx <- data.frame(fread(fit_csv, header = TRUE, check.names=F, sep=","), check.names=F)

            # calculate the annual mean of the result matrix
            ncl <- ncol(fit_mtx)
            colnames(fit_mtx) <- c("year",c(1:(ncl-1)))
            fit_mtx$year <- trunc(fit_mtx$year)
            fit_mtx <- aggregate(.~year, data=fit_mtx, FUN=mean)

            tem_mtx <- data.frame(t(fit_mtx[,c(-1)]))
            colnames(tem_mtx) <- fit_mtx[,c(1)]
            rownames(tem_mtx) <- NULL
        }
    }
    # return the result
    return(tem_mtx)

}

# get the climate variable from the specified source
# fol: folder name
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src_lst: variable list
# obs_src: observation source
# exp: the regression method
# preindust: based on pre-industrial period or not
get_tem_mtx <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src="HadCRUTv5.0.2", exp="AntVolSolMeiAmo", preindust=F){
    tem_mtx <- NULL

    for(src in src_lst){
        # print(src)
        for(sec in def_sector){
            # define the csv file name
            src_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src, "_", exp,"_mean.csv")
            # print(src_csv)
            if(file.exists(src_csv)){
                # print(src_csv)
                # load the csv file
                src_mtx <- data.frame(read.table(src_csv, header = TRUE, check.names=F, sep=","), check.names=F)
                
                src_sub <- data.frame(year=src_mtx$year, source=c(src), 
                                                        p025=src_mtx$p025,
                                                        p500=src_mtx$p500,
                                                        p975=src_mtx$p975)                       

                # create the pre-industrial basis time series
                if(preindust){

                    src_pre <- subset(src_sub, year>=1855&year<=1900)
                    if(nrow(src_pre)>0){
                        pre_val <- mean(src_pre$p500)
                        # print(paste0(src,": ",pre_val))
                    }else{
                        pre_val <- 0
                    }
                    src_sub$p025 <- src_sub$p025 - pre_val
                    src_sub$p500 <- src_sub$p500 - pre_val
                    src_sub$p975 <- src_sub$p975 - pre_val
                }

                # combine the matrix
                if(is.null(tem_mtx)){
                    tem_mtx <- src_sub
                }else{
                    tem_mtx <- rbind(tem_mtx, src_sub)
                }         
            }            
        }


    }

    tem_mtx$year <- as.numeric(as.character(tem_mtx$year))
    # return the matrix
    return(tem_mtx)
}

# get the climate variable from the specified source
# fol: folder name
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# grp: variable group name
# obs_src: observation source
# preindust: based on pre-industrial period or not
# rmean: using rolling mean or not
# return the matrix
get_rfc_period <- function(fol, reg, hsrc, psrc, sce, tag, grp, obs_src="HadCRUTv5.0.2", preindust=F, rmean=0){
    src_mtx <- NULL
    # print(grp_lst)
    # get the variable mapping
    grp_lst <- gp_rfc_mapping[grp][[1]]
    for(src in grp_lst){
        # print(paste0(src))
        # define the csv file name
        src_csv <- paste0(period_temp_dir, fol, "/", reg, "_EMISS_IND_", hsrc, "_",psrc, "_", sce,"_", src , "_", tag,"_mtx.csv")
        
        # print(src_csv)
        if(file.exists(src_csv)){
            # load the csv file
            src_sub <- data.frame(read.csv(src_csv, header = TRUE, check.names=F, sep=","), check.names=F)
            src_sub <- src_sub[,as.character(c(1850:2100))]

            # append the matrix
            if(is.null(src_mtx)){
                src_mtx <- src_sub
            }else{
                src_mtx <- src_mtx + src_sub
            }
        }                            
    }
    # return the matrix
    return(src_mtx)
}

# get the climate variable from the specified source (annual mean)
# fol: folder name
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# grp: variable group name
# obs_src: observation source
# return the matrix
get_rfc_annual_mtx <- function(fol, reg, hsrc, psrc, sce, tag, grp, obs_src="HadCRUTv5.0.2"){
    src_mtx <- NULL
    # print(grp_lst)

    # define the csv file name
    src_csv <- paste0(cali_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", grp,"_", tag,"_mtx.csv")
    # print(src_csv)
    if(file.exists(src_csv)){
        # load the csv file
        src_sub <- data.frame(fread(src_csv, header = TRUE, check.names=F, sep=","), check.names=F)

        # calculate the annual mean
        src_sub$yr <- trunc(src_sub$yr)

        src_ann <- aggregate(.~yr, data=src_sub, FUN=mean)

        src_sub <- data.frame(src_ann, row.names=1, check.names=F)
        src_mtx <- t(src_sub)
        # print(head(src_mtx[c(1:10),c(1:10)]))
    }                            

    # return the matrix
    return(src_mtx)
}

# get the climate variable from the specified source (monthly)
# fol: folder name
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# grp: variable group name
# obs_src: observation source
# return the matrix
get_rfc_mtx <- function(fol, reg, hsrc, psrc, sce, tag, grp, obs_src="HadCRUTv5.0.2"){
    src_mtx <- NULL
    # print(grp_lst)

    # define the csv file name
    src_csv <- paste0(cali_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", grp,"_", tag,"_mtx.csv")
    # print(src_csv)
    if(file.exists(src_csv)){
        # load the csv file
        src_sub <- data.frame(fread(src_csv, header = TRUE, check.names=F, sep=","), row.names=1, check.names=F)
        src_mtx <- t(src_sub)
        # print(head(src_mtx))
    }       

    # return the matrix
    return(src_mtx)
}

# get the decad trend of a variable with specified period
# fol: folder name
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src_lst: variable list
# st_yr: start year
# ed_yr: end year
# obs_src: observation source
# exp: the regression method
# return the matrix
get_tem_inc <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, st_yr, ed_yr, obs_src="HadCRUTv5.0.2",exp="AntVolSolMeiAmo"){
    tem_mtx <- NULL

    # iterate the variable list
    for(src in src_lst){

        for(sec in def_sector){
            # define the csv file name
            src_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp, "_", st_yr, "_", ed_yr,".csv")
            # print(src_csv)
            if(file.exists(src_csv)){
                # print(src_csv)
                # load the csv file
                src_tbl <- data.frame(read.table(src_csv, header = F, check.names=F, sep=",", skipNul = TRUE), check.names=F)
                src_sub <- data.frame(index=c(1:nrow(src_tbl)), source=c(src), value=src_tbl)   
 
                colnames(src_sub) <- c("index", "source", "value")   
                # print(tail(src_sub))   
                
                # combine the matrix 
                if(is.null(tem_mtx)){
                    tem_mtx <- src_sub
                }else{
                    tem_mtx <- rbind(tem_mtx, src_sub)
                }         
            }            
        }


    }
    # print(tem_mtx)

    # return the matrix
    return(data.frame(tem_mtx))
}

# get the decad trend of a variable with specified period
# fol: folder name
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src_lst: variable list
# st_yr: start year
# ed_yr: end year
# obs_src: observation source
# exp: the regression method
# return the matrix
get_tem_dec <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, st_yr, ed_yr, obs_src="HadCRUTv5.0.2",exp="AntVolSolMeiAmo"){
    tem_mtx <- NULL

    # iterate the variable list
    for(src in src_lst){

        for(sec in def_sector){
            # define the csv file name
            src_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp, "_", st_yr, "_", ed_yr,".csv")
            # print(src_csv)
            if(file.exists(src_csv)){
                # print(src_csv)
                # load the csv file
                src_tbl <- data.frame(read.table(src_csv, header = F, check.names=F, sep=",", skipNul = TRUE), check.names=F)
                src_sub <- data.frame(source=c(src), t(quantile(src_tbl$V1,probs=c(0.025,0.5,0.975)))) 
                colnames(src_sub) <- c("source","p025","p500","p975")  

                # combine the matrix 
                if(is.null(tem_mtx)){
                    tem_mtx <- src_sub
                }else{
                    tem_mtx <- rbind(tem_mtx, src_sub)
                }         
            }            
        }


    }
    # return the matrix
    return(data.frame(tem_mtx))
}

# read the climate variability index with delayed months
# osrc: observation source
# delay: delayed months
# return the matrix
read_IV <- function(osrc,delay){
    # define the csv file name
    if(osrc=="mei1950"|osrc=="meiv2"|osrc=="ncep-ncar_reanalysis_mei_table_1948-jun_2019"){
        src_csv <- paste0("./data/sum/MEI/", osrc, "_yr", delay,".csv")
    }else if(osrc=="enso.ts.1mn"){
        src_csv <- paste0("./data/sum/BEST/", osrc, "_yr", delay,".csv")
    }else if(osrc=="cti1854"){
        src_csv <- paste0("./data/sum/CTI/", osrc, "_yr", delay,".csv")
    }else{
        src_csv <- paste0("./data/sum/IV/", osrc, "_yr", delay,".csv")
    }
    src_tbl <- NULL
    if(file.exists(src_csv)){
        # load the csv file
        src_tbl <- data.frame(read.table(src_csv, header = F, check.names=F, sep=","), check.names=F)
        colnames(src_tbl) <- c("year","value")
        src_tbl <- subset(src_tbl)
    }
    
    # return the matrix
    return(src_tbl)    
}

# text_mtx <- read_IV("mei1950",0)