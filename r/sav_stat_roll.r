source("./r/plot_deriv_ems.r")

# fl_ext <- ".pdf"
fl_ext <- ".png"

parse_args <- function(args){
    args_lst <- unlist(strsplit(args, " "))
    rslt <- NULL
    for(iarg in args_lst){
        arg <- unlist(strsplit(iarg, "="))
        print(arg)
        arg_name <- sub("--", "",  arg[[1]])
        arg_val <- arg[[2]]
        if(is.null(rslt)){
            rslt <- c(arg_val)
        }else{
            rslt <- c(rslt, arg_val)
        }
        names(rslt)[length(rslt)] <- arg_name
    }
    return(rslt)
}

args <- parse_args(commandArgs(TRUE))
var <- ""

if(!is.null(args["var"])){
    var <- args["var"]
}

# save the rolling trends for the temperature increase
# fol: the folder to save the results
# reg: the region
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the tag of the results
# obs_src: the source of the observations
# src: the source of the climate variables
# exp_tag: the tag of the regression

# Please note that this can be very time consuming, taking about 2-3 hours per variable.
sav_stat_roll <- function(fol, reg, hsrc, psrc, sce, tag, obs_src, src, exp_tag="AntVolSolMeiAmo",k=11){
    # all the climate variables
    src_lst <- c("r_ghg", "r_othanthro", "rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")
    if(src%in%src_lst){
        print(src)
        sub_dir <- paste0(obs_src,"_stat_rfc/")
        var_csv <- paste0(slow_dat,sub_dir, "est_", src,"_", obs_src,"_", exp_tag,"_all.csv")
        if(file.exists(var_csv)){
            stat_mtx <- data.frame(fread(var_csv, header = TRUE, check.names=F, sep=","), check.names=F) 

            # calculate the 11-year rolling trends for the temperature increase
            roll_mtx <- get_dec_par_roll(stat_mtx, start_yr=1855,k)

            # save the 11-year rolling trends for the temperature increase
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src, "_", exp_tag,"_roll_", k, ".csv")
            fwrite(roll_mtx, file = dst_csv, row.names=F)
            print(paste0(dst_csv, " saved."))     
        }

    }  
}

# save the rolling trends (from annual mean) for the temperature increase
# fol: the folder to save the results
# reg: the region
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the tag of the results
# obs_src: the source of the observations
# src: the source of the climate variables
# exp_tag: the tag of the regression
sav_stat_annual_roll <- function(fol, reg, hsrc, psrc, sce, tag, obs_src, src, exp_tag="AntVolSolMeiAmo",k=11){
    # all the climate variables
    src_lst <- c("r_ghg", "r_othanthro", "rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")
    if(src%in%src_lst){
        print(src)
        sub_dir <- paste0(obs_src,"_stat_rfc/")
        var_csv <- paste0(slow_dat,sub_dir, "est_", src,"_", obs_src,"_", exp_tag,".csv")
        if(file.exists(var_csv)){
            stat_mtx <- data.frame(fread(var_csv, header = TRUE, check.names=F, sep=","), check.names=F) 
            # calculate the 11-year rolling trends for the temperature increase
            roll_mtx <- get_dec_par_annual_roll(stat_mtx, start_yr=1855,k)

            # save the 11-year rolling trends for the temperature increase
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src, "_", exp_tag,"_roll_", k, ".csv")
            fwrite(roll_mtx, file = dst_csv, row.names=F)
            print(paste0(dst_csv, " saved."))     
        }

    }  
}

sav_stat_annual_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", var, obs_src="HadCRUTv5.0.2", exp_tag="AntVolSolMeiAmo")
sav_stat_annual_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", var, obs_src="GlobalTemp_v6", exp_tag="AntVolSolMeiAmo")
sav_stat_annual_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", var, obs_src="Berkeley_Earth", exp_tag="AntVolSolMeiAmo")