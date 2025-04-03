source("r/spike_header.r")

spike_tot_sum <- c("est")

ghg_ems_lst <- c(
    "rfc_CO2",
    "rfc_CH4",
    "rfc_N2O",
    "r_fgs"
)

clim_sol_var_lst <- c(
    "rfc_solar",
    "mei",
    "amo"    
)

clim_var_lst <- c(
    "mei",
    "amo"    
)

slowdown_tot_pos <- c(
    "rfc_CO2",
    "rfc_N2O",
    "r_abd",
    "r_aer",
    "rfc_cloud",
    "rfc_volc"
)

slowdown_tot_neg <- c(
    "rfc_CH4",
    "r_fgs",
    "r_o3",
    "rfc_H2Os",
    "rfc_solar",
    "mei",
    "amo"    
)

coolphase_tot_pos <- c(
    "rfc_CO2", 
    "rfc_CH4", 
    "rfc_N2O", 
    "r_fgs",
    "r_o3", 
    "rfc_H2Os", 
    "rfc_solar",
    "mei"
)

coolphase_tot_neg <- c(
    "rfc_LCC",
    "rfc_BCsnow", 
    "r_aer",
    "rfc_cloud",
    "rfc_volc",
    "amo"
)

# get the decadal trend of a matrix for review
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src: variable source
# obs_src: observation source
# start_yr: start year
# end_yr: end year
# exp_tag: regression tag
# print the result
get_stat_annual <- function(fol, reg, hsrc, psrc, sce, tag, src, obs_src, start_yr, end_yr, exp_tag="AntVolSolMeiAmo"){

    print(src)

    # define the csv file name
    sub_dir <- paste0(obs_src,"_stat_rfc/")
    var_csv <- paste0(slow_dat,sub_dir, "est_", src,"_", obs_src,"_", exp_tag,".csv")
    if(file.exists(var_csv)){

        # load data frame from csv file
        stat_mtx <- data.frame(fread(var_csv, header = TRUE, check.names=F, sep=","), check.names=F)


        src_mtx <- data.frame(year=as.numeric(colnames(stat_mtx)),t(stat_mtx))

        # get the decadal trend, using parallel processing
        inc_ens <- get_decadal_inc_par_ens(src_mtx, start_yr, end_yr)

        dst_df <- data.frame(source=c(src), start_year=c(start_yr), end_year=c(end_yr), t(quantile(inc_ens,probs=c(0.025,0.5,0.975))))     

        # print the result
        print(dst_df)
    }

}

# get the decadal trend of a matrix for review
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src_lst: list of sources
# obs_src: observation source
# start_yr: start year
# end_yr: end year
# exp_tag: regression tag
# return the result matrix
get_decadal_inc_src <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src, start_yr, end_yr, exp_tag="AntVolSolMeiAmo"){
    stat_all <- NULL
    dst_ens <- NULL
    # iterate over the sources
    for(src in src_lst){
        # define the csv file name
        sub_dir <- paste0(obs_src,"_stat_rfc/")
        var_csv <- paste0(slow_dat,sub_dir, "est_", src,"_", obs_src,"_", exp_tag,".csv")
        # print(var_csv)
        if(file.exists(var_csv)){
            # print(var_csv)
            # load data frame from csv file
            stat_mtx <- data.frame(fread(var_csv, header = TRUE, check.names=F, sep=","), check.names=F)
            stat_mtx <- stat_mtx[,c(as.character(1855:2025))]

            # print(paste(src, start_yr, end_yr, sep= " ")) 
            # print(dim(stat_mtx))

            if(is.null(stat_all)){
                stat_all <- stat_mtx
            }else{
                # print(src)
                # print(dim(stat_all))
                # print(dim(stat_mtx))
                stat_all <- stat_all + stat_mtx
            }

        }
    }
    if(!is.null(stat_all)){
        src_mtx <- data.frame(year=as.numeric(colnames(stat_all)),t(stat_all))

        # get the decadal trend, using parallel processing
        dst_ens <- get_decadal_inc_par_ens(src_mtx, start_yr, end_yr)  
    }
    return(dst_ens)
} 

# get the contribution % of the variable of src, from start_yr to end_yr
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src: variable source
# obs_src: observation source
# start_yr: start year
# end_yr: end year
# exp_tag: regression tag
# print the result matrix
get_coolphase_percent_in_total_negative <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src,  start_yr=1944, end_yr=1976, exp_tag="AntVolSolMeiAmo"){
    # get the decadal trends
    src_mtx <- get_decadal_inc_src(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src,  start_yr, end_yr, exp_tag)
    neg_mtx <- get_decadal_inc_src(fol, reg, hsrc, psrc, sce, tag, coolphase_tot_neg, obs_src,  start_yr, end_yr, exp_tag)

    # calculate the contribution %
    dst_pct <- NULL
    if(!is.null(src_mtx) & !is.null(neg_mtx)){
        # calculate the contribution %
        avg_inc <- src_mtx/neg_mtx

        dst_pct <- data.frame(source=c(paste0(src_lst[1],"_",length(src_lst))), t(quantile(avg_inc,probs=c(0.025,0.5,0.975)))) 

        colnames(dst_pct) <- c("source","p025","p500","p975")  
    }
    # print the result matrix
    print(dst_pct)
}


# get the temperature contributions % for the warming slowdown, using the difference of decadal trends (DDT) for 1998-2012 compared to 1970-2012 
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src_lst: list of sources
# obs_src: observation source
# prev_st_yr: start year of the reference period
# prev_ed_yr: end year of the reference period
# curr_st_yr: start year of the current period
# curr_ed_yr: end year of the current period
# exp_tag: regression tag
# return the result matrix
get_slowdown_percent_in_total_negative <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src,  prev_st_yr=1970, prev_ed_yr=2012, curr_st_yr=1998, curr_ed_yr=2012, exp_tag="AntVolSolMeiAmo"){
    # get the decadal trends
    pre_src_mtx <- get_decadal_inc_src(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src,  prev_st_yr, prev_ed_yr, exp_tag)
    pre_neg_mtx <- get_decadal_inc_src(fol, reg, hsrc, psrc, sce, tag, slowdown_tot_neg, obs_src,  prev_st_yr, prev_ed_yr, exp_tag)

    curr_src_mtx <- get_decadal_inc_src(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src,  curr_st_yr, curr_ed_yr, exp_tag)
    curr_neg_mtx <- get_decadal_inc_src(fol, reg, hsrc, psrc, sce, tag, slowdown_tot_neg, obs_src,  curr_st_yr, curr_ed_yr, exp_tag)    

    src_dif <- curr_src_mtx - pre_src_mtx
    neg_dif <- curr_neg_mtx - pre_neg_mtx  

    # calculate the contribution %
    dst_pct <- NULL
    if(!is.null(src_dif) &!is.null(neg_dif)){
        # calculate the contribution %
        avg_inc <- src_dif/neg_dif
        dst_pct <- data.frame(source=c(paste0(src_lst[1],"_",length(src_lst))), t(quantile(avg_inc,probs=c(0.025,0.5,0.975))))
        colnames(dst_pct) <- c("source","p025","p500","p975")
    }

    # print the result matrix
    print(dst_pct)
}

# get the temperature increases for the temperature spike, i.e., the temperature increase from 2010-2021 to 2022-2024
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src_lst: list of sources
# obs_src: observation source
# prev_st_yr: start year of the reference period
# prev_ed_yr: end year of the reference period
# curr_st_yr: start year of the current period
# curr_ed_yr: end year of the current period
# exp_tag: regression tag
# return the result matrix
get_spike_src <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src,  prev_st_yr, prev_ed_yr, curr_st_yr, curr_ed_yr, exp_tag){
    avg_inc <- NULL
    ref_all <- NULL
    bom_all <- NULL
    # iterate over the sources
    for(src in src_lst){#
        # print(src)
        # define the csv file name
        sub_dir <- paste0(obs_src,"_stat_rfc/")
        var_csv <- paste0(slow_dat,sub_dir, "est_", src,"_", obs_src,"_", exp_tag,".csv")

        if(file.exists(var_csv)){
            # load data frame from csv file
            stat_mtx <- data.frame(fread(var_csv, header = TRUE, check.names=F, sep=","), check.names=F)

            # calculate the temperature increases for the temperature spike, i.e., the temperature increase from 2010-2021 to 2022-2024

            # get the temperature increases for the temperature spike
            src_ref <- stat_mtx[,c(as.character(prev_st_yr:prev_ed_yr))]
            src_bom <- stat_mtx[,c(as.character(curr_st_yr:curr_ed_yr))]
# print(dim(src_ref))
            if(is.null(ref_all)){
                ref_all <- src_ref
            }else{
                ref_all <- ref_all + src_ref
            }

            if(is.null(bom_all)){
                bom_all <- src_bom
            }else{
                bom_all <- bom_all + src_bom
            }

        }     
    }
    if(!is.null(ref_all) &!is.null(bom_all)){
        avg_ref <- rowMeans(ref_all)
        avg_bom <- rowMeans(bom_all)   
# print(length(avg_ref))
        avg_inc <- avg_bom - avg_ref        
    }

    # return the result matrix
    return(avg_inc)
}

# get the temperature contributions % for the temperature spike, i.e., the temperature increase from 2010-2021 to 2022-2024
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src_lst: list of sources
# obs_src: observation source
# prev_st_yr: start year of the reference period
# prev_ed_yr: end year of the reference period
# curr_st_yr: start year of the current period
# curr_ed_yr: end year of the current period
# exp_tag: regression tag
# print the result matrix
get_spike_percent_in_total <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src,  prev_st_yr=2010, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024, exp_tag="AntVolSolMeiAmo"){
    # get the temperature increases from the specified sources
    src_mtx <- get_spike_src(fol, reg, hsrc, psrc, sce, tag, src_lst, obs_src,  prev_st_yr, prev_ed_yr, curr_st_yr, curr_ed_yr, exp_tag)
    tot_mtx <- get_spike_src(fol, reg, hsrc, psrc, sce, tag, spike_tot_sum, obs_src,  prev_st_yr, prev_ed_yr, curr_st_yr, curr_ed_yr, exp_tag)

    dst_pct <- NULL
    if(!is.null(src_mtx) & !is.null(tot_mtx)){
        # calculate the contribution %
        avg_inc <- src_mtx/tot_mtx

        dst_pct <- data.frame(source=c(paste0(src_lst[1],"_",length(src_lst))), t(quantile(avg_inc,probs=c(0.025,0.5,0.975)))) 

        colnames(dst_pct) <- c("source","p025","p500","p975")  
    }
    # print the result matrix
    print(dst_pct)
}

print(">>>>>>>>>>>>>> Attribution % for the temperature spike")

print("GHG attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", ghg_ems_lst, "HadCRUTv5.0.2")

print("CO2 attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_CO2"), "HadCRUTv5.0.2")

print("rfc_CH4 attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_CH4"), "HadCRUTv5.0.2")

print("rfc_N2O attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_N2O"), "HadCRUTv5.0.2")

print("r_fgs attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("r_fgs"), "HadCRUTv5.0.2")

print("Solar attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_solar"), "HadCRUTv5.0.2")

print("Solar and variability attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", clim_sol_var_lst, "HadCRUTv5.0.2")

print("Variability attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", clim_var_lst, "HadCRUTv5.0.2")

print("CTI attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("mei"), "HadCRUTv5.0.2")

print("AMO attribution %")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("amo"), "HadCRUTv5.0.2")

dif_mtx <- get_tem_bom("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "r_ghg", prev_st_yr=2010, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024, obs_src="HadCRUTv5.0.2")
print("GHG contribution to the temperature spike")
print(dif_mtx)

print("CO2 attribution %, 2022-2024 cf. 2019-2021")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_CO2"), "HadCRUTv5.0.2", prev_st_yr=2019, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024)

print("AMO attribution %, 2022-2024 cf. 2019-2021")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("amo"), "HadCRUTv5.0.2", prev_st_yr=2019, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024)

print("Solar attribution %, 2022-2024 cf. 2019-2021")
get_spike_percent_in_total("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_solar"), "HadCRUTv5.0.2", prev_st_yr=2019, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024)

print(">>>>>>>>>>>>>> Attribution % for the warming slowdown")
print("GHG (rfc_CH4 + r_fgs) attribution %")
get_slowdown_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_CH4", "r_fgs"), "HadCRUTv5.0.2",  prev_st_yr=1970, prev_ed_yr=2012, curr_st_yr=1998, curr_ed_yr=2012, exp_tag="AntVolSolMeiAmo")

print("rfc_CH4 attribution %")
get_slowdown_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_CH4"), "HadCRUTv5.0.2",  prev_st_yr=1970, prev_ed_yr=2012, curr_st_yr=1998, curr_ed_yr=2012, exp_tag="AntVolSolMeiAmo")

print("r_fgs attribution %")
get_slowdown_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("r_fgs"), "HadCRUTv5.0.2",  prev_st_yr=1970, prev_ed_yr=2012, curr_st_yr=1998, curr_ed_yr=2012, exp_tag="AntVolSolMeiAmo")

print("rfc_solar attribution %")
get_slowdown_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_solar"), "HadCRUTv5.0.2",  prev_st_yr=1970, prev_ed_yr=2012, curr_st_yr=1998, curr_ed_yr=2012, exp_tag="AntVolSolMeiAmo")

print("variability attribution %")
get_slowdown_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("mei","amo"), "HadCRUTv5.0.2",  prev_st_yr=1970, prev_ed_yr=2012, curr_st_yr=1998, curr_ed_yr=2012, exp_tag="AntVolSolMeiAmo")

print("mei attribution %")
get_slowdown_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("mei"), "HadCRUTv5.0.2",  prev_st_yr=1970, prev_ed_yr=2012, curr_st_yr=1998, curr_ed_yr=2012, exp_tag="AntVolSolMeiAmo")

print("amo attribution %")
get_slowdown_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("amo"), "HadCRUTv5.0.2",  prev_st_yr=1970, prev_ed_yr=2012, curr_st_yr=1998, curr_ed_yr=2012, exp_tag="AntVolSolMeiAmo")

print(">>>>>>>>>>>>>> Attribution % for the cooling phase")
print("Aerosol (r_aer + rfc_cloud) attribution %")
get_coolphase_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("r_aer", "rfc_cloud"), "HadCRUTv5.0.2",  start_yr=1944, end_yr=1976, exp_tag="AntVolSolMeiAmo")

print("r_aer attribution %")
get_coolphase_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("r_aer"), "HadCRUTv5.0.2",  start_yr=1944, end_yr=1976, exp_tag="AntVolSolMeiAmo")

print("rfc_cloud attribution %")
get_coolphase_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_cloud"), "HadCRUTv5.0.2",  start_yr=1944, end_yr=1976, exp_tag="AntVolSolMeiAmo")

print("amo attribution %")
get_coolphase_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("amo"), "HadCRUTv5.0.2",  start_yr=1944, end_yr=1976, exp_tag="AntVolSolMeiAmo")

print("rfc_volc attribution %")
get_coolphase_percent_in_total_negative("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", c("rfc_volc"), "HadCRUTv5.0.2",  start_yr=1944, end_yr=1976, exp_tag="AntVolSolMeiAmo")

print(">>>>>>>>>>>>>> get the decadal trend of the variable")
# get the decadal trend of the variable of r_ghg, from 1944 to 1976, using HadCRUTv5.0.2
get_stat_annual("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "r_ghg", obs_src="HadCRUTv5.0.2", 1944, 1976, exp_tag="AntVolSolMeiAmo")

# get the decadal trend of the variable of r_aer_all, from 1944 to 1976, using HadCRUTv5.0.2
get_stat_annual("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "r_aer_all", obs_src="HadCRUTv5.0.2", 1944, 1976, exp_tag="AntVolSolMeiAmo")

# get the decadal trend of the variable of amo, from 1944 to 1976, using HadCRUTv5.0.2
get_stat_annual("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "amo", obs_src="HadCRUTv5.0.2", 1944, 1976, exp_tag="AntVolSolMeiAmo")

# get the decadal trend of the variable of rfc_solar, from 1998 to 2012, using HadCRUTv5.0.2
get_stat_annual("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "rfc_solar", obs_src="HadCRUTv5.0.2", 1998, 2012, exp_tag="AntVolSolMeiAmo")

# get the decadal trend of the variable of rfc_CO2, from 2014 to 2024, using HadCRUTv5.0.2
get_stat_annual("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "rfc_CO2", obs_src="HadCRUTv5.0.2", 2014, 2024, exp_tag="AntVolSolMeiAmo")
