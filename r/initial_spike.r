# load R library, the initial definitions and utility function
source("r/spike_header.r")

# save MEI index for the multiple regressions
# tag: a file tag to indicate the MEI source
# delay: the delay months for the spike index
sav_mei_index <- function(tag, delay){
    mei_csv <- paste0(dat_dir, "sum/MEI/", tag,".csv")
    mei_mtx <- data.frame(read.table(mei_csv, header = TRUE, check.names=F, sep=","), check.names=F)
    
    mei_df = reshape2::melt(mei_mtx,
                            id.vars=c("year"),
                            variable.name="bimon", na.rm=TRUE)
    mei_dst <- mei_df[
        with(mei_df, order(year, bimon)),
    ]
    nrw <- nrow(mei_dst)
    mei_dst <- data.frame(yr=round(mei_dst$year+(as.numeric(gsub("\\D", "", mei_dst$bimon))-1)/12,3),mei_dst)

    mei_dst <- mei_dst[
        with(mei_dst, order(yr)),
    ]

    yr0_lst <- mei_dst$yr
    mei_dst <- subset(mei_dst,value!=-999.00)

    mei_dst$value <- detrend(mei_dst$value, 'linear') 

    sav_csv <- paste0(dat_dir, "sum/MEI/", tag,"_bimon.csv")
    write.csv(mei_dst, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   

    mei_yr0 <- subset(mei_dst, select=c("yr","value"))
    
    
    if(delay>0){
        yr0_lst <- yr0_lst[-c(1:delay)]
    }
    
    nrw <- nrow(mei_yr0)
    mei_yr0$yr <- as.character(yr0_lst[1:nrw])
    colnames(mei_yr0) = NULL

    # save to the monthly file with the delay of spefified months
    sav_csv <- paste0(dat_dir, "sum/MEI/", tag,"_yr", delay,".csv")
    write.csv(mei_yr0, file = sav_csv, row.names=F, quote=T)
    print(paste0(sav_csv, " saved."))   

    # save to the annual file
    mei_ann <- aggregate(value~year, data=mei_dst, FUN=mean)
    colnames(mei_ann) <- c("year","value")
    mei_ann$value <- detrend(mei_ann$value, 'linear')
    sav_csv <- paste0(dat_dir, "sum/MEI/", tag,"_ann.csv")
    write.csv(mei_ann, file = sav_csv, row.names=F, quote=F)
    print(paste0(sav_csv, " saved."))  
}

# save CTI index for the multiple regressions
# tag: a file tag to indicate the CTI source
# delay: the delay months for the spike index
sav_cti_index <- function(tag, delay){
    cti_csv <- paste0(dat_dir, "CTI/", tag,".csv")
    cti_mtx <- data.frame(read.table(cti_csv, header = TRUE, check.names=F, sep=","), check.names=F)

    time_mtx <- data.frame(str_split(cti_mtx$year, "-", simplify=TRUE))
    colnames(time_mtx) <- c("year", "month","day")

    cti_dst <- data.frame(year=time_mtx$year,month=time_mtx$month,yr=round(as.numeric(time_mtx$year)+(as.numeric(time_mtx$month)-1)/12,3),value=cti_mtx$value)
    
    cti_yr0 <- subset(cti_dst, select=c("yr","value"))
    yr_lst <- cti_dst$yr

    max_yr <- as.numeric(max(time_mtx$year))
    yr0_sub <- yr_lst[yr_lst<=max_yr]
    yr0_ext <- yr0_sub[yr0_sub>(max_yr-2)]
    yr0_ext <- yr0_ext + 2

    yr0_lst <- c(yr0_sub,yr0_ext)
    if(delay > 0){
        yr0_lst <- yr0_lst[-c(1:delay)]
    }
    
    nrw <- nrow(cti_yr0)
    
    cti_yr0$yr <- as.character(yr0_lst[1:nrw])

    cti_yr0$value <- detrend(cti_yr0$value, 'linear')

    # save to the monthly file with the delay of spefified months
    colnames(cti_yr0) = NULL
    sav_csv <- paste0(dat_dir, "sum/CTI/", tag,"_yr", delay,".csv")
    write.csv(cti_yr0, file = sav_csv, row.names=F, quote=T)
    print(paste0(sav_csv, " saved."))   

    # save to the annual file
    mei_ann <- aggregate(value~year, data=cti_dst, FUN=mean)
    colnames(mei_ann) <- c("year","value")
    mei_ann$value <- detrend(mei_ann$value, 'linear')
    sav_csv <- paste0(dat_dir, "sum/CTI/", tag,"_ann.csv")
    write.csv(mei_ann, file = sav_csv, row.names=F, quote=F)
    print(paste0(sav_csv, " saved."))     
}

# save BEST index for the multiple regressions
# tag: a file tag to indicate the BEST source
# delay: the delay months for the spike index
sav_best_index <- function(tag, delay){
    mei_csv <- paste0(dat_dir, "sum/BEST/", tag,".csv")
    mei_mtx <- data.frame(read.table(mei_csv, header = TRUE, check.names=F, sep=","), check.names=F)
    
    mei_df = reshape2::melt(mei_mtx,
                            id.vars=c("year"),
                            variable.name="bimon", na.rm=TRUE)
    mei_dst <- mei_df[
        with(mei_df, order(year, bimon)),
    ]
    nrw <- nrow(mei_dst)
    mei_dst <- data.frame(yr=round(mei_dst$year+(as.numeric(gsub("\\D", "", mei_dst$bimon))-1)/12,3),mei_dst)

    mei_dst <- mei_dst[
        with(mei_dst, order(yr)),
    ]

    yr0_lst <- mei_dst$yr
    mei_dst <- subset(mei_dst,yr<=2021)
    mei_dst$value <- detrend(mei_dst$value, 'linear') 

    sav_csv <- paste0(dat_dir, "sum/BEST/", tag,"_bimon.csv")
    write.csv(mei_dst, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   

    mei_yr0 <- subset(mei_dst, select=c("yr","value"))
    
    
    if(delay>0){
        yr0_lst <- yr0_lst[-c(1:delay)]
    }
    
    # save to the monthly file with the delay of spefified months
    nrw <- nrow(mei_yr0)
    mei_yr0$yr <- as.character(yr0_lst[1:nrw])
    colnames(mei_yr0) = NULL
    sav_csv <- paste0(dat_dir, "sum/BEST/", tag,"_yr", delay,".csv")
    write.csv(mei_yr0, file = sav_csv, row.names=F, quote=T)
    print(paste0(sav_csv, " saved."))   

    # save to the annual file
    mei_ann <- aggregate(value~year, data=mei_dst, FUN=mean)
    colnames(mei_ann) <- c("year","value")
    mei_ann$value <- detrend(mei_ann$value, 'linear')
    sav_csv <- paste0(dat_dir, "sum/BEST/", tag,"_ann.csv")
    write.csv(mei_ann, file = sav_csv, row.names=F, quote=F)
    print(paste0(sav_csv, " saved."))  
}

# save AMO index for the multiple regressions
# tag: a file tag to indicate the AMO source
# delay: the delay months for the spike index
# skp: the number of rows to skip
# nrw: the number of rows to read
sav_amo_index <- function(tag, delay, skp, nrw){
    mei_csv <- paste0(dat_dir, "Variability/", tag)
    mei_mtx <- data.frame(read.table(mei_csv, header = T, check.names=F, sep="", skip=skp, nrow=nrw), check.names=F)   
    colnames(mei_mtx) <- c("year","month","value")

    nrw <- nrow(mei_mtx)
    mei_dst <- data.frame(yr=round(mei_mtx$year+(mei_mtx$month-1)/12,3),mei_mtx)

    yr0_lst <- mei_dst$yr
    mei_dst$value <- detrend(mei_dst$value, 'linear') 

    mei_yr0 <- subset(mei_dst, select=c("yr","value"))

    if(delay>0){
        yr0_lst <- yr0_lst[-c(1:delay)]
    }

    nrw <- nrow(mei_yr0)
    mei_yr0 <- mei_yr0[c(1:(nrw-delay)),]

    # save to the monthly file with the delay of spefified months
    mei_yr0$yr <- as.character(yr0_lst)
    colnames(mei_yr0) = NULL
    sav_csv <- paste0(dat_dir, "sum/IV/", tag,"_yr", delay,".csv")
    write.csv(mei_yr0, file = sav_csv, row.names=F, quote=T)
    print(paste0(sav_csv, " saved."))   

    # save to the annual file
    mei_ann <- aggregate(value~year, data=mei_dst, FUN=mean)
    colnames(mei_ann) <- c("year","value")
    mei_ann$value <- detrend(mei_ann$value, 'linear')
    sav_csv <- paste0(dat_dir, "sum/IV/", tag,"_ann.csv")
    write.csv(mei_ann, file = sav_csv, row.names=F, quote=F)
    print(paste0(sav_csv, " saved."))       
}

# save the climate variability index for the multiple regressions
# use a wide data table as input
# tag: a file tag to indicate the climate variability source
# delay: the delay months for the spike index
# skp: the number of rows to skip
# nrw: the number of rows to read
# yr_max: the maximum year
# hd: the header flag
sav_iv_index <- function(tag, delay, skp, nrw, yr_max, hd=T){
    mei_csv <- paste0(dat_dir, "Variability/", tag)
    mei_mtx <- data.frame(read.table(mei_csv, header = hd, check.names=F, sep="", skip=skp, nrow=nrw, skipNul =T, fill=T, row.names=NULL), check.names=F)   
    
    colnames(mei_mtx) <- c("year","1JAN","2FEB","3MAR","4APR","5MAY","6JUN","7JUL","8AUG","9SEP","10OCT","11NOV","12DEC")

    mei_df = reshape2::melt(mei_mtx,
                            id.vars=c("year"),
                            variable.name="bimon", na.rm=F)

    mei_df$year <- as.numeric(as.character(mei_df$year))
    mei_dst <- mei_df[
        with(mei_df, order(year, bimon)),
    ]
    nrw <- nrow(mei_dst)

    mei_dst <- data.frame(yr=round(mei_dst$year+(as.numeric(gsub("\\D", "", mei_dst$bimon))-1)/12,3),mei_dst)

    mei_dst <- mei_dst[
        with(mei_dst, order(yr)),
    ]

    mei_dst <- subset(mei_dst,yr<=yr_max&yr>=1850)
    yr0_lst <- mei_dst$yr
    mei_dst$value <- detrend(mei_dst$value, 'linear') 

    mei_yr0 <- subset(mei_dst, select=c("yr","value"))

    if(delay>0){
        yr0_lst <- yr0_lst[-c(1:delay)]
    }

    nrw <- nrow(mei_yr0)
    mei_yr0 <- mei_yr0[c(1:(nrw-delay)),]

    mei_yr0$yr <- as.character(yr0_lst)
    colnames(mei_yr0) = NULL
    
    
    # save to the monthly file with the delay of spefified months
    sav_csv <- paste0(dat_dir, "sum/IV/", tag,"_yr", delay,".csv")
    write.csv(mei_yr0, file = sav_csv, row.names=F, quote=T)
    print(paste0(sav_csv, " saved."))   

    # save to the annual file
    mei_ann <- aggregate(value~year, data=mei_dst, FUN=mean)
    colnames(mei_ann) <- c("year","value")
    mei_ann$value <- detrend(mei_ann$value, 'linear')
    sav_csv <- paste0(dat_dir, "sum/IV/", tag,"_ann.csv")
    write.csv(mei_ann, file = sav_csv, row.names=F, quote=F)
    print(paste0(sav_csv, " saved."))  
}

# save the climate variability index for the multiple regressions
# use a long data table as input
# tag: a file tag to indicate the climate variability source
# delay: the delay months for the spike index
# skp: the number of rows to skip
# hd: the header flag
sav_iv_index2 <- function(tag, delay, skp=1, hd=F){
    mei_csv <- paste0(dat_dir, "Variability/", tag)
    mei_mtx <- data.frame(read.table(mei_csv, header = hd, check.names=F, sep=",", skip=skp, skipNul =T, fill=T, row.names=NULL), check.names=F)   
    
    colnames(mei_mtx) <- c("yr","value")
    mei_mtx <- subset(mei_mtx, value!=-9999.000)

    time_mtx <- data.frame(str_split(mei_mtx$yr, "-", simplify=TRUE))
    colnames(time_mtx) <- c("year", "month","day")
    time_mtx$year <- as.numeric(as.character(time_mtx$year))
    time_mtx$month <- as.numeric(as.character(time_mtx$month))
    time_mtx$day <- as.numeric(as.character(time_mtx$day))

    mei_dst <- data.frame(time_mtx,mei_mtx)
    nrw <- nrow(mei_dst)

    mei_dst <- data.frame(yr=round(mei_dst$year+(as.numeric(gsub("\\D", "", mei_dst$month))-1)/12,3),mei_dst)

    mei_dst <- subset(mei_dst,yr>=1850)
    yr0_lst <- mei_dst$yr
    mei_dst$value <- detrend(mei_dst$value, 'linear') 

    mei_yr0 <- subset(mei_dst, select=c("yr","value"))

    if(delay>0){
        yr0_lst <- yr0_lst[-c(1:delay)]
    }

    nrw <- nrow(mei_yr0)
    mei_yr0 <- mei_yr0[c(1:(nrw-delay)),]

    mei_yr0$yr <- as.character(yr0_lst)
    colnames(mei_yr0) = NULL
    
    # save to the monthly file with the delay of spefified months
    sav_csv <- paste0(dat_dir, "sum/IV/", tag,"_yr", delay,".csv")
    write.csv(mei_yr0, file = sav_csv, row.names=F, quote=T)
    print(paste0(sav_csv, " saved."))   

    # save to the annual file
    mei_ann <- aggregate(value~year, data=mei_dst, FUN=mean)
    colnames(mei_ann) <- c("year","value")
    mei_ann$value <- detrend(mei_ann$value, 'linear')
    sav_csv <- paste0(dat_dir, "sum/IV/", tag,"_ann.csv")
    write.csv(mei_ann, file = sav_csv, row.names=F, quote=F)
    print(paste0(sav_csv, " saved."))  
}

# save all the climate variability indices for the multiple regressions
save_var_index_all <- function(){
    # an iteration for the delay months
    for(i in c(0:12)){
        sav_mei_index("meiv2",i)
        sav_mei_index("mei1950",i)
        sav_mei_index("ncep-ncar_reanalysis_mei_table_1948-jun_2019",i)
        sav_mei_index("MEI_ext",i)
        sav_cti_index("cti1854",i)
        sav_best_index("enso.ts.1mn",i)
        sav_amo_index("ersst.v5.amo.dat.txt", i, 1, -1)
        sav_iv_index("ersst.v5.pdo.dat.txt", i, 1, 173,2025.083)
        sav_iv_index2("nao.long.csv",i)
        sav_iv_index2("dmi.had.long.csv", i)

        sav_iv_index("nino3.long.data.txt", i, 1, 155,2024.333, hd=F)
        sav_iv_index("nino34.long.data.txt", i, 1, 155,2024.333, hd=F)
        sav_iv_index("nino4.long.data.txt", i, 1, 155,2024.333, hd=F)
        sav_iv_index("monthly.ao.index.b50.current.ascii.table.txt", i, 0, 76,2024.917, hd=T)
        sav_iv_index("tpi.timeseries.ersstv5.data.txt", i, 1, 171,2024.917, hd=F)
    }   
}

# define the observation sources with the maximum available years
obs_lst <- c(
    "HadCRUTv5.0.2"=2024.917,
    "GlobalTemp_v6"=2024.917,
    "Berkeley_Earth"=2024.917,
    "JMA"=2024.917,
    "DCENT"=2023.917
)

# save the monthly climate variables as matrices for the multiple regressions
# fol: the folder for saving source files
# reg: the region name
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the tag to identify the variables, here refers to "fscm_tatm" for the attributed DetaT induced by the associated sources
sav_cali_rfc_mtx_m <- function(fol, reg, hsrc, psrc, sce, tag){

    # define all the climate variables
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC", "rfc_solar", "rfc_volc", "rfc_LCC", "rfc_BCsnow", "rfc_O3s", "rfc_O3t", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST", "rfc_cloud", "rfc_H2Os", "r_ghg", "r_cm6ghg", "r_othanthro", "r_ofgs", "r_fgs", "r_co2", "r_ch4", "r_n2o", "r_o3", "r_h2o", "r_abd", "r_aer", "r_cld", "r_aer_all", "r_nat", "r_anthro", all_fgs_lst)

    # use parallel threads to speed up the process
    myCluster <- makeCluster(nthread, type = "FORK",  outfile="") # why "FORK"?
    registerDoParallel(myCluster)
    foreach(src = src_lst, .packages = c("stringr", "data.table"))%dopar%{#
        # get the climate variable sources
        src_tbl <- get_rfc_period(fol, reg, hsrc, psrc, sce, tag, src, obs_src)
        
        if(!is.null(src_tbl)){
            year_lst <- as.numeric(colnames(src_tbl))
            dst_mtx <- data.frame(year=year_lst,t(src_tbl))
            colnames(dst_mtx) <- c("year",c(1:(ncol(dst_mtx)-1)))
            
            mon_mtx <- adjust_to_month(dst_mtx)

            colnames(mon_mtx) <- c("yr",c(1:(ncol(mon_mtx)-1)))

            # save to the monthly matrices
            mon_csv <- paste0(cali_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_mtx.csv")

            write.csv(mon_mtx, file = mon_csv, row.names=F)
            print(paste0(mon_csv, " saved."))   
        }else{
            print(paste0(src, " not found."))
        }            
    }

    stopCluster(myCluster) 

    # create and save the observation matrices
    for(osrc in c("HadCRUTv5.0","HadCRUTv5.0.2","HadCRUTv4.6","GlobalTemp_v5.1","GlobalTemp_v6","GISTEMPv4","Berkeley_Earth","JMA", "DCENT")){
        obs_mtx <- get_obs_ens_m_1000(osrc)

        obs_dst <- data.frame(year=colnames(obs_mtx), t(obs_mtx))
        colnames(obs_dst) <- c("yr",c(1:(ncol(obs_dst)-1)))
        obs_dst$yr <- as.character(obs_dst$yr)

        # save to the monthly matrices
        mon_csv <- paste0(cali_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_obs_", tag ,"_", osrc,"_mtx.csv")

        write.csv(obs_dst, file = mon_csv, row.names=F)
        print(paste0(mon_csv, " saved."))   
    }

}

# perform the multiple regressions
# src_mtx: the matrix of the climate variables
# exp_tag: the experiment tag to identify how the regression was calculated
# start_yr: the start year for the regression
est_rfc_stat <- function(src_mtx, exp_tag, start_yr=1850){
    # read the climate variability indices
    mei0 <- read_IV("cti1854",4)
    amo0 <- read_IV("ersst.v5.amo.dat.txt",0)
    pdo0 <- read_IV("ersst.v5.pdo.dat.txt",0)

    colnames(mei0) <- c("year","mei0")
    colnames(amo0) <- c("year","amo0")
    colnames(pdo0) <- c("year","pdo0")

    max_yr <- max(mei0$year)

    src_mtx <- merge(src_mtx,mei0,by=c("year"))
    src_mtx <- merge(src_mtx,amo0,by=c("year"))
    src_mtx <- merge(src_mtx,pdo0,by=c("year"))
     
    # read the climate variability index for the NAO if needed
    if(exp_tag%in%c("Nao","AntVolSolNao","AntVolSolAll","AntVolSolMeiAmoNao","AntVolSolMeiNao","AntVolSolAmoNao",
                    "AntVolSolPdoNao","AntVolSolNaoDmi","AntVolSolPdoNaoDmi","AntVolSolMeiPdoNao","AntVolSolMeiNaoDmi",
                    "AntVolSolAmoNaoDmi","AntVolSolAmoPdoNao","AntVolSolMeiAmoPdoNao","AntVolSolMeiAmoDmiNao","AntVolSolMeiPdoDmiNao","AntVolSolAmoPdoDmiNao")){
        nao0 <- read_IV("nao.long.csv",1)
        colnames(nao0) <- c("year","nao0")
        src_mtx <- merge(src_mtx,nao0,by=c("year"))        
    }

    # read the climate variability index for the DMI if needed
    if(exp_tag%in%c("Dmi","AntVolSolDmi","AntVolSolAll","AntVolSolMeiAmoDmi","AntVolSolMeiDmi","AntVolSolAmoDmi",
                    "AntVolSolPdoDmi","AntVolSolNaoDmi","AntVolSolPdoNaoDmi","AntVolSolMeiPdoDmi","AntVolSolMeiPdoNao",
                    "AntVolSolMeiNaoDmi","AntVolSolAmoNaoDmi","AntVolSolAmoPdoDmi","AntVolSolMeiAmoPdoDmi","AntVolSolMeiAmoDmiNao","AntVolSolMeiPdoDmiNao","AntVolSolAmoPdoDmiNao")){
        dmi0 <- read_IV("dmi.had.long.csv",2)
        colnames(dmi0) <- c("year","dmi0")
        src_mtx <- merge(src_mtx,dmi0,by=c("year"))
    }

    src_mtx <- subset(src_mtx,year<=max_yr&year>=start_yr)

    tem_all <- src_mtx

    # define the associated multiple regressions

    if(exp_tag=="Vol"){
        # for the regression with VOL
        tem_fit  <- lm(obs~rfc_volc,data=tem_all)
    }else if(exp_tag=="Sol"){
        # for the regression with SOL
        tem_fit  <- lm(obs~rfc_solar,data=tem_all)
    }else if(exp_tag=="Mei"){
        # for the regression with MEI
        tem_fit  <- lm(obs~mei0,data=tem_all)
    }else if(exp_tag=="Amo"){
        # for the regression with AMO
        tem_fit  <- lm(obs~amo0,data=tem_all)
    }else if(exp_tag=="Dmi"){
        # for the regression with DMI
        tem_fit  <- lm(obs~dmi0,data=tem_all)
    }else if(exp_tag=="Nao"){
        # for the regression with NAO
        tem_fit  <- lm(obs~nao0,data=tem_all)
    }else if(exp_tag=="Pdo"){
        # for the regression with PDO
        tem_fit  <- lm(obs~pdo0,data=tem_all)
    }else if(exp_tag=="Ghg"){
        # for the regression with GHG
        tem_fit  <- lm(obs~r_ghg,data=tem_all)
    }else if(exp_tag=="Ant"){
        # for the regression with APG
        tem_fit  <- lm(obs~r_anthro,data=tem_all)
    }else if(exp_tag=="GhgAer"){
        # for the regression with GHG and other APG
        tem_fit  <- lm(obs~r_ghg+r_othanthro,data=tem_all)
    }else if(exp_tag=="AntNat"){
        # for the regression with APG and NAT
        tem_fit  <- lm(obs~r_anthro+r_nat,data=tem_all)
    }else if(exp_tag=="AntVol"){
        # for the regression with APG and VOL
        tem_fit  <- lm(obs~r_anthro+rfc_volc,data=tem_all)
    }else if(exp_tag=="AntSol"){
        # for the regression with APG and SOL
        tem_fit  <- lm(obs~r_anthro+rfc_solar,data=tem_all)
    }else if(exp_tag=="AntVolSol"){
        # for the regression with APG, VOL and SOL
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar,data=tem_all)
    }else if(exp_tag=="AntVolSolMei"){
        # for the regression with APG, VOL, SOL and MEI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0,data=tem_all)
    }else if(exp_tag=="AntVolSolAmo"){
        # for the regression with APG, VOL, SOL and AMO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+amo0,data=tem_all)
    }else if(exp_tag=="AntVolSolPdo"){
        # for the regression with APG, VOL, SOL and PDO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+pdo0,data=tem_all)
    }else if(exp_tag=="AntVolSolNao"){
        # for the regression with APG, VOL, SOL and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+nao0,data=tem_all)
    }else if(exp_tag=="AntVolSolDmi"){
        # for the regression with APG, VOL, SOL and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiAmo"){
        # for the regression with APG, VOL, SOL, MEI and AMO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+amo0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiPdo"){
        # for the regression with APG, VOL, SOL, MEI and PDO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+pdo0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiNao"){
        # for the regression with APG, VOL, SOL, MEI and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+nao0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiDmi"){
        # for the regression with APG, VOL, SOL, MEI and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolAmoPdo"){
        # for the regression with APG, VOL, SOL, AMO and PDO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+amo0+pdo0,data=tem_all)
    }else if(exp_tag=="AntVolSolAmoDmi"){
        # for the regression with APG, VOL, SOL, AMO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+amo0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolAmoNao"){
        # for the regression with APG, VOL, SOL, AMO and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+amo0+nao0,data=tem_all)
    }else if(exp_tag=="AntVolSolPdoNao"){
        # for the regression with APG, VOL, SOL, PDO and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+pdo0+nao0,data=tem_all)
    }else if(exp_tag=="AntVolSolPdoDmi"){
        # for the regression with APG, VOL, SOL, PDO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+pdo0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolNaoDmi"){
        # for the regression with APG, VOL, SOL, NAO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+nao0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolPdoNaoDmi"){
        # for the regression with APG, VOL, SOL, PDO, NAO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+pdo0+nao0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiPdoDmi"){
        # for the regression with APG, VOL, SOL, MEI, PDO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+pdo0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiPdoNao"){
        # for the regression with APG, VOL, SOL, MEI, PDO and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+pdo0+nao0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiNaoDmi"){
        # for the regression with APG, VOL, SOL, MEI, NAO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+nao0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolAmoNaoDmi"){
        # for the regression with APG, VOL, SOL, AMO, NAO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+amo0+nao0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolAmoPdoDmi"){
        # for the regression with APG, VOL, SOL, AMO, PDO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+amo0+pdo0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolAmoPdoNao"){
        # for the regression with APG, VOL, SOL, AMO, PDO and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+amo0+pdo0+nao0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiAmoDmi"){
        # for the regression with APG, VOL, SOL, MEI, AMO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+amo0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiAmoNao"){
        # for the regression with APG, VOL, SOL, MEI, AMO and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+amo0+nao0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiAmoPdo"){
        # for the regression with APG, VOL, SOL, MEI, AMO and PDO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+amo0+pdo0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiAmoPdoDmi"){
        # for the regression with APG, VOL, SOL MEI, AMO, PDO and DMI
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+amo0+pdo0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiAmoPdoNao"){
        # for the regression with APG, VOL, SOL MEI, AMO, PDO and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+amo0+pdo0+nao0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiAmoDmiNao"){
        # for the regression with APG, VOL, SOL MEI, AMO, DMI and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+amo0+nao0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolMeiPdoDmiNao"){
        # for the regression with APG, VOL, SOL MEI, PDO, DMI and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+pdo0+nao0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolAmoPdoDmiNao"){
        # for the regression with APG, VOL, SOL AMO, PDO, DMI and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+amo0+pdo0+nao0+dmi0,data=tem_all)
    }else if(exp_tag=="AntVolSolAll"){
        # for the regression with APG, VOL, SOL, MEI, AMO, PDO, DMI and NAO
        tem_fit  <- lm(obs~r_anthro+rfc_volc+rfc_solar+mei0+amo0+pdo0+nao0+dmi0,data=tem_all)
    }


    # collect the estimated values
    tem_est <- tem_all

    tem_est$est <- predict(tem_fit)

    tem_est$resi <-  tem_fit$residuals 

    # calculate annual mean
    tem_out <- tem_est
    tem_out$yr <- trunc(tem_out$year)

    agg_mtx <- aggregate(cbind(obs_original,est)~yr, data = tem_out, FUN=mean)

    # print(summary(tem_fit))
    
    print(cor(agg_mtx$est,agg_mtx$obs_original))
    # return the results, estr means the correlation between the estimated and observed annual mean values
    tem_lst <- list(fitm=tem_fit,esti=tem_est,estr=cor(agg_mtx$est,agg_mtx$obs_original))

    return(tem_lst)

}

# save all the regression results
# fol: the folder for saving source files
# reg: the region
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the tag to identify the variables, here refers to "fscm_tatm" for the attributed DetaT induced by the associated sources
# osrc: the observed source
# exp_tag: the tag to identify the regression
sav_est_rfc_stat_mtx <- function(fol, reg, hsrc, psrc, sce, tag, osrc, exp_tag){
    # get the associated climate variables
    anthro_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, c("r_anthro"), obs_src=osrc) 
    ghg_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, c("r_ghg"), obs_src=osrc) 
    otherAnthro_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, c("r_othanthro"), obs_src=osrc) 
    nat_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, c("r_nat"), obs_src=osrc) 
    volc_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, c("rfc_volc"), obs_src=osrc)
    solar_mtx <- get_rfc_mtx(fol, reg, hsrc, psrc, sce, tag, c("rfc_solar"), obs_src=osrc)

    # get the observed values
    obs_mtx <- get_obs_ens_m_1000(osrc,PreIndBasis=F)

    max_yr <- as.numeric(colnames(obs_mtx)[ncol(obs_mtx)])
    nrw <- nrow(ghg_mtx)

    obs_ens <- 1
    # HadCRUTv5.0.2 has an ensemble of 200 members
    if(osrc=="HadCRUTv5.0.2")  {
        obs_ens <- 200
    }

    btp_ens <- 100
    index <- 0

    est_cof <- NULL
    fit_cof <- NULL
    est_tem <- NULL
    est_res <- NULL
    est_obs <- NULL

    set.seed(123)

    for(i in c(1:nrw)){
        print(paste0("Processing ", i))
        # create the source matrix
        emu_mtx <- data.frame(year=as.numeric(colnames(ghg_mtx)),r_anthro=(anthro_mtx[i,]),r_ghg=(ghg_mtx[i,]),r_othanthro=(otherAnthro_mtx[i,]),r_nat=(nat_mtx[i,]),rfc_volc=(volc_mtx[i,]),rfc_solar=(solar_mtx[i,]))

        tem_mon <- emu_mtx
        tem_mon <- subset(tem_mon, year<=max_yr)
        colnames(tem_mon) <- c("year","r_anthro","r_ghg","r_othanthro","r_nat","rfc_volc","rfc_solar")
        rownames(tem_mon) <- NULL

        o <- sample(1:obs_ens, 1, replace = TRUE)
        obs_df <- data.frame(year=as.numeric(colnames(obs_mtx)),obs=obs_mtx[o,])
        vari_mtx <- merge(tem_mon,obs_df,by=c("year"))
        vari_mtx$year <- as.numeric(as.character(vari_mtx$year))

        vari_mtx$obs_original <- vari_mtx$obs
        
        # carry out the multiple regression
        vari_fit <- est_rfc_stat(vari_mtx, exp_tag)


        for (b in c(1:btp_ens)){
            esti_vt <- vari_fit$esti[,c("est")]
            resi_vt <- vari_fit$esti[,c("resi")]

            # bootstrap residuals
            nlen <- length(resi_vt)
            resi_resample <- sample(resi_vt, replace = TRUE)

            # bootstrap response values
            bootstrap_df <- data.frame(year=vari_fit$esti[,c("year")],obs=esti_vt + resi_resample)

            # get the lower and upper bound of the years
            min_year <- max(c(bootstrap_df$year[1],vari_mtx$year[1]))
            max_year <- min(c(bootstrap_df$year[nrow(bootstrap_df)],vari_mtx$year[nrow(vari_mtx)]))

            bootstrap_df <- subset(bootstrap_df, year>=min_year & year<=max_year)
            vari_mtx <- subset(vari_mtx, year>=min_year & year<=max_year)

            vari_mtx$obs <- bootstrap_df$obs

            vari_prime_fit <- est_rfc_stat(vari_mtx, exp_tag)
            # collect the results
            index <- index + 1
            est_vt <- c(index, summary(vari_prime_fit$fitm)$sigma,summary(vari_prime_fit$fitm)$r.squared, vari_prime_fit$estr, AIC(vari_prime_fit$fitm),BIC(vari_prime_fit$fitm))
        
            if(is.null(est_cof)){
                est_cof <- est_vt
            }else{
                est_cof <- rbind(est_cof,est_vt)
            }

            param_vt <- coef(vari_prime_fit$fitm)
            if(is.null(fit_cof)){
                fit_cof <- param_vt
            }else{
                fit_cof <- rbind(fit_cof,param_vt)
            }

            # collect the estimated results for AntVolSolMeiAmo, which were used for the analysis in the manuscript
            if(exp_tag%in%c("AntVolSolMeiAmo")){
                # collect the total sum
                if(is.null(est_tem)){
                    est_tem <- vari_prime_fit$esti[,c("year","est")]
                }else{
                    est_tem <- cbind(est_tem,vari_prime_fit$esti[,c("est")])
                }

                # collect the residuals
                if(is.null(est_res)){
                    est_res <- vari_prime_fit$esti[,c("year","resi")]
                }else{
                    est_res <- cbind(est_res,vari_prime_fit$esti[,c("resi")])
                }

                # collect the observed values
                if(is.null(est_obs)){
                    est_obs <- vari_prime_fit$esti[,c("year","obs_original")]
                }else{
                    est_obs <- cbind(est_obs,vari_prime_fit$esti[,c("obs_original")])
                }                
            }            
        }



    }

    est_cof <- as.data.frame(est_cof)
    
    colnames(est_cof) <- c("case","sigma","r.squared","estr","AIC","BIC")

    sub_dir <- paste0(osrc,"_stat_rfc/")
    
    # save the statistics for the multiple regression
    sav_csv <- paste0(slow_dat,sub_dir, "est_cof_", osrc,"_", exp_tag, ".csv")
    fwrite(est_cof, file = sav_csv, sep=",", row.names=F)
    print(paste0(sav_csv, " saved."))   

    # save the coefficients for the multiple regression
    sav_csv <- paste0(slow_dat,sub_dir, "est_fit_", osrc,"_", exp_tag, ".csv")
    fwrite(fit_cof, file = sav_csv, sep=",", row.names=F)
    print(paste0(sav_csv, " saved."))   

    # save the estimated results for AntVolSolMeiAmo, which were used for the analysis in the manuscript
    if(exp_tag%in%c("AntVolSolMeiAmo")){
        # save the total sum
        ncl <- ncol(est_tem) 
        colnames(est_tem) <- c("year",paste0("ens_",1:(ncl-1)))
        sav_csv <- paste0(slow_dat,sub_dir, "est_tem_", osrc,"_", exp_tag, ".csv")
        fwrite(est_tem, file = sav_csv, sep=",", row.names=F)
        print(paste0(sav_csv, " saved."))   

        # save the residuals
        ncl <- ncol(est_res) 
        colnames(est_res) <- c("year",paste0("ens_",1:(ncl-1)))
        sav_csv <- paste0(slow_dat,sub_dir, "est_res_", osrc,"_", exp_tag, ".csv")
        fwrite(est_res, file = sav_csv, sep=",", row.names=F)
        print(paste0(sav_csv, " saved."))   

        # save the observed values
        ncl <- ncol(est_obs) 
        colnames(est_obs) <- c("year",paste0("ens_",1:(ncl-1)))
        sav_csv <- paste0(slow_dat,sub_dir, "est_obs_", osrc,"_", exp_tag, ".csv")
        fwrite(est_obs, file = sav_csv, sep=",", row.names=F)
        print(paste0(sav_csv, " saved."))               
    }

    
}

# save the all the regression results for the climate variables of the ensemble members
# fol: the folder for saving source files
# reg: the region
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the tag to identify the variables, here refers to "fscm_tatm" for the attributed DetaT induced by the associated sources
# obs_src: the observed source
# exp_tag: the tag to identify the regression

# Please be careful with your storage space as individual files can be as large as 9GB!
sav_stat_all <- function(fol, reg, hsrc, psrc, sce, tag, obs_src, exp_tag="AntVolSolMeiAmo"){

    # all halogenated gases excluding those that were not available in the observations
    all_fgs <- all_fgs_lst[!all_fgs_lst%in%c("hfc134", "hfc143", "hfc41")]

    # all the climate variables
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC", "rfc_solar", "rfc_volc", "rfc_LCC", "rfc_BCsnow", "rfc_O3s", "rfc_O3t", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST", "rfc_cloud", "rfc_H2Os", "vari", "mei", "amo", "resi", "obs", "r_ghg", "r_othanthro", "r_ofgs", "r_fgs", "r_o3", "r_h2o", "r_abd", "r_aer", "r_cld", "r_aer_all", "r_nat", "r_anthro", "est", all_fgs)

    # use parallel computing to speed up the process
    nthread <- 20
    myCluster <- makeCluster(nthread, type = "FORK",  outfile="")
    registerDoParallel(myCluster)#
    foreach(src = src_lst, .packages = "data.table")%dopar%{
        print(src)
        # HadCRUTv5.0.2 has an ensemble of 200 members

        stat_mtx <- NULL

        # get the climate variables of the ensemble members
        tem_mtx <- get_stat_ens_mtx(fol, reg, hsrc, psrc, sce, tag, src, obs_src, exp_tag)
        if(!is.null(tem_mtx)){
            if(is.null(stat_mtx)){
                stat_mtx <- tem_mtx
            }else{
                stat_mtx <- rbind(stat_mtx, tem_mtx)
            }                
        }


        # save climate variables of the ensemble members
        if(!is.null(stat_mtx)){
            sub_dir <- paste0(obs_src,"_stat_rfc/")
            sav_csv <- paste0(slow_dat,sub_dir, "est_", src,"_", obs_src,"_", exp_tag,"_all.csv")
            fwrite(stat_mtx, file = sav_csv, sep=",", row.names=F)
            print(paste0(sav_csv, " saved.")) 	            
        }

    }
    stopCluster(myCluster)
}

# save the annual lower and upper bounds of the climate variables of the ensemble members
# fol: the folder for saving source files
# reg: the region
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the tag to identify the variables, here refers to "fscm_tatm" for the attributed DetaT induced by the associated sources
# obs_src: the observed source
# exp_tag: the tag to identify the regression
sav_stat_annual_all <- function(fol, reg, hsrc, psrc, sce, tag, obs_src, exp_tag="AntVolSolMeiAmo"){

    # all halogenated gases excluding those that were not available in the observations
    all_fgs <- all_fgs_lst[!all_fgs_lst%in%c("hfc134", "hfc143", "hfc41")]

    # all the climate variables
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC", "rfc_solar", "rfc_volc", "rfc_LCC", "rfc_BCsnow", "rfc_O3s", "rfc_O3t", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST", "rfc_cloud", "rfc_H2Os", "vari", "mei", "amo", "resi", "obs", "r_ghg", "r_othanthro", "r_ofgs", "r_fgs", "r_o3", "r_h2o", "r_abd", "r_aer", "r_cld", "r_aer_all", "r_nat", "r_anthro", "est", all_fgs)

    # use parallel computing to speed up the process
    nthread <- 20
    myCluster <- makeCluster(nthread, type = "FORK",  outfile="")
    registerDoParallel(myCluster)#
    foreach(src = src_lst, .packages = "data.table")%dopar%{
        print(src)

        stat_mtx <- NULL

            
        # get the annual lower and upper bounds of the climate variables of the ensemble members
        tem_mtx <- get_stat_ens_annual_mtx(fol, reg, hsrc, psrc, sce, tag, src, obs_src, exp_tag)
        if(!is.null(tem_mtx)){
            if(is.null(stat_mtx)){
                stat_mtx <- tem_mtx
            }else{
                stat_mtx <- rbind(stat_mtx, tem_mtx)
            }                
        }

        # save annual lower and upper bounds of the climate variables of the ensemble members
        if(!is.null(stat_mtx)){
            sub_dir <- paste0(obs_src,"_stat_rfc/")
            sav_csv <- paste0(slow_dat,sub_dir, "est_", src,"_", obs_src,"_", exp_tag,".csv")
            fwrite(stat_mtx, file = sav_csv, sep=",", row.names=F)
            print(paste0(sav_csv, " saved.")) 	            
        }

    }
    stopCluster(myCluster)
}

# save the final results for the analysis in the manuscript
# fol: the folder for saving source files
# reg: the region
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the tag to identify the variables, here refers to "fscm_tatm" for the attributed DetaT induced by the associated sources
# obs_src: the observed source
# exp_tag: the tag to identify the regression
sav_stat_final <- function(fol, reg, hsrc, psrc, sce, tag, obs_src, exp_tag="AntVolSolMeiAmo"){

    # all the climate variables
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC", "rfc_solar", "rfc_volc", "rfc_LCC", "rfc_BCsnow", "rfc_O3s", "rfc_O3t", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST", "rfc_cloud", "rfc_H2Os", "vari", "mei", "amo", "resi", "obs", "r_ghg", "r_cm6ghg", "r_othanthro", "r_ofgs", "r_fgs", "r_o3", "r_h2o", "r_abd", "r_aer", "r_cld", "r_aer_all", "r_nat", "r_anthro", "est", all_fgs_lst)

    for(src in src_lst){#
        print(src)
        sub_dir <- paste0(obs_src,"_stat_rfc/")
        var_csv <- paste0(slow_dat,sub_dir, "est_", src,"_", obs_src,"_", exp_tag,"_all.csv")
        if(file.exists(var_csv)){
            # load the matrix of the climate variables of the ensemble members
            stat_mtx <- data.frame(fread(var_csv, header = TRUE, check.names=F, sep=","), check.names=F)

            # calculate the lower and upper bounds of the climate variables of the ensemble members
            src_tbl <- get_qnt_par_mtx(stat_mtx, par_cores_no)

            src_sub <- data.frame(year=src_tbl$year, source=c(src), 
                                                    p025=src_tbl$p025,
                                                    p975=src_tbl$p975,
                                                    p500=src_tbl$p500,
                                                    mean=src_tbl$mean,
                                                    sd=src_tbl$sd)     

            # save the lower and upper bounds of the climate variables of the ensemble members
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src, "_", exp_tag,"_mean.csv")
            fwrite(src_sub, file = dst_csv, row.names=F)
            print(paste0(dst_csv, " saved."))     	

            # calculate the temperature increases for the temperature spike, i.e., the temperature increase from 2010-2021 to 2022-2024
            src_mtx <- data.frame(year=as.numeric(colnames(stat_mtx)),t(stat_mtx))

            prev_st_yr=2010
            prev_ed_yr=2021
            curr_st_yr=2022 
            curr_ed_yr=2024

            src_ref <- subset(src_mtx,year>=prev_st_yr&year<prev_ed_yr+1)
            src_bom <- subset(src_mtx,year>=curr_st_yr&year<curr_ed_yr+1)

            avg_ref <- colMeans(src_ref[,c(-1)])
            avg_bom <- colMeans(src_bom[,c(-1)])
                
            avg_inc <- avg_bom - avg_ref

            dst_sub <- data.frame(source=c(src), t(quantile(avg_inc,probs=c(0.025,0.5,0.975)))) 
            colnames(dst_sub) <- c("source","p025","p500","p975")  

            # save the temperature increases for the temperature spike
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp_tag, "_", prev_st_yr, "_", prev_ed_yr, "_", curr_st_yr, "_", curr_ed_yr,".csv")
            fwrite(dst_sub, file = dst_csv, row.names=F)
            print(paste0(dst_csv, " saved."))    

            # calculate the temperature increases for the temperature spike, i.e., the temperature increase from 2019-2021 to 2022-2024
            prev_st_yr=2019
            prev_ed_yr=2021
            curr_st_yr=2022 
            curr_ed_yr=2024

            src_ref <- subset(src_mtx,year>=prev_st_yr&year<prev_ed_yr+1)
            src_bom <- subset(src_mtx,year>=curr_st_yr&year<curr_ed_yr+1)

            avg_ref <- colMeans(src_ref[,c(-1)])
            avg_bom <- colMeans(src_bom[,c(-1)])
                
            avg_inc <- avg_bom - avg_ref

            dst_sub <- data.frame(source=c(src), t(quantile(avg_inc,probs=c(0.025,0.5,0.975)))) 
            colnames(dst_sub) <- c("source","p025","p500","p975")  
            
            # save the temperature increases for the temperature spike
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp_tag, "_", prev_st_yr, "_", prev_ed_yr, "_", curr_st_yr, "_", curr_ed_yr,".csv")
            fwrite(dst_sub, file = dst_csv, row.names=F)
            print(paste0(dst_csv, " saved."))  

            # calculate the linear trends for the temperature increase, i.e., the temperature increase from 1944-1976
            start_yr <- 1944
            end_yr <- 1976
            inc_ens <- get_decadal_inc_par_ens(src_mtx, start_yr, end_yr)

            # save the linear trends for the temperature increase
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp_tag, "_", start_yr, "_", end_yr,".csv")
            finc <- file(dst_csv, "wb")
            writeBin( paste(inc_ens, collapse="\n"), finc ) 
            close(finc)

            print(paste0(dst_csv, " saved."))  

            # calculate the linear trends for the temperature increase, i.e., the temperature increase from 1998-2012
            start_yr <- 1998
            end_yr <- 2012
            inc_ens <- get_decadal_inc_par_ens(src_mtx, start_yr, end_yr)

            # save the linear trends for the temperature increase
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp_tag, "_", start_yr, "_", end_yr,".csv")
            finc <- file(dst_csv, "wb")
            writeBin( paste(inc_ens, collapse="\n"), finc ) 
            close(finc)
            print(paste0(dst_csv, " saved."))       
        }

    }  
}

# save the final results (from annual lower and upper bounds) for the analysis in the manuscript
# fol: the folder for saving source files
# reg: the region
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the tag to identify the variables, here refers to "fscm_tatm" for the attributed DetaT induced by the associated sources
# obs_src: the observed source
# exp_tag: the tag to identify the regression
sav_stat_annual_final <- function(fol, reg, hsrc, psrc, sce, tag, obs_src, exp_tag="AntVolSolMeiAmo"){

    # all the climate variables
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC", "rfc_solar", "rfc_volc", "rfc_LCC", "rfc_BCsnow", "rfc_O3s", "rfc_O3t", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST", "rfc_cloud", "rfc_H2Os", "vari", "mei", "amo", "resi", "obs", "r_ghg", "r_cm6ghg", "r_othanthro", "r_ofgs", "r_fgs", "r_o3", "r_h2o", "r_abd", "r_aer", "r_cld", "r_aer_all", "r_nat", "r_anthro", "est", all_fgs_lst)

    for(src in src_lst){#
        print(src)
        sub_dir <- paste0(obs_src,"_stat_rfc/")
        var_csv <- paste0(slow_dat,sub_dir, "est_", src,"_", obs_src,"_", exp_tag,".csv")
        if(file.exists(var_csv)){
            stat_mtx <- data.frame(fread(var_csv, header = TRUE, check.names=F, sep=","), check.names=F)

            # calculate the lower and upper bounds of the climate variables of the ensemble members
            src_tbl <- get_qnt_par_mtx(stat_mtx, par_cores_no) 

            src_sub <- data.frame(year=src_tbl$year, source=c(src), 
                                                    p025=src_tbl$p025,
                                                    p975=src_tbl$p975,
                                                    p500=src_tbl$p500,
                                                    value=src_tbl$mean,
                                                    sd=src_tbl$sd)     

            # save the lower and upper bounds of the climate variables of the ensemble members
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src, "_", exp_tag,"_mean.csv")
            fwrite(src_sub, file = dst_csv, row.names=F)
            print(paste0(dst_csv, " saved."))     	

            src_mtx <- data.frame(year=as.numeric(colnames(stat_mtx)),t(stat_mtx))

            # calculate the temperature increases for the temperature spike, i.e., the temperature increase from 2010-2021 to 2022-2024
            prev_st_yr=2010
            prev_ed_yr=2021
            curr_st_yr=2022 
            curr_ed_yr=2024

            src_ref <- subset(src_mtx,year>=prev_st_yr&year<prev_ed_yr+1)
            src_bom <- subset(src_mtx,year>=curr_st_yr&year<curr_ed_yr+1)

            avg_ref <- colMeans(src_ref[,c(-1)])
            avg_bom <- colMeans(src_bom[,c(-1)])
                
            avg_inc <- avg_bom - avg_ref

            dst_sub <- data.frame(source=c(src), t(quantile(avg_inc,probs=c(0.025,0.5,0.975)))) 
            colnames(dst_sub) <- c("source","p025","p500","p975")  

            # save the temperature increases for the temperature spike
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp_tag, "_", prev_st_yr, "_", prev_ed_yr, "_", curr_st_yr, "_", curr_ed_yr,".csv")
            fwrite(dst_sub, file = dst_csv, row.names=F)
            print(paste0(dst_csv, " saved."))    

            # calculate the temperature increases for the temperature spike, i.e., the temperature increase from 2019-2021 to 2022-2024
            prev_st_yr=2019
            prev_ed_yr=2021
            curr_st_yr=2022 
            curr_ed_yr=2024

            src_ref <- subset(src_mtx,year>=prev_st_yr&year<prev_ed_yr+1)
            src_bom <- subset(src_mtx,year>=curr_st_yr&year<curr_ed_yr+1)

            avg_ref <- colMeans(src_ref[,c(-1)])
            avg_bom <- colMeans(src_bom[,c(-1)])
                
            avg_inc <- avg_bom - avg_ref

            dst_sub <- data.frame(source=c(src), t(quantile(avg_inc,probs=c(0.025,0.5,0.975)))) 
            colnames(dst_sub) <- c("source","p025","p500","p975")  

            # save the temperature increases for the temperature spike
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp_tag, "_", prev_st_yr, "_", prev_ed_yr, "_", curr_st_yr, "_", curr_ed_yr,".csv")
            fwrite(dst_sub, file = dst_csv, row.names=F)
            print(paste0(dst_csv, " saved."))  

            # calculate the linear trends for the temperature increase, i.e., the temperature increase from 1944-1976
            start_yr <- 1944
            end_yr <- 1976
            inc_ens <- get_decadal_inc_par_ens(src_mtx, start_yr, end_yr)

            # save the linear trends for the temperature increase
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp_tag, "_", start_yr, "_", end_yr,".csv")
            finc <- file(dst_csv, "wb")
            writeBin( paste(inc_ens, collapse="\n"), finc ) 
            close(finc)

            print(paste0(dst_csv, " saved."))  

            # calculate the linear trends for the temperature increase, i.e., the temperature increase from 1998-2012
            start_yr <- 1998
            end_yr <- 2012
            inc_ens <- get_decadal_inc_par_ens(src_mtx, start_yr, end_yr)

            # save the linear trends for the temperature increase
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp_tag, "_", start_yr, "_", end_yr,".csv")
            finc <- file(dst_csv, "wb")
            writeBin( paste(inc_ens, collapse="\n"), finc ) 
            close(finc)
            print(paste0(dst_csv, " saved."))     

            # calculate the linear trends for the temperature increase, i.e., the temperature increase from 1970-2012
            start_yr <- 1970
            end_yr <- 2012
            inc_ens <- get_decadal_inc_par_ens(src_mtx, start_yr, end_yr)

            # save the linear trends for the temperature increase
            dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp_tag, "_", start_yr, "_", end_yr,".csv")
            finc <- file(dst_csv, "wb")
            writeBin( paste(inc_ens, collapse="\n"), finc ) 
            close(finc)
            print(paste0(dst_csv, " saved."))       
        }

    }  
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

# save the rolling trends (from annual lower and upper bounds) for the temperature increase
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

# save all the climate variability indices for the multiple regressions
save_var_index_all()

# save the monthly climate variables as matrices for the multiple regressions
sav_cali_rfc_mtx_m("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm")


for(o in c(1:3)){
    myCluster <- makeCluster(nthread, type = "FORK",  outfile="")
    registerDoParallel(myCluster)
    foreach(i = c(1:length(exp_all_lst)), .packages = "data.table")%dopar%{
        exp_tag <- exp_all_lst[i]
        obs_src <- names(obs_lst)[o]
        # save all the regression results
        sav_est_rfc_stat_mtx("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src, exp_tag)
    }
    stopCluster(myCluster)
}

# save the all the regression results for the climate variables of the ensemble members
########## sav_stat_all("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="HadCRUTv5.0.2", exp_tag="AntVolSolMeiAmo")
########## sav_stat_all("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="GlobalTemp_v6", exp_tag="AntVolSolMeiAmo")
########## sav_stat_all("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="Berkeley_Earth", exp_tag="AntVolSolMeiAmo")

# save the final results for the analysis in the manuscript
########## sav_stat_final("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="HadCRUTv5.0.2", exp_tag="AntVolSolMeiAmo")
########## sav_stat_final("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="GlobalTemp_v6", exp_tag="AntVolSolMeiAmo")
########## sav_stat_final("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="Berkeley_Earth", exp_tag="AntVolSolMeiAmo")

# save the annual lower and upper bounds of the climate variables of the ensemble members
sav_stat_annual_all("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="HadCRUTv5.0.2", exp_tag="AntVolSolMeiAmo")
sav_stat_annual_all("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="GlobalTemp_v6", exp_tag="AntVolSolMeiAmo")
sav_stat_annual_all("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="Berkeley_Earth", exp_tag="AntVolSolMeiAmo")

# save the final results (from annual lower and upper bounds) for the analysis in the manuscript
sav_stat_annual_final("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="HadCRUTv5.0.2", exp_tag="AntVolSolMeiAmo")
sav_stat_annual_final("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="GlobalTemp_v6", exp_tag="AntVolSolMeiAmo")
sav_stat_annual_final("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="Berkeley_Earth", exp_tag="AntVolSolMeiAmo")

nthread <- 8

# define the climate variables for calculating the 11-year rolling trends 
roll_var_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "r_ghg", "r_othanthro", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")
# roll_var_lst <- c("rfc_volc", "rfc_solar", "mei", "amo","est", "obs")
for(var in roll_var_lst){
    for(k in c(11)){#seq(3,17,2)
    # # save the rolling trends for the temperature increase
        # ########## sav_stat_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", var, obs_src="HadCRUTv5.0.2", exp_tag="AntVolSolMeiAmo",k)
        # ########## sav_stat_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", var, obs_src="GlobalTemp_v6", exp_tag="AntVolSolMeiAmo",k)
        # ########## sav_stat_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", var, obs_src="Berkeley_Earth", exp_tag="AntVolSolMeiAmo",k)

        # save the rolling trends (from annual lower and upper bounds) for the temperature increase
        sav_stat_annual_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", var, obs_src="HadCRUTv5.0.2", exp_tag="AntVolSolMeiAmo",k)
        sav_stat_annual_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", var, obs_src="GlobalTemp_v6", exp_tag="AntVolSolMeiAmo",k)
        sav_stat_annual_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", var, obs_src="Berkeley_Earth", exp_tag="AntVolSolMeiAmo",k)

    }
 }