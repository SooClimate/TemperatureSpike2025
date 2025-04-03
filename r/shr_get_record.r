
# get the observed temperature record: HadCRUT.4.6.0.0
# return the result matrix
tem_hadcrut46 <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/HadCRUT.4.6.0.0.annual_ns_avg.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("HadCRUTv4.6"),year=tem_mtx$year, value=tem_mtx$median)
    return(tem_df)
}

# get the observed temperature record: HadCRUT.5.0.2.0
# return the result matrix
tem_hadcrut50 <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    colnames(tem_mtx) <- c("year","value","lc25","uc975")
    # print(head(tem_mtx))
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("HadCRUTv5.0.2"),year=tem_mtx$year, value=tem_mtx$value)
    return(tem_df)
}

# get the observed temperature record: JMA
# return the result matrix
tem_jma <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/JMA_mon_wld.csv")
    tem_mtx <- fread(tem_csv, header = TRUE, check.names=F, sep=",")

    colnames(tem_mtx) <- c("year",c(1:12))

    tem_df = reshape2::melt(tem_mtx,
                            id.vars=c("year"),
                            variable.name="month", na.rm=F)

    tem_df$year <- as.numeric(as.character(tem_df$year))
    tem_dst <- tem_df[
        with(tem_df, order(year, month)),
    ]
    tem_dst <- na.omit(tem_dst)
    tem_dst <- aggregate(.~year, data=tem_dst, FUN=mean)
    dst_df <- data.frame(source=c("JMA"),year=tem_dst$year, value=tem_dst$value)
    return(dst_df)
}

# get the observed temperature record: DCENT
# return the result matrix
tem_dcent <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/DCENT_mean.csv")
    tem_mtx <- data.frame(fread(tem_csv, header = F, skip=1, check.names=F, sep=" "), check.names=F)
    colnames(tem_mtx) <- c("name","year","lon","lat","value" )
    # print(head(tem_mtx))
    # tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("DCENT"),year=tem_mtx$year, value=tem_mtx$value)
    return(tem_df)
}

# get the observed monthly temperature record: HadCRUT.4.6.0.0
# return the result matrix
tem_hadcrut46_m <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/HadCRUT.4.6.0.0.median_m.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_df <- data.frame(source=c("HadCRUTv4.6"),year=round(tem_mtx$year+(tem_mtx$month-1)/12,3), value=tem_mtx$value)
    return(tem_df)
}

# get the observed monthly temperature record: HadCRUT.5.0.2.0
# return the result matrix
tem_hadcrut50_m <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/HadCRUT.5.0.2.0.analysis.summary_series.global.monthly.csv")
    tem_mtx <- fread(tem_csv, header = TRUE, check.names=F, sep=",")
    colnames(tem_mtx) <- c("year","value","lc25","uc975")

    time_mtx <- data.frame(str_split(tem_mtx$year, "-", simplify=TRUE))
    # print(time_mtx)
    colnames(time_mtx) <- c("year", "month")
    time_mtx$year <- as.numeric(as.character(time_mtx$year))
    time_mtx$month <- as.numeric(as.character(time_mtx$month))
    tem_df <- data.frame(source=c("HadCRUTv5.0.2"),year=round(time_mtx$year+(time_mtx$month-1)/12,3), value=tem_mtx$value)
    return(tem_df)
}

# get the observed monthly temperature record: JMA
# return the result matrix
tem_jma_m <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/JMA_mon_wld.csv")
    tem_mtx <- fread(tem_csv, header = TRUE, check.names=F, sep=",")

    colnames(tem_mtx) <- c("year",c(1:12))

    tem_df = reshape2::melt(tem_mtx,
                            id.vars=c("year"),
                            variable.name="month", na.rm=F)

    tem_df$year <- as.numeric(as.character(tem_df$year))
    tem_dst <- tem_df[
        with(tem_df, order(year, month)),
    ]
    tem_dst <- na.omit(tem_dst)

    tem_dst$year <- as.numeric(as.character(tem_dst$year))
    tem_dst$month <- as.numeric(as.character(tem_dst$month))
    tem_dst$value <- as.numeric(as.character(tem_dst$value))

    dst_df <- data.frame(source=c("JMA"),year=round(tem_dst$year+(tem_dst$month-1)/12,3), value=tem_dst$value)
    return(dst_df)
}

# get the observed monthly temperature record: DCENT
# return the result matrix
tem_dcent_m <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/DCENT_monmean.csv")
    tem_mtx <- data.frame(fread(tem_csv, header = F, skip=1, check.names=F, sep=" "), check.names=F)
    colnames(tem_mtx) <- c("name","year", "month", "lon","lat","value" )

    tem_dst <- na.omit(tem_mtx)

    tem_dst$year <- as.numeric(as.character(tem_dst$year))
    tem_dst$month <- as.numeric(as.character(tem_dst$month))
    tem_dst$value <- as.numeric(as.character(tem_dst$value))

    dst_df <- data.frame(source=c("DCENT"),year=round(tem_dst$year+(tem_dst$month-1)/12,3), value=tem_dst$value)
    return(dst_df)

    # print(dst_df)
}

# get the observed temperature record with ensemble: HadCRUT.5.0.2.0
# return the result matrix
tem_hadcrut502_ensemble <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/HadCRUT.5.0.2.0.analysis.ensemble_series.global.annual.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_mtx <- subset(tem_mtx, select=c("Time", paste0("Realization ", c(1:200))))
    colnames(tem_mtx) <- c("year", paste0("ens_", c(1:200)))
    # print(head(tem_mtx))
    return(data.frame(tem_mtx))    
}

# get the observed temperature record with ensemble: HadCRUT.4.6.0.0
# return the result matrix
tem_hadcrut46_ensemble <- function(){
    rslt_mtx <- NULL
    for(i in c(1:100)){
        tem_csv <- paste0(dat_dir, "sum/TEM/HadCRUT.4.6.0.0.annual_ns_avg_realisations/HadCRUT.4.6.0.0.annual_ns_avg.", i,".txt")
        # print(tem_csv)
        tem_mtx <- fread(tem_csv, header = F, check.names=F, sep=" ")

        tem_mtx <- tem_mtx[,c(1:2)]
        if(is.null(rslt_mtx)){
            rslt_mtx <- tem_mtx
        }else{
            rslt_mtx <- data.frame(rslt_mtx, tem_mtx[,2])
        }   
          
    }
    colnames(rslt_mtx) <- c("year", paste0("ens_", c(1:100)))      
    # print(head(tem_mtx))
    return(data.frame(rslt_mtx))    
}

# get the observed monthly temperature record with ensemble: HadCRUT.5.0.2.0
# return the result matrix
tem_hadcrut502_m_ensemble <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/HadCRUT.5.0.2.0.analysis.ensemble_series.global.monthly.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_mtx <- subset(tem_mtx, select=c("Time", paste0("Realization ", c(1:200))))

    time_mtx <- data.frame(str_split(tem_mtx$Time, "-", simplify=TRUE))

    colnames(time_mtx) <- c("year", "month")
    time_mtx$year <- as.numeric(as.character(time_mtx$year))
    time_mtx$month <- as.numeric(as.character(time_mtx$month))

    colnames(tem_mtx) <- c("year", paste0("ens_", c(1:200)))

    tem_mtx$year <- round(time_mtx$year+(time_mtx$month-1)/12,3)
    # print(head(tem_mtx))
    return(data.frame(tem_mtx))    
}

# get the observed monthly temperature record with ensemble: HadCRUT.4.6.0.0
# return the result matrix
tem_hadcrut46_m_ensemble <- function(){
    rslt_mtx <- NULL
    for(i in c(1:100)){
        tem_csv <- paste0(dat_dir, "sum/TEM/HadCRUT.4.6.0.0.monthly_ns_avg_realisations/HadCRUT.4.6.0.0.monthly_ns_avg.", i,".txt")
        # print(tem_csv)
        tem_mtx <- fread(tem_csv, header = F, check.names=F, sep=" ")

        tem_mtx <- tem_mtx[,c(1:2)]
        if(is.null(rslt_mtx)){
            rslt_mtx <- tem_mtx
        }else{
            rslt_mtx <- data.frame(rslt_mtx, tem_mtx[,2])
        }   
          
    }
    colnames(rslt_mtx) <- c("year", paste0("ens_", c(1:100)))      
    # print(head(tem_mtx))
    time_mtx <- data.frame(str_split(rslt_mtx[,1], "/", simplify=TRUE))

    colnames(time_mtx) <- c("year", "month")
    time_mtx$year <- as.numeric(as.character(time_mtx$year))
    time_mtx$month <- as.numeric(as.character(time_mtx$month))

    rslt_mtx$year <- round(time_mtx$year+(time_mtx$month-1)/12,3)

    return(data.frame(rslt_mtx))    
}

# get the observed monthly temperature record with ensemble: DCENT
# return the result matrix
tem_dcent_m_ensemble <- function(){
    rslt_mtx <- NULL
    for(i in c(1:200)){
        tem_csv <- paste0(dat_dir, "sum/TEM/DCENT_ensemble_monmean/DCENT_ensemble_1850_2023_member_", str_pad(i, 3, pad = "0"),"_temperature.csv")
        # print(tem_csv)
        tem_mtx <- data.frame(fread(tem_csv, header = F, skip=1, check.names=F, sep=" "), check.names=F)
        colnames(tem_mtx) <- c("name","year", "month", "lon","lat","value" )

        tem_mtx <- na.omit(tem_mtx)
# print(head(tem_mtx))
        tem_mtx <- tem_mtx[,c(2,3,6)]
        if(is.null(rslt_mtx)){
            rslt_mtx <- tem_mtx
        }else{
            rslt_mtx <- data.frame(rslt_mtx, tem_mtx[,3])
        }   
          
    }
    colnames(rslt_mtx) <- c("year","month", paste0("ens_", c(1:200)))      

    rslt_mtx$year <- round(rslt_mtx$year+(rslt_mtx$month-1)/12,3)
    rslt_mtx <- rslt_mtx[,-c(2)]

    return(data.frame(rslt_mtx))    
}

# get the observed monthly temperature record
# osrc: observation source
# return the result matrix
get_obs_m <- function(osrc){
    dst_mtx <- NULL
    if(osrc=="HadCRUTv5.0"|osrc=="HadCRUTv5.0.2"){
        dst_mtx <- tem_hadcrut50_m()
    }else if(osrc=="HadCRUTv4.6"){
        dst_mtx <- tem_hadcrut46_m()
    }else if(osrc=="GlobalTemp_v5.1"|osrc=="GlobalTemp_v6"){
        dst_mtx <- tem_noaa5_m()
    }else if(osrc=="GISTEMPv4"){
        dst_mtx <- tem_gistem4_m()
    }else if(osrc=="Berkeley_Earth"){
        dst_mtx <- tem_berkeleyearth_aasi_m()
    }else if(osrc=="JMA"){
        dst_mtx <- tem_jma_m()
    }else if(osrc=="DCENT"){
        dst_mtx <- tem_dcent_m()
    }
    
    return(dst_mtx)
}

# get the observed annual temperature record
# osrc: observation source
# return the result matrix
get_obs <- function(osrc){
    dst_mtx <- NULL
    if(osrc=="HadCRUTv5.0"|osrc=="HadCRUTv5.0.2"){
        dst_mtx <- tem_hadcrut50()
    }else if(osrc=="HadCRUTv4.6"){
        dst_mtx <- tem_hadcrut46()
    }else if(osrc=="GlobalTemp_v5.1"){
        dst_mtx <- tem_noaa5()
    }else if(osrc=="GISTEMPv4"){
        dst_mtx <- tem_gistem4()
    }else if(osrc=="Berkeley_Earth"){
        dst_mtx <- tem_berkeleyearth_aasi()
    }else if(osrc=="JMA"){
        dst_mtx <- tem_jma()
    }else if(osrc=="DCENT"){
        dst_mtx <- tem_dcent()
    }
    
    return(dst_mtx)
}

# get the observed monthly temperature record with ensemble
# osrc: observation source
# return the result matrix
get_obs_ens_m <- function(osrc){
    dst_mtx <- NULL
    if(osrc=="HadCRUTv5.0"|osrc=="HadCRUTv5.0.2"){
        dst_mtx <- tem_hadcrut502_m_ensemble()
    }else if(osrc=="HadCRUTv4.6"){
        dst_mtx <- tem_hadcrut46_m_ensemble()
    }else if(osrc=="DCENT"){
        dst_mtx <- tem_dcent_m_ensemble()
    }
    return(dst_mtx)
}

# get the observed monthly temperature record with ensemble (n=1000)
# src: observation source
# PreIndBasis: Is it relative to 1850-1900?
# return the result matrix
get_obs_ens_m_1000 <- function(src, PreIndBasis=T){
    obs_1000 <- NULL
    if(src=="HadCRUTv5.0"|src=="HadCRUTv5.0.2"){
        obs_mtx <- tem_hadcrut502_m_ensemble()
        obs_df <- t(obs_mtx[,-1])
        colnames(obs_df) <- obs_mtx[,1]
        # obs_df <- subset(obs_df, select=as.character(c(1850:2020)))
        obs_1000 <- obs_df
        for(i in c(1:4)){
            obs_1000 <- rbind(obs_1000, obs_df)
        }
        rownames(obs_1000) <-  NULL
        # print(head(obs_1000))
    }else if(src=="HadCRUTv4.6"){
        obs_mtx <- tem_hadcrut46_m_ensemble()
        obs_df <- t(obs_mtx[,-1])
        colnames(obs_df) <- obs_mtx[,1]
        # obs_df <- subset(obs_df, select=as.character(c(1850:2020)))
        obs_1000 <- obs_df
        for(i in c(1:9)){
            obs_1000 <- rbind(obs_1000, obs_df)
        }
        rownames(obs_1000) <-  NULL
    }else if(src=="GlobalTemp_v5.1"|src=="GlobalTemp_v6"){
        obs_mtx <- tem_noaa5_m()
        obs_1000 <- obs_mtx$value
        for(i in c(1:999)){
            obs_1000 <- rbind(obs_1000, obs_mtx$value)
        }
        colnames(obs_1000) <- obs_mtx$year
        rownames(obs_1000) <-  NULL
    }else if(src=="GISTEMPv4"){
        obs_mtx <- tem_gistem4_m()
        obs_1000 <- obs_mtx$value
        for(i in c(1:999)){
            obs_1000 <- rbind(obs_1000, obs_mtx$value)
        }
        colnames(obs_1000) <- obs_mtx$year
        rownames(obs_1000) <-  NULL
    }else if(src=="Berkeley_Earth"){
        obs_mtx <- tem_berkeleyearth_aasi_m()
        obs_1000 <- obs_mtx$value
        for(i in c(1:999)){
            obs_1000 <- rbind(obs_1000, obs_mtx$value)
        }
        colnames(obs_1000) <- obs_mtx$year
        rownames(obs_1000) <-  NULL
    }else if(src=="JMA"){
        obs_mtx <- tem_jma_m()
        obs_1000 <- obs_mtx$value
        for(i in c(1:999)){
            obs_1000 <- rbind(obs_1000, obs_mtx$value)
        }
        colnames(obs_1000) <- obs_mtx$year
        rownames(obs_1000) <-  NULL
    }else if(src=="DCENT"){
        obs_mtx <- tem_dcent_m_ensemble()
        obs_df <- t(obs_mtx[,-1])
        colnames(obs_df) <- obs_mtx[,1]
        # obs_df <- subset(obs_df, select=as.character(c(1850:2020)))
        obs_1000 <- obs_df
        for(i in c(1:4)){
            obs_1000 <- rbind(obs_1000, obs_df)
        }
        rownames(obs_1000) <-  NULL
    }
# 

    if(PreIndBasis){
        yr_vt <- obs_mtx$year
        yr_sub <- yr_vt[yr_vt>=1850&yr_vt<=1900]
        
        for(i in c(1:1000)){
            obs_vt <- obs_1000[i,select=as.character(c(yr_sub))]
            # print(obs_vt)
            obs_ref <- mean(obs_vt)
            obs_1000[i,] <- obs_1000[i,] - obs_ref
        }        
    }

    return(obs_1000)
}

# get the rolling mean temperature record with ensemble (n=1000)
# src: observation source
# rmean: rolling mean
# return the result matrix
get_obs_roll_m_1000 <- function(src, rmean=0){
    src_mtx <- get_obs_ens_m_1000(src)

    if(rmean>0){
        # print(ncol(src_mtx))
        r_mtx <- rollmean(t(src_mtx), k = rmean, align="center", fill = NA)
        rownames(r_mtx) <- colnames((src_mtx))
        r_mtx <- na.omit(r_mtx)
        r_mtx <- t(r_mtx)
        
        src_mtx <- r_mtx
    }
    
    src_tbl <- get_qnt_mtx(src_mtx)

    src_sub <- data.frame(year=src_tbl$year, source=c(src), 
                                            value=src_tbl$mean,
                                            sd=src_tbl$sd,
                                            dif_mean=src_tbl$dif_mean,
                                            dif_sd=src_tbl$dif_sd,
                                            dif2_mean=src_tbl$dif2_mean,
                                            dif2_sd=src_tbl$dif2_sd)   
                                            
    return(src_sub)

}

# get the observed annual temperature record: NOAAGlobalTemp_v6.0.0
# return the result matrix
tem_noaa5 <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/NOAAGlobalTemp_v6.0.0.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("GlobalTemp_v5.1"),year=tem_mtx$date, value=tem_mtx$value)
    return(tem_df)
}

# get the observed monthly temperature record: NOAAGlobalTemp_v6.0.0
# return the result matrix
tem_noaa5_m <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/NOAAGlobalTemp_v6.0.0_monthly.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0

    time_mtx <- data.frame(str_split(tem_mtx$date, "-", simplify=TRUE))

    colnames(time_mtx) <- c("year", "month","day")
    time_mtx$year <- as.numeric(as.character(time_mtx$year))
    time_mtx$month <- as.numeric(as.character(time_mtx$month))
    time_mtx$day <- as.numeric(as.character(time_mtx$day))

    tem_df <- data.frame(source=c("GlobalTemp_v5.1"),year=round(time_mtx$year+(time_mtx$month-1)/12,3), value=tem_mtx$value)
    
    return(tem_df)
}

# get the observed temperature record: GISTEMPv4
# return the result matrix
tem_gistem4 <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/GISTEMPv4_GLB.Ts_dSST.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_mtx <- tem_mtx[-c(nrow(tem_mtx)),]
    # print(as.numeric(as.character(tem_mtx$J_D)))
    tem_df <- data.frame(source=c("GISTEMPv4"),year=as.numeric(as.character(tem_mtx$Year)), value=as.numeric(as.character(tem_mtx$J_D)))
    return(tem_df)
}

# get the observed monthly temperature record: GISTEMPv4
# return the result matrix
tem_gistem4_m <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/GISTEMPv4_GLB.Ts_dSST.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_mtx <- tem_mtx[-c(nrow(tem_mtx)),]

    tem_mtx <- tem_mtx[,c("Year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")]
    colnames(tem_mtx) <- c("year",c(1:12))

    tem_mlt = reshape2::melt(tem_mtx,
                            id.vars=c("year"),
                            variable.name="value", na.rm=TRUE)

    colnames(tem_mlt) <- c("year","month","value")

    tem_mlt <- tem_mlt[
        with(tem_mlt, order(year, month)),
    ]

    tem_mlt$year <- as.numeric(as.character(tem_mlt$year))
    tem_mlt$month <- as.numeric(as.character(tem_mlt$month))
    tem_mlt$value <- as.numeric(as.character(tem_mlt$value))

    tem_df <- data.frame(source=c("GISTEMPv4"),year=round(tem_mlt$year+(tem_mlt$month-1)/12,3), value=tem_mlt$value)
    return(tem_df)
}

# get the observed annual temperature record: Berkeley Earth (Annual_above_sea_ice)
# return the result matrix
tem_berkeleyearth_aasi <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/berkeleyearth_Land_and_Ocean.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("Berkeley_Earth"),year=tem_mtx$Year, value=tem_mtx$Annual_above_sea_ice)
    return(tem_df)
}

# get the observed monthly temperature record: Berkeley Earth
# return the result matrix
tem_berkeleyearth_aasi_m <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/berkeleyearth_Land_and_Ocean_complete.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("Berkeley_Earth"),year=round(tem_mtx$year+(tem_mtx$month-1)/12,3), value=tem_mtx$monthly)
    return(tem_df)
}

# get the observed annual temperature record: Berkeley Earth (Annual_below_sea_ice)
# return the result matrix
tem_berkeleyearth_absi <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/berkeleyearth_Land_and_Ocean.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("Berkeley_Earth"),year=tem_mtx$Year, value=tem_mtx$Annual_below_sea_ice)
    return(tem_df)
}

# get the observed temperature record: coverage2013_cobe2cru_krig_v2_0_0
# return the result matrix
tem_cobe2cru_krig2 <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/coverage2013_cobe2cru_krig_v2_0_0.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("Coverage2013_cobe2cru_krig_v2"),year=tem_mtx$date, value=tem_mtx$value)
    return(tem_df)
}

# get the observed temperature record: coverage2013_cru4_krig_v2_0_0
# return the result matrix
tem_cru4_krig2 <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/coverage2013_cru4_krig_v2_0_0.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("Coverage2013_cru4_krig_v2"),year=tem_mtx$year, value=tem_mtx$value)
    return(tem_df)
}

# get the observed temperature record: coverage2013_had4_krig_v2_0_0
# return the result matrix
tem_had4_krig2 <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/coverage2013_had4_krig_v2_0_0.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_mtx <- tem_mtx[-c(nrow(tem_mtx)),]
    tem_df <- data.frame(source=c("Coverage2013_had4_krig_v2"),year=as.numeric(as.character(tem_mtx$date)), value=as.numeric(as.character(tem_mtx$value)))
    return(tem_df)
}

# get the observed temperature record: coverage2013_had4sst4_krig_v2_0_0
# return the result matrix
tem_had4sst4_krig2 <- function(){
    tem_csv <- paste0(dat_dir, "sum/TEM/coverage2013_had4sst4_krig_v2_0_0.csv")
    tem_mtx <- read.table(tem_csv, header = TRUE, check.names=F, sep=",")
    tem_mtx[is.na(tem_mtx)] <- 0
    tem_df <- data.frame(source=c("Coverage2013_had4sst4_krig_v2"),year=tem_mtx$date, value=tem_mtx$value)
    return(tem_df)
}

# get the CO2 concentration: CMIP5
# return the result matrix
co2_cmip5 <- function(){
    co2_csv <- paste0(dat_dir, "sum/CO2/conc_cmip5.csv")
    co2_mtx <- read.table(co2_csv, header = TRUE, check.names=F, sep=",", skip=1)
    co2_mtx[is.na(co2_mtx)] <- 0
    co2_df <- data.frame(source=c("CMIP5"),year=co2_mtx$year, value=co2_mtx$CO2)
    return(co2_df)    
}

# get the CO2 concentration: CMIP6
# return the result matrix
co2_cmip6 <- function(){
    co2_csv <- paste0(dat_dir, "sum/CO2/CMIP6_concentration.csv")
    co2_mtx <- read.table(co2_csv, header = TRUE, check.names=F, sep=",", skip=1)
    co2_mtx[is.na(co2_mtx)] <- 0
    co2_df <- data.frame(source=c("CMIP6"),year=co2_mtx$year, value=co2_mtx$CO2)
    co2_df <- subset(co2_df, year>=1850)
    return(co2_df)    
}

# get the CO2 concentration: IPCC_AR5
# return the result matrix
co2_ar5 <- function(){
    co2_csv <- paste0(dat_dir, "sum/CO2/conc_ipcc_ar5.csv")
    co2_mtx <- read.table(co2_csv, header = TRUE, check.names=F, sep=",", skip=1)
    co2_mtx[is.na(co2_mtx)] <- 0
    co2_df <- data.frame(source=c("IPCC_AR5"),year=co2_mtx$year, value=co2_mtx$CO2)
    return(co2_df)    
}

# get the CO2 concentration: NOAA
# return the result matrix
co2_noaa_esrl_global <- function(){
    # co2_csv <- paste0(dat_dir, "sum/CO2/conc_noaa_esrl_global.csv")
    co2_csv <- paste0(dat_dir, "sum/CO2/noaa_co2_gr_gl.csv")
    co2_mtx <- read.table(co2_csv, header = TRUE, check.names=F, sep=",")
    # print(head(co2_mtx))
    co2_mtx[is.na(co2_mtx)] <- 0
    co2_df <- data.frame(source=c("NOAA_global"),year=co2_mtx$year, value=cumsum(co2_mtx$value)+315.98-co2_mtx$value[[1]])
    return(co2_df)    
}

# get the CO2 concentration: NOAA (MaunaLoa)
# return the result matrix
co2_noaa_esrl_maunaloa <- function(){
    co2_csv <- paste0(dat_dir, "sum/CO2/conc_noaa_esrl_maunaloa.csv")
    co2_mtx <- read.table(co2_csv, header = TRUE, check.names=F, sep=",", skip=1)
    co2_mtx[is.na(co2_mtx)] <- 0
    co2_df <- data.frame(source=c("NOAA_MaunaLoa"),year=co2_mtx$year, value=co2_mtx$CO2)
    return(co2_df)    
}

# get the CO2 concentration: NOAA (Law2006)
# return the result matrix
co2_noaa_ncdc_law2006 <- function(){
    co2_csv <- paste0(dat_dir, "sum/CO2/conc_noaa_ncdc_law2006.csv")
    co2_mtx <- read.table(co2_csv, header = TRUE, check.names=F, sep=",", skip=1)
    co2_mtx[is.na(co2_mtx)] <- 0
    co2_df <- data.frame(source=c("Law2006"),year=co2_mtx$year, value=co2_mtx$CO2)
    co2_df <- subset(co2_df, year>=1850)
    return(co2_df)    
}

# get the CO2 concentration: GCP
# return the result matrix
co2_gcp <- function(){
    ems_csv <- paste0(dat_dir, "sum/EMS/GCP2020_v1.csv")
    ems_mtx <- read.table(ems_csv, header = TRUE, check.names=F, sep=",")
    ems_mtx[is.na(ems_mtx)] <- 0
    ems_df <- data.frame(source=c("GCPCO2"),year=ems_mtx$year, value=cumsum(ems_mtx$growth)/2.123+278)
    ems_df <- subset(ems_df, year>=1850)
    return(ems_df)
}

# get the CO2 concentration: NOAA (MaunaLoa)
# src_csv: source csv file
# var: variable name
# skp: skip lines
# return the result matrix
noaa_mm_conc <- function(src_csv, var, skp){
    dst_mtx <- NULL

    if(file.exists(src_csv)){
		# load data frame from csv file
        csv_mtx <- data.frame(fread(src_csv, header = TRUE, sep = ",", check.names = FALSE, skip=skp,blank.lines.skip=TRUE), check.names = FALSE)

		# calculate the bimonthly average
        # csv_mtx <- data.frame(year=csv_mtx$year+round(((csv_mtx$month-1)%/%2)/scm_dt,3), value=csv_mtx[,c(var)])  
		csv_mtx <- data.frame(csv_mtx[,c("year","month",var)])

		colnames(csv_mtx) <- c("year","month", "value")

		# calculate the annual average
		dst_mtx <- aggregate(csv_mtx$value, list(csv_mtx$year), FUN=mean)
        colnames(dst_mtx) <- c("year", "value")
        # print(dst_mtx)
        dst_mtx <- data.frame(source=c("NOAA"), year=dst_mtx$year, value=dst_mtx$value)
    }

    # return the result
    return(dst_mtx)
}