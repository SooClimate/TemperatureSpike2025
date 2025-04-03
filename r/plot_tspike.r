source("r/spike_header.r")

# get the data matrix of the difference of the decadal trends (DDT) for the variables
# fol: folder name for the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src_lst: list of the climate variables
# prev_st_yr: start year for the previous period
# prev_ed_yr: end year for the previous period
# hiat_st_yr: start year for the hiatus period
# hiat_ed_yr: end year for the hiatus period
# obs_src: observation source
# return the data matrix
get_tem_inc_dif <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, prev_st_yr=1970, prev_ed_yr=2012, hiat_st_yr=1998, hiat_ed_yr=2012, obs_src="HadCRUTv5.0"){
    
    # get the data matrix of the decadal trends for the variables
    tem_ful <- get_tem_inc(fol, reg, hsrc, psrc, sce, tag, src_lst, prev_st_yr, prev_ed_yr, obs_src)        
    tem_hia <- get_tem_inc(fol, reg, hsrc, psrc, sce, tag, src_lst, hiat_st_yr, hiat_ed_yr, obs_src)    

    # create the data matrix using both the decadal trends
    tem_dif <- merge(tem_ful, tem_hia, by=c("index","source"))
    tem_dif <- tem_dif[order(tem_dif$index),]
# print(nrow(tem_dif))
    colnames(tem_dif) <- c("index", "source", "prev", "hiat")

    # calculate the difference of the decadal trends, including mean and sd values
    tem_dif$diff <- tem_dif$hiat - tem_dif$prev

    dif_avg <- NULL 
    for (src in unique(tem_dif$source)){
        sub_tem_dif <- subset(tem_dif, source==src)
        dif_vt <- data.frame(source=c(src), t(quantile(sub_tem_dif$diff, probs = c(0.025,0.5,0.975))))
        if(is.null(dif_avg)){
            dif_avg <- dif_vt
        }else{
            dif_avg <- rbind(dif_avg, dif_vt)
        }
    }



    dif_avg <- data.frame(dif_avg)
    
    colnames(dif_avg) <- c("source","p025", "p500", "p975")
    dif_mtx <- dif_avg
    
    dif_mtx <- dif_mtx %>% arrange(factor(source, levels = src_lst)) 

# print(dif_mtx)
    # return the data matrix
    return(dif_mtx)
}   

# get the data matrix of the variables
# fol: folder name for the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# src_lst: list of the climate variables
# prev_st_yr: start year for the previous period
# prev_ed_yr: end year for the previous period
# curr_st_yr: start year for the current period
# curr_ed_yr: end year for the current period
# obs_src: observation source
# exp: the regression model
# return the data matrix
get_tem_bom <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, prev_st_yr=2010, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024, obs_src="HadCRUTv5.0.2", exp="AntVolSolMeiAmo"){
    
    tem_mtx <- NULL

    # iterate over the climate variables
    for(src in src_lst){

        for(sec in def_sector){

            # define the csv file name
            src_csv <- dst_csv <- paste0(slow_dat,  fol, "/", reg, "_", def_sector, "_", hsrc, "_",psrc, "_", sce, "_", src,"_", tag,"_",obs_src,"_", exp, "_", prev_st_yr, "_", prev_ed_yr, "_", curr_st_yr, "_", curr_ed_yr,".csv")
            # print(src_csv)
            if(file.exists(src_csv)){
                # print(src_csv)

                # read the data
                src_tbl <- data.frame(read.table(src_csv, header = TRUE, check.names=F, sep=","), check.names=F)

                # extract the data
                src_sub <- data.frame(source=c(src), p025=src_tbl$p025,	p500=src_tbl$p500,	p975=src_tbl$p975)   

                # append the data to the overall matrix
                if(is.null(tem_mtx)){
                    tem_mtx <- src_sub
                }else{
                    tem_mtx <- rbind(tem_mtx, src_sub)
                }         
            }else{
                print(paste0("Not found: ", src_csv))
            }          
        }


    }

    # return the data matrix
    return(tem_mtx)
}   

# get the solar cycle matrix
get_solar_cycle <- function(){
    cycle_mtx <- NULL
    cycle_csv <- paste0(dat_dir,"/solar/solar_cycle.csv")
    if(file.exists(cycle_csv)){
        cycle_mtx <- data.frame(read.table(cycle_csv, header = TRUE, check.names=F, sep=","), check.names=F)
    }
    return(cycle_mtx)
}

# plot the observation panel
# fol: folder name for the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: tag for the data
# obs_src: observation source
# start_yr: start year for the plot
# return the panel
plot_obs <- function(fol, reg, hsrc, psrc, sce, tag,obs_src, start_yr=1855){
    # get the observation data
    src_lst <- c("obs")
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)

    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))

    tem_mtx <- subset(src_mtx, year>=start_yr&year<2024+1)

    cm6_co2 <- co2_cmip6()
    cm6_roll <- get_roll_trend(cm6_co2, start_yr=1850, k=5)

    noa_co2 <- noaa_mm_conc("./data/sum/CO2/co2_mm_mlo.csv", "deseasonalized", 40)
    noa_roll <- get_roll_trend(noa_co2, start_yr=1850, k=5) 


	tem_min <- min(tem_mtx$p500, na.rm = TRUE)
	tem_max <- max(tem_mtx$p500, na.rm = TRUE)
	co2_min <- min(cm6_roll$p500)
	co2_max <- max(noa_roll$p500)

	tem_mtx$p500_scaled <- scales::rescale(tem_mtx$p500, to = c(co2_min, co2_max))
    tem_mtx$p025_scaled <- scales::rescale(tem_mtx$p025, to = c(co2_min, co2_max))
    tem_mtx$p975_scaled <- scales::rescale(tem_mtx$p975, to = c(co2_min, co2_max))

    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)

    # extract the data for the temperature spike period
    tem_ref <- subset(tem_mtx, year>=2010&year<2022)
    tem_bom <- subset(tem_mtx, year>=2022&year<2024+1)

    tem_ref <- aggregate(p500_scaled~source, data = tem_ref, FUN=mean)
    tem_bom <- aggregate(p500_scaled~source, data = tem_bom, FUN=mean)

    # extract the data for the cooling phase period and the warming slowdown period
    tem_1960 <- subset(tem_mtx, year>=1944&year<1976+1)
    tem_2005 <- subset(tem_mtx, year>=1998&year<2012+1)
    tem_1991 <- subset(tem_mtx, year>=1970&year<2012+1)

    tem_1960$cate <- tem_1960$source
    tem_2005$cate <- tem_2005$source
    tem_1991$cate <- tem_1991$source

    tem_ref$cate <- tem_ref$source
    tem_bom$cate <- tem_bom$source

    if(start_yr==1890){
        x_lims <- c(1890,2025)
        x_brks <- seq(1890,2025,20)
        x_lbls <- seq(1890,2025,20)
    }else{
        x_lims <- c(1850,2025)
        x_brks <- seq(1850,2025,25)
        x_lbls <- seq(1850,2025,25)        
    }

    v_mtx <- data.frame(cate=c("demo"),x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))


# print(head(noa_roll))
    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    lg_txt <- "Linear trend between"
    cl_val <- c("lt1960"=carto_pal(7, "ArmyRose")[c(1)],"lt2005"=carto_pal(7, "Earth")[c(1)],"lt1991"=carto_pal(7, "Geyser")[c(1)],"lt2022"=carto_pal(12, "Safe")[c(10)],"lt2016"=carto_pal(12, "Safe")[c(11)])
    tx_val <- c("lt1960"="1944-1976","lt2005"="1998-2012","lt1991"="1970-2012","lt2022"="2022-2024","lt2016"="2010-2021")
    lt_val <- c("lt1960"="solid","lt2005"="solid","lt1991"="longdash","lt2022"="solid","lt2016"="longdash")

    # plot the observation panel
    p <- ggplot() + 
        ggtitle(expression(Observed~atmospheric~CO[2]~and~global~warming~anomalies))+
        # labs(tag = expression(bold(A))) +

        # ylab(expression(italic(Delta*T)~relative~to~"1855-1900"~"("*degree*C*")"))+
        scale_y_continuous(
            name = expression(atop(Atmospheric~CO[2]~"5-year",running~"trend"~"(ppm per decade)")),
            sec.axis = sec_axis(
            transform = ~ scales::rescale(., from = c(co2_min, co2_max), to = c(tem_min, tem_max)),
            name = expression(italic(Delta*T)~relative~to~"1855-1900"~"("*degree*C*")")
        ))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        # scale_y_continuous() +

		geom_rect(data=v_mtx, aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
		geom_rect(data=v_mtx, aes(xmin=x3, xmax=x4, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_line(data=cm6_roll, aes(x=year, y=p500, colour="cmip6"), linewidth=line_size06, linetype="longdash") +
        geom_line(data=noa_roll, aes(x=year, y=p500, colour="noaa"), linewidth=line_size06, linetype="longdash") +
# 
        scale_colour_manual(name=expression(CO[2]~concentration),values=c("cmip6"=carto_pal(12, "Safe")[2],"noaa"=carto_pal(12, "Safe")[5]),
                            labels=c("cmip6"=expression(CMIP6),"noaa"=expression(NOAA~Mauna~Loa)),guide=guide_legend(nrow=2, order=1)) +
        # scale_colour_manual(guide=guide_legend(nrow=1, order=1)) +

        new_scale_color() +
        new_scale_fill() +

		geom_line(data=tem_mtx, aes(x=year, y=p500_scaled, colour=source), size=line_size1) +
		geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025_scaled, ymax = p975_scaled, fill=source), alpha=0.3) +

        scale_colour_manual(name="Temperature record",values=c("obs"=cl_emiss_neg),labels=c("obs"=parse(text=src_tag)),guide=guide_legend(order=2)) +
        scale_fill_manual(name="Temperature record",values=c("obs"=cl_emiss_neg),labels=c("obs"=parse(text=src_tag)),guide=guide_legend(order=2)) +

        new_scale_color() +
        new_scale_fill() +

        geom_segment(data = tem_ref, aes(x = c(2010), y = p500_scaled, xend = c(2022), yend = p500_scaled), colour="grey30", linetype="solid", size=0.5) +
        geom_segment(data = tem_bom, aes(x = c(2022), y = p500_scaled, xend = c(2024), yend = p500_scaled), colour="grey30", linetype="solid", size=0.5) +

        geom_segment(data = tem_ref, aes(x = c(1990), y = p500_scaled, xend = c(2010), yend = p500_scaled), colour="grey30", linetype="dashed", size=0.5) +
        geom_segment(data = tem_bom, aes(x = c(1990), y = p500_scaled, xend = c(2022), yend = p500_scaled), colour="grey30", linetype="dashed", size=0.5) +

        geom_segment(data = tem_ref, aes(x = 1996, y = p500_scaled+2, xend = 1996, yend = p500_scaled), color="grey30", linetype="solid", size=0.5, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = tem_bom, aes(x = 1996, y = p500_scaled-2, xend = 1996, yend = p500_scaled), color="grey30", linetype="solid", size=0.5, arrow = arrow(length = unit(0.2,"cm"))) +

        annotate(geom="text", x=1993, y=tem_ref$p500_scaled + 5.5, label="Temperature~spike", parse=T, size=4.5) +

         geom_text(aes(x = 1960, y = 1), label="italic(iii)",  color="black", size=4, parse=T) +
         geom_text(aes(x = 2005, y = 1), label="italic(ii)",  color="black", size=4, parse=T) +
         geom_text(aes(x = 2023, y = 1), label="italic(i)",  color="black", size=4, parse=T) +

        geom_segment(aes(x = 1934, y = -0.25, xend = 1944, yend = -0.25), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(aes(x = 1982, y = -0.25, xend = 1976, yend = -0.25), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(aes(x = 1944, y = -0.25, xend = 1976, yend = -0.25), color="grey30", linetype="longdash", size=0.3) +


        geom_segment(aes(x = 1993, y = -0.25, xend = 1998, yend = -0.25), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(aes(x = 2017, y = -0.25, xend = 2012, yend = -0.25), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(aes(x = 1998, y = -0.25, xend = 2012, yend = -0.25), color="grey30", linetype="longdash", size=0.3) +


        geom_segment(aes(x = 2019, y = -0.25, xend = 2022, yend = -0.25), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(aes(x = 2025, y = -0.25, xend = 2024, yend = -0.25), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(aes(x = 2022, y = -0.25, xend = 2024, yend = -0.25), color="grey30", linetype="dotted", size=0.3) +

         geom_smooth(data=tem_1960, aes(x=year, y=p500_scaled, linetype="lt1960", colour="lt1960", fill="lt1960"),method="lm", formula = y ~ x, linewidth=line_size06, alpha=0.2) +
         geom_smooth(data=tem_2005, aes(x=year, y=p500_scaled, linetype="lt2005", colour="lt2005", fill="lt2005"),method="lm", formula = y ~ x, linewidth=line_size06, alpha=0.2) +
         geom_smooth(data=tem_1991, aes(x=year, y=p500_scaled, linetype="lt1991", colour="lt1991", fill="lt1991"),method="lm", formula = y ~ x, linewidth=line_size06, alpha=0.2) +

        scale_colour_manual(name=lg_txt,values=cl_val,labels=tx_val,guide=guide_legend(nrow=3, order=3)) +
        scale_fill_manual(name=lg_txt,values=cl_val,labels=tx_val,guide=guide_legend(nrow=3, order=3)) +
        scale_linetype_manual(name=lg_txt,values=lt_val,labels=tx_val,guide=guide_legend(nrow=3, order=3)) +


        theme_few() +
        theme(
            plot.title = font_element3,
            # plot.tag = font_element4,
            legend.position = c(0.38,0.74),
            legend.spacing = unit(0.2, 'cm'),
            legend.box.spacing = unit(0.2, 'cm'),
            legend.margin = margin(0., 0., 0., 0.),
            legend.text = element_text(size=13),
            legend.title = element_text(size=13),
            legend.box = "horizontal",
            legend.direction = "vertical",
            legend.background = element_rect(fill='transparent'), #transparent legend bg
            axis.title.x = element_blank(),
            axis.title.y = font_element3,
            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = font_element3L) 

    # return the panel
    return(p)
}

# plot the regression panel
# fol: folder name for the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# return the panel
plot_mei_amo_demo <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=1850){
    stot <- function(x){
        return(subset(x,source%in%c("est")))
    }
    soth <- function(x){
        return(subset(x,!source%in%c("est")))
    }

    # define the climate variables used in the plot
    src_lst <- c("r_anthro", "rfc_solar", "rfc_volc", "mei", "amo","est")#"resi", 
    src_lst1 <- c("r_anthro", "rfc_volc", "rfc_solar", "mei", "amo", "total")#"resi",

    # get the temperature matrix of the climate variables
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)

    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))

    tem_mtx <- subset(src_mtx, year>=start_yr&year<2024+1)
    
    cor_tx12 <- tx_rfc_mapping3[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- tem_mtx$source
    tem_mtx$cate <- gsub("obs","total",tem_mtx$cate,fixed=T)
    tem_mtx$cate <- gsub("est","total",tem_mtx$cate,fixed=T)

    tem_mtx$source <-  factor(tem_mtx$source, levels  = (src_lst))

    tem_ref <- subset(tem_mtx, year>=2010&year<=2019)
    tem_bom <- subset(tem_mtx, year>=2020&year<2024+1)

    tem_ref <- aggregate(p500~source, data = tem_ref, FUN=mean)
    tem_bom <- aggregate(p500~source, data = tem_bom, FUN=mean)
    
    tem_ref$cate <- tem_ref$source
    tem_bom$cate <- tem_bom$source

    tem_ref$cate <- gsub("obs","total",tem_ref$cate,fixed=T)

    tem_bom$cate <- gsub("obs","total",tem_bom$cate,fixed=T)

    v_mtx <- data.frame(cate=c("demo"),x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    # define the x-axis limits and breaks and labels
    if(start_yr==1951){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1970, 1980, 1998, 2012,2020)
        x_lbls <- c("1951", "1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==1970){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1980, 1998, 2012,2020)
        x_lbls <- c("1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==2000){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1900){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,50)
        x_lbls <- seq(start_yr,2025,50)
    }else if(start_yr==1855){
        x_lims <- c(1850,2025)
        x_brks <- seq(1850,2025,25)
        x_lbls <- seq(1850,2025,25)
    }else{
        x_lims <- c(start_yr-1,2025)
        x_brks <- seq(start_yr,2025,20)
        x_lbls <- seq(start_yr,2025,20)        
    }
  
    cl_lst <- names(cl_rfc_mapping6)
    
    # plot the regression panel
    p_main <- ggplot() + 
        # labs(tag = expression(bold(C))) +
        ggtitle(expression(Global~warming~attribution))+
        ylab(expression(italic(Delta*T)~relative~to~"1855-1900"~"("*degree*C*")"))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        scale_y_continuous(limits = c(-0.52,1.65)) +
        

        geom_hline(yintercept=0, linetype="dotted", colour="grey50", linewidth=0.4) +

        geom_col(data=soth(tem_mtx), aes(x=year, y=p500, fill=source), position = "stack",alpha=0.8) +

        scale_fill_manual(limits=cl_lst[!cl_lst%in%c("est","obs")],
                          breaks=cl_lst[!cl_lst%in%c("est","obs")],
                          values=cl_rfc_mapping6[!names(cl_rfc_mapping6)%in%c("est","obs")], 
                          labels=tx_rfc_mapping11[!names(tx_rfc_mapping11)%in%c("est","obs")]) +

        guides(fill=guide_legend(override.aes=list(fill=NA,colour=NA))) +
        
        
        new_scale_fill() +
        new_scale_color() +

        geom_line(data=stot(tem_mtx), aes(x=year, y=p500, colour=source), linewidth=0.8) +
        geom_ribbon(data=stot(tem_mtx), aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.5) +

        scale_colour_manual(limits=cl_lst[cl_lst=="est"], breaks=cl_lst[cl_lst=="est"], values=cl_rfc_mapping6[names(cl_rfc_mapping6)=="est"], labels=tx_rfc_mapping11[names(tx_rfc_mapping11)=="est"]) +
        scale_fill_manual(limits=cl_lst[cl_lst=="est"], breaks=cl_lst[cl_lst=="est"],values=cl_rfc_mapping6[names(cl_rfc_mapping6)=="est"], labels=tx_rfc_mapping11[names(tx_rfc_mapping11)=="est"]) +
        
        theme_few() +
        theme(legend.position = c(0.5,0.09),
            plot.title = font_element3,
            # plot.tag = font_element4,
            legend.text = font_element3,
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.direction = "horizontal",

            legend.background = element_rect(fill = "transparent"),

            axis.title.x = element_blank(),
            axis.title.y = font_element3,
            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = element_blank()) 


    # plot the inset panel
    p_tot <- plot_rfc_regr_total2(fol, reg, hsrc, psrc, sce, tag, obs_src=obs_src,start_yr=start_yr)

    # combine the main panel and the inset panel
    p <-
        ggdraw() +
        draw_plot(p_main) +
        draw_plot(p_tot, x = 0.1, y = .45, width = .64, height = .42) 
        # draw_plot(p_run, x = 0.1, y = .4, width = .6, height = .35)

    return(p)
}

# plot the aerosol panel using HadCRUTv5.0.2
# fol: folder name for the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# plot the figure and return NULL
plot_mei_amo_aero_regr_full <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=1855){

    # define the climate variables used in the plot
    src_lst <- c("rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_cloud", "r_aer_all")
    src_lst1 <- c("rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_cloud", "r_aer_all")

    # get the temperature matrix of the climate variables
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)
# print(unique(src_mtx$source))
    # reshape the temperature matrix to wide format
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")

    tem_mtx <- subset(src_mtx, year>=start_yr&year<2024+1)
    
    cor_tx12 <- tx_aero_mapping[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    cor_tx12["obs"] <- paste0("bold(o)~", src_tag)

    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- tem_mtx$source

    # define the ranges for the climate variations
    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)
    tem_1960 <- subset(tem_mtx, year>=1944&year<1976+1)
    tem_2005 <- subset(tem_mtx, year>=1998&year<2012+1)
    tem_1991 <- subset(tem_mtx, year>=1970&year<2012+1)
    
    tem_ref <- subset(tem_mtx, year>=2010&year<=2021)
    tem_bom <- subset(tem_mtx, year>=2022&year<2024+1)

    tem_ref <- aggregate(p500~source, data = tem_ref, FUN=mean)
    tem_bom <- aggregate(p500~source, data = tem_bom, FUN=mean)

    tem_ref$cate <- tem_ref$source
    tem_bom$cate <- tem_bom$source

    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    nlen <- length(src_lst1)
    c_mtx <- data.frame(cate=src_lst1[c((nlen-2):nlen)],x1pos=c(1960),x2pos=c(2005),x3pos=c(2023),ypos=c(-0.185,0.0,-0.05))

    # define the x-axis limits and breaks and labels
    if(start_yr==1951){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1970, 1980, 1998, 2012,2020)
        x_lbls <- c("1951", "1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==1970){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1980, 1998, 2012,2020)
        x_lbls <- c("1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==2000){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1990){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1855){
        x_lims <- c(1850,2025)
        x_brks <- seq(1850,2025,50)
        x_lbls <- seq(1850,2025,50)
    }else{
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,20)
        x_lbls <- seq(start_yr,2025,20)        
    }
    

    # plot the figure
    p <- ggplot() + 
        ylab(expression(italic(Delta*T)~relative~to~"1855-1900"~"("*degree*C*")"))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        # scale_y_continuous() +
        geom_hline(yintercept=0, linetype="dotted", colour="grey50", linewidth=0.4) +

        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.5, , show.legend=F) +
        geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=F) +

        scale_colour_manual(values=cl_aero_mapping) +
        scale_fill_manual(values=cl_aero_mapping) +
        facet_wrap(~factor(cate, levels=src_lst1, labels = tx_aero_mapping), scales="free_y",ncol=2, labeller = label_parsed) +

        theme_few() +
        theme(legend.position = "bottom",#c(0.85,0.08)
            plot.title = font_element3,
            legend.text = font_element3,
            legend.title = font_element3,
            legend.box = "horizontal",
            legend.direction = "horizontal",

            axis.title.x = element_blank(),
            axis.title.y = font_element3,
            axis.text.y.right = element_blank(),
            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = font_element3) +
            guides(
               linetype=guide_legend(nrow=1)
               )  


    src_tag <- gsub("v5.0.2","v5_0_2",obs_src,fixed=T)

    # save the figure
    fl_plot = paste0(fig_dir,"fig_",src_tag, "_aero", fl_ext)
    ggsave(fl_plot,p, width = 24, height = 24, units = "cm")
    print(paste0(fl_plot, " saved"))

    # save the data
    sav_csv <- paste0(fig_dir,"fig_",obs_src, "_aero.csv")
    write.csv(tem_cst, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   
}

# plot the panel for halogenated gases using HadCRUTv5.0.2
# fol: folder name for the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# plot the figure and return NULL
plot_mei_amo_fgs_regr_full <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=1855){
    
    # define the climate variables used in the plot, excluding those not available in the observation data
    all_fgs <- all_fgs_lst[!all_fgs_lst%in%c("hfc134", "hfc143", "hfc41")]
    src_lst <- c(all_fgs, "r_fgs")
    src_lst1 <- c(all_fgs, "r_fgs")

    # get the temperature matrix of the climate variables
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)
    
# print(head(src_mtx))
    # reshape the temperature matrix to wide format
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,source!="halon1202",select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")

    tem_mtx <- subset(src_mtx, year>=start_yr&year<2024+1)
    
    cor_tx12 <- tx_rfc_mapping9[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    cor_tx12["obs"] <- paste0("bold(o)~", src_tag)

    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- tem_mtx$source

    # define the ranges for the climate variations
    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)
    tem_1960 <- subset(tem_mtx, year>=1944&year<1976+1)
    tem_2005 <- subset(tem_mtx, year>=1998&year<2012+1)
    tem_1991 <- subset(tem_mtx, year>=1970&year<2012+1)
    
    tem_ref <- subset(tem_mtx, year>=2010&year<2021+1)
    tem_bom <- subset(tem_mtx, year>=2022&year<2024+1)

    # print(tem_ref)
    # print(tem_bom)

    tem_ref <- aggregate(p500~source, data = tem_ref, FUN=mean)
    tem_bom <- aggregate(p500~source, data = tem_bom, FUN=mean)

    # print(tem_ref)
    # print(tem_bom)
    tem_ref$cate <- tem_ref$source
    tem_bom$cate <- tem_bom$source

    title_lbls <- data.frame(source=names(conc_name), labs=as.character(unlist(conc_name)))

    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    nlen <- length(src_lst1)
    c_mtx <- data.frame(cate=src_lst1[c((nlen-2):nlen)],x1pos=c(1960),x2pos=c(2005),x3pos=c(2023),ypos=c(-0.185,0.0,-0.05))

    # define the x-axis limits and breaks and labels
    if(start_yr==1951){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1970, 1980, 1998, 2012,2020)
        x_lbls <- c("1951", "1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==1970){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1980, 1998, 2012,2020)
        x_lbls <- c("1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==2000){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1990){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1855){
        x_lims <- c(1850,2025)
        x_brks <- seq(1850,2025,50)
        x_lbls <- seq(1850,2025,50)
    }else{
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,20)
        x_lbls <- seq(start_yr,2025,20)        
    }
    
    # plot the figure
    p <- ggplot() + 

        ylab(expression(italic(Delta*T)~relative~to~"1855-1900"~"("*degree*C*")"))+
        
        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
        geom_hline(yintercept=0, linetype="dotted", colour="grey50", linewidth=0.4) +

        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_line(data=tem_mtx, aes(x=year, y=p500), colour=carto_pal(12, "Safe")[2], size=0.5, , show.legend=F) +
        geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975), fill=carto_pal(12, "Safe")[2], alpha=0.3, show.legend=F) +

        geom_label(data = title_lbls, aes(x=1850,y=Inf,label=labs,hjust=ifelse(source=="r_fgs",0,-0.1),vjust=ifelse(source=="r_fgs",1.2,1.5)), fill=alpha(c("white"),0.3), parse = TRUE, size=3.5, label.size = NA, lineheight = 0.5) +

        facet_wrap(~factor(source, levels=names(conc_name)), scales="free_y",ncol=4) +

        theme_few() +
        theme(legend.position = "bottom",#c(0.85,0.08)
            plot.title = font_element3,
            legend.text = font_element2,
            legend.title = font_element2,
            legend.box = "horizontal",
            legend.direction = "horizontal",

            axis.title.x = element_blank(),
            axis.title.y = font_element3,
            axis.text.y.right = element_blank(),
            axis.text.x = element_text(size=11, hjust=0),
            axis.text.y = element_text(size=11, hjust=0),
            strip.text = element_blank()) +
            guides(
               linetype=guide_legend(nrow=1)
               )  


    src_tag <- gsub("v5.0.2","v5_0_2",obs_src,fixed=T)

    # save the figure
    fl_plot = paste0(fig_dir,"fig_",src_tag, "_fgs", fl_ext)
    ggsave(fl_plot,p, width = 24, height = 28, units = "cm")
    print(paste0(fl_plot, " saved"))

    # save the data
    sav_csv <- paste0(fig_dir,"fig_",obs_src, "_fgs.csv")
    write.csv(tem_cst, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   
}

# plot the panel for all climate variables using HadCRUTv5.0.2
# fol: folder name for the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# plot the figure and return NULL
plot_mei_amo_rfc_regr_full <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=1855){

    # define the climate variables used in the plot
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")#"resi", , "obs","est"
    src_lst1 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "est", "obs")#"resi",,, "total"

    # get the temperature matrix of the climate variables
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)
# print(unique(src_mtx$source))
    # reshape the temperature matrix to wide format
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")

    tem_mtx <- subset(src_mtx, year>=start_yr&year<2024+1)
    
    cor_tx12 <- tx_rfc_mapping9[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    tem_cst <- subset(tem_cst,year>=start_yr&year<2024+1)
    # print(tail(tem_cst))
    print(format(cor(detrend(tem_cst$obs),detrend(tem_cst$est)), digits=2, nsmall = 2))
    print(format(cor(tem_cst$obs,tem_cst$est), digits=2, nsmall = 2))

    cor_tx12["obs"] <- paste0("bold(O)~", src_tag)

    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- tem_mtx$source

    # define the ranges for the climate variations
    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)
    tem_1960 <- subset(tem_mtx, year>=1944&year<1976+1)
    tem_2005 <- subset(tem_mtx, year>=1998&year<2012+1)
    tem_1991 <- subset(tem_mtx, year>=1970&year<2012+1)
    
    tem_ref <- subset(tem_mtx, year>=2010&year<2021+1)
    tem_bom <- subset(tem_mtx, year>=2022&year<2024+1)

    # print(tem_ref)
    # print(tem_bom)

    tem_ref <- aggregate(p500~source, data = tem_ref, FUN=mean)
    tem_bom <- aggregate(p500~source, data = tem_bom, FUN=mean)

    # print(tem_ref)
    # print(tem_bom)
    tem_ref$cate <- tem_ref$source
    tem_bom$cate <- tem_bom$source

    # define the positions for the labels
    src_hjust <- c(
        "rfc_CO2"     = -0.5,
        "rfc_CH4"     = -0.5,
        "rfc_N2O"     = -0.5,
        "r_fgs"       = -0.15,
        "rfc_FODS"    = -0.5,
        "rfc_FHFC"    = -0.5, 
        "rfc_FPFC"    = -0.5,
        "r_o3"        = -0.4,    
        "rfc_O3s"     = -0.5,
        "rfc_O3t"     = -0.5,
        "rfc_H2Os"    = -0.3,
        "r_abd"       = -0.1,
        "rfc_LCC"     = -0.5,
        "rfc_BCsnow"  = -0.5,
        "r_aer"       = -0.2,
        "rfc_cloud"   = -0.3,
        "rfc_volc"    = -0.5,
        "rfc_solar"   = -0.5,
        "vari"        = -0.5,
        "mei"         = -1,
        "amo"         = -0.5,
        "resi"        = -0.5, 
        "est"        = -0.5,
        "obs"        = -0.5
 
    )

    src_vjust <- c(
        "rfc_CO2"     = 1.5,
        "rfc_CH4"     = 1.5,
        
        "rfc_N2O"     = 1.5,
        "r_fgs"       = 1.5,
        "rfc_FODS"    = 1.5,
        "rfc_FHFC"    = 1.5, 
        "rfc_FPFC"    = 1.5,
        "r_o3"        = 1.5,
        "rfc_O3s"     = 1.5,
        "rfc_O3s"     = 1.5,
        "rfc_O3t"     = 1.5,
        "rfc_H2Os"    = 1.5,
        "r_abd"       = 1.5,
        "rfc_LCC"     = 1.5,
        "rfc_BCsnow"  = 1.5,
        "r_aer"       = 8,
        "rfc_cloud"   = 8,
        "rfc_volc"    = 6.5,
        "rfc_solar"   = 1.5,
        
        "vari"        = 1.5,
        "mei"        = 1.5,
        "amo"        = 1.5,
        "resi"        = 1.5, 
        "est"         = 1.5,
        "obs"         = 1.5
    )


    title_lbls <- data.frame(cate=src_lst1,h_just=as.vector(unlist(src_hjust[src_lst1], use.names=FALSE)),
                                            v_just=as.vector(unlist(src_vjust[src_lst1], use.names=FALSE)),
                                            labs=as.character(unlist(cor_tx12[src_lst1])))
    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    nlen <- length(src_lst1)
    c_mtx <- data.frame(cate=src_lst1[c((nlen-2):nlen)],x1pos=c(1960),x2pos=c(2005),x3pos=c(2023),ypos=c(-0.2,-0.15,-0.2))

    # define x-axis limits and breaks and labels
    if(start_yr==1951){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1970, 1980, 1998, 2012,2020)
        x_lbls <- c("1951", "1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==1970){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1980, 1998, 2012,2020)
        x_lbls <- c("1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==2000){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1990){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1855){
        x_lims <- c(1850,2025)
        x_brks <- seq(1850,2025,50)
        x_lbls <- seq(1850,2025,50)
    }else{
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,20)
        x_lbls <- seq(start_yr,2025,20)        
    }
    

    # define indicators for the main volcanic eruptions
    volc_lst <- data.frame(cate=c("rfc_volc"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))

    volc_est_lst <- data.frame(cate=c("est"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))


    volc_obs_lst <- data.frame(cate=c("obs"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))

    # define indicators for the main climate variability events
    mei_lst <- data.frame(cate=c("mei"),
                           xpos=c(1878, 1915, 2024),
                           ypos=c(subset(tem_mtx,source=="mei"&year==1878)$p500,
                                  subset(tem_mtx,source=="mei"&year==1915)$p500,
                                  subset(tem_mtx,source=="mei"&year==2024)$p500),
                           xlpos=c(1859, 1933, 2020),
                           ylpos=c(subset(tem_mtx,source=="mei"&year==1878)$p500,
                                  subset(tem_mtx,source=="mei"&year==1915)$p500+0.02,
                                  subset(tem_mtx,source=="mei"&year==2024)$p500+0.03),
                           lbls=c("1878", "1915", "2024"))

    amo_lst <- data.frame(cate=c("amo"),
                           xpos=c(1878, 1915, 2024),
                           ypos=c(subset(tem_mtx,source=="amo"&year==1878)$p500,
                                  subset(tem_mtx,source=="amo"&year==1915)$p500,
                                  subset(tem_mtx,source=="amo"&year==2024)$p500),
                           xlpos=c(1895, 1915, 2006),
                           ylpos=c(subset(tem_mtx,source=="amo"&year==1878)$p500,
                                  subset(tem_mtx,source=="amo"&year==1915)$p500+0.05,
                                  subset(tem_mtx,source=="amo"&year==2024)$p500),
                           lbls=c("1878", "1915", "2024"))


    amo_est_lst <- data.frame(cate=c("est"),
                           xpos=c(1878, 1915, 2024),
                           ypos=c(subset(tem_mtx,source=="est"&year==1878)$p500,
                                  subset(tem_mtx,source=="est"&year==1915)$p500,
                                  subset(tem_mtx,source=="est"&year==2024)$p500),
                           xlpos=c(1878, 1915, 2006),
                           ylpos=c(subset(tem_mtx,source=="est"&year==1878)$p500+0.2,
                                  subset(tem_mtx,source=="est"&year==1915)$p500+0.2,
                                  subset(tem_mtx,source=="est"&year==2024)$p500),
                           lbls=c("1878", "1915", "2024"))

    amo_obs_lst <- data.frame(cate=c("obs"),
                           xpos=c(1878, 1915, 2024),
                           ypos=c(subset(tem_mtx,source=="obs"&year==1878)$p500,
                                  subset(tem_mtx,source=="obs"&year==1915)$p500,
                                  subset(tem_mtx,source=="obs"&year==2024)$p500),
                           xlpos=c(1878, 1915, 2006),
                           ylpos=c(subset(tem_mtx,source=="obs"&year==1878)$p500+0.2,
                                  subset(tem_mtx,source=="obs"&year==1915)$p500+0.2,
                                  subset(tem_mtx,source=="obs"&year==2024)$p500),
                           lbls=c("1878", "1915", "2024"))
# print(tail(tem_mtx))    

    # plot the main figure
    p <- ggplot() + 

        ylab(expression(italic(Delta*T)~relative~to~"1855-1900"~"("*degree*C*")"))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        # scale_y_continuous() +
        geom_hline(yintercept=0, linetype="dotted", colour="grey50", linewidth=0.4) +

		geom_rect(data=v_mtx, aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
		geom_rect(data=v_mtx, aes(xmin=x3, xmax=x4, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.5, , show.legend=F) +
        geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=F) +

        geom_segment(data = tem_ref, aes(x = c(2010), y = p500, xend = c(2022), yend = p500), colour="black", linetype="solid", size=0.5) +
        geom_segment(data = tem_bom, aes(x = c(2022), y = p500, xend = c(2024), yend = p500), colour="black", linetype="solid", size=0.5) +


        scale_colour_manual(values=cl_rfc_mapping3) +
        scale_fill_manual(values=cl_rfc_mapping3) +

        geom_smooth(data=tem_1960, aes(x=year, y=p500, linetype="lt1960"), colour="black",method="lm", formula = y ~ x, linewidth=0.5,se=F) +
        geom_smooth(data=tem_2005, aes(x=year, y=p500, linetype="lt2005"), colour="black",method="lm", formula = y ~ x, linewidth=0.5,se=F) +
        geom_smooth(data=tem_1991, aes(x=year, y=p500, linetype="lt1981"), colour="black",method="lm", formula = y ~ x, linewidth=0.5,se=F) +

        geom_label(data = title_lbls, aes(x=1850,y=Inf,label=labs,hjust=ifelse(cate=="mei",-1.2,0),vjust=v_just), fill=alpha(c("white"),0.3), parse = TRUE, size=4, label.size = NA, lineheight = 0.5) +

        geom_label(data = mei_lst, aes(x=xlpos,y=ylpos,label=lbls), fill=alpha(c("white"),0.3), size=3, label.size = NA, lineheight = 0.5) +
        geom_label(data = amo_lst, aes(x=xlpos,y=ylpos,label=lbls), fill=alpha(c("white"),0.3), size=3, label.size = NA, lineheight = 0.5) +
        geom_label(data = amo_est_lst, aes(x=xlpos,y=ylpos,label=lbls), fill=alpha(c("white"),0.3), size=3, label.size = NA, lineheight = 0.5) +
        geom_label(data = amo_obs_lst, aes(x=xlpos,y=ylpos,label=lbls), fill=alpha(c("white"),0.3), size=3, label.size = NA, lineheight = 0.5) +

        geom_point(data = volc_lst, aes(x = xpos, y = ypos), shape=24, color="grey30", size=2.5, fill = cl_group_Natural) +
        geom_point(data = volc_est_lst, aes(x = xpos, y = ypos), shape=24, color="grey30", size=2.5, fill = cl_group_Natural) +
        geom_point(data = volc_obs_lst, aes(x = xpos, y = ypos), shape=24, color="grey30", size=2.5, fill = cl_group_Natural) +

        geom_point(data = mei_lst, aes(x = xpos, y = ypos), shape=23, color="grey30", size=2.5, fill = NA) +
        geom_point(data = amo_lst, aes(x = xpos, y = ypos), shape=23, color="grey30", size=2.5, fill = NA) +
        geom_point(data = amo_est_lst, aes(x = xpos, y = ypos), shape=23, color="grey30", size=2.5, fill = NA) +
        geom_point(data = amo_obs_lst, aes(x = xpos, y = ypos), shape=23, color="grey30", size=2.5, fill = NA) +

         geom_text(data = c_mtx, aes(x = x1pos, y=ypos), label="italic(iii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x2pos, y=ypos), label="italic(ii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x3pos, y=ypos), label="italic(i)",  color="black", size=4, parse=T) +

        scale_linetype_manual(name="Linear trend between",
                              values=c("lt1960"="solid","lt2005"="solid","lt1981"="dashed"),
                              labels=c("lt1960"="1944-1976","lt2005"="1998-2012","lt1981"="1970-2012")) +
        facet_wrap(~factor(cate, levels=src_lst1), scales="free_y",ncol=3) +
        # facet_wrap(~source, scales="free_y",ncol=4) +

        theme_few() +
        theme(legend.position = "bottom",#c(0.85,0.08)
            plot.title = font_element3,
            legend.text = font_element3,
            legend.title = font_element3,
            legend.box = "horizontal",
            legend.direction = "horizontal",

            axis.title.x = element_blank(),
            axis.title.y = font_element3,
            axis.text.y.right = element_blank(),
            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = element_blank()) +
            guides(
               linetype=guide_legend(nrow=1)
               )  


    src_tag <- gsub("v5.0.2","v5_0_2",obs_src,fixed=T)

    # save the figure
    fl_plot = paste0(fig_dir,"fig_",src_tag, "_full", fl_ext)
    ggsave(fl_plot,p, width = 24, height = 24, units = "cm")
    print(paste0(fl_plot, " saved"))

    # save the data
    sav_csv <- paste0(fig_dir,"fig_",obs_src, "_full.csv")
    write.csv(tem_cst, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   
}

# plot the panel for selected climate variables using HadCRUTv5.0.2
# fol: folder name
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# plot the figure and return NULL
plot_mei_amo_rfc_regr_nat <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=1855){

    # define the variable list
    src_lst <- c("r_ghg", "r_othanthro", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")#"resi", , "obs","est"
    src_lst1 <- c("r_ghg", "r_othanthro", "rfc_volc", "rfc_solar", "mei", "amo", "est", "obs")#"resi",,, "total"

    # get the data matrix for the selected variables
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)
# print(head(src_mtx))
    # reshape the data matrix to wide format
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")


    tem_mtx <- subset(src_mtx, year>=start_yr&year<2024+1)

    # adjust the position of the variables along the y-axis for visualization
    tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p500 <- tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p500 + 3.8 
    tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p500 <- tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p500 + 3.55 
    tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p500 <- tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p500 + 2.8 
    tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p500 <- tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p500 + 2.35 - 0.2
    tem_mtx[tem_mtx$source%in%c("mei"),]$p500 <- tem_mtx[tem_mtx$source%in%c("mei"),]$p500 + 2.2 - 0.3
    tem_mtx[tem_mtx$source%in%c("amo"),]$p500 <- tem_mtx[tem_mtx$source%in%c("amo"),]$p500 + 1.9 - 0.3
    tem_mtx[tem_mtx$source%in%c("est"),]$p500 <- tem_mtx[tem_mtx$source%in%c("est"),]$p500
    tem_mtx[tem_mtx$source%in%c("obs"),]$p500 <- tem_mtx[tem_mtx$source%in%c("obs"),]$p500

    tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p025 <- tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p025 + 3.8 
    tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p025 <- tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p025 + 3.55 
    tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p025 <- tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p025 + 2.8 
    tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p025 <- tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p025 + 2.35 - 0.2
    tem_mtx[tem_mtx$source%in%c("mei"),]$p025 <- tem_mtx[tem_mtx$source%in%c("mei"),]$p025 + 2.2 - 0.3
    tem_mtx[tem_mtx$source%in%c("amo"),]$p025 <- tem_mtx[tem_mtx$source%in%c("amo"),]$p025 + 1.9 - 0.3
    tem_mtx[tem_mtx$source%in%c("est"),]$p025 <- tem_mtx[tem_mtx$source%in%c("est"),]$p025
    tem_mtx[tem_mtx$source%in%c("obs"),]$p025 <- tem_mtx[tem_mtx$source%in%c("obs"),]$p025

    tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p975 <- tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p975 + 3.8 
    tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p975 <- tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p975 + 3.55 
    tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p975 <- tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p975 + 2.8 
    tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p975 <- tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p975 + 2.35 - 0.2
    tem_mtx[tem_mtx$source%in%c("mei"),]$p975 <- tem_mtx[tem_mtx$source%in%c("mei"),]$p975 + 2.2 - 0.3
    tem_mtx[tem_mtx$source%in%c("amo"),]$p975 <- tem_mtx[tem_mtx$source%in%c("amo"),]$p975 + 1.9 - 0.3
    tem_mtx[tem_mtx$source%in%c("est"),]$p975 <- tem_mtx[tem_mtx$source%in%c("est"),]$p975
    tem_mtx[tem_mtx$source%in%c("obs"),]$p975 <- tem_mtx[tem_mtx$source%in%c("obs"),]$p975

    cor_tx12 <- tx_rfc_mapping10[src_lst1]
# print(cor_tx12)

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    tem_cst <- subset(tem_cst,year>=start_yr&year<2024+1)
    print(format(cor(detrend(tem_cst$obs),detrend(tem_cst$est)), digits=2, nsmall = 2))
    print(format(cor(tem_cst$obs,tem_cst$est), digits=2, nsmall = 2))

    cor_tx12["obs"] <- parse(text=src_tag)

    tem_mtx$cate <- tem_mtx$source

    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)
    
    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    nlen <- length(src_lst1)
    c_mtx <- data.frame(cate=src_lst1[c((nlen-2):nlen)],x1pos=c(1960),x2pos=c(2005),x3pos=c(2023),ypos=c(-0.1),y2pos=c(-0.3))


    # define the x-axis and y-axis limits and breaks and labels
    x_lims <- c(1850,2025)
    x_brks <- seq(1850,2025,25)
    x_lbls <- seq(1850,2025,25)        
    
    max_val <- max(tem_mtx$p975) 

    y_lims <- c(-0.3,max_val)
    y_brks <- seq(0,max_val,1)
    y_lbls <- seq(0,max_val,1)
    
    max_yr <- max(tem_mtx$year) 
    # print(max_yr)

    # get the solar cycle labels and positions
    cycle_mtx <- get_solar_cycle()
    cycle_mtx <- subset(cycle_mtx, start_year>=start_yr)
# print(cycle_mtx)

    # define the solar cycle labels and positions (start year + end year / 2)
    nrw <- nrow(cycle_mtx)
    cycle_label <- vector()
    cycle_pos <- vector()
    for(i in c(1:(nrw-1))){
        cycle_label <- c(cycle_label, cycle_mtx$cycle[[i]]) 
        cycle_pos <- c(cycle_pos, (cycle_mtx$start_year[[i]] + cycle_mtx$start_year[[i+1]])/2)
    }

    cycle_label <- c(cycle_label, cycle_mtx$cycle[[nrw]])
    cycle_pos <- c(cycle_pos, cycle_mtx$start_year[[nrw]] + 11/2)

	xt_brks <- cycle_pos
	xt_lbls <- cycle_label

    # define the volcano eruption labels and positions
    volc_lst <- data.frame(cate=c("rfc_volc"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))

    volc_est_lst <- data.frame(cate=c("est"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))


    volc_obs_lst <- data.frame(cate=c("obs"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))

# print(tail(tem_mtx))    
    # plot the figure
    p <- ggplot() + 

        ylab(expression(Normalized~italic(Delta*T)~"("*degree*C*")"))+

        scale_y_continuous(limits = y_lims, breaks = y_brks, labels=y_lbls) +
        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls, sec.axis = dup_axis(name="Solar cycle", breaks=xt_brks, labels=xt_lbls)) +
        geom_hline(yintercept=0, linetype="dotted", colour="grey50", linewidth=0.4) +

		geom_rect(data=v_mtx, aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), fill="grey80", alpha=0.2) +
		geom_rect(data=v_mtx, aes(xmin=x3, xmax=x4, ymin=-Inf, ymax=Inf), fill="grey80", alpha=0.2) +
        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey80", alpha=0.2) +

        geom_vline(data=cycle_mtx, aes(xintercept=start_year), colour="grey50", linetype="dotted", size=0.3) +

        geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.6, , show.legend=T) +
        geom_ribbon(data=tem_mtx, aes(x=year, ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=T) +

        scale_colour_manual(values=cl_rfc_mapping4, labels=cor_tx12) +
        scale_fill_manual(values=cl_rfc_mapping4, labels=cor_tx12) +

        geom_point(data = volc_lst, aes(x = xpos, y = ypos), shape=24, color="grey30", size=4, fill = cl_group_Natural) +

         geom_text(data = c_mtx, aes(x = x1pos, y=ypos), label="italic(iii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x2pos, y=ypos), label="italic(ii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x3pos, y=ypos), label="italic(i)",  color="black", size=4, parse=T) +

        geom_segment(data = c_mtx, aes(x = 1938, y = y2pos, xend = 1944, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = 1982, y = y2pos, xend = 1976, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = x1pos-15, y = y2pos, xend = x1pos+15, yend = y2pos), color="grey30", linetype="longdash", size=0.3) +


        geom_segment(data = c_mtx, aes(x = 1993, y = y2pos, xend = 1998, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = 2017, y = y2pos, xend = 2012, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = x2pos-7, y = y2pos, xend = x2pos+7, yend = y2pos), color="grey30", linetype="longdash", size=0.3) +

        geom_segment(data = c_mtx, aes(x = 2019, y = y2pos, xend = 2022, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = 2025, y = y2pos, xend = 2024, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = x3pos-1, y = y2pos, xend = x3pos+1, yend = y2pos), color="grey30", linetype="dotted", size=0.3) +

        theme_few() +
        theme(legend.position = "bottom",
            plot.title = font_element3,
            legend.text = font_element3,
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent'), #transparent legend bg
            axis.title.x.bottom = element_blank(),
            axis.title.x.top = font_element3,
            axis.title.y = font_element3,
            axis.text.y.right = element_blank(),
            axis.ticks.x.top=element_blank(),
            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = element_blank()) +
            guides(
               colour=guide_legend(nrow=2),
               fill=guide_legend(nrow=2)
               )  


    src_tag <- gsub("v5.0.2","v5_0_2",obs_src,fixed=T)

    # save the figure
    fl_plot = paste0(fig_dir,"fig_",src_tag, "_nat", fl_ext)
    ggsave(fl_plot,p, width = 18, height = 14, units = "cm")
    print(paste0(fl_plot, " saved"))

    # save the data
    sav_csv <- paste0(fig_dir,"fig_",obs_src, "_nat.csv")
    write.csv(tem_cst, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   
}


# plot the 11-year rolling trends for the climate varaibles
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# k: rolling window size
# plot the figure and return NULL
plot_mei_amo_rfc_regr_roll <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=1855, k=11){

    # define the variable list
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")#"resi", , "obs","est"
    src_lst1 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "est", "obs")#"resi",,, "total"

    # get the data matrix for the variable
    src_mtx <- get_dec_roll_all(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, k)

    # reshape the data matrix to wide format
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")


    tem_mtx <- subset(src_mtx, year>=start_yr&year<2024+1)
    
    cor_tx12 <- tx_rfc_mapping9[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)


    cor_tx12["obs"] <- paste0("bold(O)~", src_tag)

    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- tem_mtx$source

    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)
    tem_1960 <- subset(tem_mtx, year>=1944&year<1976+1)
    tem_2005 <- subset(tem_mtx, year>=1998&year<2012+1)
    tem_1991 <- subset(tem_mtx, year>=1970&year<2012+1)


    # define the positions of the labels
    src_hjust <- c(
        "rfc_CO2"     = -0.5,
        "rfc_CH4"     = -0.5,
        "rfc_N2O"     = -0.5,
        "r_fgs"       = -0.15,
        "rfc_FODS"    = -0.5,
        "rfc_FHFC"    = -0.5, 
        "rfc_FPFC"    = -0.5,
        "r_o3"        = -0.4,    
        "rfc_O3s"     = -0.5,
        "rfc_O3t"     = -0.5,
        "rfc_H2Os"    = -0.3,
        "r_abd"       = -0.1,
        "rfc_LCC"     = -0.5,
        "rfc_BCsnow"  = -0.5,
        "r_aer"       = -0.2,
        "rfc_cloud"   = -0.3,
        "rfc_volc"    = -0.5,
        "rfc_solar"   = -0.5,
        "vari"        = -0.5,
        "mei"         = -0.4,
        "amo"         = -0.5,
        "resi"        = -0.5, 
        "est"        = -0.5,
        "obs"        = -0.5
 
    )

    src_vjust <- c(
        "rfc_CO2"     = 1.5,
        "rfc_CH4"     = 1.5,
        
        "rfc_N2O"     = 1.5,
        "r_fgs"       = 1.5,
        "rfc_FODS"    = 1.5,
        "rfc_FHFC"    = 1.5, 
        "rfc_FPFC"    = 1.5,
        "r_o3"        = 1.5,
        "rfc_O3s"     = 1.5,
        "rfc_O3s"     = 1.5,
        "rfc_O3t"     = 1.5,
        "rfc_H2Os"    = 1.5,
        "r_abd"       = 1.5,
        "rfc_LCC"     = 1.5,
        "rfc_BCsnow"  = 1.5,
        "r_aer"       = 1.5,
        "rfc_cloud"   = 1.5,
        "rfc_volc"    = 1.5,
        "rfc_solar"   = 1.5,
        
        "vari"        = 1.5,
        "mei"        = 1.5,
        "amo"        = 1.5,
        "resi"        = 1.5, 
        "est"         = 1.5,
        "obs"         = 1.5
    )


    title_lbls <- data.frame(cate=src_lst1,h_just=as.vector(unlist(src_hjust[src_lst1], use.names=FALSE)),
                                            v_just=as.vector(unlist(src_vjust[src_lst1], use.names=FALSE)),
                                            labs=as.character(unlist(cor_tx12[src_lst1])))

    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    nlen <- length(src_lst1)
    c_mtx <- data.frame(cate=src_lst1[c((nlen-2):nlen)],x1pos=c(1960),x2pos=c(2005),x3pos=c(2023),ypos=c(-0.15,-0.3,-0.35),y2pos=c(-0.19,-0.4,-0.45))
# print(c_mtx)


    # define the x-axis limits and breaks and labels
    if(start_yr==1951){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1970, 1980, 1998, 2012,2020)
        x_lbls <- c("1951", "1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==1970){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1980, 1998, 2012,2020)
        x_lbls <- c("1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==2000){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1990){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1855){
        x_lims <- c(1850,2025)
        x_brks <- seq(1850,2025,50)
        x_lbls <- seq(1850,2025,50)
    }else{
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,20)
        x_lbls <- seq(start_yr,2025,20)        
    }
    
    # indicate important volcanic eruptions
    volc_lst <- data.frame(cate=c("rfc_volc"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))

    volc_est_lst <- data.frame(cate=c("est"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))


    volc_obs_lst <- data.frame(cate=c("obs"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))

# print(tail(tem_mtx))   
    # plot the trend
    p <- ggplot() + 

        ylab(bquote(.(k)*"-year running trend ("*degree*C*" per decade)"))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        # scale_y_continuous() +
        geom_hline(yintercept=0, linetype="dotted", colour="grey50", linewidth=0.4) +

		geom_rect(data=v_mtx, aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
		geom_rect(data=v_mtx, aes(xmin=x3, xmax=x4, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.5, , show.legend=F) +
        geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=F) +

        scale_colour_manual(values=cl_rfc_mapping3) +
        scale_fill_manual(values=cl_rfc_mapping3) +

        geom_label(data = title_lbls, aes(x=1850,y=Inf,label=labs,hjust=0,vjust=v_just), fill=alpha(c("white"),0.3), parse = TRUE, size=4, label.size = NA, lineheight = 0.5) +

        geom_point(data = volc_lst, aes(x = xpos, y = ypos), shape=24, color="grey30", size=2.5, fill = cl_group_Natural) +
        geom_point(data = volc_est_lst, aes(x = xpos, y = ypos), shape=24, color="grey30", size=2.5, fill = cl_group_Natural) +
        geom_point(data = volc_obs_lst, aes(x = xpos, y = ypos), shape=24, color="grey30", size=2.5, fill = cl_group_Natural) +

         geom_text(data = c_mtx, aes(x = x1pos, y=ypos), label="italic(iii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x2pos, y=ypos), label="italic(ii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x3pos, y=ypos+0.03), label="italic(i)",  color="black", size=4, parse=T) +

        geom_segment(data = c_mtx, aes(x = 1938, y = y2pos, xend = 1944, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = 1982, y = y2pos, xend = 1976, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = x1pos-15, y = y2pos, xend = x1pos+15, yend = y2pos), color="grey30", linetype="longdash", size=0.3) +

        geom_segment(data = c_mtx, aes(x = 1993, y = y2pos, xend = 1998, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = 2017, y = y2pos, xend = 2012, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = x2pos-7, y = y2pos, xend = x2pos+7, yend = y2pos), color="grey30", linetype="longdash", size=0.3) +

        geom_segment(data = c_mtx, aes(x = 2019, y = y2pos+0.03, xend = 2022, yend = y2pos+0.03), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = 2025, y = y2pos+0.03, xend = 2024, yend = y2pos+0.03), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = x3pos-1, y = y2pos+0.03, xend = x3pos+1, yend = y2pos+0.03), color="grey30", linetype="dotted", size=0.3) +

        facet_wrap(~factor(cate, levels=src_lst1), scales="free_y",ncol=3) +

        theme_few() +
        theme(legend.position = "bottom",#c(0.85,0.08)
            plot.title = font_element3,
            legend.text = font_element3,
            legend.title = font_element3,
            legend.box = "horizontal",
            legend.direction = "horizontal",
            # legend.spacing = unit(0.2, 'cm'),
            # legend.box.spacing = unit(0.15, 'cm'),
            # legend.margin = margin(0., 0., 0., 0.),
            # legend.key.height = unit(0.2,"line"),
            # legend.justification = "right",
            axis.title.x = element_blank(),
            axis.title.y = font_element3,
            axis.text.y.right = element_blank(),
            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = element_blank()) +
            guides(
               linetype=guide_legend(nrow=1)
               )  


    src_tag <- gsub("v5.0.2","v5_0_2",obs_src,fixed=T)

    # save the figure
    fl_plot = paste0(fig_dir,"fig_",src_tag, "_roll", fl_ext)
    ggsave(fl_plot,p, width = 24, height = 24, units = "cm")
    print(paste0(fl_plot, " saved"))

    # save the data table
    sav_csv <- paste0(fig_dir,"fig_",obs_src, "_roll.csv")
    write.csv(tem_cst, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   
}

# plot the 11-year rolling trends
# fol: folder name for the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# k: rolling window size
# plot the figure and return NULL  
plot_mei_amo_rfc_regr_roll_nat <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=1855, k=11){

    # define the variable list
    src_lst <- c("r_ghg", "r_othanthro", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")#"resi", , "obs","est"
    src_lst1 <- c("r_ghg", "r_othanthro", "rfc_volc", "rfc_solar", "mei", "amo", "est", "obs")#"resi",,, "total"


    # get the data matrix
    src_mtx <- get_dec_roll_all(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, k)

    # reshape the data matrix and get the wide table
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")

    tem_mtx <- subset(src_mtx, year>=start_yr&year<2024+1)

    # adjust the position along y-axis for visualization
    tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p500 <- tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p500 + 1.88 
    tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p500 <- tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p500 + 1.81 
    tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p500 <- tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p500 + 1.51
    tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p500 <- tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p500 + 1.15
    tem_mtx[tem_mtx$source%in%c("mei"),]$p500 <- tem_mtx[tem_mtx$source%in%c("mei"),]$p500 + 1.05 - 0.26
    tem_mtx[tem_mtx$source%in%c("amo"),]$p500 <- tem_mtx[tem_mtx$source%in%c("amo"),]$p500 + 0.8 - 0.28
    tem_mtx[tem_mtx$source%in%c("est"),]$p500 <- tem_mtx[tem_mtx$source%in%c("est"),]$p500 
    tem_mtx[tem_mtx$source%in%c("obs"),]$p500 <- tem_mtx[tem_mtx$source%in%c("obs"),]$p500

    tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p025 <- tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p025 + 1.88 
    tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p025 <- tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p025 + 1.81 
    tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p025 <- tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p025 + 1.51
    tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p025 <- tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p025 + 1.15
    tem_mtx[tem_mtx$source%in%c("mei"),]$p025 <- tem_mtx[tem_mtx$source%in%c("mei"),]$p025 + 1.05 - 0.26
    tem_mtx[tem_mtx$source%in%c("amo"),]$p025 <- tem_mtx[tem_mtx$source%in%c("amo"),]$p025 + 0.8 - 0.28
    tem_mtx[tem_mtx$source%in%c("est"),]$p025 <- tem_mtx[tem_mtx$source%in%c("est"),]$p025 
    tem_mtx[tem_mtx$source%in%c("obs"),]$p025 <- tem_mtx[tem_mtx$source%in%c("obs"),]$p025

    tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p975 <- tem_mtx[tem_mtx$source%in%c("r_ghg"),]$p975 + 1.88 
    tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p975 <- tem_mtx[tem_mtx$source%in%c("r_othanthro"),]$p975 + 1.81 
    tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p975 <- tem_mtx[tem_mtx$source%in%c("rfc_volc"),]$p975 + 1.51
    tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p975 <- tem_mtx[tem_mtx$source%in%c("rfc_solar"),]$p975 + 1.15
    tem_mtx[tem_mtx$source%in%c("mei"),]$p975 <- tem_mtx[tem_mtx$source%in%c("mei"),]$p975 + 1.05 - 0.26
    tem_mtx[tem_mtx$source%in%c("amo"),]$p975 <- tem_mtx[tem_mtx$source%in%c("amo"),]$p975 + 0.8 - 0.28
    tem_mtx[tem_mtx$source%in%c("est"),]$p975 <- tem_mtx[tem_mtx$source%in%c("est"),]$p975 
    tem_mtx[tem_mtx$source%in%c("obs"),]$p975 <- tem_mtx[tem_mtx$source%in%c("obs"),]$p975

    cor_tx12 <- tx_rfc_mapping10[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)
# print(src_tag)
    tem_cst <- subset(tem_cst,year>=start_yr)

    cor_tx12["obs"] <- parse(text=src_tag)

    tem_mtx$cate <- tem_mtx$source

    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)
    tem_1960 <- subset(tem_mtx, year>=1944&year<1976+1)
    tem_2005 <- subset(tem_mtx, year>=1998&year<2012+1)
    tem_1991 <- subset(tem_mtx, year>=1970&year<2012+1)
    
    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    nlen <- length(src_lst1)
    c_mtx <- data.frame(cate=src_lst1[c((nlen-2):nlen)],x1pos=c(1960),x2pos=c(2005),x3pos=c(2023),ypos=c(-0.4),y2pos=c(-0.5))


    # define the x-axis and y-axis limits and breaks and labels
    x_lims <- c(1850,2025)
    x_brks <- seq(1850,2025,20)
    x_lbls <- seq(1850,2025,20)        
    
    max_val <- max(tem_mtx$p500) * 1.04
    min_val <- min(tem_mtx$p500) 
    if(min_val>=-0.5){
        min_val <- -0.5
    }

    y_lims <- c(min_val,max_val)
    y_brks <- seq(-0.5,max_val,0.5)
    y_lbls <- seq(-0.5,max_val,0.5)    

    # get the solar cycle data
    cycle_mtx <- get_solar_cycle()
    cycle_mtx <- subset(cycle_mtx, start_year>=start_yr)
# print(cycle_mtx)
    nrw <- nrow(cycle_mtx)
    cycle_label <- vector()
    cycle_pos <- vector()

    # define the solar cycle labels and positions (start year + end year / 2)
    for(i in c(1:(nrw-1))){
        cycle_label <- c(cycle_label, cycle_mtx$cycle[[i]]) 
        cycle_pos <- c(cycle_pos, (cycle_mtx$start_year[[i]] + cycle_mtx$start_year[[i+1]])/2)
    }

    cycle_label <- c(cycle_label, cycle_mtx$cycle[[nrw]])
    cycle_pos <- c(cycle_pos, cycle_mtx$start_year[[nrw]] + 11/2)

	xt_brks <- cycle_pos
	xt_lbls <- cycle_label

    # define the volcano eruption labels and positions
    volc_lst <- data.frame(cate=c("rfc_volc"),
                           xpos=c(1883, 1886, 1902,1913,1963,1982,1991),
                           ypos=c(-Inf),
                           lbls=c("Krakatau", "Tarawera", "Santa Maria","Colima","Agung", "El Chichón", "Pinatubo"))
    # plot the figure
    p <- ggplot() + 

        ylab(bquote(Normalized~.(k)*"-year running trend ("*degree*C*" per decade)"))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls, sec.axis = dup_axis(name="Solar cycle", breaks=xt_brks, labels=xt_lbls)) +
        
        scale_y_continuous(limits = y_lims, breaks = y_brks, labels=y_lbls) +
        geom_hline(yintercept=0, linetype="dotted", colour="grey50", linewidth=0.4) +
        geom_vline(data=cycle_mtx, aes(xintercept=start_year), colour="grey50", linetype="dotted", size=0.3) +

		geom_rect(data=v_mtx, aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
		geom_rect(data=v_mtx, aes(xmin=x3, xmax=x4, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=T) +
        geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.6, , show.legend=T) +

        scale_colour_manual(values=cl_rfc_mapping4, labels=cor_tx12) +
        scale_fill_manual(values=cl_rfc_mapping4, labels=cor_tx12) +

        geom_point(data = volc_lst, aes(x = xpos, y = ypos), shape=24, color="grey30", size=4, fill = cl_group_Natural) +

         geom_text(data = c_mtx, aes(x = x1pos, y=ypos), label="italic(iii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x2pos, y=ypos), label="italic(ii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x3pos, y=ypos), label="italic(i)",  color="black", size=4, parse=T) +

        geom_segment(data = c_mtx, aes(x = 1938, y = y2pos, xend = 1944, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = 1982, y = y2pos, xend = 1976, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = x1pos-15, y = y2pos, xend = x1pos+15, yend = y2pos), color="grey30", linetype="longdash", size=0.3) +

        geom_segment(data = c_mtx, aes(x = 1993, y = y2pos, xend = 1998, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = 2017, y = y2pos, xend = 2012, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = x2pos-7, y = y2pos, xend = x2pos+7, yend = y2pos), color="grey30", linetype="longdash", size=0.3) +


        geom_segment(data = c_mtx, aes(x = 2019, y = y2pos, xend = 2022, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = 2025, y = y2pos, xend = 2024, yend = y2pos), color="grey30", linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +
        geom_segment(data = c_mtx, aes(x = x3pos-1, y = y2pos, xend = x3pos+1, yend = y2pos), color="grey30", linetype="dotted", size=0.3) +

        theme_few() +
        theme(legend.position = "bottom",#c(0.85,0.08)
            plot.title = font_element3,
            legend.text = font_element3,
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.direction = "horizontal",

            axis.title.x.bottom = element_blank(),
            axis.title.x.top = font_element3,
            axis.title.y = font_element3,
            axis.text.y.right = element_blank(),
            axis.ticks.x.top=element_blank(),
            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = element_blank()) +
            guides(
               colour=guide_legend(nrow=2),
               fill=guide_legend(nrow=2)
               )  


    src_tag <- gsub("v5.0.2","v5_0_2",obs_src,fixed=T)

    # save the figure
    fl_plot = paste0(fig_dir,"fig_",src_tag, "_roll_nat", fl_ext)
    ggsave(fl_plot,p, width = 18, height = 18, units = "cm")
    print(paste0(fl_plot, " saved"))

    # save the data
    sav_csv <- paste0(fig_dir,"fig_",obs_src, "_roll_nat.csv")
    write.csv(tem_cst, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   
}

# get the data matrix for the 11-year rolling trends
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario name
# tag: variable tag
# src_lst: list of the climate variables
# osrc: observation source
# k: number of years for rolling trend
# start_yr: start year for rolling trend
# return: data matrix for the 11-year rolling trends
get_dec_roll_all <- function(fol, reg, hsrc, psrc, sce, tag, src_lst, osrc, k, start_yr=1855){
    dst_mtx <- NULL

    # iterate over the climate variables
    for(src in src_lst){
        # get the 11-year rolling trend
        rol_mtx <- get_dec_roll(fol, reg, hsrc, psrc, sce, tag, src, osrc, k, start_yr) 
        # print(src)
        # print(dim(rol_mtx))
        rol_mtx <- data.frame(source=c(src),rol_mtx)

        # combine the data matrix
        if(is.null(dst_mtx)){
            dst_mtx <- rol_mtx
        }else{
            dst_mtx <- rbind(dst_mtx, rol_mtx)
        }   
    }

    # return the data matrix
    return(dst_mtx)
} 

# not used but just borrow the legend
plot_regress_tot <- function(fol, reg, hsrc, psrc, sce, tag,osrc,start_yr=1855){
    p_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, c("est", "obs"),osrc, preindust=T)

    p_mtx <- subset(p_mtx, year>=start_yr&year<2024+1)
    p_mtx$source <-  factor(p_mtx$source, levels  = c("obs","est"))

    x_lims <- c(1850,2025)
    x_brks <- seq(1850,2025,50)
    x_lbls <- seq(1850,2025,50)


    y_lims <- c(-0.2,1.42)
    y_brks <- seq(0,1.4,0.5)
    y_lbls <- seq(0,1.4,0.5)

    src_tag <- gsub("_cti","",osrc,fixed=T)
    src_tag <- gsub("_mei_amo","",src_tag,fixed=T)
    src_tag <- gsub("_et","",src_tag,fixed=T)
    src_tag <- gsub("_my","",src_tag,fixed=T)
    src_tag <- gsub("_ncep","",src_tag,fixed=T)
    src_tag <- gsub("_best","",src_tag,fixed=T)
    src_tag <- gsub("_rfc","",src_tag,fixed=T)
    src_tag <- gsub("_v5.0","v5.0",src_tag,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    leg_lbl <- c("obs"=parse(text=src_tag),"est"=bquote(Total~sum))

    v_mtx <- data.frame(cate=c("total"),x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))
    p <- ggplot() +

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +

        annotate(geom="text", x=1890, y=1, label="5-year running mean") +

        geom_line(data = p_mtx, aes(x = year, y = p500, colour=source), size = line_size08) +
        geom_ribbon(data = p_mtx, aes(x = year, ymin = p025, ymax = p975, fill=source),  alpha=0.3) +        

        scale_colour_manual(values=cl_rfc_mapping3[c("est", "obs")],labels=leg_lbl) +
        scale_fill_manual(values=cl_rfc_mapping3[c("est", "obs")],labels=leg_lbl) +

		geom_rect(data=v_mtx, aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
		geom_rect(data=v_mtx, aes(xmin=x3, xmax=x4, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        theme_few() +
        theme(legend.position = c("bottom"),

        legend.title = element_blank(),
        legend.text = font_element3,
        legend.box = "horizontal",
        legend.direction = "horizontal",
        # legend.spacing = unit(0.05, 'cm'),
        # legend.box.spacing = unit(0.05, 'cm'),
        # legend.margin = margin(0.05, 0.05, 0.05, 0.05),
        plot.title = font_element3,
        strip.text = font_element3,
        axis.text.x  = font_element3,
        axis.text.y  = font_element3,
        axis.text.x.top  = element_blank(),
        axis.text.y.right  = font_element3,
        axis.title.x  = element_blank(),
        axis.title.y  = element_blank())  

    return(p)
}

# plot the cooling phase regression result
# fol: folder name
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario name
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# end_yr: end year
# return: ggplot object
plot_rfc_regr_coolphase <- function(fol, reg, hsrc, psrc, sce, tag,obs_src, start_yr=1930, end_yr=1990){

    # define the variable list
    src_lst <- c("obs","est")#"resi", 
    src_lst1 <- c("total")#"resi",

    # get the data matrix for the variable list
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)
# print(head(src_mtx))
    # reshape the data matrix to wide format
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")


    tem_mtx <- subset(src_mtx, year>=start_yr&year<=end_yr)
    
    cor_tx12 <- tx_rfc_mapping4[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    tem_cst <- subset(tem_cst,year>=start_yr)
    print(cor(detrend(tem_cst$obs),detrend(tem_cst$est)))
    print(cor(tem_cst$obs,tem_cst$est))

    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- tem_mtx$source
    tem_mtx$cate <- gsub("obs","total",tem_mtx$cate,fixed=T)
    tem_mtx$cate <- gsub("est","total",tem_mtx$cate,fixed=T)

    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)

    tem_1960 <- subset(tem_mtx, year>=1944&year<1976+1)
    
    tem_1960$cate <- tem_1960$source

    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    c_mtx <- data.frame(cate=c("total"),x1pos=c(1960),x2pos=c(2005),x3pos=c(2023), y1pos=c(-0.1),y2pos=c(-0.21))


    # define the x-axis limits, breaks and labels
    x_lims <- c(start_yr,end_yr)
    x_brks <- seq(start_yr,end_yr,10)
    x_lbls <- seq(start_yr,end_yr,10)        

# print(tail(tem_mtx))    

    # plot the panel
    p <- ggplot() + 
        ylab(expression(italic(Delta*T)~"("*degree*C*")"))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        # scale_y_continuous() +
        geom_hline(yintercept=0, linetype="dotted", colour="grey50", linewidth=0.4) +

		geom_rect(data=v_mtx, aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.6, , show.legend=F) +
        geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=F) +

        geom_smooth(data=subset(tem_1960,source=="est"), aes(x=year, y=p500, linetype="lt1960"), colour="grey30", method="lm", formula = y ~ x, linewidth=0.3,se=F) +
        geom_smooth(data=tem_1960, aes(x=year, y=p500, colour=source), linetype="solid", method="lm", formula = y ~ x, linewidth=0.3,se=F, show.legend=F) +

        scale_colour_manual(values=cl_rfc_mapping3[src_lst]) +
        scale_fill_manual(values=cl_rfc_mapping3[src_lst]) +
        scale_linetype_manual(name="Linear trend between",values=c("lt1960"="solid"),labels=c("lt1960"="1944-1976")) +

        theme_few() +
        theme(legend.position = c(0.38,0.85),
            plot.title = element_text(size=12, hjust=0, margin=margin(0,0,0,0)),
            legend.text = element_text(size=10, hjust=0),
            legend.title = element_text(size=10, hjust=0),
            legend.box = "horizontal",
            legend.direction = "horizontal",
            panel.background = element_rect(fill='transparent'), #transparent panel bg
            plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
            panel.grid.major = element_blank(), #remove major gridlines
            panel.grid.minor = element_blank(), #remove minor gridlines
            legend.background = element_rect(fill='transparent'), #transparent legend bg

            axis.title.x = element_blank(),
            axis.title.y =  element_text(size=11, hjust=0.5),
            axis.text.y.right = element_blank(),
            axis.text.x = font_element2,
            axis.text.y = font_element2,
            strip.text = element_blank()) +
            guides(
               linetype=guide_legend(nrow=1)
               )  
    # return the panel
    return(p)
}

# plot the slowdown phase regression result
# fol: folder name
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario name
# tag: variable tag
# obs_src: observation source
# return: ggplot object
plot_rfc_regr_slowdown <- function(fol, reg, hsrc, psrc, sce, tag,obs_src){

    # define the variable list
    src_lst <- c("obs","est")#"resi", 
    src_lst1 <- c("total")#"resi",,

    # get the data matrix for the variable list
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)
# print(head(src_mtx))

    # reshape the data matrix to wide format
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")


    tem_mtx <- subset(src_mtx, year>=1960&year<=2020)
    
    cor_tx12 <- tx_rfc_mapping4[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- tem_mtx$source
    tem_mtx$cate <- gsub("obs","total",tem_mtx$cate,fixed=T)
    tem_mtx$cate <- gsub("est","total",tem_mtx$cate,fixed=T)

    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)

    tem_2005 <- subset(tem_mtx, year>=1998&year<2012+1)
    tem_1991 <- subset(tem_mtx, year>=1970&year<2012+1)
    
    tem_2005$cate <- tem_2005$source
    tem_1991$cate <- tem_1991$source

    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    c_mtx <- data.frame(cate=c("total"),x1pos=c(1960),x2pos=c(2005),x3pos=c(2023), y1pos=c(-0.1),y2pos=c(-0.21))


    # define the x-axis limits, breaks and labels
    x_lims <- c(1960,2020)
    x_brks <- seq(1960,2020,10)
    x_lbls <- seq(1960,2020,10)        

# print(tail(tem_mtx))    

    # plot the panel
    p <- ggplot() + 

        ylab(expression(italic(Delta*T)~"("*degree*C*")"))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        # scale_y_continuous() +

		geom_rect(data=v_mtx, aes(xmin=x3, xmax=x4, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

         geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.6, , show.legend=F) +
         geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=F) +

         geom_smooth(data=subset(tem_2005,source=="est"), aes(x=year, y=p500, linetype="lt2005"),colour="grey30", method="lm", formula = y ~ x, linewidth=0.3,se=F) +
         geom_smooth(data=subset(tem_1991,source=="est"), aes(x=year, y=p500, linetype="lt1981"), colour="grey30",method="lm", formula = y ~ x, linewidth=0.3,se=F) +

         geom_smooth(data=tem_2005, aes(x=year, y=p500, colour=source), linetype="solid", method="lm", formula = y ~ x, linewidth=0.3,se=F, show.legend=F) +
         geom_smooth(data=tem_1991, aes(x=year, y=p500, colour=source), linetype="longdash", method="lm", formula = y ~ x, linewidth=0.3,se=F, show.legend=F) +


        scale_colour_manual(values=cl_rfc_mapping3[src_lst]) +
        scale_fill_manual(values=cl_rfc_mapping3[src_lst]) +

        scale_linetype_manual(name="Linear trend between",values=c("lt2005"="solid","lt1981"="longdash"),labels=c("lt2005"="1998-2012","lt1981"="1970-2012")) +

        theme_few() +
        theme(legend.position = c(0.67,0.27),
            plot.title = element_text(size=12, hjust=0, margin=margin(0,0,0,0)),

            legend.text = element_text(size=10, hjust=0),
            legend.title = element_text(size=10, hjust=1),
            legend.box = "horizontal",
            legend.direction = "vertical",

            panel.background = element_rect(fill='transparent'), #transparent panel bg
            plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
            panel.grid.major = element_blank(), #remove major gridlines
            panel.grid.minor = element_blank(), #remove minor gridlines
            legend.background = element_rect(fill='transparent'), #transparent legend bg

            legend.key.height = unit(0.2,"line"),
            # legend.justification = "right",
            axis.title.x = element_blank(),
            axis.title.y = font_element2,
            axis.text.y.right = element_blank(),
            axis.text.x = font_element2,
            axis.text.y = font_element2,
            strip.text = element_blank()) +
            guides(
               linetype=guide_legend(nrow=1)
               )  

    # return the panel
    return(p)
}

# plot the temperature spike regression result
# fol: folder name
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario name
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# prev_st_yr: previous start year
# return: ggplot object
plot_rfc_regr_spike <- function(fol, reg, hsrc, psrc, sce, tag,obs_src, start_yr=2000, prev_st_yr=2010){

    # define the variable list
    src_lst <- c("obs","est")#"resi", 
    src_lst1 <- c("total")#"resi",,

    # get the data matrix for the variable list
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)
# print(head(src_mtx))

    # reshape the data matrix to wide format
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")

# print(start_yr)
    tem_mtx <- subset(src_mtx, year>=start_yr&year<2025)
    
    cor_tx12 <- tx_rfc_mapping4[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- tem_mtx$source
    tem_mtx$cate <- gsub("obs","total",tem_mtx$cate,fixed=T)
    tem_mtx$cate <- gsub("est","total",tem_mtx$cate,fixed=T)

    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)

    # define the data ranges for the temperature spike period
    tem_ref <- subset(tem_mtx, year>=prev_st_yr&year<2021+1)
    tem_bom <- subset(tem_mtx, year>=2022&year<2024+1)

    tem_ref <- aggregate(p500~source, data = tem_ref, FUN=mean)
    tem_bom <- aggregate(p500~source, data = tem_bom, FUN=mean)
    
    tem_ref$cate <- tem_ref$source
    tem_bom$cate <- tem_bom$source

    tem_ref$cate <- gsub("obs","total",tem_ref$cate,fixed=T)
    tem_ref$cate <- gsub("est","total",tem_ref$cate,fixed=T)

    tem_bom$cate <- gsub("obs","total",tem_bom$cate,fixed=T)
    tem_bom$cate <- gsub("est","total",tem_bom$cate,fixed=T)


    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    c_mtx <- data.frame(cate=c("total"),x1pos=c(1960),x2pos=c(2005),x3pos=c(2023), y1pos=c(-0.1),y2pos=c(-0.21))

    # define the x-axis limits, breaks and labels
    x_lims <- c(start_yr,2025)
    x_brks <- seq(start_yr,2025,5)
    x_lbls <- seq(start_yr,2025,5)        


# print(tail(tem_mtx))    

    # plot the panel
    p <- ggplot() + 

        ylab(expression(italic(Delta*T)~"("*degree*C*")"))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        # scale_y_continuous() +

        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.6, , show.legend=F) +
        geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=F) +

        geom_segment(data = subset(tem_ref,source=="est"), aes(x = c(prev_st_yr), y = p500, xend = c(2022), yend = p500, linetype="lt2015"), size=0.3) +
        geom_segment(data = subset(tem_bom,source=="est"), aes(x = c(2022), y = p500, xend = c(2024), yend = p500, linetype="lt2022"), size=0.3) +

        geom_segment(data = tem_ref, aes(x = c(prev_st_yr), y = p500, xend = c(2022), yend = p500, colour=source), linetype="longdash", size=0.3, show.legend=F) +
        geom_segment(data = tem_bom, aes(x = c(2022), y = p500, xend = c(2024), yend = p500, colour=source), linetype="solid", size=0.3, show.legend=F) +

        scale_colour_manual(values=cl_rfc_mapping3) +
        scale_fill_manual(values=cl_rfc_mapping3) +
        scale_linetype_manual(name="Mean over",values=c("lt2015"="longdash","lt2022"="solid"),labels=c("lt2015"=paste0(prev_st_yr,"-2021"),"lt2022"="2022-2024")) +

        theme_few() +
        theme(legend.position = "none", #c(0.45,0.82),
            plot.title = element_text(size=12, hjust=0, margin=margin(0,0,0,0)),
            legend.text = font_element3,
            legend.title = font_element3,
            legend.box = "vertical",
            legend.direction = "vertical",

            panel.background = element_rect(fill='transparent'), #transparent panel bg
            plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
            panel.grid.major = element_blank(), #remove major gridlines
            panel.grid.minor = element_blank(), #remove minor gridlines
            legend.background = element_rect(fill='transparent'), #transparent legend bg
            # legend.box.background = element_rect(fill='transparent'), #transparent legend panel

            # legend.spacing = unit(0.2, 'cm'),
            # legend.box.spacing = unit(0.15, 'cm'),
            # legend.margin = margin(0., 0., 0., 0.),
            # legend.key.height = unit(0.2,"line"),
            # legend.justification = "right",
            axis.title.x = element_blank(),
            axis.title.y = font_element2,
            axis.text.y.right = element_blank(),
            axis.text.x = font_element2,
            axis.text.y = font_element2,
            strip.text = element_blank()) +
            guides(
               linetype=guide_legend(nrow=2)
               )  
    # return the panel
    return(p)
}

# plot the inset panel
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag name
# obs_src: observation source
# start_yr: start year
plot_rfc_regr_total2 <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=1855){

    # define the variable names
    src_lst <- c("obs","est")#"resi", 
    src_lst1 <- c("total")#"resi",,

    # get the data matrix for the variables
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)

    # reshape the data matrix to get the wide table
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")
    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")

    tem_mtx <- subset(src_mtx, year>=start_yr&year<2024+1)
    # run_mtx <- subset(run_mtx, year>=start_yr&year<2024+1)
    
    cor_tx12 <- tx_rfc_mapping4[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    tem_cst <- subset(tem_cst,year>=start_yr)
    tem_cst <- na.omit(tem_cst)
    # print(tem_cst)
    # print the correlation
    print(cor(detrend(tem_cst$obs),detrend(tem_cst$est)))
    print(cor(tem_cst$obs,tem_cst$est))

    rval <- format(cor(tem_cst$obs,tem_cst$est), digits=2, nsmall = 2)

    # define the labels for the variables
    cor_tx12["total"] <- paste0("Total~sum~'('*blue*','~italic(r)*'='*", rval,"*','~annual*')'~and~", src_tag, "~'(grey)'")


    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- c("total")

    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)

    # define the data ranges for the temperature spike period
    tem_ref <- subset(tem_mtx, year>=2010&year<2022)
    tem_bom <- subset(tem_mtx, year>=2022&year<2024+1)

    tem_ref <- aggregate(p500~source, data = tem_ref, FUN=mean)
    tem_bom <- aggregate(p500~source, data = tem_bom, FUN=mean)
    
    tem_ref$cate <- tem_ref$source
    tem_bom$cate <- tem_bom$source

    tem_ref$cate <- gsub("obs","total",tem_ref$cate,fixed=T)
    tem_ref$cate <- gsub("est","total",tem_ref$cate,fixed=T)

    tem_bom$cate <- gsub("obs","total",tem_bom$cate,fixed=T)
    tem_bom$cate <- gsub("est","total",tem_bom$cate,fixed=T)

    # define the data ranges for the cooling phase and warming slowdown periods
    tem_1960 <- subset(tem_mtx, year>=1944&year<1976+1)
    tem_2005 <- subset(tem_mtx, year>=1998&year<2012+1)
    tem_1991 <- subset(tem_mtx, year>=1970&year<2012+1)
    
    tem_1960$cate <- tem_1960$source
    tem_2005$cate <- tem_2005$source
    tem_1991$cate <- tem_1991$source

    v_mtx <- data.frame(cate=c("total"),x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(2022), x6=c(2024))

    bias_mtx <- data.frame(cate=c("total"),x1=c(1900), x2=c(1930),x3=(1939),x4=c(1945))

    c_mtx <- data.frame(cate=c("total"),x1pos=c(1960),x2pos=c(2005),x3pos=c(2023), y1pos=c(-0.18),y2pos=c(-0.21))

    # define the x-axis limits and breaks and lables
    if(start_yr==1951){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1970, 1980, 1998, 2012,2020)
        x_lbls <- c("1951", "1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==1970){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1980, 1998, 2012,2020)
        x_lbls <- c("1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==2000){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1990){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==1855){
        x_lims <- c(1850,2025)
        x_brks <- seq(1850,2025,50)
        x_lbls <- seq(1850,2025,50)
    }else{
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,20)
        x_lbls <- seq(start_yr,2025,20)        
    }
    
    # define the labels for the legend
    legend_tx <- c("obs"=parse(text=src_tag),"est"=bquote(Total~sum~"("*italic(r)*"="*.(rval)*","~annual*")"))
  
    # plot the inset panel
    p <- ggplot() + 

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        # scale_y_continuous() +
        geom_hline(yintercept=0, linetype="dotted", colour="grey50", linewidth=0.4) +

		geom_rect(data=v_mtx, aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
		geom_rect(data=v_mtx, aes(xmin=x3, xmax=x4, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +
        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.8, , show.legend=T) +
        geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=T) +

        scale_colour_manual(values=cl_rfc_mapping3[src_lst], labels=legend_tx) +
        scale_fill_manual(values=cl_rfc_mapping3[src_lst], labels=legend_tx) +

         geom_text(data = c_mtx, aes(x = x1pos, y=y1pos), label="italic(iii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x2pos, y=y1pos), label="italic(ii)",  color="black", size=4, parse=T) +
         geom_text(data = c_mtx, aes(x = x3pos, y=y1pos), label="italic(i)",  color="black", size=4, parse=T) +


        theme_few() +
        theme(legend.position = c(0.28,0.66),

            panel.background = element_rect(fill='transparent'), #transparent panel bg
            plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg

            legend.text = element_text(size=12, hjust=0),
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.direction = "vertical",

            panel.grid.major = element_blank(), #remove major gridlines
            panel.grid.minor = element_blank(), #remove minor gridlines
            legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
            legend.box.background = element_rect(fill='transparent', color=NA), #transparent legend panel

            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y.right = element_blank(),
            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = element_blank()) +
            guides(
               linetype=guide_legend(nrow=1)
               )  

    # return the inset panel
    return(p)
}

# plot the time series of the variables for the temperature spike period
# fol: folder name of the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year of the plot
# prev_st_yr: start year of the previous period
# prev_ed_yr: end year of the previous period
# curr_st_yr: start year of the current period
# curr_ed_yr: end year of the current period
# return: the plot of the variables for the temperature spike period
plot_mei_amo_rfc_diff_2010 <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=2008, prev_st_yr=2010, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024){

    # define the variable list 
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")#"resi", 
    src_lst1 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")#"resi",,

    # get the data matrix for the variables
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)

    src_mtx <- subset(src_mtx,select=c("year", "source", "p025", "p500", "p975"))
    colnames(src_mtx) <- c("year", "source", "p025", "p500", "p975")

    tem_mtx <- subset(src_mtx, year>=start_yr&year<=curr_ed_yr)
    
    cor_tx12 <- tx_rfc_mapping9[src_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    cor_tx12["est"] <- paste0("bold(N)~Total~sum~'('*bold(a)*'~'*bold(m)*')'")
    cor_tx12["obs"] <- paste0("bold(O)~", src_tag)


    if(grepl("mei_amo",obs_src,fixed = FALSE)){
        cor_tx12["vari"] <-  "bold(p)~variability"
    }else if(obs_src=="HadCRUTv5.0_amo"){
        cor_tx12["vari"]    <- "bold(p)~AMO"
    }else if(obs_src=="HadCRUTv5.0_pdo"){
        cor_tx12["vari"]    <- "bold(p)~PDO"
    }else if(obs_src=="HadCRUTv5.0_nao"){
        cor_tx12["vari"]    <- "bold(p)~NAO"
    }else if(obs_src=="HadCRUTv5.0_dmi"){
        cor_tx12["vari"]    <- "bold(p)~DMI"
    }else if(obs_src=="HadCRUTv5.0_cti"){
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }else{
        cor_tx12["vari"]    <- "bold(p)~CTI"
    }

    tem_mtx$cate <- tem_mtx$source

    tem_mtx$source <-  factor(tem_mtx$source, levels  = src_lst)

    # define the range of the temperature spike period
    tem_ref <- subset(tem_mtx, year>=prev_st_yr&year<=prev_ed_yr)
    tem_bom <- subset(tem_mtx, year>=curr_st_yr&year<=curr_ed_yr)

    tem_ref <- aggregate(p500~source, data = tem_ref, FUN=mean)
    tem_bom <- aggregate(p500~source, data = tem_bom, FUN=mean)
    
    tem_ref$cate <- tem_ref$source
    tem_bom$cate <- tem_bom$source

    tem_arr <- merge(tem_ref, tem_bom, by=c("source","cate"))

    # define the positions of the variable lables
    src_hjust <- c(
        "rfc_CO2"     = -0.5,
        "rfc_CH4"     = -0.5,
        "rfc_N2O"     = -0.5,
        "r_fgs"       = -0.15,
        "rfc_FODS"    = -0.5,
        "rfc_FHFC"    = -0.5, 
        "rfc_FPFC"    = -0.5,
        "r_o3"        = -0.4,    
        "rfc_O3s"     = -0.5,
        "rfc_O3t"     = -0.5,
        "rfc_H2Os"    = -0.3,
        "r_abd"       = -0.1,
        "rfc_LCC"     = -0.5,
        "rfc_BCsnow"  = -0.5,
        "r_aer"       = -0.2,
        "rfc_cloud"   = -0.3,
        "rfc_volc"    = -0.4,
        "rfc_solar"   = -0.5,
        "vari"        = -0.5,
        "mei"         = -0.4,
        "amo"         = -0.5,
        "resi"        = -0.5, 
        "est"         = -0.5,
        "obs"         = -0.5
    )

    src_vjust <- c(
        "rfc_CO2"     = 1.5,
        "rfc_CH4"     = 1.5,
        
        "rfc_N2O"     = 1.5,
        "r_fgs"       = 1.5,
        "rfc_FODS"    = 1.5,
        "rfc_FHFC"    = 1.5, 
        "rfc_FPFC"    = 1.5,
        "r_o3"        = 1.5,
        "rfc_O3s"     = 1.5,
        "rfc_O3s"     = 1.5,
        "rfc_O3t"     = 1.5,
        "rfc_H2Os"    = 1.5,
        "r_abd"       = 1.5,
        "rfc_LCC"     = 1.5,
        "rfc_BCsnow"  = 1.5,
        "r_aer"       = 1.5,
        "rfc_cloud"   = 1.5,
        "rfc_volc"    = 1.5,
        "rfc_solar"   = 1.5,
        
        "vari"        = 1.5,
        "mei"        = 1.5,
        "amo"        = 1.5,
        "resi"        = 1.5, 
        "est"         = 1.5,
        "obs"         = 1.5
    )

print(src_lst1)
print(as.vector(unlist(src_hjust[src_lst1])))

    title_lbls <- data.frame(cate=src_lst1,h_just=as.vector(unlist(src_hjust[src_lst1], use.names=FALSE)),
                                            v_just=as.vector(unlist(src_vjust[src_lst1], use.names=FALSE)),
                                            labs=as.character(unlist(cor_tx12[src_lst1])))

    v_mtx <- data.frame(cate=src_lst1,x1=c(1944), x2=c(1976),x3=(1998),x4=c(2012), x5=c(curr_st_yr), x6=c(curr_ed_yr))


    # define the x-axis limits and breaks and labels
    if(start_yr==1951){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1970, 1980, 1998, 2012,2020)
        x_lbls <- c("1951", "1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==1970){
        x_lims <- c(start_yr,2020)
        x_brks <- c(start_yr, 1980, 1998, 2012,2020)
        x_lbls <- c("1970   ", "   1980", "1998 ", "2012","          2020")
    }else if(start_yr==2000){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,10)
        x_lbls <- seq(start_yr,2025,10)
    }else if(start_yr==2008){
        x_lims <- c(start_yr,2025)
        x_brks <- seq(2010,2025,5)
        x_lbls <- seq(2010,2025,5)
    }else if(start_yr==1855){
        x_lims <- c(1850,2025)
        x_brks <- seq(1850,2025,50)
        x_lbls <- seq(1850,2025,50)
    }else{
        x_lims <- c(start_yr,2025)
        x_brks <- seq(start_yr,2025,20)
        x_lbls <- seq(start_yr,2025,20)        
    }
    

    # plot the variable panel
    p <- ggplot() + 

        ylab(expression(italic(Delta*T)~relative~to~"1855-1900"~"("*degree*C*")"))+

        scale_x_continuous(limits = x_lims, breaks = x_brks, labels=x_lbls) +
        # scale_y_continuous() +

        geom_rect(data=v_mtx, aes(xmin=x5, xmax=x6, ymin=-Inf, ymax=Inf), fill="grey70", inherit.aes = FALSE, alpha=0.2) +

        geom_line(data=tem_mtx, aes(x=year, y=p500, colour=source), size=0.6, , show.legend=F) +
        geom_ribbon(data=tem_mtx, aes(x=year,ymin = p025, ymax = p975, fill=source), alpha=0.3, show.legend=F) +

        geom_segment(data = tem_ref, aes(x = c(prev_st_yr), y = p500, xend = c(prev_ed_yr+1), yend = p500), colour="grey30", linetype="dashed", size=0.3) +
        geom_segment(data = tem_bom, aes(x = c(curr_st_yr), y = p500, xend = c(curr_ed_yr), yend = p500), colour="grey30", linetype="solid", size=0.3) +

        geom_segment(data = tem_arr, aes(x = c(curr_st_yr), y = p500.x, xend = c(curr_st_yr), yend = p500.y, colour=source), linetype="solid", size=0.3, arrow = arrow(length = unit(0.2,"cm"))) +

        scale_colour_manual(values=cl_rfc_mapping3) +
        scale_fill_manual(values=cl_rfc_mapping3) +

         geom_label(data = title_lbls, aes(x=2000,y=Inf,label=labs,hjust=0,vjust=v_just), fill=alpha(c("white"),0.3), parse = TRUE, label.size = NA, lineheight = 0.5, size=3.6) +

        facet_wrap(~factor(cate, levels=src_lst1), scales="free_y",ncol=3) +

        theme_few() +
        theme(legend.position = "none",
            plot.title = element_text(size=12, hjust=0, margin=margin(0,0,0,0)),
            legend.text = element_text(size=12, hjust=0),
            legend.title = element_text(size=12, hjust=0),
            legend.box = "horizontal",
            legend.direction = "horizontal",

            axis.title.x = element_blank(),
            axis.title.y = font_element3,
            axis.text.y.right = element_blank(),
            axis.text.x = font_element3,
            axis.text.y =  element_text(size=12),
            strip.text = element_blank()) +
            guides(
               linetype=guide_legend(nrow=1)
               )  

    # return the plot
    return(p)
}


# plot the warming slowdown panel
# fol: folder name of the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year
# end_yr: end year
# comTag: tag for indicating the panel index
# return: a ggplot object
plot_tem_slowdown <- function(fol, reg, hsrc, psrc, sce, tag, obs_src="HadCRUTv5.0.2",start_yr, end_yr, comTag=3){
    stot <- function(x){
        return(subset(x,source%in%c("est","obs")))
    }
    soth <- function(x){
        return(subset(x,!source%in%c("est","obs")))
    }

    # define the variable list
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "est", "obs")#"resi", 
    src_lst1 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "esti", "obse")#"resi",,
    src_lst2 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "obs")

    # get the data matrix for the variable list
    dif_mtx <- get_tem_inc_dif(fol, reg, hsrc, psrc, sce, tag, src_lst, prev_st_yr=start_yr, prev_ed_yr=end_yr, hiat_st_yr=1998, hiat_ed_yr=2012, obs_src)
    dif_mtx$cate <- dif_mtx$source

    dif_mtx[dif_mtx$cate%in%c("rfc_volc", "rfc_solar"),]$cate <- c("r_nat")

    dif_mtx[dif_mtx$cate%in%c("mei", "amo"),]$cate <- c("vari")
	
    dif_mtx$cate1 <- c("esti")
    # dif_mtx$grp <- c("esti")
    dif_mtx[dif_mtx$source%in%c("obs"),]$cate1 <- c("obse")


    # define the category list
    cate_lst  <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os", "r_abd", "r_aer", "rfc_cloud","r_nat", "vari", "obs", "est") #"vari","resi", "est","obs"
    cate_lst1 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os", "r_abd", "r_aer", "rfc_cloud","r_nat", "vari", "esti", "obse")

    # get the data matrix for the category list
    cat_mtx <- get_tem_inc_dif(fol, reg, hsrc, psrc, sce, tag, cate_lst, prev_st_yr=start_yr, prev_ed_yr=end_yr, hiat_st_yr=1998, hiat_ed_yr=2012, obs_src)
    cat_mtx$cate <- cat_mtx$source
    cat_mtx$pos <- cat_mtx$p975
# print(soth(cat_mtx))

    # define and calculate the position of the category list
    nrw <- nrow(cat_mtx)
    for(i in c(1:nrw)){

        if(cat_mtx[i,]$cate == "obs"){
            cat_mtx[i,]$pos <- max(cat_mtx[cat_mtx$cate=="est",]$p975,0) 
        }else if(cat_mtx[i,]$cate == "est"){
            cat_mtx[i,]$pos <- max(cat_mtx[cat_mtx$cate=="est",]$p975,0, sum(subset(soth(dif_mtx),p500>=0)$p500)) 
        }
        else{
            cat_mtx[i,]$pos <- max(c(cat_mtx[i,]$p975,0,dif_mtx[dif_mtx$cate==cat_mtx[i,]$cate,]$p500))
        }
        
    }

    # define the y-axis limits and breaks and labels
    y_lims <- c(-0.138, 0.068)
    y_brks <- seq(-0.1, 0.068, 0.05)
    y_lbls <- seq(-0.1, 0.068, 0.05)         

    x_lbls <- tx_rfc_mapping5[cate_lst1]
    
    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    x_lbls["obse"] <- parse(text=src_tag)

    dif_mtx$cate <- gsub("est","total",dif_mtx$cate,fixed=T)
    dif_mtx$cate <- gsub("obs","total",dif_mtx$cate,fixed=T)

    cat_mtx$cate <- gsub("est","total",cat_mtx$cate,fixed=T)
    cat_mtx$cate <- gsub("obs","total",cat_mtx$cate,fixed=T)

    cat_mtx$cate1 <- c("esti")
    # dif_mtx$grp <- c("esti")
    cat_mtx[cat_mtx$source%in%c("obs"),]$cate1 <- c("obse")

    dif_mtx$source <- factor(dif_mtx$source, levels=rev(src_lst))
    cat_mtx$source <- factor(cat_mtx$source, levels=rev(cate_lst))
    

print("slowdown")
print(dif_mtx)
    if(comTag==3){
        ttl <- expression("1998-2012"~warming~slowdown)
    }else{
        ttl <- expression("1998-2012"~warming~slowdown)
    }

    # plot the main panel
    p_main <- ggplot() + 
        ggtitle(ttl) +
        ylab(bquote(atop("DDT between 1998-2012 & ",.(start_yr)*"-"*.(end_yr)~" ("*degree*C*" per decade)")))+
        scale_y_continuous(limits = y_lims, breaks = y_brks, labels=y_lbls) +
        scale_x_discrete(limits = cate_lst1, breaks = cate_lst1, labels=parse(text=x_lbls)) +
        geom_hline(yintercept = 0, colour="grey30", size=0.3) +

        geom_col(data=soth(dif_mtx), aes(x=cate, y=p500, fill=source), colour="black",linewidth=0.4, width=0.5) +
        geom_point(data=soth(cat_mtx), aes(x=cate, y=p500), colour="grey30", size=1.5) +
        geom_errorbar(data=soth(cat_mtx),aes(x=cate, ymin=p025, ymax=p975), colour="black", width=0.25,linewidth=0.4)+    
        geom_text(data = soth(cat_mtx), aes(x=cate,y=pos,label=format(round(p500,3), nsmall = 2.8)), vjust=-1, size=4) + 

        geom_col(data=subset(dif_mtx,source!="est"), aes(x=cate1, y=p500, fill=source), colour="black",linewidth=0.4, width=0.5) +
        geom_point(data=stot(cat_mtx), aes(x=cate1, y=p500), colour="grey30", size=1.5) +
        geom_errorbar(data=stot(cat_mtx),aes(x=cate1, ymin=p025, ymax=p975), colour="black", width=0.25, linewidth=0.4)+    
        geom_text(data = stot(cat_mtx), aes(x=cate1,y=pos ,label=format(round(p500,3), nsmall = 3)), vjust=-1, size=4) + 

        scale_fill_manual(name=NULL,breaks=src_lst2, limits=src_lst2, values=cl_rfc_mapping3, labels=tx_rfc_mapping6) +

        geom_vline(xintercept = 11.5, colour="black", linetype = "longdash", linewidth=0.4) +

        theme_few() +
        theme(legend.position = "none", 
            plot.title = font_element3,
            legend.background = element_rect(fill="transparent"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=14,hjust=0.5),
            axis.text.x = element_blank(),
            axis.text.y = font_element3,
            strip.text = element_blank()) +
            guides(
               fill=guide_legend(nrow=3))  
         

    # plot the inset panel
    p_in <- plot_rfc_regr_slowdown(fol, reg, hsrc, psrc, sce, tag,obs_src)

    # combine the main panel and inset panel
    p <-
        ggdraw() +
        draw_plot(p_main) +
        draw_plot(p_in, x = 0.2, y = 0.04, width = 0.47, height = .45)

    # return the combined panel
    return(p)
}

# plot the cooling phase panel
# fol: folder name of the data
# reg: region name
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# start_yr: start year of the cooling phase
# end_yr: end year of the cooling phase
# comTag: a tag for indicating the index of the panel
# return: a ggplot object
plot_tem_coolphase <- function(fol, reg, hsrc, psrc, sce, tag, obs_src="HadCRUTv5.0.2",start_yr, end_yr, comTag=3){
    stot <- function(x){
        return(subset(x,source%in%c("est","obs")))
    }
    soth <- function(x){
        return(subset(x,!source%in%c("est","obs")))
    }


    # define the variable list
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "est", "obs")#"resi", 
    src_lst1 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "esti", "obse")#"resi",,
    src_lst2 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "obs")

    # get the data matrix for the variable list
    dif_mtx <- get_tem_dec(fol, reg, hsrc, psrc, sce, tag, src_lst, st_yr=start_yr, ed_yr=end_yr, obs_src)

    dif_mtx$cate <- dif_mtx$source

    dif_mtx[dif_mtx$cate%in%c("rfc_volc", "rfc_solar"),]$cate <- c("r_nat")

    dif_mtx[dif_mtx$cate%in%c("mei", "amo"),]$cate <- c("vari")
	
    dif_mtx$cate1 <- c("esti")
    # dif_mtx$grp <- c("esti")
    dif_mtx[dif_mtx$source%in%c("obs"),]$cate1 <- c("obse")

    # define the category list
    cate_lst  <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os", "r_abd", "r_aer", "rfc_cloud","r_nat", "vari", "obs", "est") #"vari","resi", "est","obs"
    cate_lst1 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os", "r_abd", "r_aer", "rfc_cloud","r_nat", "vari", "esti", "obse")

    # get the data matrix for the category list
    cat_mtx <- get_tem_dec(fol, reg, hsrc, psrc, sce, tag, cate_lst, st_yr=start_yr, ed_yr=end_yr, obs_src)
    # print(head(cat_mtx))
    cat_mtx$cate <- cat_mtx$source
    cat_mtx$pos <- cat_mtx$p975

    # define and claculate the label position for the category list
    nrw <- nrow(cat_mtx)
    for(i in c(1:nrw)){

        if(cat_mtx[i,]$cate == "obs"){
            cat_mtx[i,]$pos <- max(cat_mtx[cat_mtx$cate=="est",]$p975,0) 
        }else if(cat_mtx[i,]$cate == "est"){
            cat_mtx[i,]$pos <- max(cat_mtx[cat_mtx$cate=="est",]$p975,0, sum(subset(soth(dif_mtx),p500>=0)$p500)) 
        }
        else{
            cat_mtx[i,]$pos <- max(c(cat_mtx[i,]$p975,0,dif_mtx[dif_mtx$cate==cat_mtx[i,]$cate,]$p500))
        }
        
    }

    dif_mtx$cate <- factor(dif_mtx$cate, levels=cate_lst)
    cat_mtx$cate <- factor(cat_mtx$cate, levels=cate_lst)

    # define the y-axis limits and breaks and labels
    y_lims <- c(-0.13, 0.14)
    y_brks <- seq(-0.1, 0.1, 0.05)
    y_lbls <- seq(-0.1, 0.1, 0.05)         

    x_lbls <- tx_rfc_mapping5[cate_lst1]

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    x_lbls["obse"] <- parse(text=src_tag)
    tx_rfc <- tx_rfc_mapping6
    tx_rfc["obs"] <- parse(text=src_tag)

    dif_mtx$cate <- gsub("est","total",dif_mtx$cate,fixed=T)
    dif_mtx$cate <- gsub("obs","total",dif_mtx$cate,fixed=T)

    cat_mtx$cate <- gsub("est","total",cat_mtx$cate,fixed=T)
    cat_mtx$cate <- gsub("obs","total",cat_mtx$cate,fixed=T)

    cat_mtx$cate1 <- c("esti")
    # dif_mtx$grp <- c("esti")
    cat_mtx[cat_mtx$source%in%c("obs"),]$cate1 <- c("obse")

    dif_mtx$source <- factor(dif_mtx$source, levels=rev(src_lst))
    cat_mtx$source <- factor(cat_mtx$source, levels=rev(cate_lst))
    

print("coolphase")
print(dif_mtx)

    if(comTag==3){
        ttl <- expression("1944-1976"~cooling~phase)
    }else{
        ttl <- expression("1944-1976"~cooling~phase)
    }

    # plot the main figure
    p_main <- ggplot() + 
        ggtitle(ttl) +
        ylab(bquote(atop("Decadal trend between"~.(start_yr)*"-"*.(end_yr)," ("*degree*C*" per decade)")))+
        scale_y_continuous(limits = y_lims, breaks = y_brks, labels=y_lbls) +
        scale_x_discrete(limits = cate_lst1, breaks = cate_lst1, labels=parse(text=x_lbls)) +
        geom_hline(yintercept = 0, colour="grey30", size=0.3) +

        geom_col(data=soth(dif_mtx), aes(x=cate, y=p500, fill=source), colour="black",linewidth=0.4, width=0.5) +
        geom_point(data=soth(cat_mtx), aes(x=cate, y=p500), colour="grey30", size=1.5) +
        geom_errorbar(data=soth(cat_mtx),aes(x=cate, ymin=p025, ymax=p975), colour="black", width=0.25,linewidth=0.4)+    
        geom_text(data = soth(cat_mtx), aes(x=cate,y=pos,label=format(round(p500,3), nsmall = 2.8)), vjust=-1, size=4) + 

        geom_col(data=subset(dif_mtx,source!="est"), aes(x=cate1, y=p500, fill=source), colour="black",linewidth=0.4, width=0.5) +
        geom_point(data=stot(cat_mtx), aes(x=cate1, y=p500), colour="grey30", size=1.5) +
        geom_errorbar(data=stot(cat_mtx),aes(x=cate1, ymin=p025, ymax=p975), colour="black", width=0.25, linewidth=0.4)+    
        geom_text(data = stot(cat_mtx), aes(x=cate1,y=pos ,label=format(round(p500,3), nsmall = 3)), vjust=-1, size=4) + 

        scale_fill_manual(name=NULL,breaks=src_lst2, limits=src_lst2, values=cl_rfc_mapping3, labels=tx_rfc) +

        geom_vline(xintercept = 11.5, colour="black", linetype = "longdash", linewidth=0.4) +

        theme_few() +
        theme(legend.position = "none", 
            plot.title = font_element3,
            legend.title = element_blank(),
            legend.text = font_element3,
            legend.background = element_rect(fill="transparent"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=14,hjust=1),
            axis.text.x = element_blank(),
            axis.text.y = font_element3,
            strip.text = element_blank()) +
            guides(
               fill=guide_legend(nrow=3))  
         

    if(comTag==2){
        p_main <- p_main + theme(
            axis.text.x = element_text(size=14.,hjust=1,vjust=0.9,angle=20)
        )
    }
    
    # plot the inset figure
    p_in <- plot_rfc_regr_coolphase(fol, reg, hsrc, psrc, sce, tag,obs_src, start_yr=1930, end_yr=1990)

    if(comTag==2){
        inset_y <- 0.24
        inset_h <- 0.32
    }else{
        inset_y <- 0.02
        inset_h <- 0.43
    }
    
    # combine the main figure and the inset figure
    p <- 
        ggdraw() +
        draw_plot(p_main) +
        draw_plot(p_in, x = 0.12, y = inset_y, width = 0.47, height = inset_h)

    # return the main figure and the inset figure
    return(list(pall=p,pbar=p_main))
}

# plot the bar plot of the variables
# fol: folder name of the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# prev_st_yr: start year of the previous period
# prev_ed_yr: end year of the previous period
# curr_st_yr: start year of the current period
# curr_ed_yr: end year of the current period
# return the plot
plot_tem_bom_bar2 <- function(fol, reg, hsrc, psrc, sce, tag, obs_src="HadCRUTv5.0.2_mei_amo_rfc", prev_st_yr=2010, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024){

    # define the variable list
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "est", "obs")#"resi", 
    src_lst1 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "esti", "obse")#"resi",,
    src_lst2 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo", "obs")

    # get the data matrix of the variables
    dif_mtx <- get_tem_bom(fol, reg, hsrc, psrc, sce, tag, src_lst, prev_st_yr, prev_ed_yr, curr_st_yr, curr_ed_yr, obs_src=obs_src)
    # print(dif_mtx)
    dif_mtx$cate <- dif_mtx$source

    dif_mtx[dif_mtx$cate%in%c("rfc_volc", "rfc_solar"),]$cate <- c("r_nat")


    dif_mtx[dif_mtx$cate%in%c("mei", "amo"),]$cate <- c("vari")
	

    dif_mtx$cate1 <- c("esti")

    dif_mtx[dif_mtx$source%in%c("obs"),]$cate1 <- c("obse")
    
    # define the category list
    cate_lst  <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os", "r_abd", "r_aer", "rfc_cloud","r_nat", "vari", "obs", "est") #"vari","resi", "est","obs"
    cate_lst1 <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os", "r_abd", "r_aer", "rfc_cloud","r_nat", "vari", "esti", "obse")

    # get the data matrix of the categories
    cat_mtx <- get_tem_bom(fol, reg, hsrc, psrc, sce, tag, cate_lst, prev_st_yr, prev_ed_yr, curr_st_yr, curr_ed_yr, obs_src=obs_src)
    # print(head(cat_mtx))
    cat_mtx$cate <- cat_mtx$source
    cat_mtx$pos <- cat_mtx$p025
    cat_mtx$low <- cat_mtx$p975

    stot <- function(x){
        return(subset(x,source%in%c("est","obs")))
    }
    soth <- function(x){
        return(subset(x,!source%in%c("est","obs")))
    }

    dif_mtx$cate <- gsub("est","total",dif_mtx$cate,fixed=T)
    dif_mtx$cate <- gsub("obs","total",dif_mtx$cate,fixed=T)

    cat_mtx$cate <- gsub("est","total",cat_mtx$cate,fixed=T)
    cat_mtx$cate <- gsub("obs","total",cat_mtx$cate,fixed=T)

    cat_mtx$cate1 <- c("esti")

    cat_mtx[cat_mtx$source%in%c("obs"),]$cate1 <- c("obse")

    nrw <- nrow(cat_mtx)
    # define the position (upper) and low of the categories
    for(i in c(1:nrw)){
        cat_mtx[i,]$pos <- max(c(cat_mtx[i,]$p975,0))
        cat_mtx[i,]$low <- min(c(cat_mtx[i,]$p025,0))
    }

    # deine the position and size of the inset panel
    y_max <- max(cat_mtx$pos) * 1.13
    if(prev_st_yr==2010){
        y_min <- -0.022
        inset_x = 0.15
        inset_y = .43
        inset_h = .46
    }else{
        y_min <- -0.004
        inset_x = 0.12
        inset_y = .39
        inset_h = .48
    }
    

    y_lims <- c(y_min, y_max)

    print(y_lims)
    # y_brks <- seq(0, 0.1, 0.02)
    # y_lbls <- seq(0, 0.1, 0.02)    

    x_lbls <- tx_rfc_mapping5[cate_lst1]
    
    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","~",src_tag,fixed=T)

    x_lbls["obse"] <- parse(text=src_tag)
    tx_rfc <- tx_rfc_mapping6
    tx_rfc["obs"] <- parse(text=src_tag)


    dif_mtx$source <- factor(dif_mtx$source, levels=rev(src_lst))
    cat_mtx$source <- factor(cat_mtx$source, levels=rev(cate_lst))

print("spike")
print(dif_mtx)

    # plot the main bar plot
    p_main <- ggplot() + 
        ggtitle(expression(bold(P)~Attribution~of~temperature~spike~"(2022-2024)")) +
        ylab(bquote(atop(italic(Delta*T)~between,"2022-2024 & "*.(prev_st_yr)*"-"*.(prev_ed_yr)*" ("*degree*C*")")))+
        scale_y_continuous(limits=y_lims) +
        scale_x_discrete(limits = cate_lst1, breaks = cate_lst1, labels=x_lbls) +
        geom_hline(yintercept = 0, colour="grey30", size=0.3) +

        geom_col(data=soth(dif_mtx), aes(x=cate, y=p500, fill=source), colour="black",linewidth=0.4, width=0.5) +
        geom_point(data=soth(cat_mtx), aes(x=cate, y=p500), colour="grey30", size=1.5) +
        geom_errorbar(data=soth(cat_mtx),aes(x=cate, ymin=p025, ymax=p975), colour="black", width=0.25,linewidth=0.4)+    
        geom_text(data = soth(cat_mtx), aes(x=cate,y=pos,label=format(round(p500,3), nsmall = 2.8)), vjust=-1, size=4) + 

        geom_col(data=subset(dif_mtx,source!="est"), aes(x=cate1, y=p500, fill=source), colour="black",linewidth=0.4, width=0.5) +
        geom_point(data=stot(cat_mtx), aes(x=cate1, y=p500), colour="grey30", size=1.5) +
        geom_errorbar(data=stot(cat_mtx),aes(x=cate1, ymin=p025, ymax=p975), colour="black", width=0.25, linewidth=0.4)+    
        geom_text(data = stot(cat_mtx), aes(x=cate1,y=pos ,label=format(round(p500,3), nsmall = 3)), vjust=-1, size=4) + 

        scale_fill_manual(name=NULL, breaks=src_lst2, limits=src_lst2, values=cl_rfc_mapping3, labels=tx_rfc) +

        geom_vline(xintercept = 11.5, colour="black", linetype = "longdash", linewidth=0.4) +

        theme_few() +
        theme(legend.position = "none", 
            legend.title = element_blank(),
            legend.text = element_text(size=12.3,hjust=0.),
            legend.background = element_rect(fill="transparent"),
            plot.title = element_text(size=13,hjust=0.),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=13,hjust=0.),
            axis.text.x = element_text(size=13,hjust=1,vjust=1,angle=20),
            axis.text.y = element_text(size=13,hjust=0.5),
            strip.text = element_blank()) +
            guides(
               fill=guide_legend(nrow=3))  

    # plot the inset panel
    p_in <- plot_rfc_regr_spike(fol, reg, hsrc, psrc, sce, tag,obs_src, start_yr=2000, prev_st_yr)

    # combine the main bar plot and inset panel
    p <-
        ggdraw() +
        draw_plot(p_main) +
        draw_plot(p_in, x = inset_x, y = inset_y, width = .6, height = inset_h)

    # return the plot
    return(list(p_all=p,p_inset=p_in,p_bar=p_main))
}

# plot the temperature spike figure using HadCRUTv5.0.2
# fol: folder name of the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# prev_st_yr: previous start year
# prev_ed_yr: previous end year
# curr_st_yr: current start year
# curr_ed_yr: current end year
plot_regre_spike <- function(fol, reg, hsrc, psrc, sce, tag, obs_src, prev_st_yr=2010, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024){

    # plot the time series of the variables
    p_var <- plot_mei_amo_rfc_diff_2010(fol, reg, hsrc, psrc, sce, tag, obs_src=obs_src,start_yr=2000, prev_st_yr, prev_ed_yr, curr_st_yr, curr_ed_yr)

    # plot the bar plot of the variables
    p_tot <- plot_tem_bom_bar2(fol, reg, hsrc, psrc, sce, tag, obs_src, prev_st_yr, prev_ed_yr, curr_st_yr, curr_ed_yr)

    # plot the legend
    l_var <- g_legend(p_tot$p_inset + theme(legend.position = "bottom"))
    l_tot <- g_legend(p_tot$p_bar + theme(legend.position = "bottom"))

    # plot the combined legend
    p_leg <- arrangeGrob(
        l_var,
        l_tot,
        nrow = 1,
        widths=c(0.16,0.84)
    )


    p_com <- arrangeGrob(
        p_var,
        p_tot$p_all,
        p_leg,
        nrow = 3,
        heights=c(0.58,0.32,0.1)
    )



    src_tag <- gsub("v5.0.2","v5_0_2",obs_src,fixed=T)

    # save the plot
    fl_plot = paste0(fig_dir,"fig_spike_",src_tag, "_", prev_st_yr, fl_ext)
    ggsave(fl_plot, p_com, width = 26, height = 32, units = "cm")

    print(paste0(fl_plot, " saved."))
}

# plot the climate variation panel for the cooling phase and warming slowdown
# fol: folder name of the data
# reg: region
# hsrc: historical source
# psrc: projection source
# sce: scenario
# tag: variable tag
# obs_src: observation source
# plot the figure and return NULL
plot_anorm_hist <- function(fol, reg, hsrc, psrc, sce, tag, obs_src){
    # plot the cooling phase panel
    p_cool <- plot_tem_coolphase(fol, reg, hsrc, psrc, sce, tag, obs_src,start_yr=1944, end_yr=1976, comTag=2)

    # plot the warming slowdown panel
    p_slow <- plot_tem_slowdown(fol, reg, hsrc, psrc, sce, tag, obs_src,start_yr=1970, end_yr=2012, comTag=2)

# print(p_cool)
    # plot the legend
    p_inset_leg <- g_legend(plot_regress_tot(fol, reg, hsrc, psrc, sce, tag,obs_src))
    p_tot_leg <- g_legend(p_cool$pbar + theme(legend.position = "bottom"))
    
    # combine the panels
    p_com <- ggarrange(
        p_slow,
        p_cool$pall,
        ncol = 1,
        heights=c(0.45,0.58),
        labels = c("A", "B"),
        font.label = list(size = 14, face = "bold")
    )

    p_all <- arrangeGrob(
        p_com,
        p_inset_leg,
        p_tot_leg,
        ncol = 1,
        heights=c(0.805,0.04,0.14)
    )

    src_tag <- gsub("v5.0.2","v5_0_2",obs_src,fixed=T)


    fl_plot = paste0(fig_dir,"fig_anorm_hist_",src_tag, fl_ext)
    ggsave(fl_plot, p_all, width = 24, height = 22, units = "cm")

    print(paste0(fl_plot, " saved."))
}

# plot the demonstration of the regression, as Fig. 1 in the manuscript
# fol: the folder name of the data
# reg: the region name
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the tag of the data
# obs_src: the observation source
# start_yr: the start year of the data
plot_demo <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr){
    # the observation panel
    p_obse <- plot_obs(fol, reg, hsrc, psrc, sce, tag,obs_src, start_yr)
    # the model selection panel
    p_stat <- plot_stat_coef(obs_src, "BIC")
    # the regression panel
    p_demo <- plot_mei_amo_demo(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr)

    # combine the panels
    p_all <- ggarrange(
        p_obse,
        p_stat,
        p_demo,
        ncol = 1,
        heights=c(0.4,0.4,0.4),
        labels = c("A", "B", "C"),
        font.label = list(size = 14, face = "bold")
    )

    src_tag <- gsub("v5.0.2","v5_0_2",obs_src,fixed=T)

    # save to figure file
    fl_plot = paste0(fig_dir,"fig_demo_",src_tag, fl_ext)
    ggsave(fl_plot, p_all, width = 24, height = 30, units = "cm")

    print(paste0(fl_plot, " saved."))
}

# save the main climate variables for review
# fol: the folder name of the data
# reg: the region name
# hsrc: the historical source
# psrc: the projection source
# sce: the scenario
# tag: the variable tag
# obs_src: the observation source
# start_yr: the start year of the data
# end_yr: the end year of the data
# save the data and return NULL
sav_corr_mtx <- function(fol, reg, hsrc, psrc, sce, tag,obs_src,start_yr=1855, end_yr =2024){

    # define the variable list
    src_lst <- c("rfc_CO2", "rfc_CH4", "rfc_N2O", "r_fgs", "r_o3", "rfc_H2Os",  "r_abd", "r_aer", "rfc_cloud", "rfc_volc", "rfc_solar", "mei", "amo","est", "obs")#"resi", , "obs","est"

    # get the data matrix for the variables
    src_mtx <- get_tem_mtx(fol, reg, hsrc, psrc, sce, tag, src_lst,obs_src, preindust=T)
# print(length(unique(src_mtx$source)))

    # reshape the data matrix to wide format (median)
    tem_cst <- reshape2::dcast(src_mtx, year ~ source, value.var = "p500")    
    tem_cst <- tem_cst[,c("year",src_lst)]
    tem_cst <- subset(tem_cst, year>=start_yr&year<=end_yr)

    # reshape the data matrix to wide format (0.025 quantile)
    tem_cst_025 <- reshape2::dcast(src_mtx, year ~ source, value.var = "p025")    
    tem_cst_025 <- tem_cst_025[,c("year",src_lst)]
    tem_cst_025 <- subset(tem_cst_025, year>=start_yr&year<=end_yr)

    # reshape the data matrix to wide format (0.975 quantile)   
    tem_cst_975 <- reshape2::dcast(src_mtx, year ~ source, value.var = "p975")
    tem_cst_975 <- tem_cst_975[,c("year",src_lst)]
    tem_cst_975 <- subset(tem_cst_975, year>=start_yr&year<=end_yr)

    tx_rfc <- tx_rfc_mapping10

    src_tag <- gsub("_v5.0","v5.0",obs_src,fixed=T)
    src_tag <- gsub("_","",src_tag,fixed=T)

    tx_rfc["obs"] <- src_tag

    ncl <- ncol(tem_cst)
    for(i in c(2:ncl)){
        colnames(tem_cst)[i] <- tx_rfc_mapping10[colnames(tem_cst)[i]][[1]]
        colnames(tem_cst_025)[i] <- tx_rfc_mapping10[colnames(tem_cst_025)[i]][[1]]
        colnames(tem_cst_975)[i] <- tx_rfc_mapping10[colnames(tem_cst_975)[i]][[1]]
    }

    # detrend the data
    tem_cst_detr <- tem_cst
    ncl <- ncol(tem_cst_detr)
    for(i in c(2:ncl)){
        tem_cst_detr[,c(i)] <- detrend(tem_cst_detr[,c(i)])
    }

    corr_mtx <- round(cor(tem_cst[,-c(1)]),3)
    corr_detr_mtx <- round(cor(tem_cst_detr[,-c(1)]),3)

    # save the data for the variables (median)
    sav_csv <- paste0(final_dat, src_tag, "_", start_yr, "_", end_yr,"_median.csv")
    write.csv(tem_cst, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   

    # save the data for the variables (0.025 quantile)
    sav_csv <- paste0(final_dat, src_tag, "_", start_yr, "_", end_yr,"_025.csv")
    write.csv(tem_cst_025, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   

    # save the data for the variables (0.975 quantile)
    sav_csv <- paste0(final_dat, src_tag, "_", start_yr, "_", end_yr,"_975.csv")
    write.csv(tem_cst_975, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   

    # save the correlation matrix
    sav_csv <- paste0(final_dat, src_tag, "_", start_yr, "_", end_yr,"_cor.csv")
    write.csv(corr_mtx, file = sav_csv, row.names=T)
    print(paste0(sav_csv, " saved."))   

    fl_tex <- paste0("./latex/", src_tag, "_", start_yr, "_", end_yr,"_cor.tex")

    # print the correlation matrix to a latex table 
    print(xtable(corr_mtx, type = "latex",digits=3), file = fl_tex, include.rownames=T, sanitize.text.function = function(x){x})
    print(paste0(fl_tex, " saved."))

    # save the detrended data
    sav_csv <- paste0(final_dat, src_tag, "_", start_yr, "_", end_yr,"_detr.csv")
    write.csv(tem_cst_detr, file = sav_csv, row.names=F)
    print(paste0(sav_csv, " saved."))   

    # save the correlation matrix for the detrended data
    sav_csv <- paste0(final_dat, src_tag, "_", start_yr, "_", end_yr,"_cor_detr.csv")
    write.csv(corr_detr_mtx, file = sav_csv, row.names=T)
    print(paste0(sav_csv, " saved."))   
}

# get the statistical coefficient matrix for the specified regression
# osrc: the observation source
# exp_tag: the regression tag
# coef: the statistical coefficient name
get_stat_coef <- function(osrc, exp_tag, coef) {
    
    coef_all <- NULL
    coef_vt <- NULL 

    # define the csv data file
    sub_dir <- paste0(osrc,"_stat_rfc/")
    stat_csv <- paste0(slow_dat,sub_dir, "est_cof_", osrc,"_", exp_tag, ".csv")  
        
    # load the statistical coefficient matrix from the csv file
    if(file.exists(stat_csv)){
        coef_mtx <- read.table(stat_csv, header = TRUE, check.names=F, sep=",")

        # append the statistical coefficient matrix of ens to the total coefficient matrix
        if(is.null(coef_all)){
            coef_all <- coef_mtx
        }else{
            coef_all <- rbind(coef_all, coef_mtx)
        }
    }    	
    

    # create the data frame for the statistical coefficient
    if(!is.null(coef_all)){
        print(nrow(coef_all))
        coef_vt <- data.frame(obs=c(osrc),experiment=c(exp_tag),coefficient=c(coef), mean=mean(coef_all[,c(coef)]),sd=sd(coef_all[,c(coef)]))
    }

    # return the statistical coefficient data frame
    return(coef_vt)
}

# get the fitting coefficient matrix for the specified regression and coefficient
# osrc: the observation source
# exp_tag: the regression tag
# coef: the statistical coefficient name
# return the fitting coefficient data frame
get_stat_fit <- function(osrc, exp_tag, coef) {
    sub_dir <- paste0(osrc,"_stat_rfc/")

    coef_vt <- NULL 
    coef_all <- NULL


    stat_csv <- paste0(slow_dat,sub_dir, "est_fit_", osrc, "_", exp_tag, ".csv")  
    if(file.exists(stat_csv)){
        # load the fitting coefficient matrix from the csv file
        coef_mtx <- read.table(stat_csv, header = TRUE, check.names=F, sep=",")
        if(coef%in%colnames(coef_mtx)){
            # print(coef)
            # append the fitting coefficient matrix to the overall matrix
            if(is.null(coef_all)){
                coef_all <- coef_mtx
            }else{
                coef_all <- rbind(coef_all, coef_mtx)
            }               
        }
        
    }    	
    
    # define the data frame for the statistical coefficient
    if(!is.null(coef_all)){
        print(nrow(coef_all))
        coef_vt <- data.frame(obs=c(osrc),experiment=c(exp_tag),coefficient=c(coef), mean=mean(coef_all[,c(coef)]),sd=sd(coef_all[,c(coef)]))
    }
    # return the fitting coefficient data frame
    return(coef_vt)
}

# get the statistical coefficient matrix for the selected regressions
# osrc: the observation source
# coef: the statistical coefficient name
get_stat_coef_all <- function(osrc, coef) {
    coef_mtx <- NULL
    # iterate over the selected regressions
    for(exp in exp_lst){
        # get the confidence interval of statistical coefficient for the specified regression
        coef_vt <- get_stat_coef_ci(osrc, exp, coef)

        # append the statistical coefficient vector to the overall matrix
        if(is.null(coef_mtx)){
            coef_mtx <- data.frame(obs=c(osrc),experiment=c(exp),coefficient=c(coef), lwr=c(coef_vt["lwr"]), med=c(coef_vt["med"]), upr=c(coef_vt["upr"]))
        }else{
            coef_mtx <- rbind(coef_mtx,c(obs=c(osrc),experiment=c(exp),coefficient=c(coef), lwr=c(coef_vt["lwr"]), med=c(coef_vt["med"]), upr=c(coef_vt["upr"])))
        }
        coef_mtx <- data.frame(coef_mtx)
        coef_mtx$lwr <- as.numeric(as.character(coef_mtx$lwr))
        coef_mtx$med <- as.numeric(as.character(coef_mtx$med))
        coef_mtx$upr <- as.numeric(as.character(coef_mtx$upr))
    }

    # return the statistical coefficient matrix
    return(coef_mtx)
}

# get the statistical coefficient matrix for the specified regression
# osrc: the observation source
# exp: the regression tag
# return the statistical coefficient matrix
get_stat_exp <- function(osrc, exp) {
    coef_mtx <- NULL

    # iterate over the selected coefficients
    for(coef in c("sigma","r.squared","estr","AIC","BIC")){
        # print(coef)
        # get the statistical coefficient matrix for the specified regression and coefficient
        coef_vt <- get_stat_coef(osrc,exp, coef)
        # append the statistical coefficient matrix to the overall matrix
        if(is.null(coef_mtx)){
            coef_mtx <- coef_vt
        }else{
            coef_mtx <- rbind(coef_mtx,coef_vt)
        }        
    }
    # return the statistical coefficient matrix
    return(coef_mtx)
}

# get the fitting coefficient matrix for the specified regression
# osrc: the observation source
# exp: the regression tag
# return the fitting coefficient matrix
get_fit_exp <- function(osrc, exp) {
    coef_mtx <- NULL
    # iterate over the selected coefficients
    for(coef in c("(Intercept)","r_anthro","r_ghg", "r_othanthro", "r_nat", "rfc_volc","rfc_solar","mei0","amo0","pdo0","nao0","dmi0")){
        
        # get the fitting coefficient matrix for the specified regression and coefficient
        coef_vt <- get_stat_fit(osrc,exp, coef)

        # append the fitting coefficient matrix to the overall matrix
        if(!is.null(coef_vt)){
            if(is.null(coef_mtx)){
                coef_mtx <- coef_vt
            }else{
                coef_mtx <- rbind(coef_mtx,coef_vt)
            }               
        }
     
    }
    # return the fitting coefficient matrix
    return(coef_mtx)
}

# get the statistical coefficient values (mean and sd) for the specified coefficient
# src_mtx: the statistical coefficient matrix
# coef: the statistical coefficient name
# return the associated latex text
get_stat_range <- function(src_mtx, coef) {
    stat_tex <- NULL
    if(coef%in%c(unique(src_mtx$coefficient))){
        stat_mtx <- subset(src_mtx, coefficient==coef)
        stat_tex <- paste0(format(round(stat_mtx$mean,coef_digit[coef]),nsmall=coef_digit[coef]),"$\\pm$",
                        format(round(stat_mtx$sd,coef_digit[coef]),nsmall=coef_digit[coef]))        
    }

    return(stat_tex)
}

# get the statistical coefficient matrix a regression model
# osrc: the source of the observation
# exp_tag: the regression model tag
# coef: the statistical coefficient name
# return the statistical coefficient matrix
get_stat_coef_value <- function(osrc, exp_tag, coef) {
    coef_all <- NULL
    coef_vt <- NULL 

    # define the csv data file
    sub_dir <- paste0(osrc,"_stat_rfc/")
    stat_csv <- paste0(slow_dat,sub_dir, "est_cof_", osrc,"_", exp_tag, ".csv")  
        
    # load the statistical coefficient matrix from the csv file
    if(file.exists(stat_csv)){
        coef_mtx <- read.table(stat_csv, header = TRUE, check.names=F, sep=",")

        # append the statistical coefficient matrix of ens to the total coefficient matrix
        if(is.null(coef_all)){
            coef_all <- coef_mtx
        }else{
            coef_all <- rbind(coef_all, coef_mtx)
        }
    }    	
    

    # create the data frame for the statistical coefficient
    if(!is.null(coef_all)){
        print(nrow(coef_all))
        coef_vt <- data.frame(obs=c(osrc),experiment=c(exp_tag),coefficient=c(coef), value=coef_all[,c(coef)])
    }

    # return the statistical coefficient data frame
    return(coef_vt)
}

# get the fitting coefficient matrix a regression model
# osrc: the source of the observation
# exp_tag: the regression model tag
# coef: the fitting coefficient name
# return the fitting coefficient matrix
get_fit_coef_value <- function(osrc, exp_tag, coef) {
    # HadCRUTv5.0.2 has 200 ensemble members

    sub_dir <- paste0(osrc,"_stat_rfc/")

    coef_vt <- NULL 
    coef_all <- NULL

    # iterate over the ensemble members

    stat_csv <- paste0(slow_dat,sub_dir, "est_fit_", osrc, "_", exp_tag, ".csv")  
    if(file.exists(stat_csv)){
        # load the fitting coefficient matrix from the csv file
        coef_mtx <- read.table(stat_csv, header = TRUE, check.names=F, sep=",")
        if(coef%in%colnames(coef_mtx)){
            # print(coef)
            # append the fitting coefficient matrix to the overall matrix
            if(is.null(coef_all)){
                coef_all <- coef_mtx
            }else{
                coef_all <- rbind(coef_all, coef_mtx)
            }               
        }
        
    }    	
    
    # define the data frame for the statistical coefficient
    if(!is.null(coef_all)){
        print(nrow(coef_all))
        coef_vt <- data.frame(obs=c(osrc),experiment=c(exp_tag),coefficient=c(coef), value=coef_all[,c(coef)])
    }
    # return the fitting coefficient data frame
    return(coef_vt)
}


# get the statistical coefficient matrix for all regression models
# osrc: the source of the observation
# coef: the statistical coefficient name
# return the statistical coefficient matrix
get_stat_coef_value_all <- function(osrc, coef) {
    coef_mtx <- NULL
    # iterate through all regression models
    for(exp in exp_lst){
        # get the statistical coefficient matrix for a regression model
        coef_vt <- get_stat_coef_value(osrc,exp, coef)
        # print(exp)
        # print(nrow(coef_vt))
        # append the statistical coefficient matrix to the main matrix
        if(is.null(coef_mtx)){
            coef_mtx <- coef_vt
        }else{
            coef_mtx <- rbind(coef_mtx,coef_vt)
        }
    }
    # return the main matrix
    return(coef_mtx)
}

# get the statistical coefficient matrix for the specified regression
# osrc: the source of the observation
# exp: the regression model tag
# return the statistical coefficient matrix
get_stat_coef_exp_value <- function(osrc, exp) {
    coef_mtx <- NULL
    # iterate through all statistical coefficients
    for(coef in c("r.squared","estr","AIC","BIC")){
        # get the statistical coefficient matrix for a regression model
        coef_vt <- get_stat_coef_value(osrc,exp, coef)

        # append the statistical coefficient matrix to the main matrix
        if(is.null(coef_mtx)){
            coef_mtx <- coef_vt
        }else{
            coef_mtx <- rbind(coef_mtx,coef_vt)
        }
    }
    # return the main matrix
    return(data.frame(coef_mtx))
}

# get the fitting coefficient matrix for the specified regression
# osrc: the source of the observation
# exp: the regression model tag
# return the fitting coefficient matrix
get_fit_coef_exp_value <- function(osrc, exp) {
    coef_mtx <- NULL
    # iterate through all fitting coefficients
    for(coef in c("(Intercept)","r_anthro","rfc_volc","rfc_solar","mei0","amo0")){
        # get the statistical coefficient matrix for a regression model
        coef_vt <- get_fit_coef_value(osrc,exp, coef)

        # append the statistical coefficient matrix to the main matrix
        if(is.null(coef_mtx)){
            coef_mtx <- coef_vt
        }else{
            coef_mtx <- rbind(coef_mtx,coef_vt)
        }
    }
    # return the main matrix
    return(data.frame(coef_mtx))
}

# get the statistical coefficient confidence interval for the specified regression
# osrc: the observation source
# exp_tag: the regression tag
# return the matrix of confidence interval
get_stat_coef_exp_ci <- function(osrc, exp) {
    coef_mtx <- NULL
    # iterate through all regression models
    for(coef in c("r.squared","estr","AIC","BIC")){
        # get the statistical coefficient matrix for a regression model
        coef_vt <- get_stat_coef_ci(osrc,exp, coef,F)

        # append the statistical coefficient matrix to the main matrix
        if(is.null(coef_mtx)){
            coef_mtx <- c(coefficient=c(coef), coef_vt)
        }else{
            coef_mtx <- rbind(coef_mtx,c(coefficient=c(coef), coef_vt))
        }
    }
    # return the main matrix
    coef_mtx <- data.frame(coef_mtx)
    coef_mtx$lwr <- as.numeric(as.character(coef_mtx$lwr))
    coef_mtx$med <- as.numeric(as.character(coef_mtx$med))
    coef_mtx$upr <- as.numeric(as.character(coef_mtx$upr))
    return(coef_mtx)
}

# get the fitting coefficient confidence interval for the specified regression
# osrc: the observation source
# exp_tag: the regression tag
# return the matrix of confidence interval
get_fit_coef_exp_ci <- function(osrc, exp) {
    coef_mtx <- NULL
    # iterate through all coefficients
    for(coef in c("(Intercept)","r_anthro","rfc_volc","rfc_solar","mei0","amo0")){
        # get the fitting coefficient matrix for a regression model
        coef_vt <- get_fit_coef_ci(osrc,exp, coef,F)

        # append the fitting coefficient matrix to the main matrix
        if(is.null(coef_mtx)){
            coef_mtx <- c(coefficient=c(coef), coef_vt)
        }else{
            coef_mtx <- rbind(coef_mtx,c(coefficient=c(coef), coef_vt))
        }
    }

    # return the main matrix
    coef_mtx <- data.frame(coef_mtx)

    coef_mtx$lwr <- as.numeric(as.character(coef_mtx$lwr))
    coef_mtx$med <- as.numeric(as.character(coef_mtx$med))
    coef_mtx$upr <- as.numeric(as.character(coef_mtx$upr))

    return(coef_mtx)
}


# get the statistical coefficient confidence interval for the specified regression
# osrc: the observation source
# exp_tag: the regression tag
# coef: the statistical coefficient name
# return the confidence interval
get_stat_coef_ci <- function(osrc, exp_tag, coef, rounded=T) {
    dst_vt <- NULL
    stat_mtx <- get_stat_coef_value(osrc, exp_tag, coef)

    if(!is.null(stat_mtx)){
        # calculate the confidence interval using the Wilcoxon test
        qnt_vt <- quantile(stat_mtx$value,probs=c(0.025,0.5,0.975))
        if(rounded){
            dst_vt <- round(qnt_vt,coef_digit[coef])  
        }else{
            dst_vt <- qnt_vt
        }
        
        names(dst_vt) <- c("lwr","med","upr")	
    }

    return(dst_vt)
}

# get the fitting coefficient confidence interval for the specified regression
# osrc: the observation source
# exp_tag: the regression tag
# coef: the fitting coefficient name
# return the confidence interval
get_fit_coef_ci <- function(osrc, exp_tag, coef, rounded=T) {
    dst_vt <- NULL
    stat_mtx <- get_fit_coef_value(osrc, exp_tag, coef)

    if(!is.null(stat_mtx)){
        # calculate the confidence interval using the Wilcoxon test
        qnt_vt <- quantile(stat_mtx$value,probs=c(0.025,0.5,0.975))
        if(rounded){
            dst_vt <- round(qnt_vt,coef_digit[coef])  
        }else{
            dst_vt <- qnt_vt
        }
        names(dst_vt) <- c("lwr","med","upr")
    }

    return(dst_vt)
}

# get the statistical coefficient matrix for the specified regression
# osrc: the observation source
# exp: the regression tag
# return the associated latex text
get_stat_tex <- function(osrc, exp) {
    # print(paste0(osrc, exp))

    # get the statistical coefficient values (mean and sd)
    intercept_ci <- get_fit_coef_ci(osrc, exp, "(Intercept)")
    r_anthro_ci <- get_fit_coef_ci(osrc, exp, "r_anthro")

    rfc_volc_ci <- get_fit_coef_ci(osrc, exp, "rfc_volc")
    rfc_solar_ci <- get_fit_coef_ci(osrc, exp, "rfc_solar")

    # define the latex text for the statistical coefficients
    intercept_tex <- paste0("\\begin{tabular}[c]{@{}l@{}}Inter=", intercept_ci["med"],"\\\\(",intercept_ci["lwr"], ", ", intercept_ci["upr"], ")\\end{tabular}")
    apg_tex <- "" 
    sub_list <- c(
        "Vol",
        "Sol",
        "Mei",
        "Amo",
        "Dmi",
        "Nao",
        "Pdo")
    if(!exp%in%sub_list){
        apg_tex <- paste0("\\begin{tabular}[c]{@{}l@{}}$\\beta_{APG}$=", r_anthro_ci["med"],"\\\\(",r_anthro_ci["lwr"], ", ", r_anthro_ci["upr"], ")\\end{tabular}")
    }
    
    nat_tex <- NULL
    if(!is.null(rfc_volc_ci)&&!is.null(rfc_solar_ci)){
        nat_tex <- paste0("\\begin{tabular}[c]{@{}l@{}}$\\beta_{Vol}$=",rfc_volc_ci["med"],  "\\\\(",rfc_volc_ci["lwr"],  ", ", rfc_volc_ci["upr"], ")", 
                                                 "\\\\ $\\beta_{Sol}$=",rfc_solar_ci["med"], "\\\\(",rfc_solar_ci["lwr"], ", ", rfc_solar_ci["upr"], ")\\end{tabular}")
    }else if(!is.null(rfc_volc_ci)){
        nat_tex <- paste0("\\begin{tabular}[c]{@{}l@{}}$\\beta_{Vol}$=",rfc_volc_ci["med"],  "\\\\(",rfc_volc_ci["lwr"],  ", ", rfc_volc_ci["upr"], ")\\end{tabular}")
    }else if(!is.null(rfc_solar_ci)){
        nat_tex <- paste0("\\begin{tabular}[c]{@{}l@{}}$\\beta_{Sol}$=",rfc_solar_ci["med"], "\\\\(",rfc_solar_ci["lwr"], ", ", rfc_solar_ci["upr"], ")\\end{tabular}")
    }

    # get the statistical coefficient values (mean and sd) for the climate variability
    mei0_ci <- get_fit_coef_ci(osrc, exp, "mei0")
    amo0_ci <- get_fit_coef_ci(osrc, exp, "amo0")
    pdo0_ci <- get_fit_coef_ci(osrc, exp, "pdo0")
    nao0_ci <- get_fit_coef_ci(osrc, exp, "nao0")
    dmi0_ci <- get_fit_coef_ci(osrc, exp, "dmi0")

    mei0_row <- NULL
    if(!is.null(mei0_ci)){
        mei0_row <- paste0("$\\beta_{CTI}$=",mei0_ci["med"],"\\\\","(",mei0_ci["lwr"],", ",mei0_ci["upr"],")\\\\")
    }

    amo0_row <- NULL
    if(!is.null(amo0_ci)){
        amo0_row <- paste0("$\\beta_{AMO}$=",amo0_ci["med"],"\\\\","(",amo0_ci["lwr"],", ",amo0_ci["upr"],")\\\\")
    }

    pdo0_row <- NULL
    if(!is.null(pdo0_ci)){
        pdo0_row <- paste0("$\\beta_{PDO}$=",pdo0_ci["med"],"\\\\","(",pdo0_ci["lwr"],", ",pdo0_ci["upr"],")\\\\")
    }

    nao0_row <- NULL
    if(!is.null(nao0_ci)){
        nao0_row <- paste0("$\\beta_{NAO}$=",nao0_ci["med"],"\\\\","(",nao0_ci["lwr"],", ",nao0_ci["upr"],")\\\\")
    }

    dmi0_row <- NULL
    if(!is.null(dmi0_ci)){
        dmi0_row <- paste0("$\\beta_{DMI}$=",dmi0_ci["med"],"\\\\","(",dmi0_ci["lwr"],", ",dmi0_ci["upr"],")\\\\")
    }

    # define the latex text for the climate variability
    var_tex <- paste("\\begin{tabular}[c]{@{}l@{}}", mei0_row, amo0_row, pdo0_row, nao0_row, dmi0_row, "\\end{tabular}", sep=" ")

# print(var_tex)

    # get the statistical coefficient values (mean and sd) for the model evaluation
    aic_ci <- get_stat_coef_ci(osrc, exp, "AIC")
    bic_ci <- get_stat_coef_ci(osrc, exp, "BIC")
    sigma_ci <- get_stat_coef_ci(osrc, exp, "sigma")
    rsq_ci <- get_stat_coef_ci(osrc, exp, "r.squared")
    estr_ci <- get_stat_coef_ci(osrc, exp, "estr")
   
    mdl_name <- exp_all_txt[exp]
    if(exp=="Ant"){
        mdl_name <- "Anthropogenic (APG)"
    }else if(exp=="Vol"){
        mdl_name <- "Vol(canic)"
    }else if(exp=="Sol"){
        mdl_name <- "Sol(ar)"
    }

    # highlight the best model
    dst_mtx <- data.frame(experiment=ifelse(exp=="AntVolSolMeiAmo",paste0("\\textbf{",gsub("+"," + ",mdl_name, fixed=T),"}"),gsub("+"," + ",mdl_name, fixed=T)) ,
                          intercept=ifelse(exp=="AntVolSolMeiAmo",paste0("\\textbf{",intercept_tex, "}"),intercept_tex),
                          anthro=ifelse(exp=="AntVolSolMeiAmo",paste0("\\textbf{",apg_tex, "}"),apg_tex),
                          nat=ifelse(exp=="AntVolSolMeiAmo",paste0("\\textbf{",ifelse(is.null(nat_tex),"",nat_tex), "}"),ifelse(is.null(nat_tex),"",nat_tex)),
                          var=ifelse(exp=="AntVolSolMeiAmo",paste0("\\textbf{",c(var_tex),  "}"),c(var_tex)),
                          aic=ifelse(exp=="AntVolSolMeiAmo",paste0("\\textbf{",paste0("\\begin{tabular}[c]{@{}l@{}}AIC=", aic_ci["med"],"\\\\(",aic_ci["lwr"], ", ", aic_ci["upr"], ")\\end{tabular}"),  "}"),
                                                                               paste0("\\begin{tabular}[c]{@{}l@{}}AIC=", aic_ci["med"],"\\\\(",aic_ci["lwr"], ", ", aic_ci["upr"], ")\\end{tabular}")),
                          bic=ifelse(exp=="AntVolSolMeiAmo",paste0("\\textbf{",paste0("\\begin{tabular}[c]{@{}l@{}}BIC=", bic_ci["med"],"\\\\(",bic_ci["lwr"], ", ", bic_ci["upr"], ")\\end{tabular}"),  "}"),
                                                                               paste0("\\begin{tabular}[c]{@{}l@{}}BIC=", bic_ci["med"],"\\\\(",bic_ci["lwr"], ", ", bic_ci["upr"], ")\\end{tabular}")),
                          rsq=ifelse(exp=="AntVolSolMeiAmo",paste0("\\textbf{",paste0("\\begin{tabular}[c]{@{}l@{}}$R^{2}$=", rsq_ci["med"],"\\\\(",rsq_ci["lwr"], ", ", rsq_ci["upr"], ")\\end{tabular}"),  "}"),
                                                                               paste0("\\begin{tabular}[c]{@{}l@{}}$R^{2}$=", rsq_ci["med"],"\\\\(",rsq_ci["lwr"], ", ", rsq_ci["upr"], ")\\end{tabular}")),
                          estr=ifelse(exp=="AntVolSolMeiAmo",paste0("\\textbf{",paste0("\\begin{tabular}[c]{@{}l@{}}$r$=", estr_ci["med"],"\\\\(",estr_ci["lwr"], ", ", estr_ci["upr"], ")\\end{tabular}"), "}"),
                                                                                paste0("\\begin{tabular}[c]{@{}l@{}}$r$=", estr_ci["med"],"\\\\(",estr_ci["lwr"], ", ", estr_ci["upr"], ")\\end{tabular}"))
                          )

    if(exp=="AntVolSolMeiAmo"){
        dst_mtx$anthro <- gsub("$\\beta_{APG}$", "\\boldmath{$\\beta_{APG}$}", dst_mtx$anthro, fixed=T)
        dst_mtx$nat <- gsub("$\\beta_{Vol}$", "\\boldmath{$\\beta_{Vol}$}", dst_mtx$nat, fixed=T)
        dst_mtx$nat <- gsub("$\\beta_{Sol}$", "\\boldmath{$\\beta_{Sol}$}", dst_mtx$nat, fixed=T)
        dst_mtx$var <- gsub("$\\beta_{CTI}$", "\\boldmath{$\\beta_{CTI}$}", dst_mtx$var, fixed=T)
        dst_mtx$var <- gsub("$\\beta_{AMO}$", "\\boldmath{$\\beta_{AMO}$}", dst_mtx$var, fixed=T)
        dst_mtx$var <- gsub("$\\beta_{PDO}$", "\\boldmath{$\\beta_{PDO}$}", dst_mtx$var, fixed=T)
        dst_mtx$var <- gsub("$\\beta_{NAO}$", "\\boldmath{$\\beta_{NAO}$}", dst_mtx$var, fixed=T)
        dst_mtx$var <- gsub("$\\beta_{DMI}$", "\\boldmath{$\\beta_{DMI}$}", dst_mtx$var, fixed=T)   
        dst_mtx$rsq <- gsub("$R^{2}$", "\\boldmath{$R^{2}$}", dst_mtx$rsq, fixed=T)  
        dst_mtx$estr <- gsub("$r$", "\\boldmath{$r$}", dst_mtx$estr, fixed=T)       
    }

    # return the associated latex text
    return(dst_mtx)
}



# export the statistical coefficients to a latex table used in the manuscript
# osrc: the observation source
sav_stat_tex_all <- function(osrc) {
    stat_tex <- NULL
    # iterate over the selected regressions
    for(exp in exp_all_lst){
        # get the statistical coefficient matrix for the specified regression
        stat_exp <- get_stat_tex(osrc, exp)
        # print(stat_exp)
        # append the statistical coefficient matrix to the overall matrix
        if(is.null(stat_tex)){
            stat_tex <- stat_exp
        }else{
            stat_tex <- rbind(stat_tex, stat_exp)
        }
    }
    fl_tex <- paste0("./latex/", osrc,".tex")

    # define the column names
    colnames(stat_tex) <- c("Multiple regression model", "Intercept", "Anthropogenic", "Natural forcing", "Variability", "AIC", "BIC", "$R^2$", "$r$ (annual)") #"$\\sigma$", 

    # print the statistical coefficient matrix to a latex table 
    print(xtable(stat_tex, type = "latex"), file = fl_tex, include.rownames=FALSE, sanitize.text.function = function(x){x})
    print(paste0(fl_tex, " saved."))
    # return(stat_tex)
}

# get the index for the model regression
get_exp_index <- function(){
    lbl_mtx <- NULL
    txt_mtx <- exp_all_txt[exp_lst]
    nrw <- length(txt_mtx)

    # iterate over the rows
    for(i in c(1:nrw)){
        txt_tag <- strsplit(txt_mtx[[i]], split = "+", fixed=T)
        lbl_vt <- data.frame(experiment=names(txt_mtx)[i], factor=c(txt_tag[[1]]))

        # append the label matrix to the overall matrix
        if(is.null(lbl_mtx)){
            lbl_mtx <- lbl_vt
        }else{
            lbl_mtx <- rbind(lbl_mtx, lbl_vt)
        }
    }

    nrw <- nrow(lbl_mtx)
    lbl_mtx$index <- 0
    for(i in c(1:nrw)){
        # print(exp_index[lbl_mtx$factor[[i]]])
        lbl_mtx$index[i] <- exp_index[lbl_mtx$factor[[i]]]
    }
    # return the label matrix
    return(lbl_mtx)
}

# plot the model selection panel
# osrc: the source of the observation
# coef: the coefficient to be plotted, default to use BIC in the analysis
# return the panel
plot_stat_coef <- function(osrc, coef){
    # get the statistical coefficient matrix
    coef_mtx <- get_stat_coef_all(osrc, coef)
    coef_mtx$experiment <-  factor(coef_mtx$experiment, levels  = exp_lst)

    max_val <- max(coef_mtx$lwr) 
    min_val <- min(coef_mtx$upr) 

    # get the index for the model regression
    exp_idx <- get_exp_index()

    # define the label for the model regression
    exp_lbl <- data.frame(experiment=names(exp_lbl_lst),factor=exp_lbl_lst,index=exp_index[as.vector(unlist(exp_lbl_lst, use.names=FALSE))])

    exp_lbl$lbl <- exp_lbl$factor 
    exp_lbl$lbl <- gsub("APG", "Anthropogenic", exp_lbl$lbl, fixed=T) 
    exp_lbl$lbl <- gsub("Sol", "Solar", exp_lbl$lbl, fixed=T) 
    exp_lbl$lbl <- gsub("Vol", "Volcanic", exp_lbl$lbl, fixed=T) 

    # print out the min and max value for the x axis
    print(c(min_val, max_val))

    # define the x axis limits, breaks and labels, use "BIC" as the default
    if(osrc=="HadCRUTv5.0.2"&coef%in%c("AIC","BIC")){
        x_lims = c(-2650,-975) 
        x_brks = seq(-2600,-1000,200) 
        x_lbls = seq(-2600,-1000,200)  
        if(coef=="AIC"){
            xname <- "Akaike Information Criterion (AIC)"
        }else if(coef=="BIC"){
            xname <- "Potential climate change impact factors                      Bayesian Information Criterion (BIC)"
        }      
    }else if(osrc=="GlobalTemp_v6"&coef%in%c("AIC","BIC")){
        x_lims = c(-3150,-1300) 
        x_brks = seq(-3000,-1400,200) 
        x_lbls = seq(-3000,-1400,200)   
        if(coef=="AIC"){
            xname <- "Potential climate change impact factors                      Akaike Information Criterion (AIC)"
        }else if(coef=="BIC"){
            xname <- "Potential climate change impact factors                      Bayesian Information Criterion (BIC)"
        }        
    }else if(osrc=="Berkeley_Earth"&coef%in%c("AIC","BIC")){
        x_lims = c(-2400,-980) 
        x_brks = seq(-2400,-1000,200) 
        x_lbls = seq(-2400,-1000,200)   
        if(coef=="AIC"){
            xname <- "Potential climate change impact factors                      Akaike Information Criterion (AIC)"
        }else if(coef=="BIC"){
            xname <- "Potential climate change impact factors                      Bayesian Information Criterion (BIC)"
        }        
    }else if(osrc=="HadCRUTv5.0.2"&coef%in%c("r.squared")){
        x_lims = c(0.8,0.91) 
        x_brks = seq(0.8,0.91,0.02) 
        x_lbls = seq(0.8,0.91,0.02)  

        xname <- expression(italic(R)^2)

    }else if(osrc=="HadCRUTv5.0.2"&coef%in%c("estr")){
        x_lims = c(0.93,0.99) 
        x_brks = seq(0.93,0.99,0.01) 
        x_lbls = seq(0.93,0.99,0.01)  

        xname <- expression(italic(R)~"(Annual)")

    }else if(osrc=="GlobalTemp_v6"&coef%in%c("r.squared")){
        x_lims = c(0.8,0.92) 
        x_brks = seq(0.8,0.92,0.02) 
        x_lbls = seq(0.8,0.92,0.02)  

        xname <- expression(italic(R)^2)

    }else if(osrc=="GlobalTemp_v6"&coef%in%c("estr")){
        x_lims = c(0.93,0.99) 
        x_brks = seq(0.93,0.99,0.01) 
        x_lbls = seq(0.93,0.99,0.01)  

        xname <- expression(italic(R)~"(Annual)")

    }else if(osrc=="Berkeley_Earth"&coef%in%c("r.squared")){
        x_lims = c(0.82,0.91) 
        x_brks = seq(0.82,0.91,0.02) 
        x_lbls = seq(0.82,0.91,0.02)  

        xname <- expression(italic(R)^2)

    }else if(osrc=="Berkeley_Earth"&coef%in%c("estr")){
        x_lims = c(0.94,0.99) 
        x_brks = seq(0.94,0.99,0.01) 
        x_lbls = seq(0.94,0.99,0.01)  

        xname <- expression(italic(R)~"(Annual)")

    }

    x_offset <- 800
    x_lims[1] <- x_lims[1] - x_offset

    # indicate the model used in the study
    def_mdl <- data.frame(experiment=c("AntVolSolMeiAmo"), xpos=x_lims[1])

    # plot the histogram
    p <- ggplot() + 
        # labs(tag = expression(bold(B))) +
        ggtitle(expression(Model~selection))+

        scale_x_continuous(name=xname, limits = x_lims, breaks = x_brks, labels=x_lbls) +

        scale_y_discrete(limits = c("empty0", exp_lst)) +

        geom_point(data = exp_idx, aes(y = experiment, x = x_lims[1] + x_offset - index - 50, fill=factor), size=4, shape=21, color="grey10", alpha = 0.8, show.legend=F) +

        geom_point(data = coef_mtx, aes(y = experiment, x = med), size=3, shape=23, color="grey30", fill=carto_pal(12, "Safe")[c(4)], alpha = 0.8) +
        geom_errorbarh(data = coef_mtx, aes(y = experiment, xmin=lwr, xmax=upr), height=.5, color=carto_pal(12, "Safe")[c(4)], linewidth=0.6,
                 position=position_dodge(.9)) +

        geom_segment(aes(x = c(x_lims[1] + x_offset + 30), y = Inf, xend = c(x_lims[1] + x_offset + 30), yend = 2), colour="grey50", linetype="longdash", linewidth=0.4) +
        geom_label(data = exp_lbl, aes(x=x_lims[1] + x_offset - index - 100,y=experiment,label=lbl), hjust=0.4, vjust=1.3, fill=alpha(c("white"),0.3), size=4, label.size = NA, lineheight = 0.5) +

         geom_rect(aes(xmin = x_lims[1], xmax = x_lims[1] + x_offset, ymin = 13.5, ymax = 14.5), fill=alpha("white",0), colour="grey50", linetype="solid", linewidth=0.5) +

         annotate("text", x = x_lims[1] + 1900, y = 14, label=("Model used in this study"), size=4) +
         annotate("segment", x = x_lims[1] + 1610, y = 14, xend = x_lims[1] + 1510, yend = 14,
                                        arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +

        scale_fill_manual(values=cl_exp_index) +
        theme_few() +
        theme(legend.position = "none",
            plot.title = font_element3,
            # plot.tag = element_text(size=font_size4, hjust=-10),
            plot.margin = margin(0.32, 0.2, 0.4, 2, unit = "cm"),
            legend.text = font_element3,
            legend.title = font_element3,
            legend.box = "horizontal",
            legend.direction = "horizontal",

            panel.grid.major = element_blank(), #remove major gridlines
            panel.grid.minor = element_blank(), #remove minor gridlines
            legend.background = element_rect(fill='transparent'), #transparent legend bg
            legend.box.background = element_rect(fill='transparent'), #transparent legend panel

            axis.ticks.y=element_blank(),
            axis.title.x = font_element3L,
            axis.title.y = element_blank(),
            
            axis.text.x = font_element3,
            axis.text.y = element_blank(),
            strip.text = element_blank()) 

    # return the plot
    return(p)

}

# plot the histogram of the statistical coefficients
# osrc: source data
# exp: experiment
# return the plot
plot_stat_hist <- function(osrc, exp){
    # get the statistical coefficients
	coef_value <- get_stat_coef_exp_value(osrc, exp)

    # get the statistical coefficients confidence interval
    dst_vt <- get_stat_coef_exp_ci(osrc, exp)

    # plot the histogram
    p <- ggplot() + 
        xlab(expression(Model~statistical~parameters))+

        geom_histogram(data = coef_value, aes(x=value), fill=carto_pal(12, "Safe")[c(2)], colour="grey60", bins=60, alpha=0.5) +

        geom_vline(data = dst_vt, aes(xintercept=med), colour="grey50", linetype="dashed", linewidth=0.8) +
        geom_vline(data = dst_vt, aes(xintercept=lwr), colour="grey50", linetype="dashed", linewidth=0.8) +
        geom_vline(data = dst_vt, aes(xintercept=upr), colour="grey50", linetype="dashed", linewidth=0.8) +

        geom_label(data = dst_vt, aes(x=med,y=Inf,label="50%"), hjust=0.5, vjust=1.5, fill=alpha(c("white"),0.5), size=4, label.size = NA, lineheight = 0.5) +
        geom_label(data = dst_vt, aes(x=lwr,y=Inf,label="2.5%"), hjust=0.5, vjust=1.5, fill=alpha(c("white"),0.5), size=4, label.size = NA, lineheight = 0.5) +
        geom_label(data = dst_vt, aes(x=upr,y=Inf,label="97.5%"), hjust=0.5, vjust=1.5, fill=alpha(c("white"),0.5), size=4, label.size = NA, lineheight = 0.5) +

        theme_few() +
        theme(legend.position = "none",
            plot.title = font_element3,

            legend.text = font_element3,
            legend.title = font_element3,
            legend.box = "horizontal",
            legend.direction = "horizontal",

            panel.grid.major = element_blank(), #remove major gridlines
            panel.grid.minor = element_blank(), #remove minor gridlines
            legend.background = element_rect(fill='transparent'), #transparent legend bg
            legend.box.background = element_rect(fill='transparent'), #transparent legend panel

            axis.title.x = font_element3,
            axis.title.y = element_blank(),

            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = font_element3) +
            facet_wrap(~factor(coefficient, levels=c("AIC","BIC","r.squared","estr"), labels = c("AIC"="AIC","BIC"="BIC","r.squared"="italic(R^{2})","estr"="italic(r)~(Annual)")), scales="free",ncol=2, labeller = label_parsed) 


    # fl_plot = paste0(fig_dir,"fig_stat_",osrc, "_", exp, "_hist", fl_ext)
    # ggsave(fl_plot,p, width = 24, height = 24, units = "cm")
    # print(paste0(fl_plot, " saved"))

    return(p)
}

# plot the histogram of the fitting coefficients
# osrc: source data
# exp: experiment
# return the plot
plot_fit_hist <- function(osrc, exp){
	coef_value <- get_fit_coef_exp_value(osrc, exp)

    dst_vt <- get_fit_coef_exp_ci(osrc, exp)

    p <- ggplot() + 
        xlab(expression(Fitting~coefficients))+

        geom_histogram(data = coef_value, aes(x=value), fill=carto_pal(12, "Safe")[c(2)], colour="grey70", bins=60, alpha=0.5) +

        geom_vline(data = dst_vt, aes(xintercept=med), colour="grey50", linetype="dashed", linewidth=0.8) +
        geom_vline(data = dst_vt, aes(xintercept=lwr), colour="grey50", linetype="dashed", linewidth=0.8) +
        geom_vline(data = dst_vt, aes(xintercept=upr), colour="grey50", linetype="dashed", linewidth=0.8) +

        geom_label(data = dst_vt, aes(x=med,y=Inf,label="50%"), hjust=0.5, vjust=1.5, fill=alpha(c("white"),0.5), size=4, label.size = NA, lineheight = 0.5) +
        geom_label(data = dst_vt, aes(x=lwr,y=Inf,label="2.5%"), hjust=0.5, vjust=1.5, fill=alpha(c("white"),0.5), size=4, label.size = NA, lineheight = 0.5) +
        geom_label(data = dst_vt, aes(x=upr,y=Inf,label="97.5%"), hjust=0.5, vjust=1.5, fill=alpha(c("white"),0.5), size=4, label.size = NA, lineheight = 0.5) +

        theme_few() +
        theme(legend.position = "none",
            plot.title = font_element3,
            legend.text = font_element3,
            legend.title = font_element3,
            legend.box = "horizontal",
            legend.direction = "horizontal",

            panel.grid.major = element_blank(), #remove major gridlines
            panel.grid.minor = element_blank(), #remove minor gridlines
            legend.background = element_rect(fill='transparent'), #transparent legend bg
            legend.box.background = element_rect(fill='transparent'), #transparent legend panel
            axis.title.x = font_element3,
            axis.title.y = element_blank(),
            axis.text.x = font_element3,
            axis.text.y = font_element3,
            strip.text = font_element3) +
            facet_wrap(~factor(coefficient, levels=c("(Intercept)","r_anthro","rfc_volc","rfc_solar","mei0","amo0"), labels = c("(Intercept)"="Intercept","r_anthro"="italic(beta[APG])","rfc_volc"="italic(beta[Vol])","rfc_solar"="italic(beta[Sol])","mei0"="italic(beta[CTI])","amo0"="italic(beta[AMO])")), scales="free",ncol=2, labeller = label_parsed)



    # fl_plot = paste0(fig_dir,"fig_fit_",osrc, "_", exp, "_hist", fl_ext)
    # ggsave(fl_plot,p, width = 24, height = 24, units = "cm")
    # print(paste0(fl_plot, " saved"))

    return(p)
}

# plot the histogram of the fitting coefficients and statistical coefficients
# osrc: source data
# exp: experiment
# plot the figure
plot_model_hist <- function(osrc, exp){

    # the fitting coefficients panel
    p_fit <- plot_fit_hist(osrc, exp)
    # the statistical coefficients panel
    p_stat <- plot_stat_hist(osrc, exp)

    text_y <- textGrob(expression(Probabilty~density), hjust = 0.5, gp = gp_font4, rot = 90)

    # combine the panels
    p_com <- ggarrange(
        p_fit,
        p_stat,
        ncol = 1,
        heights=c(0.58,0.4),
        labels = c("A", "B"),
        font.label = list(size = 14, face = "bold")
    )

    p_all <- arrangeGrob(
        text_y,
        p_com,
        nrow = 1,
        widths=c(0.05,0.95)
    )

    src_tag <- gsub("v5.0.2","v5_0_2",osrc,fixed=T)

    # save to figure file
    fl_plot = paste0(fig_dir,"fig_model_",src_tag, "_", exp, fl_ext)
    ggsave(fl_plot, p_all, width = 24, height = 28, units = "cm")

    print(paste0(fl_plot, " saved."))
}

# save the figure as a png file or pdf file
fl_ext <- ".png"
fl_ext <- ".pdf"

nthread <- 4

# # save the main climate variables for review using HadCRUTv5.0.2
# sav_corr_mtx("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "HadCRUTv5.0.2",start_yr=1855, end_yr =2024)

# # save the main climate variables for review using GlobalTemp_v6
# sav_corr_mtx("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "GlobalTemp_v6",start_yr=1855, end_yr =2024)

# # save the main climate variables for review using Berkeley_Earth
# sav_corr_mtx("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "Berkeley_Earth",start_yr=1855, end_yr =2024)

# export the statistical coefficients to a latex table used in the manuscript, using HadCRUTv5.0.2
sav_stat_tex_all("HadCRUTv5.0.2")

# export the statistical coefficients to a latex table used in the manuscript, using GlobalTemp_v6
sav_stat_tex_all("GlobalTemp_v6")

# export the statistical coefficients to a latex table used in the manuscript, using Berkeley_Earth
sav_stat_tex_all("Berkeley_Earth")


# # plot the histogram of the fitting coefficients and statistical coefficients, using HadCRUTv5.0.2
# plot_model_hist("HadCRUTv5.0.2", "AntVolSolMeiAmo")

# # plot the histogram of the fitting coefficients and statistical coefficients, using GlobalTemp_v6
# plot_model_hist("GlobalTemp_v6", "AntVolSolMeiAmo")

# # plot the histogram of the fitting coefficients and statistical coefficients, using Berkeley_Earth
# plot_model_hist("Berkeley_Earth", "AntVolSolMeiAmo")


# # plot the demonstration of the regression using HadCRUTv5.0.2, as Fig. 1 in the manuscript
# plot_demo("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="HadCRUTv5.0.2",start_yr=1855)

# # plot the demonstration of the regression using GlobalTemp_v6
# plot_demo("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="GlobalTemp_v6",start_yr=1855)

# # plot the demonstration of the regression using Berkeley_Earth
# plot_demo("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="Berkeley_Earth",start_yr=1855)


# # plot the temperature spike figure using HadCRUTv5.0.2, comparing the 2022-2024 with the 2019-2021
# plot_regre_spike("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "HadCRUTv5.0.2",prev_st_yr=2019, prev_ed_yr=2021, curr_st_yr=2022, curr_ed_yr=2024)

# #plot the temperature spike figure using HadCRUTv5.0.2, comparing the 2022-2024 with the 2010-2021 as in Fig. 2 in the manuscript
# plot_regre_spike("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "HadCRUTv5.0.2")

# # plot the temperature spike figure using GlobalTemp_v6, comparing the 2022-2024 with the 2010-2021
# plot_regre_spike("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "GlobalTemp_v6")

# # plot the temperature spike figure using Berkeley_Earth, comparing the 2022-2024 with the 2010-2021
# plot_regre_spike("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "Berkeley_Earth")


# # plot the 11-year rolling trends for the climate varaibles using HadCRUTv5.0.2
# plot_mei_amo_rfc_regr_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "HadCRUTv5.0.2",start_yr=1855, k=11)

# # plot the 11-year rolling trends for the climate varaibles using GlobalTemp_v6
# plot_mei_amo_rfc_regr_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "GlobalTemp_v6",start_yr=1855, k=11)

# # plot the 11-year rolling trends for the climate varaibles using Berkeley_Earth
# plot_mei_amo_rfc_regr_roll("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "Berkeley_Earth",start_yr=1855, k=11)


# # plot the 11-year rolling trends using HadCRUTv5.0.2
# plot_mei_amo_rfc_regr_roll_nat("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "HadCRUTv5.0.2",start_yr=1855, k=11)

# # plot the 11-year rolling trends using GlobalTemp_v6
# plot_mei_amo_rfc_regr_roll_nat("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "GlobalTemp_v6",start_yr=1855, k=11)


# # plot the 11-year rolling trends using Berkeley_Earth
# plot_mei_amo_rfc_regr_roll_nat("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "Berkeley_Earth",start_yr=1855, k=11)


# # plot the aerosol panel using HadCRUTv5.0.2
# plot_mei_amo_aero_regr_full("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "HadCRUTv5.0.2",start_yr=1855)


# # plot the panel for halogenated gases using HadCRUTv5.0.2
# plot_mei_amo_fgs_regr_full("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "HadCRUTv5.0.2",start_yr=1855)


# # plot the panel for all climate variables using HadCRUTv5.0.2
# plot_mei_amo_rfc_regr_full("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "HadCRUTv5.0.2",start_yr=1855)

# # plot the panel for all climate variables using GlobalTemp_v6
# plot_mei_amo_rfc_regr_full("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "GlobalTemp_v6",start_yr=1855)

# # plot the panel for all climate variables using Berkeley_Earth
# plot_mei_amo_rfc_regr_full("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "Berkeley_Earth",start_yr=1855)


# # plot the panel for selected climate variables using HadCRUTv5.0.2
# plot_mei_amo_rfc_regr_nat("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "HadCRUTv5.0.2",start_yr=1855)

# # plot the panel for selected climate variables using GlobalTemp_v6
# plot_mei_amo_rfc_regr_nat("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "GlobalTemp_v6",start_yr=1855)

# # plot the panel for selected climate variables using Berkeley_Earth
# plot_mei_amo_rfc_regr_nat("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", "Berkeley_Earth",start_yr=1855)

# # plot the climate variation panel for the cooling phase and warming slowdown using HadCRUTv5.0.2
# plot_anorm_hist("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="HadCRUTv5.0.2")

# # plot the climate variation panel for the cooling phase and warming slowdown using GlobalTemp_v6
# plot_anorm_hist("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="GlobalTemp_v6")

# # plot the climate variation panel for the cooling phase and warming slowdown using Berkeley_Earth
# plot_anorm_hist("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src="Berkeley_Earth")

