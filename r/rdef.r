library(RColorBrewer)
library(ggplot2)

font_size_tiny = 6
font_size00= 8
font_size0 = 9
font_size1 = 10
font_size2 = 12
font_size3 = 14
font_size4 = 16
font_size5 = 20

annot_size00 = 2
annot_size0 = 3
annot_size = 3.4
#font_name = "Helvetica"
#family=font_name 
font_element_tiny = element_text(size=font_size_tiny)
font_element00 = element_text(size=font_size00)
font_element00M = element_text(size=font_size00,hjust=0.5)
font_element00VT = element_text(size=font_size00,vjust=0)
font_element0 = element_text(size=font_size0)
font_element0M = element_text(size=font_size0,hjust=0.5)
font_element1 = element_text(size=font_size1)
font_element1L = element_text(size=font_size1, hjust=0)
font_element2 = element_text(size=font_size2)
font_element2M = element_text(size=font_size2,hjust=0.5)
font_element_tinyM = element_text(size=font_size_tiny,hjust=0.5)
font_element2L = element_text(size=font_size2,hjust=0,face="bold")
font_element2L1 = element_text(size=font_size2, hjust=0)
font_element2L2 = element_text(size=font_size2, hjust=1)
font_element3 = element_text(size=font_size3)
font_element3v = element_text(size=font_size3, vjust=1.8)
font_element2v = element_text(size=font_size2, vjust=1.8)
font_element1v = element_text(size=font_size1, vjust=1.8)
font_element3M = element_text(size=font_size3, hjust=0.5)
font_element3L = element_text(size=font_size3,hjust=0)
font_element3R = element_text(size=font_size3, hjust=1)
font_element3L1 = element_text(size=font_size3,vjust=0)
font_element4 = element_text(size=font_size4)
font_element4L = element_text(size=font_size4, hjust=0)
font_element5 = element_text(size=font_size5)

gp_font00 <- gpar(fontsize = 8, fontface = "bold", cex = 1)
gp_font0 <- gpar(fontsize = 10, fontface = "bold", cex = 1)
gp_font1 <- gpar(fontsize = 11, fontface = "bold", cex = 1)
gp_font3 <- gpar(fontsize = 14, fontface = "bold", cex = 1)
gp_font4 <- gpar(fontsize = 15, fontface = "bold", cex = 1)
gp_font5 <- gpar(fontsize = 16, fontface = "bold", cex = 1)
gp_font5Red <- gpar(fontsize = 16, fontface = "bold", cex = 1, col="red")
gp_font2 <- gpar(fontsize = font_size2, cex = 1)

point_size06 = 0.6
point_size10 = 1
point_size12 = 1.2
point_size15 = 1.5
point_size20 = 2
point_size25 = 2.5
point_size30 = 3
point_size40 = 4

line_size02 = 0.2
line_size03 = 0.3
line_size04 = 0.4
line_size05 = 0.5
line_size06 = 0.6
line_size08 = 0.8
line_size1 = 1
line_size12 = 1.2
line_size15 = 1.5
line_size2 = 2
line_size25 = 2.5
line_size3  = 3
line_size4  = 4
line_size11 = 1.1
line_size12 = 1.2
line_size15 = 1.5
line_size18 = 1.8

fill_alpha1 = 0.3

figure_width1 = 12
figure_height1 = 8

figure_width2 = 24
figure_height2 = 16

figure_width3 = 24
figure_height3 = 14

figure_width4 = 24
figure_height4 = 6

figure_width5 = 10
figure_height5 = 18

figure_width6 = 18
figure_height6 = 12

figure_width7 = 20
figure_height7 = 24

figW_2col = 9 
figH_2col = 6

kelly_colors = c('#F2F3F4', '#222222', '#F3C300', '#875692', '#F38400', '#A1CAF1', '#BE0032', '#C2B280', '#848482', '#008856', '#E68FAC', '#0067A5', '#F99379', '#604E97', '#F6A600', '#B3446C', '#DCD300', '#882D17', '#8DB600', '#654522', '#E25822', '#2B3D26')

cl_brt_Palette <- c("#A00000", "#00A000", "#5060D0", "#F25900", "#1D4599", "#2F6C3D", "#E62B17", "#E69F17", 
                    "#2F3F60", "#11AD34", "#8F463F", "#8F743F", "#031A49", "#025214", "#6D0D03", "#6D4903")
cl_brt_Palette2 <- c("#FBB735", "#E98931", "#EB403B", "#B32E37", "#6C2A6A", "#5C4399", "#274389", "#1F5EA8", 
                     "#227FB0", "#2AB0C5", "#39C0B3")

cl_brt_Palette3 <- c("#B32E37","#E41A1C","#EB403B","#E98931","#FBB735", "#FFFF33",    
                     "#2AB0C5")

cl_brt_seq_Palette <- c("#2CA25F", "#66C2A4",  "#99D8C9", "#CCECE6",
                        "#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C","#BD0026", "#800026")

cl_ln_gr  <- "grey50" 
cl_fl_gr  <- "grey90"
cl_tx_gr  <- "grey50"

cl_sc_bau0 <- brewer.pal(12, "Paired")[6] 
cl_sc_opt0 <- brewer.pal(12, "Paired")[8] 
cl_sc_t150 <- brewer.pal(12, "Paired")[4] 
cl_sc_t200 <- brewer.pal(12, "Paired")[2] 

cl_sc_bau1 <- brewer.pal(12, "Paired")[5] 
cl_sc_opt1 <- brewer.pal(12, "Paired")[7] 
cl_sc_t151 <- brewer.pal(12, "Paired")[3] 
cl_sc_t201 <- brewer.pal(12, "Paired")[1] 

cl_sc_1 <- brewer.pal(12, "Paired")[2] 
cl_sc_2 <- brewer.pal(12, "Paired")[4] 
cl_sc_3 <- brewer.pal(12, "Paired")[6] 
cl_sc_4 <- brewer.pal(12, "Paired")[8] 
cl_sc_5 <- brewer.pal(12, "Paired")[10] 
cl_sc_6 <- brewer.pal(12, "Paired")[12] 

cl_sc_11 <- brewer.pal(12, "Paired")[1] 
cl_sc_21 <- brewer.pal(12, "Paired")[3] 
cl_sc_31 <- brewer.pal(12, "Paired")[5] 
cl_sc_41 <- brewer.pal(12, "Paired")[7] 
cl_sc_51 <- brewer.pal(12, "Paired")[9] 
cl_sc_61 <- brewer.pal(12, "Paired")[11] 

if(avoid_color_deficient_vision){
  cl_sc_3 <- brewer.pal(8, "Dark2")[4] 
}

cl_sc_10 <- brewer.pal(8, "Dark2")[1] 
cl_sc_20 <- brewer.pal(8, "Dark2")[2] 
cl_sc_30 <- brewer.pal(8, "Dark2")[3] 
cl_sc_40 <- brewer.pal(8, "Dark2")[4] 
cl_sc_50 <- brewer.pal(8, "Dark2")[5] 
cl_sc_60 <- brewer.pal(8, "Dark2")[6]
cl_sc_70 <- brewer.pal(8, "Dark2")[7] 
cl_sc_80 <- brewer.pal(8, "Dark2")[8] 

cl_sl_co2a    = cl_sc_3
cl_sl_ch4     = cl_sc_4 
cl_sl_n2o     = cl_sc_31 
cl_sl_hfc     = cl_sc_51
cl_sl_pfc     = cl_sc_21
cl_sl_ods     = cl_sc_5 
cl_sl_aero    = cl_sc_2 
cl_sl_lcc     = cl_sc_41
cl_sl_volc    = brewer.pal(9, "Set1")[9]
cl_sl_sola    = cl_sc_6
cl_sl_vari    = cl_sc_1
cl_sl_obs     = cl_sc_61
#cl_em_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(22)
cl_em_palette <- c(rev(colorRampPalette(brewer.pal(9, "Reds"))(11)),colorRampPalette(brewer.pal(9, "Greens"))(11))
#cl_em_palette <- c(brewer.pal(9, "Set1"),brewer.pal(12, "Set3"))
cl_em_co2 <- cl_em_palette[1]
cl_em_ch4 <- cl_em_palette[2]
cl_em_n2o <- cl_em_palette[3]
cl_em_fgs <- cl_em_palette[4]
cl_em_mgs <- cl_em_palette[5]
cl_em_cog <- cl_em_palette[7]
cl_em_voc <- cl_em_palette[8]
cl_em_nox <- cl_em_palette[18]
cl_em_nh3 <- cl_em_palette[17]
cl_em_sox <- cl_em_palette[19]
cl_em_bcg <- cl_em_palette[6]
cl_em_ocg <- cl_em_palette[15]

cl_em_slp <- brewer.pal(12, "Paired")[2]
cl_em_ghg <- brewer.pal(12, "Paired")[4]
cl_em_tot <- brewer.pal(12, "Paired")[6]
cl_em_co2i <- cl_em_co2
cl_em_co2l <- cl_em_co2
cl_em_nco2 <- brewer.pal(12, "Paired")[8]
cl_em_nco2i <- brewer.pal(12, "Paired")[8]
cl_em_nco2l <- brewer.pal(12, "Paired")[8]

#cl_rf_palette          <- colorRampPalette(brewer.pal(11, "RdBu"))(22)
cl_rf_palette          <- c(rev(colorRampPalette(brewer.pal(9, "Reds"))(11)),colorRampPalette(brewer.pal(9, "Greens"))(11))
#cl_rf_palette <- c(brewer.pal(9, "Set1"),brewer.pal(12, "Set3"))
cl_rf_CO2_RF           <- cl_rf_palette[1]
cl_rf_CH4_RF           <- cl_rf_palette[2]
cl_rf_N2O_RF           <- cl_rf_palette[3]
cl_rf_FGASSUM_RF       <- cl_rf_palette[4]
cl_rf_MHALOSUM_RF      <- cl_rf_palette[5]
cl_rf_BCI_RF           <- cl_rf_palette[6]
cl_rf_BIOMASSAER_RF    <- cl_rf_palette[7]
cl_rf_TROPOZ_RF        <- cl_rf_palette[8]
cl_rf_CH4OXSTRATH2O_RF <- cl_rf_palette[9]
cl_rf_BCSNOW_RF        <- cl_rf_palette[10]
cl_rf_OCI_RF           <- cl_rf_palette[15]
cl_rf_SOXI_RF          <- cl_rf_palette[19]
cl_rf_NOXI_RF          <- cl_rf_palette[18]
cl_rf_MINERALDUST_RF   <- cl_rf_palette[17]
cl_rf_CLOUD_TOT_RF     <- cl_rf_palette[16]
cl_rf_STRATOZ_RF       <- cl_rf_palette[14]
cl_rf_LANDUSE_RF       <- cl_rf_palette[13]

cl_rf_VOLCANIC_ANNUAL_RF<- cl_rf_palette[12]	
cl_rf_SOLAR_RF         <- cl_rf_palette[11]


cl_src_mapping <- c(
  "MPIMET"   = colorRampPalette(brewer.pal(9, "Set1"))(11)[1],
  "PRIMAP"   = colorRampPalette(brewer.pal(9, "Set1"))(11)[2],
  "SR2013"   = colorRampPalette(brewer.pal(9, "Set1"))(11)[3],
  "GCP2018"  = colorRampPalette(brewer.pal(9, "Set1"))(11)[4],
  "CEDS"     = colorRampPalette(brewer.pal(9, "Set1"))(11)[5],
  "EDGAR"    = colorRampPalette(brewer.pal(9, "Set1"))(11)[6],
  "RCPhis"   = colorRampPalette(brewer.pal(9, "Set1"))(11)[7],
  "AIM"      = colorRampPalette(brewer.pal(9, "Set1"))(11)[9],
  "IAMC"     = colorRampPalette(brewer.pal(9, "Set1"))(11)[10],
  "BB4CMIP6" = colorRampPalette(brewer.pal(9, "Set1"))(11)[11]
)


lbl_src_mapping <- c(
  "MPIMET"   = expression(MPIMET),
  "PRIMAP"   = expression("PRIMAP v2.0"),
  "SR2013"   = expression("Smith & Rothwell(2013)"),
  "GCP2018"  = expression("GCP 2018"),
  "CEDS"     = expression(CEDS),
  "EDGAR"    = expression("EDGAR v4.3.2"),
  "RCPhis"   = expression("RCP historical"),
  "AIM"      = expression(AIM),
  "IAMC"     = expression("SSP for CMIP6"),
  "BB4CMIP6" = expression(BB4CMIP6)
)

cl_reg_WORLD  <- colorRampPalette(brewer.pal(9, "Set1"))(9)[8]
cl_reg_LAM    <- colorRampPalette(brewer.pal(9, "Set1"))(9)[2]
cl_reg_OAS    <- colorRampPalette(brewer.pal(9, "Set1"))(9)[3]
cl_reg_AFR    <- colorRampPalette(brewer.pal(9, "Set1"))(9)[4]
cl_reg_ROW    <- colorRampPalette(brewer.pal(9, "Set1"))(9)[5]
cl_reg_MEA    <- cl_sc_20
cl_reg_EUR    <- cl_sc_30
cl_reg_CHN    <- colorRampPalette(brewer.pal(9, "Set1"))(9)[1]
cl_reg_IND    <- colorRampPalette(brewer.pal(9, "Set1"))(9)[9]
cl_reg_JPN    <- cl_sc_40
cl_reg_USA    <- cl_sc_10
cl_reg_RUS    <- colorRampPalette(brewer.pal(9, "Set1"))(9)[7]
cl_reg_nonattrib <- "grey30"
cl_reg_res    <- "white"

cl_reg_mapping <- c(
    "WORLD"	= cl_reg_WORLD,
    "LAM"		= cl_reg_LAM,
    "OAS"		= cl_reg_OAS,
    "AFR"		= cl_reg_AFR,
    "ROW"		= cl_reg_ROW,
    "MEA"		= cl_reg_MEA,
    "EUR"		= cl_reg_EUR,
    "CHN"		= cl_reg_CHN,
    "IND"		= cl_reg_IND,
    "JPN"		= cl_reg_JPN,
    "USA"		= cl_reg_USA,
    "RUS"		= cl_reg_RUS,
    "nonattrib"=  cl_reg_nonattrib
)

cl_reg_mapping2 <- c(
    "WORLD"	= cl_reg_WORLD,
    "LAM"		= cl_reg_LAM,
    "OAS"		= cl_reg_OAS,
    "AFR"		= cl_reg_AFR,
    "ROW"		= cl_reg_ROW,
    "MEA"		= cl_reg_MEA,
    "EUR"		= cl_reg_EUR,
    "CHN"		= cl_reg_CHN,
    "IND"		= cl_reg_IND,
    "JPN"		= cl_reg_JPN,
    "USA"		= cl_reg_USA,
    "RUS"		= cl_reg_RUS,
    "nonattrib"=  cl_reg_nonattrib,
    "tem_res"= "grey20"
)

cl_reg_mapping3 <- c(
    "WORLD"	= "grey20",
    "LAM"		= "grey20",
    "OAS"		= "grey20",
    "AFR"		= "grey20",
    "ROW"		= "grey20",
    "MEA"		= "grey20",
    "EUR"		= "grey20",
    "CHN"		= "grey20",
    "IND"		= "grey20",
    "JPN"		= "grey20",
    "USA"		= "grey20",
    "RUS"		= "grey20",
    "nonattrib"=  "grey20",
    "tem_res"= "white"
)

lt_reg_mapping2 <- c(
    "WORLD"	= 1,
    "LAM"		= 1,
    "OAS"		= 1,
    "AFR"		= 1,
    "ROW"		= 1,
    "MEA"		= 1,
    "EUR"		= 1,
    "CHN"		= 1,
    "IND"		= 1,
    "JPN"		= 1,
    "USA"		= 1,
    "RUS"		= 1,
    "nonattrib"=  1,
    "tem_res"= 2
)

# tx_reg_mapping <- c(
#     "LAM" =	"Latin America and the Caribbean (LAM)",
#     "OAS" =	"Other Asia (OAS)",
#     "AFR" =	"Sub Saharan Africa (AFR)",
#     "ROW" =	"The rest of the World (ROW)",
#     "MEA" =	"Middle East and North Africa (MEA)",
#     "EUR" =	"Europe (EUR)",
#     "CHN" =	"China (CHN)",
#     "IND" =	"India (IND)",
#     "JPN" =	"Japan (JPN)",
#     "USA" =	"United States of America (USA)",
#     "RUS" =	"Russia (RUS)",
#     "nonattrib" = "Non-attributable"
# )

tx_reg_mapping <- c(
    "LAM" =	"LAM",
    "OAS" =	"OAS",
    "AFR" =	"AFR",
    "ROW" =	"ROW",
    "MEA" =	"MEA",
    "EUR" =	"EUR",
    "CHN" =	"CHN",
    "IND" =	"IND",
    "JPN" =	"JPN",
    "USA" =	"USA",
    "RUS" =	"RUS",
    "nonattrib" = "Other"
)

tx_reg_mapping2 <- c(
    "LAM" =	"LAM",
    "OAS" =	"OAS",
    "AFR" =	"AFR",
    "ROW" =	"ROW",
    "MEA" =	"MEA",
    "EUR" =	"EUR",
    "CHN" =	"CHN",
    "IND" =	"IND",
    "JPN" =	"JPN",
    "USA" =	"USA",
    "RUS" =	"RUS",
    "nonattrib" = "Other",
    "tem_res"= ""
)

br_reg_mapping <- c(
    "CHN", # =	"China",
    "IND", # =	"India",
    "JPN", # =	"Japan",
    "RUS", # =	"Russia",
    "USA", # =	"United States of America",

    "AFR", # =	"Sub Saharan Africa",
    "EUR", # =	"Europe",
    "LAM", # =	"Latin America and the Caribbean",
    "MEA", # =	"Middle East and North Africa",
    
    "OAS", # =	"Other Asia",
    "ROW",  # =	"The rest of the World"
    "nonattrib"
)

br_reg_mapping2 <- c(
    "CHN", # =	"China",
    "IND", # =	"India",
    "JPN", # =	"Japan",
    "RUS", # =	"Russia",
    "USA", # =	"United States of America",

    "AFR", # =	"Sub Saharan Africa",
    "EUR", # =	"Europe",
    "LAM", # =	"Latin America and the Caribbean",
    "MEA", # =	"Middle East and North Africa",
    
    "OAS", # =	"Other Asia",
    "ROW",  # =	"The rest of the World"
    "nonattrib",
    "tem_res"
)

sh_reg_mapping <- c(
    "CHN" = 0,
    "IND" = 1,
    "JPN" = 2,
    "RUS" = 3,
    "USA" = 4,

    "AFR" = 5,
    "EUR" = 6,
    "LAM" = 7,
    "MEA" = 8,
    
    "OAS" = 9,
    "ROW" = 10,
    "nonattrib" = 12
)

sh_reg_mapping2 <- c(
    "CHN" = 0,
    "IND" = 1,
    "JPN" = 2,
    "RUS" = 3,
    "USA" = 4,

    "AFR" = 5,
    "EUR" = 6,
    "LAM" = 7,
    "MEA" = 8,
    
    "OAS" = 9,
    "ROW" = 10,
    "nonattrib" = 12,
    "tem_res"=13
)

# cl_sec_neg <- cl_sc_1
# cl_sec_lnd <- colorRampPalette(brewer.pal(8, "Dark2"))(8)[2]  
# cl_sec_agr <- colorRampPalette(brewer.pal(8, "Dark2"))(8)[3]
# cl_sec_bur <- colorRampPalette(brewer.pal(8, "Dark2"))(8)[4]   
# cl_sec_hou <- colorRampPalette(brewer.pal(8, "Dark2"))(8)[5]    
# cl_sec_ene <- colorRampPalette(brewer.pal(8, "Dark2"))(8)[6]     
# cl_sec_ind <- colorRampPalette(brewer.pal(8, "Dark2"))(8)[7] 
# cl_sec_tra <- cl_sc_3
# cl_sec_wst <- colorRampPalette(brewer.pal(8, "Dark2"))(8)[8]  
# cl_sec_other <- "grey30"

cl_emiss_agr = colorRampPalette(brewer.pal(12, "Paired"))(12)[1]
cl_emiss_ene = colorRampPalette(brewer.pal(12, "Paired"))(12)[2]
cl_emiss_ind = colorRampPalette(brewer.pal(12, "Paired"))(12)[3]
cl_emiss_slv = colorRampPalette(brewer.pal(12, "Paired"))(12)[4]
cl_emiss_tra = colorRampPalette(brewer.pal(12, "Paired"))(12)[5]
cl_emiss_air = colorRampPalette(brewer.pal(12, "Paired"))(12)[6]
cl_emiss_shp = colorRampPalette(brewer.pal(12, "Paired"))(12)[7]
cl_emiss_dom = colorRampPalette(brewer.pal(12, "Paired"))(12)[8]
cl_emiss_wst = colorRampPalette(brewer.pal(12, "Paired"))(12)[9]
cl_emiss_awb = colorRampPalette(brewer.pal(12, "Paired"))(12)[10]
cl_forestfire= colorRampPalette(brewer.pal(12, "Paired"))(12)[11]
cl_grassfire = colorRampPalette(brewer.pal(12, "Paired"))(12)[12]
cl_emiss_neg = "#999999"
cl_emiss_luc = "#b35806"

cl_group_co2neg  <- cl_emiss_neg #colorRampPalette(brewer.pal(9, "Set1"))(16)[2]
cl_group_co2luc  <- cl_emiss_luc #colorRampPalette(brewer.pal(9, "Set1"))(16)[7]
cl_group_CO2     <- kelly_colors[7]
cl_group_CH4     <- kelly_colors[16]
cl_group_CH4ox   <- kelly_colors[15]
cl_group_H2Os    <- kelly_colors[5]
cl_group_N2O     <- kelly_colors[3]
cl_group_F_Gases <- kelly_colors[8]
cl_group_O3S     <- kelly_colors[11]
cl_group_O3T     <- kelly_colors[21]
cl_group_SO2     <- kelly_colors[10]
cl_group_NOX     <- kelly_colors[12]
cl_group_POA     <- kelly_colors[13]
cl_group_SOA     <- kelly_colors[6]
cl_group_BC      <- kelly_colors[14]
cl_group_BCsnow  <- kelly_colors[4]
cl_group_cloud   <- kelly_colors[17]
cl_group_DUST       <- kelly_colors[18]
cl_group_LandAlbedo <- kelly_colors[19]
cl_group_Natural    <- kelly_colors[20]

cl_sec_mapping <- c(
    "emiss_agr" =  cl_emiss_agr,
    "emiss_ene" =  cl_emiss_ene,
    "emiss_ind" =  cl_emiss_ind,
    "emiss_slv" =  cl_emiss_slv,
    "emiss_tra" =  cl_emiss_tra,
    "emiss_air" =  cl_emiss_air,
    "emiss_shp" =  cl_emiss_shp,
    "emiss_dom" =  cl_emiss_dom,
    "emiss_wst" =  cl_emiss_wst,
    "emiss_awb" =  cl_emiss_awb,
    "forestfire"=  cl_forestfire,
    "grassfire" =  cl_grassfire,
    "emiss_neg" =  cl_emiss_neg,
    "emiss_luc" =  cl_emiss_luc,
    "emiss_lcc" =  cl_group_LandAlbedo,
    "lcc"       =  cl_group_LandAlbedo,
    "dust"      =  cl_group_DUST,
    "natural"   =  cl_group_Natural,
    "other"     =  cl_reg_nonattrib
)

cl_sec_mapping2 <- c(
    "emiss_agr" =  cl_emiss_agr,
    "emiss_ene" =  cl_emiss_ene,
    "emiss_ind" =  cl_emiss_ind,
    "emiss_tra" =  cl_emiss_tra,
    "emiss_air" =  cl_emiss_air,
    "emiss_shp" =  cl_emiss_shp,
    "emiss_dom" =  cl_emiss_dom,
    "emiss_wst" =  cl_emiss_wst,
    "lcc"       =  cl_group_LandAlbedo,
    "natural"   =  cl_group_Natural,
    "emiss_awb" =  cl_emiss_awb,
    "emiss_neg" =  cl_emiss_neg,
    "emiss_luc" =  cl_emiss_luc,
    "dust"      =  cl_group_DUST

)

cl_sec_mapping2_1 <- c(
    "emiss_agr" =  "grey20",
    "emiss_ene" =  "grey20",
    "emiss_ind" =  "grey20",
    "emiss_tra" =  "grey20",
    "emiss_air" =  "grey20",
    "emiss_shp" =  "grey20",
    "emiss_dom" =  "grey20",
    "emiss_wst" =  "grey20",
    "lcc"       =  "grey20",
    "natural"   =  "grey20",
    "emiss_awb" =  "grey20",
    "emiss_neg" =  "grey20",
    "emiss_luc" =  "grey20",
    "dust"      =  "grey20"
    
)

tx_sec_mapping <- c(
      "emiss_agr" = expression("Agriculture"),
      "emiss_ene" = expression("Energy"),
      "emiss_ind" = expression("Industry"),
      "emiss_slv" = expression("Solvent"),
      "emiss_tra" = expression("Surface transport"),
      "emiss_air" = expression("Aviation"),
      "emiss_shp" = expression("Intl' shipping"),
      "emiss_dom" = expression("Housing"),
      "emiss_wst" = expression("Waste"),
      "emiss_awb" = expression("Agricultural waste"),
      "forestfire" = expression("Forest burning"),
      "grassfire" = expression("Grass burning"),
      "emiss_neg" = expression("Negative"~CO[2]),
      "emiss_luc" = expression("Land use"~CO[2]),
      "emiss_lcc" =  expression("Land albedo"),
      "lcc"       =  expression("Land albedo"),
      "dust"      =  expression("Mineral dust"),
      "natural"   =  expression("Natural"),
      "other"     =  expression("Other")
)

tx_sec_mapping2 <- c(
      "emiss_agr" = expression("Agriculture"),
      "emiss_ene" = expression("Energy"),
      "emiss_ind" = expression("Industry"),
      "emiss_tra" = expression("Surface transport"),
      "emiss_air" = expression("Aviation"),
      "emiss_shp" = expression("Intl' shipping"),
      "emiss_dom" = expression("Housing"),
      "emiss_wst" = expression("Waste"),
      "lcc"       = expression("Land albedo"),
      "natural"   = expression("Natural"),
      "emiss_awb" = expression("Open burning"),
      "emiss_neg" = expression("Negative"~CO[2]),
      "emiss_luc" = expression("Land use"~CO[2]),
      "dust"      = expression("Mineral dust")
)

cl_sec2_mapping <- c(
    "Negative" = cl_emiss_neg,
    "Land"     = cl_emiss_luc,   
    "Agriculture" = cl_emiss_agr,
    "Open burning" = cl_emiss_awb,
    "Housing" = cl_emiss_dom,
    "Energy" = cl_emiss_ene,
    "Industry" = cl_emiss_ind,
    "Transport" = cl_emiss_tra,
    "Waste" = cl_emiss_wst
)

tx_sec2_mapping <- c(
    "Negative" = expression(Negative~CO[2]),
    "Land"     = expression(LUC~CO[2]),   
    "Agriculture" = expression(Agriculture),
    "Open burning" = expression(Open~burning),
    "Housing" = expression(Housing),
    "Energy" = expression(Energy),
    "Industry" = expression(Industry),
    "Transport" = expression(Transport),
    "Waste" = expression(Waste)
)

sh_sec2_mapping <- c(
    "Negative" = 15,
    "Land"     = 16,   
    "Agriculture" = 17,
    "Open burning" = 18,
    "Housing" = 19,
    "Energy" = 21,
    "Industry" = 22,
    "Transport" = 23,
    "Waste" = 24
)

# cl_group_co2neg  <- cl_emiss_neg #colorRampPalette(brewer.pal(9, "Set1"))(16)[2]
# cl_group_co2luc  <- cl_emiss_luc #colorRampPalette(brewer.pal(9, "Set1"))(16)[7]
# cl_group_CO2     <- colorRampPalette(brewer.pal(12, "Paired"))(18)[1]
# cl_group_CH4     <- colorRampPalette(brewer.pal(12, "Paired"))(18)[2]
# cl_group_CH4ox   <- colorRampPalette(brewer.pal(12, "Paired"))(18)[3]
# cl_group_H2Os    <- colorRampPalette(brewer.pal(12, "Paired"))(18)[4]
# cl_group_N2O     <- colorRampPalette(brewer.pal(12, "Paired"))(18)[5]
# cl_group_F_Gases <- colorRampPalette(brewer.pal(12, "Paired"))(18)[6]
# cl_group_O3S     <- colorRampPalette(brewer.pal(12, "Paired"))(18)[7]
# cl_group_O3T     <- colorRampPalette(brewer.pal(12, "Paired"))(18)[8]
# cl_group_SO2     <- colorRampPalette(brewer.pal(12, "Paired"))(18)[9]
# cl_group_NOX     <- colorRampPalette(brewer.pal(12, "Paired"))(18)[10]
# cl_group_POA     <- colorRampPalette(brewer.pal(12, "Paired"))(18)[11]
# cl_group_SOA     <- colorRampPalette(brewer.pal(12, "Paired"))(18)[12]
# cl_group_BC      <- colorRampPalette(brewer.pal(12, "Paired"))(18)[13]
# cl_group_BCsnow  <- colorRampPalette(brewer.pal(12, "Paired"))(18)[14]
# cl_group_cloud   <- colorRampPalette(brewer.pal(12, "Paired"))(18)[15]
# cl_group_DUST       <- colorRampPalette(brewer.pal(12, "Paired"))(18)[16]
# cl_group_LandAlbedo <- colorRampPalette(brewer.pal(12, "Paired"))(18)[17]
# cl_group_Natural    <- colorRampPalette(brewer.pal(12, "Paired"))(18)[18]

cl_ems_mapping <- c(
    "CO2ff"   = cl_group_CO2,
    "CH4"     = cl_group_CH4,
    "N2O"     = cl_group_N2O,
    "fgs"     = cl_group_F_Gases,
    "BC"      = cl_group_BC,
    "CO"      = cl_group_O3T,
    "NOX"     = cl_group_NOX,
    "lcc"     = cl_group_LandAlbedo,
    "natural" = cl_group_Natural,

    "CO2luc"  = cl_group_co2luc,
    "CO2neg"  = cl_group_co2neg,
    "NH3"     = cl_group_O3S,
    "OC"      = cl_group_POA,
    "SO2"     = cl_group_SO2,
    "VOC"     = cl_group_SOA,
    "dust"    = cl_group_DUST
)

cl_ems_mapping2 <- c(
    "BC"      = "grey20",
    "CO2luc"  = "grey20",
    "CO2neg"  = "grey20",
    "CO2ff"   = "grey20",
    "CH4"     = "grey20",
    "N2O"     = "grey20",
    "fgs"     = "grey20",
    "CO"      = "grey20",
    "NH3"     = "grey20",
    "NOX"     = "grey20",
    "OC"      = "grey20",
    "SO2"     = "grey20",
    "VOC"     = "grey20",
    "lcc"     = "grey20",
    "dust"    = "grey20",
    "natural" = "grey20",
    "tem_res" =  "white"
)


cl_ems_mapping3 <- c(
    "co2"   = cl_group_CO2,
    "ch4"     = cl_group_CH4,
    "n2o"     = cl_group_N2O,
    "fgs"     = cl_group_F_Gases,
    "bc"      = cl_group_BC,
    "co"      = cl_group_O3T,
    "nox"     = cl_group_NOX,
    "lcc"     = cl_group_LandAlbedo,
    "solar"   = cl_group_Natural,
    "volc"    = cl_group_BCsnow,

    "co2l"  = cl_group_co2luc,
    "co2n"  = cl_group_co2neg,
    "nh3"     = cl_group_O3S,
    "oc"      = cl_group_POA,
    "so2"     = cl_group_SO2,
    "voc"     = cl_group_SOA,
    "dust"    = cl_group_DUST
)

cl_group_mapping <- c(
    "co2neg" = cl_group_co2neg,
    "co2luc" = cl_group_co2luc,
    "CO2" = cl_group_CO2,
    "CH4" = cl_group_CH4,
    # "CH4ox" = cl_group_CH4ox,
    "H2Os" = cl_group_H2Os,
    "N2O" = cl_group_N2O,
    "F_Gases" = cl_group_F_Gases,
    "o3s" = cl_group_O3S,
    "o3t" = cl_group_O3T,
    "SO4" = cl_group_SO2,
    "NO3" = cl_group_NOX,
    "POA" = cl_group_POA,
    "SOA" = cl_group_SOA,
    "BC" = cl_group_BC,
    "BCsnow"=cl_group_BCsnow,
    "cloud" = cl_group_cloud,
    "LandAlbedo" = cl_group_LandAlbedo,
    "DUST" = cl_group_DUST,
    "Natural" = cl_group_Natural
)

tx_ems_mapping <- c(
    "BC"      = expression(BC),
    "CO2luc"  = expression(LUC~CO[2]),
    "CO2neg"  = expression(Negative~CO[2]),
    "CO2ff"   = expression(FF~CO[2]),
    "CH4"     = expression(CH[4]),
    "N2O"     = expression(N[2]*O),
    "fgs"     = expression(Halogenated),
    "CO"      = expression(CO),
    "NH3"     = expression(NH[3]),
    "NOX"     = expression(Nitrate),
    "SO2"     = expression(Sulfate),
    "OC"      = expression(OC),
    "VOC"     = expression(VOC),
    "lcc"     = expression(Land~albedo),
    "dust"    = expression(Dust),
    "natural" = expression(Natural)
)

tx_ems_mapping3 <- c(
    "bc"      = expression(BC),
    "co2l"  = expression(LUC~CO[2]),
    "co2n"  = expression(Negative~CO[2]),
    "co2"   = expression(FF~CO[2]),
    "ch4"     = expression(CH[4]),
    "n2o"     = expression(N[2]*O),
    "fgs"     = expression(Halogenated),
    "co"      = expression(CO),
    "nh3"     = expression(NH[3]),
    "nox"     = expression(Nitrate),
    "so2"     = expression(Sulfate),
    "oc"      = expression(OC),
    "voc"     = expression(VOC),
    "lcc"     = expression(Land~albedo),
    "dust"    = expression(Dust),
    "volc" = expression(Volcanic),
    "solar" = expression(Solar~irradiance)
)

tx_group_mapping <- c(
                    "co2neg"          = expression(Negative~CO[2]),     
                    "co2luc"          = expression(LUC~CO[2]),  
                    "CO2"             = expression(FF~CO[2]),           
                    "CH4"             = expression(CH[4]), 
                    # "CH4ox"           = expression(CH[4]~oxidized),
                    "H2Os"            = expression(Strat.~H[2]*O),    
                    "N2O"             = expression(N[2]*O), 
                    "F_Gases"         = expression(Halogenated),
                    "o3s"             = expression(Strat.~O[3]),  
                    "o3t"             = expression(Tropo.~O[3]),  
                    "SO4"             = expression(Sulfate), 
                    "NO3"             = expression(Nitrate), 
                    "POA"             = expression(POA),    
                    "SOA"             = expression(SOA),    
                    "BC"              = expression(BC),  
                    "BCsnow"          = expression(BC~on~snow), 
                    "cloud"           = expression(Cloud),           
                    "LandAlbedo"      = expression(Land~albedo),
                    "DUST"            = expression(Dust),
                    "Natural"         = expression(Natural)
            )
            
cl_gmt_mapping <- c(
  "SCM4OPTv2" = cl_sc_3,
  "GISTEMPv4"   = cl_sc_4,
  "HadCRUT46"   = cl_sc_2,
  "JMA"     = cl_sc_5,
  "NOAA"   = cl_sc_6,
  "GISS"    = cl_sc_1
)

fl_gmt_mapping <- c(
  "SCM4OPTv2" = cl_sc_31,
  "GISTEMPv4"   = cl_sc_41,
  "HadCRUT46"   = cl_sc_21,
  "JMA"     = cl_sc_51,
  "NOAA"   = cl_sc_61,
  "GISS"    = cl_sc_11
)

lt_gmt_mapping <- c(
  "SCM4OPTv2" = 1,
  "GISTEMPv4"   = 2,
  "HadCRUT46"   = 3,
  "JMA"     = 4,
  "NOAA"   = 5,
  "GISS"    = 6
)

sh_gmt_mapping <- c(
  "SCM4OPTv2" = 21,
  "GISTEMPv4"   = 3,
  "HadCRUT46"   = 4,
  "JMA"     = 8,
  "NOAA"   = 24,
  "GISS"    = 23
)

tx_gmt_mapping <- c(
  "SCM4OPTv2" = "SCM4OPT v2.0",
  "GISTEMPv4"   = "GISTEMP v4",
  "HadCRUT46"   = "HadCRUT 4.6",
  "JMA"     = "Japan Meteorological Agency (JMA)",
  "NOAA"   = "National Climatic Data Center (NCDC)",
  "GISS"    = "Goddard Institute for Space Studies (GISS)"
)

cl_ref_mapping <- c(
  "SCM4OPTv2" = cl_sc_3,
  "MAGICC6"   = cl_sc_2,
  "IPCCAR5"   = cl_sc_4,
  "CMIP5"     = cl_sc_5,
  "IPCCAR52011"   = cl_sc_6,
  "OSCAR2"    = cl_sc_1
)

fl_ref_mapping <- c(
  "SCM4OPTv2" = cl_sc_31,
  "MAGICC6"   = cl_sc_21,
  "IPCCAR5"   = cl_sc_41,
  "CMIP5"     = cl_sc_51,
  "IPCCAR52011"   = cl_sc_61,
  "OSCAR2"    = cl_sc_11
)

lt_ref_mapping <- c(
  "SCM4OPTv2" = 1,
  "MAGICC6"   = 2,
  "IPCCAR5"   = 3,
  "CMIP5"     = 4,
  "IPCCAR52011"   = 5,
  "OSCAR2"    = 6
)

sh_ref_mapping <- c(
  "SCM4OPTv2" = 21,
  "MAGICC6"   = 3,
  "IPCCAR5"   = 4,
  "CMIP5"     = 8,
  "IPCCAR52011"   = 24,
  "OSCAR2"    = 23
)

tx_ref_mapping <- c(
  "SCM4OPTv2" = "SCM4OPT v2.0",
  "MAGICC6"   = "MAGICC6",
  "IPCCAR5"   = "IPCC AR5",
  "CMIP5"     = "CMIP5",
  "IPCCAR52011"   = "IPCC AR5 (1750-2011)",
  "OSCAR2"    = "OSCAR v2.2"
)

cl_fl_mapping <- c(
    "FL19" = cl_sc_2,
    "FL26" = cl_sc_21,
    "FL34" = cl_sc_1,
    "FL45" = cl_sc_11,
    "FL60" = cl_sc_4,
    "FL70" = cl_sc_41,
    "FL85" = cl_sc_3
)

fl_fl_mapping <- c(
    "FL19" = cl_sc_2,
    "FL26" = cl_sc_21,
    "FL34" = cl_sc_1,
    "FL45" = cl_sc_11,
    "FL60" = cl_sc_4,
    "FL70" = cl_sc_41,
    "FL85" = cl_sc_3
)

lt_fl_mapping <- c(
    "FL19" = 1,
    "FL26" = 2,
    "FL34" = 3,
    "FL45" = 4,
    "FL60" = 5,
    "FL70" = 6,
    "FL85" = 7
)

tx_fl_mapping <- c(
    "Hist" = expression(Historical),
    "FL19" = expression("1.9"~Wm^{-2}),
    "FL26" = expression("2.6"~Wm^{-2}),
    "FL34" = expression("3.4"~Wm^{-2}),
    "FL45" = expression("4.5"~Wm^{-2}),
    "FL60" = expression("6.0"~Wm^{-2}),
    "FL70" = expression("7.0"~Wm^{-2}),
    "FL85" = expression("8.5"~Wm^{-2})
)

tx_fl_mapping2 <- c(
    "1.9~Wm^{-2}",
    "2.6~Wm^{-2}",
    "3.4~Wm^{-2}",
    "4.5~Wm^{-2}",
    "6.0~Wm^{-2}",
    "7.0~Wm^{-2}",
    "8.5~Wm^{-2}"
)
cl_lnd_mapping <- c(
  "land_des"  = brewer.pal(9, "Set1")[1],
  "land_for"  = brewer.pal(9, "Set1")[2],   
  "land_gra"  = brewer.pal(9, "Set1")[3],  
  "land_cro"  = brewer.pal(9, "Set1")[4],  
  "land_pas"  = brewer.pal(9, "Set1")[5]
)

fl_lnd_mapping <- c(
  "land_des"  = brewer.pal(9, "Set1")[1],
  "land_for"  = brewer.pal(9, "Set1")[2],   
  "land_gra"  = brewer.pal(9, "Set1")[3],  
  "land_cro"  = brewer.pal(9, "Set1")[4],  
  "land_pas"  = brewer.pal(9, "Set1")[5]
)

tx_lnd_mapping <- c(
  "land_des"  = "Desert & urban",
  "land_for"  = "Forest",   
  "land_gra"  = "Grassland & shrubland",  
  "land_cro"  = "Cropland",  
  "land_pas"  = "Pasture"
)

theme_singleplot_color0 <- function(){
  theme( 
    plot.title   = font_element0,
    axis.text.x  = font_element0,
    axis.text.y  = font_element0,  
    axis.title.x = font_element0,
    axis.title.y = font_element0,
    legend.text  = font_element0, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_singleplot_color2 <- function(){
  theme( 
    plot.title   = font_element2,
    axis.text.x  = font_element2,
    axis.text.y  = font_element2,  
    axis.title.x = font_element2,
    axis.title.y = font_element2,
    legend.text  = font_element2, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    #legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_singleplot_color3 <- function(){
  theme( 
    plot.title   = font_element2,
    axis.text.x  = font_element2,
    axis.text.y  = font_element2,  
    axis.title.x = font_element2,
    axis.title.y = font_element2,
    legend.text  = font_element2, 
    #legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    #legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color00 <- function(){
  theme( 
    plot.title   = font_element00,
    axis.text.x  = font_element00,
    axis.text.y  = font_element00,  
    axis.title.x = element_blank(),
    axis.title.y = font_element00,
    legend.text  = font_element00, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color0_with_legtitle <- function(){
  theme( 
    plot.title   = font_element0,
    axis.text.x  = font_element0,
    axis.text.y  = font_element0,  
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text  = font_element0, 
    legend.title = font_element0,
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color0 <- function(){
  theme( 
    plot.title   = font_element0,
    axis.text.x  = font_element0,
    axis.text.y  = font_element0,  
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text  = font_element0, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color <- function(){
  theme( 
    plot.title   = font_element1,
    axis.text.x  = font_element1,
    axis.text.y  = font_element1,  
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text  = font_element1, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_simple <- function(){
  theme_bw()+
  theme( 
    plot.title   = font_element1,
    axis.text.x  = font_element1,
    axis.text.y  = font_element1,  
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text  = font_element1, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black")
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color_with_xlab <- function(){
  theme( 
    plot.title   = font_element1,
    axis.text.x  = font_element1,
    axis.text.y  = font_element1,  
    axis.title.x = font_element1,
    axis.title.y = element_blank(),
    legend.text  = font_element1, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color_with_xlab <- function(){
  theme( 
    plot.title   = font_element1,
    axis.text.x  = font_element1,
    axis.text.y  = font_element1,  
    axis.title.x = font_element1,
    axis.title.y = element_blank(),
    legend.text  = font_element1, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color2 <- function(){
  theme( 
    plot.title   = font_element2,
    axis.text.x  = font_element2,
    axis.text.y  = element_blank(),  
    axis.title.x = font_element2,
    axis.title.y = element_blank(),
    legend.text  = font_element2, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color_without_xlab2 <- function(){
  theme( 
    plot.title   = font_element2,
    axis.text.x  = font_element2,
    axis.text.y  = element_blank(),  
    axis.title.x = font_element2,
    axis.title.y = element_blank(),
    legend.text  = font_element2, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color_with_xlab2 <- function(){
  theme( 
    plot.title   = font_element2,
    axis.text.x  = font_element2,
    axis.text.y  = font_element2,  
    axis.title.x = font_element2,
    axis.title.y = font_element2,
    legend.text  = font_element2, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color_with_xlab_title_left2 <- function(){
  theme( 
    plot.title   = font_element2L,
    axis.text.x  = font_element2,
    axis.text.y  = element_blank(),  
    axis.title.x = font_element2,
    axis.title.y = element_blank(),
    legend.text  = font_element2, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color_with_xlab_migrid <- function(){
  theme( 
    plot.title   = font_element1,
    axis.text.x  = font_element1,
    axis.text.y  = font_element1,  
    axis.title.x = font_element1,
    axis.title.y = element_blank(),
    legend.text  = font_element1, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent")
    #      panel.grid.major = element_line(linetype = "dashed"),
    #panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color_with_ylab <- function(){
  theme( 
    plot.title   = font_element1,
    axis.text.x  = font_element1,
    axis.text.y  = font_element1,  
    axis.title.x = element_blank(),
    axis.title.y = font_element1,
    legend.text  = font_element1, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

theme_multiplot_color_with_xylab <- function(){
  theme( 
    plot.title   = font_element1,
    axis.text.x  = font_element1,
    axis.text.y  = font_element1,  
    axis.title.x = font_element1,
    axis.title.y = font_element1,
    legend.text  = font_element1, 
    legend.title = element_blank(),
    #legend.position  = c(0.64, 1.18), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill="transparent"),
    #      panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
    #      panel.background = element_blank(),
    #      panel.border = element_rect(colour = "black")
    #      axis.line = element_line(colour = "black")
  )
}

colMax <- function(X) apply(X, 2, max)

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

ggplot.ccf = function(x, y, lag.min=NULL, lag.max=NULL) {
  ccf.data = ccf(x,y,plot=F)
  
  indices = which(ccf.data$lag[,1,1] %in% lag.min:lag.max)
  ccf.df = data.frame(lag = ccf.data$lag[indices,1,1],
                      correlation = ccf.data$acf[indices,1,1])
  
  ggplot(ccf.df,
         aes(x = lag, y = correlation)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept=.3, color = 'blue', linetype = 'dashed') +
    geom_hline(yintercept=-.3, color = 'blue', linetype = 'dashed')
}