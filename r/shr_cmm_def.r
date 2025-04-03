# non-CO2 emissions
ems_nco2_lst = c(
    "BC",
    "CH4",
    "N2O",
    "CO",
    "NH3",
    "NOX",
    "OC",
    "SO2",
    "VOC"
)

# sector definition
def_sector <- c("EMISS_IND")
ems_nonco2 <- tolower(ems_nco2_lst)
# ems_fgs_all <- tolower(all_fgs_lst)

# CMIP6 forcing folder definition
cmip6_forcing_dir <- paste0(scm_dir, "src/CMIP6-forcing/")

# figure and data folder definition
# fig_dir <- "./figs/"
fig_dir <- "./figs-slow/"
cali_dat <- "./data-cali/"
# fl_ext <- ".png"
fl_ext <- ".pdf"

# time step defined as bimonthly (x6) in the simple climate model
scm_dt <- 6
scm_dm <- 12

# all observation source definition
src_all <- c(
    "GCP2020v1.0",
    "PRIMAPv2.2",
    "EDGARv5.0",
    "CEDS_v2021",
    "HadCRUTv4.6",
    "HadCRUTv5.0",
    "GlobalTemp_v5.1",
    "GISTEMPv4",
    "Berkeley_Earth",
    "Coverage2013_cobe2cru_krig_v2",
    "Coverage2013_cru4_krig_v2",
    "Coverage2013_had4_krig_v2",
    "Coverage2013_had4sst4_krig_v2",
    "CMIP5",
    "CMIP6",
    "IPCC_AR5",
    "NOAA_global",
    "NOAA_MaunaLoa",
    "GCPCO2",
    "Law2006"
)

# all emission source definition
src_ems <- c(
    "GCP2020v1.0",
    "PRIMAPv2.2",
    "EDGARv5.0",
    "CEDS_v2021"    
)

# all temperature source definition
src_tem <- c(
    "HadCRUTv4.6",
    "HadCRUTv5.0.2",
    "GlobalTemp_v5.1",
    "GISTEMPv4",
    "Berkeley_Earth",
    "Coverage2013_cobe2cru_krig_v2",
    "Coverage2013_cru4_krig_v2",
    "Coverage2013_had4_krig_v2",
    "Coverage2013_had4sst4_krig_v2"    
)

# all CO2 source definition
src_co2 <- c(
    "CMIP5",
    "CMIP6",
    "IPCC_AR5",
    "NOAA_global",
    "NOAA_MaunaLoa",
    "Law2006",
    "GCPCO2"
)

# line type definition for plotting
cor_lt_ems_mapping <- c(
    "GCP2020v1.0"   = 1,
    "PRIMAPv2.2"    = 2,
    "EDGARv5.0"     = 3,
    "CEDS_v2021"    = 4   
)

# point shape definition for plotting
cor_sh_ems_mapping <- c(
    "GCP2020v1.0"   = 1,
    "PRIMAPv2.2"    = 2,
    "EDGARv5.0"     = 3,
    "CEDS_v2021"    = 4   
)

# label definition for plotting
cor_tx_ems_mapping <- c(
    "GCP2020v1.0"   = "GCP2020 v1.0",
    "PRIMAPv2.2"    = "PRIMAP v2.2",
    "EDGARv5.0"     = "EDGAR v5.0",
    "CEDS_v2021"    = "CEDS v2021"   
)

# line type definition for plotting
cor_lt_co2_mapping <- c(
    "NOAA_global"   = 1,
    "NOAA_MaunaLoa" = 2,
    "GCPCO2"        = 3
)

# point shape definition for plotting
cor_sh_co2_mapping <- c(
    "NOAA_global"   = 1,
    "NOAA_MaunaLoa" = 2,
    "GCPCO2"        = 3
)

# label definition for plotting
cor_tx_co2_mapping <- c(
    "NOAA_global"   = "NOAA global",
    "NOAA_MaunaLoa" = "NOAA MaunaLoa",
    "GCPCO2"        = expression(GCP~atmospheric~CO[2]~growth)
)

# line mapping definition for plotting
cor_lm_co2_mapping <- c(
    "NOAA_global"   ,
    "NOAA_MaunaLoa" ,
    "GCPCO2"        
)

# line type definition for plotting
cor_lt_co2_mapping2 <- c(
    "NOAA_global"   = 1,
    "NOAA_MaunaLoa" = 2,
    "GCPCO2"        = 3,
    "CMIP6"         = 4,
    "RCMIP2"        = 5,
    "SCM4OPTv32"    = 6
)

# point shape definition for plotting
cor_sh_co2_mapping2 <- c(
    "NOAA_global"   = 1,
    "NOAA_MaunaLoa" = 2,
    "GCPCO2"        = 3,
    "CMIP6"         = 4,
    "RCMIP2"        = 5,
    "SCM4OPTv32"    = 6
)

# colour definition for plotting
cor_cl_co2_mapping2 <- c(
    "NOAA_global"   = cl_sc_1,
    "NOAA_MaunaLoa" = cl_sc_2,
    "GCPCO2"        = cl_sc_3,
    "CMIP6"         = cl_sc_4,
    "RCMIP2"        = "grey60",
    "SCM4OPTv32"    = cl_sc_5
)

# fill colour definition for plotting
cor_fl_co2_mapping2 <- c(
    "NOAA_global"   = cl_sc_11,
    "NOAA_MaunaLoa" = cl_sc_21,
    "GCPCO2"        = cl_sc_31,
    "CMIP6"         = cl_sc_41,
    "RCMIP2"        = "grey60",
    "SCM4OPTv32"    = cl_sc_51
)

# label definition for plotting
cor_tx_co2_mapping2 <- c(
    "NOAA_global"   = "NOAA global",
    "NOAA_MaunaLoa" = "NOAA MaunaLoa",
    "GCPCO2"        = expression(GCP~atmospheric~CO[2]~growth),
    "CMIP6"         = "CMIP6",
    "RCMIP2"        = expression(RCMIP~phase~2),
    "SCM4OPTv32"    = "SCM4OPTv3.2"
)

# line mapping definition for plotting
cor_lm_co2_mapping2 <- c(
    "NOAA_global"   ,
    "NOAA_MaunaLoa" ,
    "GCPCO2"        ,
    "CMIP6"         ,
    "RCMIP2"        ,
    "SCM4OPTv32"
)

# colour definition for plotting
cl_gmt_mapping <- c(
  "berkeleyearth" = cl_sc_3,
  "HadCRUT50"     = cl_sc_2,  
  "RCMIP2"        = "grey60",  
  "SCM4OPTv3"     = cl_sc_5
)

# fill colour definition for plotting
fl_gmt_mapping <- c(
  "berkeleyearth" = cl_sc_31,
  "HadCRUT50"     = cl_sc_21,  
  "RCMIP2"        = "grey60",  
  "SCM4OPTv3"     = cl_sc_51
)

# line type definition for plotting
lt_gmt_mapping <- c(
  "berkeleyearth" = 1,
  "HadCRUT50"     = 2,  
  "RCMIP2"        = 3,  
  "SCM4OPTv3"     = 4
)

# point shape definition for plotting
sh_gmt_mapping <- c(
  "berkeleyearth" = 1,
  "HadCRUT50"     = 2,  
  "RCMIP2"        = 3,  
  "SCM4OPTv3"     = 4
)

# label definition for plotting
tx_gmt_mapping <- c(
  "berkeleyearth" = "Berkeley Earth", 
  "HadCRUT50"   = "HadCRUT 5.0",
  "RCMIP2"    = "RCMIP phase 2",
  "SCM4OPTv3" = "SCM4OPT v3.2"
)

# line type definition for plotting
cor_lt_tem_mapping <- c(
    "HadCRUTv5.0"       = 1,
    "GlobalTemp_v5.1"   = 2,
    "GISTEMPv4"         = 3,
    "Berkeley_Earth"    = 4
)

# point shape definition for plotting
cor_sh_tem_mapping <- c(
    "HadCRUTv5.0"       = 1,
    "GlobalTemp_v5.1"   = 2,
    "GISTEMPv4"         = 3,
    "Berkeley_Earth"    = 4
)

# label definition for plotting
cor_tx_tem_mapping <- c(
    "HadCRUTv5.0"       = "HadCRUT5",
    "GlobalTemp_v5.1"   = "GlobalTemp v5.1",
    "GISTEMPv4"         = "GISTEMP v4",
    "Berkeley_Earth"    = "Berkeley Earth"
)

# line type definition for plotting
cor_lt_tem_mapping0 <- c(
    "HadCRUTv5.0"       = 1,
    "GlobalTemp_v5.1"   = 2,
    "GISTEMPv4"         = 3,
    "Berkeley_Earth"    = 4, 
    "obsrv_mean"        = 1
)

# point shape definition for plotting
cor_sh_tem_mapping0 <- c(
    "HadCRUTv5.0"       = 1,
    "GlobalTemp_v5.1"   = 2,
    "GISTEMPv4"         = 3,
    "Berkeley_Earth"    = 4, 
    "obsrv_mean"        = NULL
)

# label definition for plotting
cor_tx_tem_mapping0 <- c(
    "HadCRUTv5.0"       = "HadCRUT5",
    "GlobalTemp_v5.1"   = "GlobalTemp v5.1",
    "GISTEMPv4"         = "GISTEMP v4",
    "Berkeley_Earth"    = "Berkeley Earth",
    "obsrv_mean"        = "Observation mean"
)

# label definition for plotting
cor_tx_tem_mapping0_idx <- c(
    "HadCRUTv4.6"       = "e. HadCRUT v4.6",
    "HadCRUTv5.0"       = "d. HadCRUT5",
    "GlobalTemp_v5.1"   = "c. GlobalTemp v5.1",
    "GISTEMPv4"         = "b. GISTEMP v4",
    "Berkeley_Earth"    = "a. Berkeley Earth",
    "obsrv_mean"        = "f. Observation mean"
)

# label definition for plotting
cor_tx_tem_mapping0_idx0 <- c(
    "HadCRUTv4.6"       = "HadCRUT v4.6",
    "HadCRUTv5.0"       = "HadCRUT5",
    "GlobalTemp_v5.1"   = "GlobalTemp v5.1",
    "GISTEMPv4"         = "GISTEMP v4",
    "Berkeley_Earth"    = "Berkeley Earth",
    "obsrv_mean"        = "Observation mean"
)

# fill colour definition for plotting
cor_fl_tem_mapping2 <- c(
    "scm_tatm_mean_qnt"   = "grey70",
    "co2a"        = cl_sl_co2a,
    "nco2_anthro" = cl_sl_ch4,
    "nat"         = cl_sl_volc,
    "vari"        = cl_sl_vari,
    "vari1"        = cl_sl_vari,
    "obsrv_mean"  = "grey40",
    "obsrv_mean1"  = "grey70"
)

# colour definition for plotting
cor_cl_tem_mapping2 <- c(
    "scm_tatm_mean_qnt"   = "grey50",
    "co2a"        = cl_sl_co2a,
    "nco2_anthro" = cl_sl_ch4,
    "nat"         = cl_sl_volc,
    "vari"        = cl_sl_vari,
    "vari1"        = cl_sl_vari,
    "obsrv_mean"  = "grey20",
    "obsrv_mean1"  = "grey50"
)

# line type definition for plotting
cor_lt_tem_mapping2 <- c(
    "scm_tatm_mean_qnt"   = "solid",
    "co2a"        = "solid",
    "nco2_anthro" = "solid",
    "nat"         = "solid",
    "vari"        = "solid",
    "vari1"        = "solid",
    "obsrv_mean"  = "solid",
    "obsrv_mean1"  = "solid"
)

# label definition for plotting
cor_tx_tem_mapping2 <- c(
    "scm_tatm_mean_qnt"         = expression(Rebuilt~total~warming),
    "co2a"              = expression(Rebuilt~CO[2]~warming),
    "nco2_anthro"       = expression(Rebuilt~non*"-"*CO[2]~anthropogenic),
    "nat"               = expression(Rebuilt~Natural),
    "obsrv_mean"        = expression(Observation~mean),
    "obsrv_mean1"        = expression(Observation~mean),
    "vari"              = expression(ENSO),
    "vari1"              = expression(ENSO)
)

# label definition for plotting
cor_tx_tem_mapping3 <- c(
    # "scm_tatm_mean_qnt"         = expression(Total~warming),
    "co2a"              = expression(CO[2]~warming),
    "nco2_anthro"       = expression(Non*"-"*CO[2]~anthropogenic),
    "nat"               = expression(Natural),
    "obsrv_mean"        = expression(Observation~mean),
    "obsrv_mean1"        = expression(Observation~mean),
    "vari"              = expression(ENSO),
    "vari1"              = expression(ENSO)
)

# colour definition for plotting
cor_cl_co2conc_mapping2 <- c(
    "GCPCO2"       = cl_sc_4,
    "INVE"         = cl_sc_3,
    "obsrv"        = "grey40"
)

# fill colour definition for plotting
cor_fl_co2conc_mapping2 <- c(
    "GCPCO2"       = cl_sc_41,
    "INVE"         = cl_sc_31,
    "obsrv"        = "grey70"
)

# line type definition for plotting
cor_lt_co2conc_mapping2 <- c(
    "GCPCO2"       = "solid",
    "INVE"         = "solid",
    "obsrv"        = "solid"
)

# label definition for plotting
cor_tx_co2conc_mapping2 <- c(
    "GCPCO2"       = expression(GCP~atmospheric~CO[2]~growth),
    "INVE"         = expression(Rebuilt~CO[2]~growth),
    "obsrv"        = expression(Observation~mean)
)

# colour definition for plotting
cor_cl_ems_mapping2 <- c(
    "GCP2020v1.0"  = cl_sc_4,
    "INVE"         = cl_sc_3   
)

# label definition for plotting
cor_tx_ems_mapping2 <- c(
    "GCP2020v1.0"  = expression(GCP~CO[2]~emissions),
    "INVE"         = expression(Inversed~CO[2]~rates)   
)

# colour definition for plotting
cor_cl_ems_mapping5 <- c(
  "co2a"    = cl_sl_co2a, 
  "ch4"     = cl_sl_ch4, 
  "n2o"     = cl_sl_n2o, 
  "fgs"     = cl_sl_ods, 
  "aero"    = cl_sl_aero, 
  "lcc"     = cl_sl_lcc, 
  "volc"     = cl_sl_volc,
  "solar"    = cl_sl_sola
)

# label definition for plotting
cor_tx_ems_mapping5 <- c(
  "co2a"    = expression(CO[2]), 
  "ch4"     = expression(CH[4]), 
  "n2o"     = expression(N[2]*O), 
  "fgs"     = expression(Halogenated), 
  "aero"    = expression(Aerosols), 
  "lcc"     = expression(Land~albedo), 
  "volc"     = expression(Volcanic),
  "solar"    = expression(Solar~irradiance)
)

# label definition for plotting
cor_tx_ems_mapping51 <- c(
  "co2a"    = "CO[2]", 
  "ch4"     = "CH[4]", 
  "n2o"     = "N[2]*O", 
  "fgs"     = "Halogenated", 
  "aero"    = "Aerosols", 
  "lcc"     = "Land~albedo", 
  "volc"     = "Volcanic",
  "solar"    = "Solar~irradiance"
)

# line type definition for plotting
cor_lt_ems_mapping5 <- c(
  "co2a"    = 1, 
  "ch4"     = 2, 
  "n2o"     = 3, 
  "fgs"     = 4, 
  "aero"    = 5, 
  "lcc"     = 6, 
  "volc"     = 7,
  "solar"    = 8

)

# emission mapping for plotting
cor_od_ems_mapping6 <- c(
  "co2a", 
  "nghg", 
  "aero",
  "lcc", 
  "nat",
  "vari"
)

# colour definition for plotting
cor_cl_ems_mapping6 <- c(
  "co2a"    = cl_sl_co2a, 
  "nghg"    = cl_sl_ch4, 
  "aero"    = cl_sl_aero,
  "lcc"     = cl_sl_lcc, 
  "nat"     = cl_sl_volc,
  "vari"    = cl_sl_vari
)

# label definition for plotting
cor_tx_ems_mapping6 <- c(
  "co2a"    = expression(CO[2]), 
  "nghg"     = expression(GHGs~excl*"."~CO[2]), #expression(GHGs~"w/o"~CO[2]), #expression("Non-"*CO[2]~GHGs), 
  "aero"    = expression(Aerosols), 
  "lcc"     = expression(Land~albedo), 
  "nat"     = expression(Natural),
  "vari"    = expression(ENSO)
)

# line type definition for plotting
cor_lt_ems_mapping6 <- c(
  "co2a"    = 1, 
  "n2o"     = 2, 
  "aero"    = 3, 
  "lcc"     = 4, 
  "nat"     = 5,
  "vari"    = 6
)

# colour definition for plotting
cor_cl_ems_mapping7 <- c(
#   "scm_tatm_mean_qnt"   = "grey30",
  "co2a"    = cl_sl_co2a, 
  "ch4"     = cl_sl_ch4, 
  "n2o"     = cl_sl_n2o, 
  "fgs"     = cl_sl_ods, 
  "aero"    = cl_sl_aero, 
  "lcc"     = cl_sl_lcc, 
  "nat"     = cl_sl_volc,
  "vari"    = cl_sl_vari
#   "obsrv_mean"  = "grey50"
)

# label definition for plotting
cor_tx_ems_mapping7 <- c(
#   "scm_tatm_mean_qnt"   =  expression(Total~warming),
  "co2a"    = expression(CO[2]),
  "ch4"     = expression(CH[4]), 
  "n2o"     = expression(N[2]*O), 
  "fgs"     = expression(Halogenated), 
  "aero"    = expression(Aerosols), 
  "lcc"     = expression(Land~albedo), 
  "nat"     = expression(Natural),
  "vari"    = expression(ENSO)
#   "obsrv_mean"  =  expression(Observation~mean)

)

# label definition for plotting
cor_tx_ems_mapping71 <- c(
#   "scm_tatm_mean_qnt"   =  expression(Total~warming),
  "co2a"    = expression(CO[2]),
  "ch4"     = expression(CH[4]), 
  "n2o"     = expression(N[2]*O), 
  "fgs"     = expression(Halogenated), 
  "aero"    = expression(Aerosols), 
  "lcc"     = expression(Land~albedo), 
  "nat"     = expression(Natural),
  "vari"    = expression(ENSO),
  "resi"    = expression(Residual),
  "est"    = expression(Total~sum),
  "obs"     = expression("HadCRUTv5.0"),
  "total"   = expression(Total)

)

# label definition for plotting
cor_tx_ems_mapping72 <- c(
#   "scm_tatm_mean_qnt"   =  expression(Total~warming),
  "co2a"    = "CO[2]",
  "ch4"     = "CH[4]", 
  "n2o"     = "N[2]*O", 
  "fgs"     = "Halogenated", 
  "aero"    = "Aerosols", 
  "lcc"     = "Land~albedo", 
  "nat"     = "Natural",
  "vari"    = "ENSO",
  "resi"    = "Residual",
#   "est"    = "Total~sum",
#   "obs"     = "HadCRUTv5.0",
  "total"   = "Total"

)

# line type definition for plotting
cor_lt_ems_mapping7 <- c(
#   "scm_tatm_mean_qnt"   = "longdash",
  "co2a"    = "solid", 
  "n2o"     = "solid", 
  "aero"    = "solid", 
  "lcc"     = "solid", 
  "nat"     = "solid",
  "vari"    = "solid"
#   "obsrv_mean"  = "longdash"
)

# colour definition for plotting
cor_cl_tem_mapping8 <- c(
    "co2a"        = cl_sl_co2a,
    "nco2_anthro" = cl_sl_ch4,
    "nat"         = cl_sl_volc,
    "vari"        = cl_sl_vari,
    "obsrv_mean"  = "grey40"
)

# line type definition for plotting
cor_lt_tem_mapping8 <- c(
    "co2a"        = "solid",
    "nco2_anthro" = "solid",
    "nat"         = "solid",
    "vari"        = "solid",
    "obsrv_mean"  = "solid"
)

# label definition for plotting
cor_tx_tem_mapping8 <- c(
    "co2a"              = expression(CO[2]~warming),
    "nco2_anthro"       = expression(Non*"-"*CO[2]~anthropogenic),
    "nat"               = expression(Natural),
    "obsrv_mean"        = expression(Observation~mean),
    "vari"              = expression(ENSO)
)

# fill colour definition for plotting
cor_fl_tem_mapping9 <- c(
    "co2a"        = cl_sl_co2a,
    # "nco2_anthro" = cl_sc_41,
    "nco2" = cl_sl_ch4
    # "nat"         = cl_sc_51
    # "vari"        = cl_sc_11
)

# colour definition for plotting
cor_cl_tem_mapping9 <- c(
    "co2a"        = cl_sl_co2a,
    # "nco2_anthro" = cl_sc_4,
    "nco2" = cl_sl_ch4
    # "nat"         = cl_sc_5
    # "vari"        = cl_sc_1
)

# label definition for plotting
cor_tx_tem_mapping9 <- c(
    "co2a"              = expression(CO[2]~warming),
    "nco2"       = expression(Non*"-"*CO[2]~anthropogenic~"+"~Natural)
    # "nco2_anthro"       = expression(Non*"-"*CO[2]~anthropogenic),
    # "nat"               = expression(Natural)
    # "vari"              = expression(Internal~variability)
)

# fill colour definition for plotting
cor_fl_tem_mapping10 <- c(
    # "co2a"        = cl_sc_31,
    # "nco2_anthro" = cl_sc_41,
    # "nco2" = cl_sc_41
    # "nat"         = cl_sc_51
    "obsrv_mean"  = "grey70",
    "vari"        = cl_sl_vari
)

# colour definition for plotting
cor_cl_tem_mapping10 <- c(
    # "co2a"        = cl_sc_3,
    # "nco2_anthro" = cl_sc_4,
    # "nco2" = cl_sc_4
    # "nat"         = cl_sc_5
    "obsrv_mean"  = "grey50",
    "vari"        = cl_sl_vari
)

# label definition for plotting
cor_tx_tem_mapping10 <- c(
    # "co2a"              = expression(CO[2]~warming),
    # "nco2"       = expression(Non*"-"*CO[2]~anthropogenic~"+"~Natural)
    # "nco2_anthro"       = expression(Non*"-"*CO[2]~anthropogenic),
    # "nat"               = expression(Natural)
    "obsrv_mean"  =  expression(Observation~mean),
    "vari"              = expression(ENSO)
)

# colour definition for plotting
cor_cl_ems_mapping11 <- c(
  "co2a"    = cl_sl_co2a, 
  "ch4"     = cl_sl_ch4, 
  "n2o"     = cl_sl_n2o, 
  "hfc"     = cl_sl_hfc, 
  "pfc"     = cl_sl_pfc, 
  "ods"     = cl_sl_ods, 
  "aero"    = cl_sl_aero, 
  "lcc"     = cl_sl_lcc,
  "nat"     = cl_sl_volc
)

# colour definition for plotting
cor_cl_ems_mapping111 <- c(
  "co2a"    = cl_sc_3, 
  "ch4"     = cl_sc_31, 
  "n2o"     = cl_sc_2, 
  "ods"     = cl_sc_21, 
  "ofgs"     = cl_sc_1, 
  "aero"    = cl_sc_11, 
  "lcc"     = cl_sc_5,
  "solar"    = cl_sc_51,
  "volc"     = cl_sc_4,
  "vari"     = cl_sc_41,
  "resi"     = "gold4",
  "obs"     = "grey60",
  "est"     = cl_sc_6

)

# colour definition for plotting
cor_cl_ems_mapping112 <- c(
  "co2a"    = cl_sc_3, 
  "ch4"     = cl_sc_31, 
  "n2o"     = cl_sc_2, 
  "ods"     = cl_sc_21, 
  "ofgs"     = cl_sc_1, 
  "aero"    = cl_sc_11, 
  "lcc"     = cl_sc_5,
  "solar"    = cl_sc_51,
  "volc"     = cl_sc_4,
  "mei"     = brewer.pal(8, "Pastel2")[1],
  "amo"     = brewer.pal(8, "Pastel2")[2],
  "pdo"     = brewer.pal(8, "Pastel2")[3],
  "nao"     = brewer.pal(8, "Pastel2")[4],
  "dmi"     = brewer.pal(8, "Pastel2")[5],
  "resi"     = "gold4",
  "obs"     = "grey60",
  "est"     = cl_sc_6
)

# line type definition for plotting
cor_lt_ems_mapping111 <- c(
  "co2a"    = 1, 
  "ch4"     = 1, 
  "n2o"     = 1, 
  "ods"     = 1, 
  "ofgs"     = 1, 
  "aero"    = 1, 
  "lcc"     = 1,
  "solar"    = 1,
  "volc"     = 1,
  "vari"     = 1,
  "resi"     = 1,
  "obs"     = 1,
  "est"     = 1
)

# line type definition for plotting
cor_lt_ems_mapping112 <- c(
  "co2a"    = 1, 
  "ch4"     = 1, 
  "n2o"     = 1, 
  "ods"     = 1, 
  "ofgs"     = 1, 
  "aero"    = 1, 
  "lcc"     = 1,
  "solar"    = 1,
  "volc"     = 1,
  "mei"     = 1,
  "amo"     = 1,
  "pdo"     = 1,
  "nao"     = 1,
  "dmi"     = 1,
  "resi"     = 1,
  "obs"     = 1,
  "est"     = 1
)

# label definition for plotting
cor_tx_ems_mapping11 <- c(
  "co2a"    = expression(CO[2]), 
  "ch4"     = expression(CH[4]), 
  "n2o"     = expression(N[2]*O), 
  "fgs"     = expression(Halogenated), 
  "aero"    = expression(Aerosols), 
  "lcc"     = expression(Land~albedo),
  "nat"     = expression(Natural)

)

# label definition for plotting
cor_tx_ems_mapping12 <- c(
  "co2a"    = expression(CO[2]), 
  "ch4"     = expression(CH[4]), 
  "n2o"     = expression(N[2]*O), 
  "ods"     = expression(ODS), 
  "ofgs"     = Other~fluorinated, 
  "aero"    = expression(Aerosols), 
  "lcc"     = expression(Land~albedo),
  "solar" = expression(Solar~irradiance),
  "volc"     = expression(Volcanic),
  "vari"     = expression(ENSO),
  "obs"      = expression(Observation)
)

# label definition for plotting
cor_tx_ems_mapping121 <- c(
  "co2a"    = "bold(a)~CO[2]", 
  "ch4"     = "bold(b)~CH[4]", 
  "n2o"     = "bold(c)~N[2]*O", 
  "ods"     = "bold(d)~ODS", 
  "ofgs"     = "bold(e)~Other~fluorinated", 
  "aero"    = "bold(f)~Aerosols", 
  "lcc"     = "bold(g)~Land~albedo",
  "solar"   = "bold(h)~Solar~irradiance",
  "volc"    = "bold(i)~Volcanic",
  "vari"    = "bold(j)~ENSO",
  "obs"     = "bold(k)~HadCRUTv5.0"
)

# label definition for plotting
cor_tx_ems_mapping122 <- c(
  "co2a"    = "bold(a)~CO[2]", 
  "ch4"     = "bold(b)~CH[4]", 
  "n2o"     = "bold(c)~N[2]*O", 
  "ods"     = "bold(d)~ODS", 
  "ofgs"     = "bold(e)~Other~fluorinated", 
  "aero"    = "bold(f)~Aerosols", 
  "lcc"     = "bold(g)~Land~albedo",
  "solar"   = "bold(h)~Solar~irradiance",
  "volc"    = "bold(i)~Volcanic",
  "vari"    = "bold(j)~ENSO",
  "resi"    = "bold(k)~Residual",
  "est"    = "bold(k)~Total~sum",
  "obs"     = "bold(l)~HadCRUTv5.0",
  "total"    = "bold(l)~Total~sum"
)

# label definition for plotting
cor_tx_ems_mapping123 <- c(
  "co2a"    = "bold(a)~CO[2]", 
  "ch4"     = "bold(b)~CH[4]", 
  "n2o"     = "bold(c)~N[2]*O", 
  "ods"     = "bold(d)~ODS", 
  "ofgs"     = "bold(e)~Other~fluorinated", 
  "aero"    = "bold(f)~Aerosols", 
  "lcc"     = "bold(g)~Land~albedo",
  "solar"   = "bold(h)~Solar~irradiance",
  "volc"    = "bold(i)~Volcanic",
  "mei"    = "bold(j)~MEI",
  "amo"    = "bold(k)~AMO",
  "pdo"    = "bold(l)~PDO",
  "nao"    = "bold(m)~NAO",
  "dmi"    = "bold(n)~DMI",
  "resi"    = "bold(o)~Residual",
  "est"    = "bold(p)~Total~sum",
  "obs"     = "bold(q)~HadCRUTv5.0",
  "total"    = "bold(q)~Total~sum"
)

# label definition for plotting
cor_tx_grp_mapping122 <- c(
  "ghg"    = "bold(a)~GHG", 
  "othanthro" = "bold(b)~Other~anthropogenic", 
  "nat"     = "bold(c)~Natural~forcings", 
  "vari"    = "bold(d)~ENSO",
  "resi"    = "bold(e)~Residual",
  "est"    = "bold(f)~Total~sum",
  "obs"     = "bold(g)~HadCRUTv5.0",
  "total"    = "bold(g)~Total~sum"
)

# colour definition for plotting
cor_cl_group_mapping111 <- c(
  "ghg"    = cl_sc_3, 
  "othanthro"    = cl_sc_11, 
  "nat"    = cl_sc_51,
  "vari"     = cl_sc_41,
  "resi"     = "gold4",
  "obs"     = "grey60",
  "est"     = cl_sc_6

)

# colour definition for plotting
cor_cl_grp_mapping111 <- c(
  "ghg"    = cl_sc_3, 
  "othanthro"    = cl_sc_11, 
  "nat"    = cl_sc_51,
  "vari"     = cl_sc_41,
  "resi"     = "gold4",
  "obs"     = "grey60",
  "est"     = cl_sc_6

)

# line type definition for plotting
cor_lt_grp_mapping111 <- c(
  "ghg"    = 1, 
  "othanthro"    = 1, 
  "nat"    = 1,
  "vari"     = 1,
  "resi"     = 1,
  "obs"     = 1,
  "est"     = 1
)


# label definition for plotting
cor_tx_grp_mapping72 <- c(
#   "scm_tatm_mean_qnt"   =  expression(Total~warming),
  "ghg"    = "GHG", 
  "othanthro" = "Other~anthropogenic", 
  "nat"     = "Natural~forcings", 
  "vari"    = "ENSO",
  "resi"    = "Residual",
#   "est"    = "Total~sum",
#   "obs"     = "HadCRUTv5.0",
  "total"   = "Total"

)

# label definition for plotting
cor_tx_ems_mapping1220 <- c(
  "co2a"    = "CO[2]", 
  "ch4"     = "CH[4]", 
  "n2o"     = "N[2]*O", 
  "ods"     = "ODS", 
  "ofgs"     = "Other~fluorinated", 
  "aero"    = "Aerosols", 
  "lcc"     = "Land~albedo",
  "solar"   = "Solar~irradiance",
  "volc"    = "Volcanic",
  "vari"    = "ENSO",
  "est"    = "Total~sum",
  "obs"     = "HadCRUTv5.0"
)
# trunk/dat/CMIP6/CMIP6_GAS.csv
# halogenated gases efficiency (CMIP6)
cm6_effi <- c(
    "ccl4"	=    	0.17,
    "cfc11"	=    	0.26,
    "cfc12"	=    	0.32,
    "cfc13"	=    	0.25,
    "cfc113"	=    	0.3,
    "cfc114"	=    	0.31,
    "cfc115"	=    	0.2,
    "ch2cl2"	=    	0.03,
    # Hodnebrog et al., 2020: Updated global warming potentials and radiative efficiencies of halocarbons and other weak atmospheric absorbers.
    # https://echa.europa.eu/documents/10162/733515ca-7d61-463c-9cde-af560097ce25
    # Tetrachloroethene is also known as tetrachloroethylene and perchloroethylene and is commonly abbreviated to PCE or Perc.
    # Tetrachloroethylene is not thought to contribute significantly to tropospheric ozone formation. Its potential as an ozone depletor in the stratosphere is thought to be significantly less than other ozone depleting chemicals.
    "ccl2ccl2"  =   0.05,
    "ch3br"	=    	0.004,
    "ch3ccl3"	=    	0.07,
    "ch3cl"	=    	0.01,
    "chcl3"	=    	0.08,
    "halon1211"	=    	0.29,
    "halon1301"	=    	0.3,
    "halon2402"	=    	0.31,
    "hcfc22"	=    	0.21,
    "hcfc141b"	=    	0.16,
    "hcfc142b"	=    	0.19,
    "c2f6"	=    	0.25,
    "c3f8"	=    	0.28,
    "c4f10"	=    	0.36,
    "c5f12"	=    	0.41,
    "c6f14"	=    	0.44,
    "c7f16"	=    	0.5,
    "c8f18"	=    	0.55,
    "cc4f8"	=    	0.32,
    "cf4"	=    	0.09,
    "hfc23"	=    	0.18,
    "hfc32"	=    	0.11,
    "hfc4310mee"	=    	0.42,
    "hfc125"	=    	0.23,
    "hfc134a"	=    	0.16,
    "hfc143a"	=    	0.16,
    "hfc152a"	=    	0.1,
    "hfc227ea"	=    	0.26,
    "hfc236fa"	=    	0.24,
    "hfc245fa"	=    	0.24,
    "hfc365mfc"	=    	0.22,
    "nf3"	=    	0.2,
    "sf6"	=    	0.57,
    "so2f2"	=    	0.2
)

# halogenated gases efficiency (SCM)
scm_effi <- c(
    "hfc23"	=    0.18,
    "hfc32"	=    0.11,
    "hfc125" =	    0.23,
    "hfc134a" =	    0.16,
    "hfc143a" =	    0.16,
    "hfc152a" =	    0.1,
    "hfc227ea" =	0.26,
    "hfc236fa" =	0.24,
    "hfc245fa" =	0.24,
    "hfc365mfc" =	0.22,
    "hfc4310mee" =	0.42,
        	
    "hfc134" =      0.19,
    "hfc143" =     0.13,
    "hfc41"  =     0.02,

    "sf6" =	        0.57,
    "nf3" =	        0.2,
    "cf4" =	        0.09,
    "c2f6" =	    0.25,
    "c3f8" =    0.28,
    "cc4f8"	=    0.32,
    "c4f10"	=    0.36,
    "c5f12"	=    0.41,
    "c6f14"	=    0.44,
    # "c7f16"	=    0.5,
        	
    "cfc11"	=    0.26,
    "cfc12"	=    0.32,
    "cfc113" =	    0.3,
    "cfc114" =	    0.31,
    "cfc115" =	    0.2,
    "ccl4"	=    0.17,
    "ch3ccl3" =	    0.07,
    "hcfc22" =	    0.21,
    "hcfc141b"	=    0.16,
    "hcfc142b"	=    0.19,
    "halon1211"	= 0.29,
    "halon1202"	= 0.27,
    "halon1301"	= 0.3,
    "halon2402"	= 0.31,
    "ch3br"	    = 0.004,
    "ch3cl"	    = 0.01
)

# halogenated gases ODS (SCM)
scm_ods <- c(
    "cfc11",
    "cfc12",
    "cfc113",
    "cfc114",
    "cfc115",
    "ccl4",
    "ch3ccl3",
    "hcfc22",
    "hcfc141b",
    "hcfc142b",
    "halon1211",
    "halon1202",
    "halon1301",
    "halon2402",
    "ch3br",
    "ch3cl"
)

# halogenated gases ODS (CMIP6)
cm6_ods <- c(
    "cfc11",
    "cfc12",
    "cfc113",
    "cfc114",
    "cfc115",
    "hcfc22",
    "hcfc141b",
    "hcfc142b",
    "ch3ccl3",
    "ccl4",
    "ch3cl",
    "ch2cl2",
    "chcl3",
    "ch3br",
    "halon1211",
    "halon1301",
    "halon2402"
)

# halogenated gases ODS (NOAA)
noaa_ods <- c(
    "ccl4",
    "cfc11",
    "cfc113",
    "cfc12",
    "ch3br",
    "ch3ccl3",
    "h1211",
    "h1301",
    "h2402",
    "hcfc141b",
    "hcfc142b",
    "hcfc22",
    "hfc125",
    "hfc134a",
    "hfc143a",
    "hfc152a",
    "hfc32"
)

ods_all <- union(scm_ods,cm6_ods)
ods_all <- union(ods_all,noaa_ods)

# AGAGE gases (md)
agage_fgs_md <- c(
    "cfc11",
    "cfc12",
    "cfc113",
    "ch3ccl3",
    "ccl4",
    "chcl3"
)

# AGAGE gases (ms)
agage_fgs_ms <- c(
    "hfc23",
    "hfc32",
    "hfc125",
    "hfc134a",
    "hfc143a",
    "hfc152a",
    "hfc227ea",
    "hfc236fa",
    "hfc245fa",
    "hfc365mfc",
    "hfc4310mee",
    "hcfc22",
    "hcfc141b",
    "hcfc142b",
    "halon1211",
    "halon1301",
    "halon2402",
    "ch3br",
    "ch3cl",
    "ch2cl2",
    "ccl2ccl2",
    "chcl3",
    "ch3ccl3",
    "sf6",
    "so2f2",
    "nf3",
    "cf4",
    "c2f6",
    "c3f8",
    "cfc13",
    "cfc113",
    "cfc114",
    "cfc115"
)

src_lst_non_overlapping <- 
      c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_O3s", "rfc_O3t", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_BCsnow", "rfc_DUST", "rfc_cloud", "rfc_H2Os", "rfc_solar", "rfc_volc", "rfc_LCC", 
        all_fgs_lst[!all_fgs_lst%in%c("hfc134", "hfc143", "hfc41")])

# colour definition for emission types
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
cl_group_CH4     <- kelly_colors[8]
cl_group_CH4ox   <- kelly_colors[15]
cl_group_H2Os    <- kelly_colors[5]
cl_group_N2O     <- kelly_colors[3]
cl_group_F_Gases <- kelly_colors[16]
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

# colour definition for forcing agents
cl_rfc_mapping <- c(
    "rfc_CO2"     = cl_group_CO2,
    "rfc_CH4"     = cl_group_CH4,
    "rfc_N2O"     = cl_group_N2O,
    "rfc_FODS"     = cl_group_F_Gases,
    "rfc_FHFC"      = cl_group_NOX ,
    "rfc_FPFC"      = cl_group_POA  ,

    "rfc_O3s"     = cl_group_O3S,
    "rfc_O3t"     = cl_group_O3T,
    "rfc_H2Os"    = cl_group_H2Os,
    "rfc_LCC"     = cl_group_LandAlbedo,
    "rfc_BCsnow"  = cl_group_BCsnow,

    "r_aer"       = cl_group_SO2,
    "rfc_cloud"   = cl_group_cloud,
    "rfc_volc"    = cl_group_Natural,
    "rfc_solar"   = cl_group_CH4ox,
    "vari"        = cl_group_SOA, 
    "resi"        = cl_group_co2luc, 
    "obs"         = cl_emiss_neg,
    "est"       = cl_group_BC
    # 
)

# colour definition for forcing agents
cl_rfc_mapping2 <- c(
    "rfc_CO2"     = cl_group_CO2,
    "rfc_CH4"     = cl_group_CH4,
    "rfc_N2O"     = cl_group_N2O,
    "rfc_FODS"     = cl_group_F_Gases,
    "rfc_FHFC"      = cl_group_NOX ,
    "rfc_FPFC"      = cl_group_POA  ,

    "rfc_O3s"     = cl_group_O3S,
    "rfc_O3t"     = cl_group_O3T,
    "rfc_H2Os"    = cl_group_H2Os,
    "rfc_LCC"     = cl_group_LandAlbedo,
    "rfc_BCsnow"  = cl_group_BCsnow,

    "r_aer"       = cl_group_SO2,
    "rfc_cloud"   = cl_group_cloud,
    "rfc_volc"    = cl_group_Natural,
    "rfc_solar"   = cl_group_CH4ox,
    "vari"        = cl_group_SOA, 
    "mei"        = cl_group_SOA, 
    "amo"        = cl_group_DUST, 
    "resi"        = cl_group_co2luc, 
    "obs"         = cl_emiss_neg,
    "est"       = cl_group_BC
    # 
)

# label definition for forcing agents
tx_rfc_mapping <- c(
    "rfc_CO2"       = "bold(a)~CO[2]",           
    "rfc_CH4"       = "bold(b)~CH[4]", 
    "rfc_N2O"       = "bold(c)~N[2]*O", 
    "rfc_FODS"      = "bold(d)~ODS",
    "rfc_FHFC"      = "bold(e)~HFC",
    "rfc_FPFC"      = "bold(f)~PFC",
 
    "rfc_O3s"       = "bold(g)~Strat.~O[3]",  
    "rfc_O3t"       = "bold(h)~Tropo.~O[3]",  
    "rfc_H2Os"      = "bold(i)~Strat.~H[2]*O",   

    "rfc_LCC"       = "bold(j)~Land~albedo",
    "rfc_BCsnow"    = "bold(k)~BC~on~snow", 

    "r_aer"         = "bold(l)~Aerosols",
    "rfc_cloud"     = "bold(m)~Cloud",           
    "rfc_volc"      = "bold(n)~Volcanic",
    "rfc_solar"     = "bold(o)~Solar",
    "vari"          = "bold(p)~ENSO", 
    "resi"          = "bold(q)~Residual", 
    "total"         = "bold(l)~Total~sum"
    # "obs"           = "bold(r)~Total",
    # "est"           = "bold(s)~This~study"
            )

# label definition for forcing agents
tx_rfc_mapping1 <- c(
    "rfc_CO2"       = "CO[2]",           
    "rfc_CH4"       = "CH[4]", 
    "rfc_N2O"       = "N[2]*O", 
    "r_fgs"         = "Fluorinated",
    "rfc_FODS"      = "ODS",
    "rfc_FHFC"      = "HFC",
    "rfc_FPFC"      = "PFC",
 
    "r_o3"          = "Ozone",
    "rfc_O3s"       = "Strat.~O[3]",  
    "rfc_O3t"       = "Tropo.~O[3]",  
    "rfc_H2Os"      = "Strat.~H[2]*O",   

    "r_abd"         = "Albedo",
    "rfc_LCC"       = "Land~albedo",
    "rfc_BCsnow"    = "BC~on~snow", 

    "r_aer"         = "Aerosols",
    "rfc_cloud"     = "Cloud",     

    "r_nat"         = "Natural" , 
    "rfc_volc"      = "Volcanic",
    "rfc_solar"     = "Solar",
    "vari"          = "ENSO", 
    "resi"          = "Residual", 
    "total"         = "Total~sum"
    # "obs"           = "Total",
    # "est"         = "This~study"
            )

# label definition for forcing agents
tx_rfc_mapping3 <- c(
    "rfc_CO2"       = "bold(a)~CO[2]",           
    "rfc_CH4"       = "bold(b)~CH[4]", 
    "rfc_N2O"       = "bold(c)~N[2]*O", 
    "rfc_FODS"      = "bold(d)~ODS",
    "rfc_FHFC"      = "bold(e)~HFC",
    "rfc_FPFC"      = "bold(f)~PFC",
 
    "rfc_O3s"       = "bold(g)~Strat.~O[3]",  
    "rfc_O3t"       = "bold(h)~Tropo.~O[3]",  
    "rfc_H2Os"      = "bold(i)~Strat.~H[2]*O",   

    "rfc_LCC"       = "bold(j)~Land~albedo",
    "rfc_BCsnow"    = "bold(k)~BC~on~snow", 

    "r_aer"         = "bold(l)~Aerosols",
    "rfc_cloud"     = "bold(m)~Cloud",           
    "rfc_volc"      = "bold(n)~Volcanic",
    "rfc_solar"     = "bold(o)~Solar",
    "mei"          = "bold(p)~ENSO", 
    "amo"          = "bold(q)~AMO", 
    "resi"          = "bold(q)~Residual", 
    "total"         = "bold(r)~Total~sum"
    # "obs"           = "bold(r)~Total",
    # "est"           = "bold(s)~This~study"
            )

# group mapping for forcing agents
gp_rfc_mapping <- list(
    "r_co2" = c("rfc_CO2"), 
    "r_ch4" = c("rfc_CH4"), 
    "r_n2o" = c("rfc_N2O"), 
    "r_fgs" = c("rfc_FODS", "rfc_FHFC", "rfc_FPFC"), 
    "r_ofgs"= c("rfc_FHFC", "rfc_FPFC"),
    "r_o3"  = c("rfc_O3t", "rfc_O3s"), 
    "r_h2o" = c("rfc_H2Os"),
    "r_abd" = c("rfc_LCC","rfc_BCsnow"), 
    "r_aer" = c("rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST"), 
    "r_cld" = c("rfc_cloud"), 
    "r_aer_all" = c("rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST","rfc_cloud"), 
    "r_nat" = c("rfc_volc", "rfc_solar"), 
    "r_vari"= c("vari"), 
    "r_resi"= c("resi"), 
    "r_obs" = c("obs"),
    "r_est" = c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC", "rfc_O3s","rfc_O3t", "rfc_H2Os",  "rfc_LCC","rfc_BCsnow", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST", "rfc_cloud", "rfc_volc", "rfc_solar", "vari"),
   "r_anthro"=c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC","rfc_O3s","rfc_O3t", "rfc_H2Os",  "rfc_LCC","rfc_BCsnow", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST", "rfc_cloud"),
    "r_ghg" = c("rfc_CO2", "rfc_CH4", "rfc_N2O", "rfc_FODS", "rfc_FHFC", "rfc_FPFC"),
    "r_othanthro" = c("rfc_O3s","rfc_O3t", "rfc_H2Os",  "rfc_LCC","rfc_BCsnow", "rfc_SO4","rfc_NO3",  "rfc_POA", "rfc_SOA", "rfc_BC", "rfc_DUST", "rfc_cloud"),
    "rfc_CO2"     = c("rfc_CO2"),
    "rfc_CH4"     = c("rfc_CH4"),
    "rfc_H2Os"    = c("rfc_H2Os"),
    "rfc_N2O"     = c("rfc_N2O"),
    "rfc_FODS"    = c("rfc_FODS"),
    "rfc_FHFC"    = c("rfc_FHFC"),
    "rfc_FPFC"    = c("rfc_FPFC"),
    "rfc_O3s"     = c("rfc_O3s"),
    "rfc_O3t"     = c("rfc_O3t"),
    "rfc_SO4"     = c("rfc_SO4"),
    "rfc_NO3"     = c("rfc_NO3"),
    "rfc_POA"     = c("rfc_POA"),
    "rfc_SOA"     = c("rfc_SOA"),
    "rfc_BC"      = c("rfc_BC"),
    "rfc_BCsnow"  = c("rfc_BCsnow"),
    "rfc_cloud"   = c("rfc_cloud"),
    "rfc_LCC"     = c("rfc_LCC"),
    "rfc_DUST"    = c("rfc_DUST"),
    "rfc_solar"   = c("rfc_solar"),
    "rfc_volc"    = c("rfc_volc"),
    "vari"        = c("vari"), 
    "resi"        = c("resi"), 
    "obs"         = c("obs"),

    "hfc23"	    = c("hfc23"),
    "hfc32"	    =c("hfc32"),
    "hfc125"	  =c("hfc125"),
    "hfc134a"	  =c("hfc134a"),
    "hfc143a"	  =c("hfc143a"),
    "hfc152a"	  =c("hfc152a"),
    "hfc227ea"	  =c("hfc227ea"),
    "hfc236fa"	  =c("hfc236fa"),
    "hfc245fa"	  =c("hfc245fa"),
    "hfc365mfc"	  =c("hfc365mfc"),
    "hfc4310mee"	  =c("hfc4310mee"),
    "hfc134"	  =c("hfc134"), 
    "hfc143"	  =c("hfc143"),     
    "hfc41"	  =c("hfc41"),      
    "sf6"	  =c("sf6"),
    "nf3"	  =c("nf3"),
    "cf4"	  =c("cf4"),
    "c2f6"	  =c("c2f6"),
    "c3f8"	  =c("c3f8"),
    "cc4f8"	  =c("cc4f8"),
    "c4f10"	  =c("c4f10"),
    "c5f12"	  =c("c5f12"),
    "c6f14"	  =c("c6f14"),
    "cfc11"	  =c("cfc11"),
    "cfc12"	  =c("cfc12"),
    "cfc113"	  =c("cfc113"),
    "cfc114"	  =c("cfc114"),
    "cfc115"	  =c("cfc115"),
    "ccl4"	  =c("ccl4"),
    "ch3ccl3"	  =c("ch3ccl3"),
    "hcfc22"	  =c("hcfc22"),
    "hcfc141b"	  =c("hcfc141b"),
    "hcfc142b"	  =c("hcfc142b"),
    "halon1211"	  =c("halon1211"),
    "halon1202"	  =c("halon1202"),
    "halon1301"	  =c("halon1301"),
    "halon2402"	  =c("halon2402"),
    "ch3br"	  =c("ch3br"),
    "ch3cl"	  =c("ch3cl")
)

# label definition for ODS
tx_ods <- c(
    "cfc11"       = "CFC-11",
    "cfc12"       = "CFC-12",
    "cfc113"      = "CFC-113",
    "cfc114"      = "CFC-114",
    "cfc115"      = "CFC-115",
    "hcfc22"      = "HCFC-22",
    "hcfc141b"    = "HCFC-141*b",
    "hcfc142b"    = "HCFC-142*b",
    "ch3ccl3"     = "CH[3]*CCl[3]",
    "ccl4"        = "CCl[4]",
    "ch3cl"       = "CH[3]*Cl",
    "ch2cl2"      = "CH[2]*Cl[2]",
    "chcl3"       = "CHCl[3]",
    "ch3br"       = "CH[3]*Br",
    "halon1211"   = "Halon-1211",
    "halon1202"   = "Halon-1202",
    "halon1301"   = "Halon-1301",
    "halon2402"   = "Halon-2402"
)

# regression models
exp_lst <- 
  c(
    # "Ghg"="GHG",
    "Ant",
    # "GhgAer",
    # "AntNat",
    "AntSol",
    "AntVol",
    "AntVolSol",
    "AntVolSolDmi",
    "AntVolSolNao",
    "AntVolSolPdo",
    "AntVolSolMei",
    "AntVolSolAmo",
  ########
    # "AntVolSolPdoNao",
    # "AntVolSolPdoDmi",
    # "AntVolSolNaoDmi",
    
    # "AntVolSolPdoNaoDmi",
    # "AntVolSolMeiPdoDmi",
    # "AntVolSolMeiPdoNao",
    # "AntVolSolMeiNaoDmi",
    # "AntVolSolAmoNaoDmi",
    # "AntVolSolAmoPdoDmi",
    # "AntVolSolAmoPdoNao",

    # "AntVolSolMeiAmoPdoDmi",
    # "AntVolSolMeiAmoPdoNao",
    # "AntVolSolMeiAmoDmiNao",
    # "AntVolSolMeiPdoDmiNao",
    # "AntVolSolAmoPdoDmiNao",
  ########

    # "AntVolSolMeiPdo",
    # "AntVolSolMeiNao",
    # "AntVolSolMeiDmi",

    "AntVolSolAmoNao",
    "AntVolSolAmoDmi",
    "AntVolSolAmoPdo",
    "AntVolSolMeiAmo",
    "AntVolSolMeiAmoDmi", 
    "AntVolSolMeiAmoNao", 
    "AntVolSolMeiAmoPdo",
    "AntVolSolAll"
    )

# all regression models
  exp_all_lst <- 
  c(
    # "Ghg",
    "Vol",
    "Sol",
    "Mei",
    "Amo",
    "Dmi",
    "Nao",
    "Pdo",
    "Ant",
    # "GhgAer",
    "AntVol",
    "AntSol",
    # "AntNat",
    "AntVolSol",
    
    "AntVolSolDmi",
    "AntVolSolNao",
    "AntVolSolPdo",
    "AntVolSolMei",
    "AntVolSolAmo",
  ########

    "AntVolSolNaoDmi",
    "AntVolSolPdoDmi",
    "AntVolSolPdoNao",

    "AntVolSolMeiDmi",
    "AntVolSolMeiNao",
    "AntVolSolMeiPdo",

    "AntVolSolAmoDmi",
    "AntVolSolAmoNao",
    "AntVolSolAmoPdo",
    "AntVolSolMeiAmo",

    "AntVolSolPdoNaoDmi",
    "AntVolSolMeiPdoDmi",
    "AntVolSolMeiPdoNao",
    "AntVolSolMeiNaoDmi",
    "AntVolSolAmoNaoDmi",
    "AntVolSolAmoPdoDmi",
    "AntVolSolAmoPdoNao",
    "AntVolSolMeiAmoDmi", 
    "AntVolSolMeiAmoNao", 
    "AntVolSolMeiAmoPdo",

    "AntVolSolMeiPdoDmiNao",
    "AntVolSolAmoPdoDmiNao",
    "AntVolSolMeiAmoDmiNao",
    "AntVolSolMeiAmoPdoDmi",
    "AntVolSolMeiAmoPdoNao",
  ########

    "AntVolSolAll"
    )

# index definition
exp_index <- c(
  "APG"=0,
  "Sol"=100,
  "Vol"=200,
  "DMI"=300,
  "NAO"=400,
  "PDO"=500,
  "CTI"=600,
  "AMO"=700
)

# label definition for regression models
exp_lbl_lst <- 
  c(
    "Ant"="APG",
    "AntSol"="Sol",
    "AntVol"="Vol",
    "AntVolSolDmi"="DMI",
    "AntVolSolNao"="NAO",
    "AntVolSolPdo"="PDO",
    "AntVolSolMei"="CTI",
    "AntVolSolAmo"="AMO"
  )

# label definition for regression models
  exp_all_txt1 <- 
  c(
    # "Ghg"="GHG",
    "Ant"="APG",
    # "GhgAer"="GHG+OtherAPG",
    # "AntNat"="APG+Nat(ural)",
    "AntVol"="APG+Vol",
    "AntSol"="APG+Sol",
    "AntVolSol"="APG+Vol+Sol",
    "AntVolSolMei"="APG+Vol+Sol+CTI",
    "AntVolSolAmo"="APG+Vol+Sol+AMO",
    "AntVolSolPdo"="APG+Vol+Sol+PDO",
    "AntVolSolNao"="APG+Vol+Sol+NAO",
    "AntVolSolDmi"="APG+Vol+Sol+DMI",

    "AntVolSolPdoNao"="APG+Vol+Sol+PDO+NAO",
    "AntVolSolPdoDmi"="APG+Vol+Sol+PDO+DMI",
    "AntVolSolNaoDmi"="APG+Vol+Sol+NAO+DMI",

    "AntVolSolPdoNaoDmi"="APG+Vol+Sol+PDO+DMI+NAO",
    "AntVolSolMeiPdoDmi"="APG+Vol+Sol+CTI+PDO+DMI",
    "AntVolSolMeiPdoNao"="APG+Vol+Sol+CTI+PDO+NAO",
    "AntVolSolMeiNaoDmi"="APG+Vol+Sol+CTI+NAO+DMI",
    "AntVolSolAmoNaoDmi"="APG+Vol+Sol+AMO+DMI+NAO",
    "AntVolSolAmoPdoDmi"="APG+Vol+Sol+AMO+PDO+DMI",
    "AntVolSolAmoPdoNao"="APG+Vol+Sol+AMO+PDO+NAO",

    "AntVolSolMeiAmoPdoDmi"="APG+Vol+Sol+AMO+CTI+PDO+DMI",
    "AntVolSolMeiAmoPdoNao"="APG+Vol+Sol+AMO+CTI+PDO+NAO",
    "AntVolSolMeiAmoDmiNao"="APG+Vol+Sol+AMO+CTI+DMI+NAO",
    "AntVolSolMeiPdoDmiNao"="APG+Vol+Sol+CTI+PDO+DMI+NAO",
    "AntVolSolAmoPdoDmiNao"="APG+Vol+Sol+AMO+PDO+DMI+NAO",
  ########
    "AntVolSolMeiAmo"="APG+Vol+Sol+AMO+CTI",
    "AntVolSolMeiPdo"="APG+Vol+Sol+CTI+PDO",
    "AntVolSolMeiNao"="APG+Vol+Sol+CTI+NAO",
    "AntVolSolMeiDmi"="APG+Vol+Sol+CTI+DMI",
    "AntVolSolAmoPdo"="APG+Vol+Sol+AMO+PDO",
    "AntVolSolAmoDmi"="APG+Vol+Sol+AMO+DMI",
    "AntVolSolAmoNao"="APG+Vol+Sol+AMO+NAO",
    "AntVolSolMeiAmoDmi"="APG+Vol+Sol+AMO+CTI+DMI", 
    "AntVolSolMeiAmoNao"="APG+Vol+Sol+AMO+CTI+NAO", 
    "AntVolSolMeiAmoPdo"="APG+Vol+Sol+AMO+CTI+PDO",
    "AntVolSolAll"="APG+Vol+Sol+AMO+CTI+PDO+DMI+NAO"
    )

# label definition for regression models
  exp_all_txt <- 
  c(
    "Vol" = "Vol",
    "Sol" = "Sol",
    "Mei" = "CTI",
    "Amo" = "AMO",
    "Dmi" = "DMI",
    "Nao" = "NAO",
    "Pdo" = "PDO",
    # "Ghg"="GHG",
    "Ant"="APG",
    # "GhgAer"="GHG+OtherAPG",
    # "AntNat"="APG+Nat(ural)",
    "AntVol"="APG+Vol",
    "AntSol"="APG+Sol",
    "AntVolSol"="APG+Vol+Sol",
    "AntVolSolMei"="APG+Vol+Sol+CTI",
    "AntVolSolAmo"="APG+Vol+Sol+AMO",
    "AntVolSolPdo"="APG+Vol+Sol+PDO",
    "AntVolSolNao"="APG+Vol+Sol+NAO",
    "AntVolSolDmi"="APG+Vol+Sol+DMI",

    "AntVolSolPdoNao"="APG+Vol+Sol+PDO+NAO",
    "AntVolSolPdoDmi"="APG+Vol+Sol+PDO+DMI",
    "AntVolSolNaoDmi"="APG+Vol+Sol+NAO+DMI",

    "AntVolSolPdoNaoDmi"="APG+Vol+Sol+PDO+DMI+NAO",
    "AntVolSolMeiPdoDmi"="APG+Vol+Sol+CTI+PDO+DMI",
    "AntVolSolMeiPdoNao"="APG+Vol+Sol+CTI+PDO+NAO",
    "AntVolSolMeiNaoDmi"="APG+Vol+Sol+CTI+NAO+DMI",
    "AntVolSolAmoNaoDmi"="APG+Vol+Sol+AMO+DMI+NAO",
    "AntVolSolAmoPdoDmi"="APG+Vol+Sol+AMO+PDO+DMI",
    "AntVolSolAmoPdoNao"="APG+Vol+Sol+AMO+PDO+NAO",

    "AntVolSolMeiAmoPdoDmi"="APG+Vol+Sol+AMO+CTI+PDO+DMI",
    "AntVolSolMeiAmoPdoNao"="APG+Vol+Sol+AMO+CTI+PDO+NAO",
    "AntVolSolMeiAmoDmiNao"="APG+Vol+Sol+AMO+CTI+DMI+NAO",
    "AntVolSolMeiPdoDmiNao"="APG+Vol+Sol+CTI+PDO+DMI+NAO",
    "AntVolSolAmoPdoDmiNao"="APG+Vol+Sol+AMO+PDO+DMI+NAO",
  ########
    "AntVolSolMeiAmo"="APG+Vol+Sol+AMO+CTI",
    "AntVolSolMeiPdo"="APG+Vol+Sol+CTI+PDO",
    "AntVolSolMeiNao"="APG+Vol+Sol+CTI+NAO",
    "AntVolSolMeiDmi"="APG+Vol+Sol+CTI+DMI",
    "AntVolSolAmoPdo"="APG+Vol+Sol+AMO+PDO",
    "AntVolSolAmoDmi"="APG+Vol+Sol+AMO+DMI",
    "AntVolSolAmoNao"="APG+Vol+Sol+AMO+NAO",
    "AntVolSolMeiAmoDmi"="APG+Vol+Sol+AMO+CTI+DMI", 
    "AntVolSolMeiAmoNao"="APG+Vol+Sol+AMO+CTI+NAO", 
    "AntVolSolMeiAmoPdo"="APG+Vol+Sol+AMO+CTI+PDO",
    "AntVolSolAll"="APG+Vol+Sol+AMO+CTI+PDO+DMI+NAO"
    )

# digit definition for regression coefficients
coef_digit <- c(
  "sigma" = 2,
  "r.squared" = 2,
  "estr" = 2,
  "AIC" = 1,
  "BIC" = 1,
  "(Intercept)" = 2,
  "r_ghg" = 2,
  "r_othanthro" = 2,
  "r_nat" = 2,
  "r_anthro" = 2,
  "rfc_volc"  = 2,
  "rfc_solar" = 2,
  "mei0"  = 3,
  "amo0"  = 3,
  "pdo0"  = 3,
  "nao0"  = 3,
  "dmi0"  = 3
)

# calculate the difference of a matrix
get_diff_col_mtx <- function(src_mtx){
    in_mtx <- src_mtx[,-c(1)]
    ncl <- ncol(in_mtx)
    diff_mtx <- apply(in_mtx, 2, diff)
    diff_mtx <- rbind(rep(0,ncl),diff_mtx)
    diff_mtx <- data.frame(year=src_mtx$year, diff_mtx)
    return(diff_mtx)
}

# calculate the confidence interval
# x: vector
# return: confidence interval and median
get_ci <- function(x){
    # Calculate the mean and standard error
    l.model <- lm(x ~ 1)

    # Calculate the 95% confidence interval
    dst_vt <- c(confint(l.model, level=0.95),quantile(x, probs=c(0.5))) 
    colnames(dst_vt) <- c("lwr","upr","med")
    return(dst_vt)
}

# calculate mean, sd and confidence interval
# src_mtx: source matrix
# no_cores: number of cores used in the parallel processing
get_qnt_par_mtx <- function(src_mtx, no_cores = 5){
    quantfun <- function(x)  {quantile(x, probs=prob)}
    # meanfun <- function(x) {mean(x)}
    # sdfun <- function(x) {sd(x)}

    # meanfun_dif <- function(x) {mean(x)}
    # sdfun_dif <- function(x) {sd(x)}

    # print("diff")
    # nsample <- nrow(src_mtx)
    # clust <- makeCluster(no_cores)
    # clusterExport(clust, NULL)
    # diff_mtx <- parApply(clust, src_mtx, 1, diff) 
    # diff_mtx <- data.frame(a=rep(0,nsample), t(diff_mtx))
    # stopCluster(clust)    

    # print("diff2")
    # clust <- makeCluster(no_cores)
    # clusterExport(clust, NULL)
    # diff2_mtx <- parApply(clust, diff_mtx, 1, diff) 
    # diff2_mtx <- data.frame(a=rep(0,nsample), t(diff2_mtx))
    # colnames(diff2_mtx) <- colnames(diff_mtx)
    # stopCluster(clust)

    print("Calculate mean, sd and confidence interval.")
    clust <- makeCluster(no_cores)
    clusterExport(clust, NULL)
    mean_vt <- parApply(clust, src_mtx, 2, mean) 
    sd_vt <- parApply(clust, src_mtx, 2, sd) 
    stopCluster(clust)

    clust <- makeCluster(par_cores_no)
    prob <- c(0.025,0.5,0.975)
    clusterExport(clust, "prob",envir = environment())
    qnt_vt <- parApply(clust, src_mtx, 2, quantfun)
    stopCluster(clust)
    
    # print("mean_dif")
    # clust <- makeCluster(no_cores)
    # clusterExport(clust, NULL)
    # dif_mean_vt <- parApply(clust, diff_mtx, 2, mean) 
    # dif_sd_vt <- parApply(clust, diff_mtx, 2, sd) 
    # stopCluster(clust)

    # print("mean_dif2")
    # clust <- makeCluster(no_cores)
    # clusterExport(clust, NULL)
    # dif2_mean_vt <- parApply(clust, diff2_mtx, 2, mean) 
    # dif2_sd_vt <- parApply(clust, diff2_mtx, 2, sd) 
    # stopCluster(clust)

    # merge_vt <- data.frame(mean=(mean_vt),sd=(sd_vt), dif_mean=(dif_mean_vt), dif_sd=(dif_sd_vt), dif2_mean=(dif2_mean_vt), dif2_sd=(dif2_sd_vt))

    # define the result matrix
    merge_vt <- data.frame(t(qnt_vt), mean=(mean_vt),sd=(sd_vt))

    rslt <- data.frame(year=as.numeric(rownames(merge_vt)), merge_vt)
    # colnames(rslt) <- c("year", "mean","sd", "dif_mean", "dif_sd","dif2_mean","dif2_sd")
    colnames(rslt) <- c("year", "p025","p500","p975", "mean","sd")

    # return the result
    return(rslt)
}

# calculate mean, sd and quantile parameters
# src_mtx: source matrix
get_qnt_mtx <- function(src_mtx){

    # define the quantile function
    quantfun <- function(x,prob)  {quantile(x, probs=prob)}

    # define the mean function
    meanfun <- function(x) {mean(x)}

    # define the sd function
    sdfun <- function(x) {sd(x)}

    # calculate the mean
    meanfun_dif <- function(x) {mean(x)}

    # calculate the sd
    sdfun_dif <- function(x) {sd(x)}

    # calculate the lagged differences
    diff_mtx <- apply(src_mtx, 1, diff)
    diff_mtx <- data.frame(a=rep(0,nsample), t(diff_mtx))
    colnames(diff_mtx) <- colnames(src_mtx)

    # calculate the lagged differences
    diff2_mtx <- apply(diff_mtx, 1, diff)
    diff2_mtx <- data.frame(a=rep(0,nsample), t(diff2_mtx))
    colnames(diff2_mtx) <- colnames(diff_mtx)

    # calculate the quantile
    qnt_vt <- apply(src_mtx, 2, quantfun, prob=c(0.05, 0.95, .17, .83, 0.25, 0.75, 0.34, 0.66, 0.5)) 

    # calculate the mean and sd
    mean_vt <- apply(src_mtx, 2, meanfun) 
    sd_vt <- apply(src_mtx, 2, sdfun) 

    # calculate the mean and sd
    dif_mean_vt <- apply(diff_mtx, 2, meanfun_dif) 
    dif_sd_vt <- apply(diff_mtx, 2, sdfun_dif) 

    # calculate the mean and sd
    dif2_mean_vt <- apply(diff2_mtx, 2, meanfun_dif) 
    dif2_sd_vt <- apply(diff2_mtx, 2, sdfun_dif) 

    # merge the results
    merge_vt <- data.frame(t(qnt_vt), mean=(mean_vt),sd=(sd_vt), dif_mean=(dif_mean_vt), dif_sd=(dif_sd_vt), dif2_mean=(dif2_mean_vt), dif2_sd=(dif2_sd_vt))

    rslt <- data.frame(year=as.numeric(rownames(merge_vt)), merge_vt)
    colnames(rslt) <- c("year", "p5", "p95", "p17", "p83", "p25", "p75", "p34", "p66", "p50", "mean","sd", "dif_mean", "dif_sd","dif2_mean","dif2_sd")

    # return the result
    return(rslt)
}