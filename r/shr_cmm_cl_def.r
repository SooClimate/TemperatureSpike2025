library(rcartocolor)

# define the colors for the different groups, use color blind friendly palette
cl_group_CO2     <- carto_pal(12, "Safe")[10]
cl_group_CH4     <- carto_pal(12, "Safe")[9]
cl_group_CH4ox   <- carto_pal(12, "Safe")[8]
cl_group_H2Os    <- carto_pal(12, "Safe")[1]
cl_group_N2O     <- carto_pal(12, "Safe")[2]
cl_group_F_Gases <- carto_pal(12, "Safe")[3]
cl_group_O3S     <- carto_pal(12, "Safe")[4]
cl_group_O3T     <- carto_pal(7, "Earth")[1]
cl_group_SO2     <- carto_pal(12, "Safe")[6]
cl_group_NOX     <- carto_pal(12, "Safe")[7]
cl_group_POA     <- carto_pal(12, "Safe")[11]
cl_group_SOA     <- carto_pal(7, "Earth")[7]
cl_group_BC      <-  carto_pal(12, "Safe")[5]
cl_group_BCsnow  <- carto_pal(7, "Earth")[3]
cl_group_cloud   <- carto_pal(7, "Earth")[5]
cl_group_DUST       <- carto_pal(7, "Earth")[6]
cl_group_LandAlbedo <- carto_pal(7, "Earth")[4]
cl_group_Natural    <- carto_pal(7, "Earth")[2]

cl_emiss_neg = carto_pal(12, "Prism")[12]
# cl_emiss_luc = "#b35806"


# label definition for the plotting
tx_aero_mapping <- c(
    "bc"      = "a~BC",
    "co"      = "b~CO",
    "nh3"     = "c~NH[3]",
    "nox"     = "d~Nitrate",
    "so2"     = "e~Sulfate",
    "oc"      = "f~OC",
    "voc"     = "g~VOC",
    # "dust"    = "h~Dust",
    "aero"    = "atop(h~Total~aerosols,(emissions~induced))"
)

# color definition for the plotting
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

# color definition for the plotting
cl_rfc_mapping3 <- c(
    "rfc_CO2"     = cl_group_CO2,
    "rfc_CH4"     = cl_group_CH4,
    "rfc_N2O"     = cl_group_N2O,
    "r_fgs"     = cl_group_F_Gases,

    "r_o3"     = cl_group_O3S,
    "rfc_H2Os"    = cl_group_H2Os,
    "rfc_abd"     = cl_group_LandAlbedo,

    "r_aer"       = cl_group_SO2,
    "rfc_cloud"   = cl_group_cloud,
    "rfc_volc"    = cl_group_Natural,
    "rfc_solar"   = cl_group_CH4ox,
    # "vari"        = cl_group_SOA, 
    "mei"        = cl_group_SOA, 
    "amo"        = cl_group_DUST, 
    # "resi"        = cl_group_co2luc, 
    "obs"         = cl_emiss_neg,
    "est"       = cl_group_BC
    # 
)

# color definition for the plotting
cl_rfc_mapping4 <- c(
    "r_ghg"     = cl_group_CO2,
    # "rfc_CH4"     = cl_group_CH4,
    # "rfc_N2O"     = cl_group_N2O,
    # "r_fgs"     = cl_group_F_Gases,

    # "r_o3"     = cl_group_O3S,
    # "rfc_H2Os"    = cl_group_H2Os,
    # "rfc_abd"     = cl_group_LandAlbedo,

    "r_othanthro"       = cl_group_SO2,
    # "rfc_cloud"   = cl_group_cloud,
    "rfc_volc"    = cl_group_Natural,
    "rfc_solar"   = cl_group_CH4ox,
    # "vari"        = cl_group_SOA, 
    "mei"        = cl_group_SOA, 
    "amo"        = cl_group_DUST, 
    # "resi"        = cl_group_co2luc, 
    "obs"         = cl_emiss_neg,
    "est"       = cl_group_BC
    # 
)

# color definition for the plotting
cl_rfc_mapping5 <- c(
    "obs"         = cl_emiss_neg,
    "est"       = cl_group_BC,
    "wos"       = cl_group_O3T
    # 
)

# color definition for the plotting
cl_rfc_mapping6 <- c(
    "r_anthro"     = cl_group_CO2,
    # "rfc_CH4"     = cl_group_CH4,
    # "rfc_N2O"     = cl_group_N2O,
    # "r_fgs"     = cl_group_F_Gases,

    # "r_o3"     = cl_group_O3S,
    # "rfc_H2Os"    = cl_group_H2Os,
    # "rfc_abd"     = cl_group_LandAlbedo,

    # "r_othanthro"       = cl_group_SO2,
    # "rfc_cloud"   = cl_group_cloud,
    "rfc_volc"    = cl_group_Natural,
    "rfc_solar"   = cl_group_CH4ox,
    # "vari"        = cl_group_SOA, 
    "mei"        = cl_group_SOA, 
    "amo"        = cl_group_DUST, 
    # "resi"        = cl_group_co2luc, 
    "obs"         = cl_emiss_neg,
    "est"       = cl_group_BC
    # 
)

# color definition for the plotting
cl_exp_index <- c(
  "APG"=cl_group_CO2,
  "Sol"=cl_group_CH4ox,
  "Vol"=cl_group_Natural,
  "DMI"=cl_group_O3S,
  "NAO"=cl_group_H2Os,
  "PDO"=cl_group_LandAlbedo,
  "CTI"=cl_group_SOA,
  "AMO"=cl_group_DUST
)

# label definition for the plotting
tx_rfc_mapping4 <- c(
    "rfc_CO2"       = "bold(a)~CO[2]",           
    "rfc_CH4"       = "bold(b)~CH[4]", 
    "rfc_N2O"       = "bold(c)~N[2]*O", 
	"r_fgs"         = "bold(d)~Halogenated",
	"r_o3"          = "bold(e)~Ozone",  
    "rfc_H2Os"      = "bold(f)~Strat.~H[2]*O",   
    "r_abd"         = "bold(g)~Land~use~'&'~BC~on~snow",
    "r_aer"         = "bold(h)~Aerosol*'-'*radiation",
    "rfc_cloud"     = "bold(i)~Aerosol*'-'*cloud",           
    "rfc_volc"      = "bold(j)~Volcanic",
    "rfc_solar"     = "bold(k)~Solar",
    "mei"           = "bold(l)~CTI", 
    "amo"           = "bold(m)~AMO",  
    "total"         = "bold(n)~Total~sum"
	)

# label definition for the plotting
tx_rfc_mapping5 <- c(
     "rfc_CO2"        = expression(CO[2]),           
     "rfc_CH4"        = expression(CH[4]), 
     "rfc_N2O"        = expression(N[2]*O), 
     "r_fgs"          = expression(Halogenated),
     "r_o3"           = expression(Ozone),  
     "rfc_H2Os"       = expression(Strat.~H[2]*O),   
     "r_abd"          = expression(Land~use~'&'~BC~on~snow),
     "r_aer"          = expression(Aerosol*'-'*radiation),
     "rfc_cloud"      = expression(Aerosol*'-'*cloud),           
     "r_nat"          = expression(Natural~forcing),
     "vari"           = expression(Internal~variability),
     "esti"           = expression(This~study),
     "obse"           = expression(Observation),
     "total"          = expression(Total~sum)
	)

# label definition for the plotting
tx_rfc_mapping6 <- c(
     rfc_CO2        = expression(CO[2]),           
     rfc_CH4        = expression(CH[4]), 
     rfc_N2O        = expression(N[2]*O), 
     r_fgs          = expression(Halogenated),
     r_o3           = expression(Ozone),  
     rfc_H2Os       = expression(Strat.~H[2]*O),   
     r_abd          = expression(Land~use~'&'~BC~on~snow),
     r_aer          = expression(Aerosol*'-'*radiation),
     rfc_cloud      = expression(Aerosol*'-'*cloud),           
     rfc_volc       = expression(Volcanic),
     rfc_solar      = expression(Solar),
     mei            = expression(CTI), 
     amo            = expression(AMO),  
     obs            = expression(Observation)
	)

# label definition for the plotting
tx_rfc_mapping7 <- c(
    "rfc_CO2"       = expression(CO[2]),           
    "rfc_CH4"       = expression(CH[4]), 
    "rfc_N2O"       = expression(N[2]*O), 
    # "r_fgs"         = expression(Fluorinated),
    "rfc_FODS"      = expression(ODS),
    "rfc_FHFC"      = expression(HFC),
    "rfc_FPFC"      = expression(PFC),
    # "r_o3"          = expression(Ozone),
    "rfc_O3s"       = expression(Strat.~O[3]),  
    "rfc_O3t"       = expression(Tropo.~O[3]),  
    "rfc_H2Os"      = expression(Strat.~H[2]*O),   
    # "r_abd"         = expression(Albedo),
    "rfc_LCC"       = expression(Land~albedo),
    "rfc_BCsnow"    = expression(BC~on~snow), 
    "r_aer"         = expression(Aerosols),
    "rfc_cloud"     = expression(Cloud),     
    # "r_nat"         = expression(Natural" , 
    "rfc_volc"      = expression(Volcanic),
    "rfc_solar"     = expression(Solar),
    # "vari"          = expression(Variability),
    "mei"           = expression(CTI), 
    "amo"           = expression(AMO),
    "obs"           = expression(Observation)
    # "resi"          = expression(Residual), 
    # "esti"          = expression(This~study),
    # "obse"          = expression(Observation)
    
)

# label definition for the plotting
tx_rfc_mapping8 <- c(
    "r_ghg"         = expression(Greenhouse~gases),
    "r_othanthro"   = expression(Other~human~drivers),           
    # "rfc_CH4"       = expression(CH[4]), 
    # "rfc_N2O"       = expression(N[2]*O), 
    # # "r_fgs"         = expression(Fluorinated),
    # "rfc_FODS"      = expression(ODS),
    # "rfc_FHFC"      = expression(HFC),
    # "rfc_FPFC"      = expression(PFC),
    # # "r_o3"          = expression(Ozone),
    # "rfc_O3s"       = expression(Strat.~O[3]),  
    # "rfc_O3t"       = expression(Tropo.~O[3]),  
    # "rfc_H2Os"      = expression(Strat.~H[2]*O),   
    # # "r_abd"         = expression(Albedo),
    # "rfc_LCC"       = expression(Land~albedo),
    # "rfc_BCsnow"    = expression(BC~on~snow), 
    # "r_aer"         = expression(Aerosols),
    # "rfc_cloud"     = expression(Cloud),     
    # "r_nat"         = expression(Natural" , 
    "rfc_volc"      = expression(Volcanic),
    "rfc_solar"     = expression(Solar),
    # "vari"          = expression(Variability),
    "mei"           = expression(CTI), 
    "amo"           = expression(AMO),
    # "obs"           = expression(Observation)
    # "resi"          = expression(Residual), 
    "est"          = expression(Total~sum)
    # "obse"          = expression(Observation)
    
)

# label definition for the plotting
tx_rfc_mapping9 <- c(
    "rfc_CO2"       = "bold(A)~CO[2]",           
    "rfc_CH4"       = "bold(B)~CH[4]", 
    "rfc_N2O"       = "bold(C)~N[2]*O", 
	"r_fgs"         = "bold(D)~Halogenated",
	"r_o3"          = "bold(E)~Ozone",  
    "rfc_H2Os"      = "bold(F)~Strat.~H[2]*O",   
    "r_abd"         = "bold(G)~Land~use~'&'~BC~on~snow",
    "r_aer"         = "bold(H)~Aerosol*'-'*radiation",
    "rfc_cloud"     = "bold(I)~Aerosol*'-'*cloud",           
    "rfc_volc"      = "bold(J)~Volcanic",
    "rfc_solar"     = "bold(K)~Solar",
    "mei"           = "bold(L)~CTI", 
    "amo"           = "bold(M)~AMO",  
    "est"         = "bold(N)~Total~sum",
    "obs"         = "bold(O)~Observation"
)

# label definition for the plotting
tx_rfc_mapping10 <- c(
    "r_ghg"         = "Greenhouse gases",
    "r_othanthro"   = "Other human drivers",
    "r_anthro"       = "Anthropogenic", 
    "rfc_CO2"       = "CO2",           
    "rfc_CH4"       = "CH4", 
    "rfc_N2O"       = "N2O", 
    "r_fgs"         = "Halogenated",
    "r_o3"          = "Ozone",  
    "rfc_H2Os"      = "Strat. H2O",   
    "r_abd"         = "Land use & BC on snow",
    "r_aer"         = "Aerosol-radiation",
    "rfc_cloud"     = "Aerosol-cloud",           
    "rfc_volc"      = "Volcanic",
    "rfc_solar"     = "Solar",
    "mei"           = "CTI", 
    "amo"           = "AMO",  
    "est"           = "Total sum",
    "obs"           = "Observation"
)

# label definition for the plotting
tx_rfc_mapping11 <- c(
    "r_anthro"         = expression(Anthropogenic),     
    # "rfc_CH4"       = expression(CH[4]), 
    # "rfc_N2O"       = expression(N[2]*O), 
    # # "r_fgs"         = expression(Fluorinated),
    # "rfc_FODS"      = expression(ODS),
    # "rfc_FHFC"      = expression(HFC),
    # "rfc_FPFC"      = expression(PFC),
    # # "r_o3"          = expression(Ozone),
    # "rfc_O3s"       = expression(Strat.~O[3]),  
    # "rfc_O3t"       = expression(Tropo.~O[3]),  
    # "rfc_H2Os"      = expression(Strat.~H[2]*O),   
    # # "r_abd"         = expression(Albedo),
    # "rfc_LCC"       = expression(Land~albedo),
    # "rfc_BCsnow"    = expression(BC~on~snow), 
    # "r_aer"         = expression(Aerosols),
    # "rfc_cloud"     = expression(Cloud),     
    # "r_nat"         = expression(Natural" , 
    "rfc_volc"      = expression(Volcanic),
    "rfc_solar"     = expression(Solar),
    # "vari"          = expression(Variability),
    "mei"           = expression(CTI), 
    "amo"           = expression(AMO),
    # "obs"           = expression(Observation)
    # "resi"          = expression(Residual), 
    "est"          = expression(Total~sum)
    # "obse"          = expression(Observation)
    
)

# color definition for the plotting
cl_aero_mapping <- c(
    "rfc_SO4" = cl_group_SO2,
    "rfc_NO3" = cl_group_NOX,
    "rfc_POA" = cl_group_POA,
    "rfc_SOA" = cl_group_SOA,
    "rfc_BC" = cl_group_BC,
    "rfc_cloud" = cl_group_cloud,
    "r_aer_all" = cl_group_DUST
)

# label definition for the plotting
tx_aero_mapping <- c(
    # "rfc_CO2"       = expression(CO[2]),           
    # "rfc_CH4"       = expression(CH[4]), 
    # "rfc_H2Os"      = expression(Strat.~H[2]*O),    
    # "rfc_N2O"       = expression(N[2]*O), 
    # "rfc_FHFC"      = expression(HFCs),
    # "rfc_FPFC"      = expression(PFCs),
    # "rfc_FODS"      = expression(ODSs),
    # "rfc_O3s"       = expression(Strat.~O[3]),  
    # "rfc_O3t"       = expression(Tropo.~O[3]),  
    "rfc_SO4"       = expression(Sulfate), 
    "rfc_NO3"       = expression(Nitrate), 
    "rfc_POA"       = expression(POA),    
    "rfc_SOA"       = expression(SOA),    
    "rfc_BC"        = expression(BC),  
    # "rfc_BCsnow"    = expression(BC~on~snow), 
    "rfc_cloud"     = expression(Cloud),   
    "r_aer_all"     = expression(Total~aerosols)        
    # "rfc_LCC"       = expression(Land~albedo),
    # "rfc_DUST"      = expression(Dust),
    # "rfc_solar"     = expression(Solar),
    # "rfc_volc"      = expression(Volcanic)
            )

# name definition for the GHGs
conc_name <- c(
	# "co2"="CO[2]",
	# "ch4"="CH[4]",
	# "n2o"="N[2]*O",
	"hfc23"="HFC*'-'*23",
	"hfc32"="HFC*'-'*32",
	"hfc125"="HFC*'-'*125",
	"hfc134a"="HFC*'-'*134*a",
	"hfc143a"="HFC*'-'*143*a",
	"hfc152a"="HFC*'-'*152*a",
	"hfc227ea"="HFC*'-'*227*ea",
	"hfc236fa"="HFC*'-'*236*fa",
	"hfc245fa"="HFC*'-'*245*fa",
	"hfc365mfc"="HFC*'-'*365*mfc",
	"hfc4310mee"="HFC*'-'*43*'-'*10*mee",
	# "hfc134"="HFC*'-'*134",
	# "hfc143"="HFC*'-'*143",
	# "hfc41"="HFC*'-'*41",
	"sf6"="SF[6]",
	"nf3"="NF[3]",
	"cf4"="CF[4]",
	"c2f6"="C[2]*F[6]",
	"c3f8"="C[3]*F[8]",
	"cc4f8"="c*'-'*C[4]*F[8]",
	"c4f10"="C[4]*F[10]",
	"c5f12"="C[5]*F[12]",
	"c6f14"="C[6]*F[14]",
	"cfc11"="CFC*'-'*11",
	"cfc12"="CFC*'-'*12",
	"cfc113"="CFC*'-'*113",
	"cfc114"="CFC*'-'*114",
	"cfc115"="CFC*'-'*115",
	"ccl4"="CCl[4]",
	"ch3ccl3"="CH[3]*CCl[3]",
	"hcfc22"="HCFC*'-'*22",
	"hcfc141b"="HCFC*'-'*141*b",
	"hcfc142b"="HCFC*'-'*142*b",
	"halon1211"="Halon*'-'*1211",
	# "halon1202"="Halon*'-'*1202",
	"halon1301"="Halon*'-'*1301",
	"halon2402"="Halon*'-'*2402",
	"ch3br"="CH[3]*Br",
	"ch3cl"="CH[3]*Cl",
    "r_fgs"="atop(Total~halogenated,gases)"
)