# A multiple regression model for climate change attribution analysis

## For article:

	Continued increase in atmospheric carbon dioxide exacerbated the 2022-2024 global temperature spike

## Description:

	This repository contains the code for reproducing the findings of the paper "Continued increase in atmospheric carbon dioxide exacerbated the 2022-2024 global temperature spike." This README describes the code and data for reproducibility. The code is written in R and is designed to be run on a Mac OS or Linux system (not tested under Windows). It is tested under R version 4.4.3. The code is divided into two parts: the first part is for running the multiple linear regression model, and the second part is for plotting the results.

## Dataset used in this study:

### GMT
- HadCRUT.5.0.2.0
- NOAAGlobalTemp_v6.0.0
- Berkeley Earth
- JMA (https://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/list/mon_wld.html)

### Concentration (used in SCM4OPT v3.3)
- Mauna Loa CO2 (NOAA)
- Global CH4 Monthly Means (NOAA)
- Global N2O Monthly Means (NOAA)
- Global SF6 Monthly Means (NOAA)
- Advanced Global Atmospheric Gases Experiment: Agage_gcmd_gcms.data.2025_02_01

### Emissions (used in SCM4OPT v3.3)
- Carbon Monitor (2024-12-16)
- CEDS v_2024_07_08
- EDGAR_2024_GHG 1970-2023 (CO2, CH4, N2O, F-gases)
- EDGAR v8.1_AP 1970-2022 (Gases and Aerosols) 

### Solar and volcanic (used in SCM4OPT v3.3)
- tsis_tsi_L3_c24h_latest (https://lasp.colorado.edu/data/tsis/tsi_data/tsis_tsi_L3_c24h_latest.txt)
- GloSSAC_V2.22

### Internal Climate Variability
- Bivariate ENSO Timeseries (BEST) (https://psl.noaa.gov/data/climateindices/list/)
- Multivariate ENSO Index (MEI) (https://psl.noaa.gov/enso/mei.old/mei.html)
- NCEP-NCAR MULTIVARIATE ENSO INDEX (MEI) (https://www.webberweather.com/multivariate-enso-index.html)
- Extended Multivariate ENSO Index (MEI.ext) (https://psl.noaa.gov/enso/mei.ext/)
- NOAA Extended Reconstructed SST V5 (https://psl.noaa.gov/data/gridded/data.noaa.ersst.v5.html)
- Multivariate ENSO Index Version 2 (MEI.v2) (https://psl.noaa.gov/enso/mei/)
- AMO (Atlantic Multidecadal Oscillation) Index (https://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.amo.dat)
- Pacific Decadal Oscillation (PDO) (https://www.ncei.noaa.gov/access/monitoring/pdo/)
- North Atlantic Oscillation (NAO) (https://psl.noaa.gov/data/timeseries/month/)
- Dipole Mode Index (DMI) (https://psl.noaa.gov/data/timeseries/month/)
- Niño 3 SST Index (https://psl.noaa.gov/data/timeseries/month/)
- Niño 3.4 SST Index (https://psl.noaa.gov/data/timeseries/month/)
- Niño 4 SST Index (https://psl.noaa.gov/data/timeseries/month/)
- Arctic Oscillation (AO) (https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table)
- Tripole Index for the Interdecadal Pacific Oscillation (TPI) (https://psl.noaa.gov/data/timeseries/IPOTPI/)

## Directory Structure 

	./
	├── README.md: This file.
	├── data: Contains the data used in the study.
	│   ├── AGAGE: Contains the data from the Advanced Global Atmospheric Gases Experiment.
	│   │   ├── Agage2004: Contains the summary data from the AGAGE.
	│   │   ├── Agage2004_mean: Contains the mean data from the AGAGE.
	│   │   └── Agage_gcmd_gcms.data.2025_02_01: Contains the data downloaded from the AGAGE.
	│   ├── BEST: Contains the data from the Bivariate ENSO Timeseries (BEST).
	│   ├── CH4: Contains the CH4 concentration data.
	│   ├── CO2: Contains the CO2 concentration data.
	│   │   ├── CMIP6: Contains the CO2 concentration data from the CMIP6.
	│   │   ├── GOSAT: Contains the CO2 concentration data from the GOSAT.
	│   │   └── NOAA: Contains the NOAA CO2 concentration data.
	│   ├── CTI: Contains the CTI data produced from the NOAA Extended Reconstructed SST V5.
	│   │   ├── cti.sh: Script for processing the CTI data.
	│   ├── GMT: Contains the GMT data.
	│   │   ├── CowtanAndWay: Contains the CowtanAndWay data.
	│   │   │   ├── cdo.sh: Script for processing the annual CowtanAndWay data.
	│   │   │   ├── cdom.sh: Script for processing the monthly CowtanAndWay data.
	│   │   ├── GISTEMPv4: Contains the GISTEMPv4 data.
	│   │   ├── HadCRUT.4.6: Contains the HadCRUT.4.6 data.
	│   │   │   └── cdom.sh: Script for processing the monthly HadCRUT.4.6 data.
	│   │   ├── HadCRUT.5.0: Contains the HadCRUT.5.0 data.
	│   │   │   └── cdo.sh: Script for processing the HadCRUT.5.0 data.
	│   │   ├── HadCRUT.5.0.1: Contains the HadCRUT.5.0.1 data.
	│   │   ├── HadCRUT.5.0.2: Contains the HadCRUT.5.0.2 data.
	│   │   │   └── cdo.sh: Script for processing the HadCRUT.5.0.2 data.
	│   │   ├── HadCRUT3: Contains the HadCRUT3 data.
	│   │   │   └── cdo.sh: Script for processing the HadCRUT3 data.
	│   │   ├── NOAAGlobalTempv5: Contains the NOAAGlobalTempv5 data.
	│   │   │   └── cdo.sh: Script for processing the NOAAGlobalTempv5 data.
	│   │   ├── NOAAGlobalTempv51: Contains the NOAAGlobalTempv51 data.
	│   │   │   └── cdo.sh: Script for processing the NOAAGlobalTempv51 data.
	│   │   ├── NOAAGlobalTempv6: Contains the NOAAGlobalTempv6 data.
	│   │   │   └── cdo.sh: Script for processing the NOAAGlobalTempv6 data.
	│   │   └── berkeleyearth: Contains the Berkeley Earth data.
	│   ├── MEI: Contains the MEI data.
	│   ├── N2O: Contains the N2O concentration data.
	│   ├── ODS: Contains the ODS data.
	│   ├── SST: Contains the SST data.
	│   │   └── cti.sh: Script for processing the CTI data (not used).
	│   ├── Variability: Contains the internal climate variability data.
	│   ├── solar: Contains the solar data.
	│   └── sum: Contains the summary data after the processing.
	│       ├── AR6: Contains the AR6 data.
	│       ├── BEST: Contains the BEST data.
	│       ├── CH4: Contains the CH4 data.
	│       ├── CO2: Contains the CO2 data.
	│       ├── CTI: Contains the CTI data.
	│       ├── IV: Contains the climate internal variability data.
	│       ├── MEI: Contains the MEI data.
	│       ├── N2O: Contains the N2O data.
	│       ├── ODS: Contains the ODS data.
	│       ├── OTH: Contains the other data.
	│       ├── SF6: Contains the SF6 data.
	│       └── TEM: Contains the temperature data.
	├── data-cali: Contains the data used in the calibration and regression.
	│   └── periodcrfc_tAll: Contains the attribution data.
	├── data-final: Contains the final output.	
	│	├── BerkeleyEarth_1855_2024_025.csv: The 2.5% confidence interval for the output using the Berkeley Earth.
	│	├── BerkeleyEarth_1855_2024_975.csv: The 97.5% confidence interval for the output using the Berkeley Earth.
	│	├── BerkeleyEarth_1855_2024_cor.csv: The correlation matrix for the output using the Berkeley Earth.
	│	├── BerkeleyEarth_1855_2024_cor_detr.csv: The detrended correlation matrix for the output using the Berkeley Earth.
	│	├── BerkeleyEarth_1855_2024_detr.csv: The detrended data for the output using the Berkeley Earth.
	│	├── BerkeleyEarth_1855_2024_median.csv: The median data for the output using the Berkeley Earth.
	│	├── GlobalTempv6_1855_2024_025.csv: The 2.5% confidence interval for the output using the NOAA GlobalTemp_v6.
	│	├── GlobalTempv6_1855_2024_975.csv: The 97.5% confidence interval for the output using the NOAA GlobalTemp_v6.
	│	├── GlobalTempv6_1855_2024_cor.csv: The correlation matrix for the output using the NOAA GlobalTemp_v6.
	│	├── GlobalTempv6_1855_2024_cor_detr.csv: The detrended correlation matrix for the output using the NOAA GlobalTemp_v6.
	│	├── GlobalTempv6_1855_2024_detr.csv: The detrended data for the output using the NOAA GlobalTemp_v6.
	│	├── GlobalTempv6_1855_2024_median.csv: The median data for the output using the NOAA GlobalTemp_v6.
	│	├── HadCRUTv5.0.2_1855_2024_025.csv: The 2.5% confidence interval for the output using HadCRUTv5.0.2.
	│	├── HadCRUTv5.0.2_1855_2024_975.csv: The 97.5% confidence interval for the output using HadCRUTv5.0.2.
	│	├── HadCRUTv5.0.2_1855_2024_cor.csv: The correlation matrix for the output using HadCRUTv5.0.2.
	│	├── HadCRUTv5.0.2_1855_2024_cor_detr.csv: The detrended correlation matrix for the output using HadCRUTv5.0.2.
	│	├── HadCRUTv5.0.2_1855_2024_detr.csv: The detrended data for the output using HadCRUTv5.0.2.
	│	└── HadCRUTv5.0.2_1855_2024_median.csv: The median data for the output using HadCRUTv5.0.2.	
	├── data-spike: Contains the data during or after the processing.
	│   ├── Berkeley_Earth_stat_rfc: Contains the data processed using Berkeley Earth.
	│   ├── GlobalTemp_v6_stat_rfc: Contains the data processed using NOAA GlobalTemp_v6.
	│   ├── HadCRUTv5.0.2_stat_rfc: Contains the data processed using HadCRUTv5.0.2.
	│   └── periodcrfc_tAll: Contains the final output.
	├── figs-spike: Contains the figures used in the paper.
	├── latex: Contains the LaTeX table files used in the paper.
	│   ├── BerkeleyEarth_1855_2024_cor.tex: The LaTeX table for the correlation matrix using Berkeley Earth.
	│   ├── Berkeley_Earth.tex: The LaTeX table for the model test results using Berkeley Earth.
	│   ├── GlobalTemp_v6.tex: The LaTeX table for the model test results using NOAA GlobalTemp_v6.
	│   ├── GlobalTempv6_1855_2024_cor.tex: : The LaTeX table for the correlation matrix using NOAA GlobalTemp_v6.
	│   ├── HadCRUTv5.0.2.tex: The LaTeX table for the model test results using HadCRUTv5.0.2.
	│   └── HadCRUTv5.0.2_1855_2024_cor.tex: : The LaTeX table for the correlation matrix using HadCRUTv5.0.2.
	├── md: Contains the markdown files.
	├── r: Contains the R scripts.
	│   ├── GHG_conc.r: R Script for processing the GHG concentration data.
	│   ├── initial_spike.r: R Script for initializing the data for the regression.
	│   ├── plot_tspike.r: R Script for plotting the figures used in the paper.
	│   ├── print_project.r: R Script for printing the project information.
	│   ├── rdef.r: R Script for defining the functions used in the study.
	│   ├── sav_stat_final.r: R Script for processing the final results for the analysis in the manuscript.
	│   ├── sav_stat_rfc.r: R Script for performing the regression.
	│   ├── sav_stat_roll.r: R Script for processing the rolling mean data.
	│   ├── shr_cal_decade.r: R Script for calculating the decadal trends.
	│   ├── shr_cmm_cl_def.r: R Script for defining the functions used in the study.
	│   ├── shr_cmm_def.r: R Script for defining the functions used in the study.
	│   ├── shr_get_period.r: R Script for getting the period data matrix.
	│   ├── shr_get_record.r: R Script for getting the record data matrix.
	│   ├── shr_installer.r: R Script for installing the required packages.
	│   ├── spike_header.r: R Script for defining the functions used in the study.
	│   ├── sum_agage.r: R Script for processing the AGAGE data.
	│   └── sum_cti.r: R Script for processing the CTI data.
	├── sh: Contains the shell scripts.
	│   ├── sav_stat1.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   ├── sav_stat2.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   ├── sav_stat3.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   ├── sav_stat4.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   ├── sav_stat5.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   ├── sav_stat6.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   ├── sav_stat7.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   ├── sav_stat8.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   ├── sav_stat9.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   ├── sav_stat_ext.sh: Shell Script for processing the rolling mean data, used in a parallel computation environment.
	│   └── sav_stat_final.sh: Shell Script for processing the final results for the analysis in the manuscript.
	└── sum_period: Contains the summary data produced by the SCM4OPT v3.3.
		└── periodcrfc_tAll: Contains the summary data produced by the SCM4OPT v3.3. These data are generated using SCM4OPT v3.3. Please contact the authors for more information.

## Reproduction of Figures:

- Download the available data as described in the "Dataset used in this study" section.
- Install the required packages using the R script shr_installer.r.
- Process the data using the R script initial_spike.r. We recommend using a parallel computation environment to process the data, which could take several days.
- Run the R script plot_tspike.r to generate the figures used in the paper.
- You can use plot_tspike.r to run a demonstration, as all the final results are already available.
- Use the bash scripts in the sh folder to run the calculation in a parallel computation environment.

## License

This project is licensed under the MIT License. For more details, see the [LICENSE](LICENSE) file.

## Developed and maintained by: 

Xuanming Su & Hideo Shiogama
