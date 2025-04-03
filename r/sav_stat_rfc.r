source("./r/plot_deriv_ems.r")

# fl_ext <- ".pdf"
fl_ext <- ".png"

IV_lst <- c("mei_amo", "amo","cti","dmi","nao","pdo")
obs_lst <- c(
    "HadCRUTv5.0.2"=2024.917,
    "GlobalTemp_v6"=2024.917,
    "Berkeley_Earth"=2024.917,
    "JMA"=2024.917,
    "DCENT"=2023.917
)

nthread <- 3
for(o in c(1:3)){
    myCluster <- makeCluster(nthread, type = "FORK",  outfile="")
    registerDoParallel(myCluster)
    sub_list <- c("Ghg", "GhgAer", "AntNat") #
    # sub_list <- c("AntVolSolMei")
    foreach(i = c(1:length(sub_list)), .packages = "data.table")%dopar%{
    # for(i in c(1:length(sub_list))){
        exp_tag <- sub_list[i]
        obs_src <- names(obs_lst)[o]
        sav_est_rfc_stat_mtx("periodcrfc_tAll", "WORLD", "COMB", "IAMC", "SSP1_19", "fscm_tatm", obs_src, exp_tag)
    }
    stopCluster(myCluster)
}

