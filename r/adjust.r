adjust_to_season <- function(src_df,scale=1) {  
    ncount <- nrow(src_df)
    n_col <- ncol(src_df)
    tar_df <- as.data.frame(matrix(,ncol=n_col,nrow=0))
    yr_df <- as.numeric(as.character(src_df[,1]))

    for (i in 1:(ncount - 1)) {
        grow_gap <- (yr_df[i + 1] - yr_df[i]) * scm_dt 
        grow_step <- (src_df[i + 1,] - src_df[i,]) / grow_gap
        for (j in 1:grow_gap) {
            base_index <- nrow(tar_df) + 1
            tar_df[base_index,] <- src_df[i,] + grow_step * (j - 1)
        }
    }
    dst_len <- nrow(tar_df) + 1
    tar_df[dst_len,] <- src_df[ncount,]

    colnames(tar_df) <- colnames(src_df)
    
    if(scale != 1){
        tar_df[,2:n_col] <- tar_df[,2:n_col] * scale
    }

    tar_df$year <- as.character(round(tar_df$year,3))


    return(tar_df)
}

adjust_to_month <- function(src_df,scale=1) {  
    ncount <- nrow(src_df)
    n_col <- ncol(src_df)
    tar_df <- as.data.frame(matrix(,ncol=n_col,nrow=0))
    yr_df <- as.numeric(as.character(src_df[,1]))

    for (i in 1:(ncount - 1)) {
        grow_gap <- (yr_df[i + 1] - yr_df[i]) * scm_dm 
        grow_step <- (src_df[i + 1,] - src_df[i,]) / grow_gap
        for (j in 1:grow_gap) {
            base_index <- nrow(tar_df) + 1
            tar_df[base_index,] <- src_df[i,] + grow_step * (j - 1)
        }
    }
    dst_len <- nrow(tar_df) + 1
    tar_df[dst_len,] <- src_df[ncount,]

    colnames(tar_df) <- colnames(src_df)
    
    if(scale != 1){
        tar_df[,2:n_col] <- tar_df[,2:n_col] * scale
    }

    tar_df$year <- as.character(round(tar_df$year,3))


    return(tar_df)
}


adjust_to_year <- function(src_df,scale=1) {  
    ncount <- nrow(src_df)
    n_col <- ncol(src_df)
    tar_df <- as.data.frame(matrix(,ncol=n_col,nrow=0))
    yr_df <- as.numeric(as.character(src_df[,1]))

    for (i in 1:(ncount - 1)) {
        grow_gap <- (yr_df[i + 1] - yr_df[i]) * 1 
        grow_step <- (src_df[i + 1,] - src_df[i,]) / grow_gap
        for (j in 1:grow_gap) {
            base_index <- nrow(tar_df) + 1
            tar_df[base_index,] <- src_df[i,] + grow_step * (j - 1)
        }
    }
    dst_len <- nrow(tar_df) + 1
    tar_df[dst_len,] <- src_df[ncount,]

    colnames(tar_df) <- colnames(src_df)
    
    if(scale != 1){
        tar_df[,2:n_col] <- tar_df[,2:n_col] * scale
    }

    # tar_df$year <- as.character(tar_df$year)

    return(tar_df)
}

adjust_to_season_cm6 <- function(src_df,scale=1) {  
    ncount <- nrow(src_df)
    n_col <- ncol(src_df)

    tar_df <- as.data.frame(matrix(,ncol=n_col,nrow=0))
    # yr_df <- as.numeric(as.character(src_df[,1]))
    yr_df <- src_df[,1]

    for (i in 1:(ncount - 1)) {
        grow_gap <- ((yr_df[i + 1] - yr_df[i]) * scm_dt)[[1]]

        grow_step <- (src_df[i + 1,] - src_df[i,]) / grow_gap
        for (j in 1:grow_gap) {
            base_index <- nrow(tar_df) + 1
            tar_df[base_index,] <- src_df[i,] + grow_step * (j - 1)
        }
    }
    dst_len <- nrow(tar_df) + 1
    tar_df[dst_len,] <- src_df[ncount,]

    colnames(tar_df) <- colnames(src_df)
    
    if(scale != 1){
        tar_df$co2fl <- tar_df$co2fl * scale
        tar_df$co2fo <- tar_df$co2fo * scale
    }

    tar_df$year <- as.character(round(tar_df$year,3))


    return(tar_df)
}