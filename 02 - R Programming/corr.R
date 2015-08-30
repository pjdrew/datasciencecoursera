corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    all_files <- list.files(directory, full.names=TRUE)

    v <- vector(mode="numeric", length=0)

    for (i in 1:length(all_files)) {
        if (sum(complete.cases(read.csv(all_files[i]))) > threshold) 
            {v <- c(v, cor(read.csv(all_files[i])$sulfate,
                           read.csv(all_files[i])$nitrate,
                           use="complete.obs") 
                   )
            }
        }

    v

}
