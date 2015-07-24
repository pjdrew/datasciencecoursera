complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    all_files <- list.files(directory, full.names=TRUE)
    df <- data.frame(id=character(),nobs=integer(),stringsAsFactors=FALSE)
    for (i in id) {
        count <- sum(complete.cases(read.csv(all_files[i])))
        df[nrow(df) +1,] <- c(toString(i), count)
        } 

    df

}

