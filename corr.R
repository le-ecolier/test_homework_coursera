corr <- function(directory, threshold = 0){
      dir_list <-dir(directory)
      nobs <-numeric()
      j=1
      for (i in seq_along(dir_list)){
            mi <-read.csv(paste(directory,dir_list[i],sep="/")) 
            mic <- complete.cases(mi)
            if ( length(mic[mic==TRUE])>=threshold){
                  # determines if there are enough complete cases
                   #print("found one!")
                  cn <-mi$nitrate[mic]
                  #vector of nitrate values from complete cases
                  cs<-mi$sulfate[mic]
                  #vector of sulfate values from complete cases
                  
                  nobs[j] <- round(cor(cn,cs),digits=5) 
                  #calculating correlation between nitrate and sulfate
                  j=j+1
            }                  
            
      }
      nobs
}