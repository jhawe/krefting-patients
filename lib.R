# ------------------------------------------------------------------------------
#' Method to handle the 'Stammdaten' sheet of a patient's excel data file
#' 
#' @param file The csv file extracted from the xlsx file holding the 'Stammdaten'
#' 
#' @author Johann Hawe
#' 
# ------------------------------------------------------------------------------
process_basedata <- function(file) {
  if(!file.exists(file)) {
    warning(paste0("File ", file, " does not exist (process_basedata)."))
    return(NULL)
  }
  d <-read.csv(file, header = F, 
               sep = ",", stringsAsFactors = F)
  
  # create list of values to be returned
  
  # ----------------------------------------------------------------------------
  # Get necessary columns and create index vectors
  # ----------------------------------------------------------------------------
  # name, weight, dates
  md <- d$V4
  # perfusion/ischemic time/surgeon
  md2 <- d$V2
  
  # ----------------------------------------------------------------------------
  # Create final return list
  # ----------------------------------------------------------------------------
  # TODO do the naming using the fields in the csv sheet! (for consistency, the
  # below part is but a test)
  ret <- list(name=md[2],
              surname=md[3],
              sex=md[4],
              date_of_birth=(md[5]),
              age=as.numeric(md[6]),
              date_of_entry=(md[7]),
              date_of_release=(md[8]),
              perfusion_time=(md2[6]),
              ischemic_time=(md2[7]),
              surgeon=md2[3])
  
  return(ret)
}

# ------------------------------------------------------------------------------
#' Method to handle the 'EUROSCORE II' sheet of a patient's excel data file
#' 
#' @param file The csv file extracted from the xlsx file holding the 
#' 'EUROSCORE II' data
#' 
#' @author Johann Hawe
#' 
# ------------------------------------------------------------------------------
process_euroscore <- function(file) {
  if(!file.exists(file)) {
    warning(paste0("File ", file, " does not exist (process_euroscore)."))
    return(NULL)
  }
  
  d <-read.csv(file, header = F, 
               sep = ",", stringsAsFactors = F)
  
  # ----------------------------------------------------------------------------
  # Get necessary columns and create index vectors
  # ----------------------------------------------------------------------------
  
  # name column
  names <- trimws(d$V1)
  # indicator column
  indic <- d$V3
  
  # create indices which we need to process
  # was created manually by looking at examples!
  idxs <- c(5:9, 11:14, 16:19, 21:23, 
            25:29, 31:32, 34:37, 39:43)
  headers <- c(4,10,15,20,24,30,33,38)
  
  # ----------------------------------------------------------------------------
  # build final list
  # ----------------------------------------------------------------------------
  
  res <- list()
  
  # get the final mortality score
  res[["euroscore2"]] <- as.numeric(d[47,4])
  
  # iterate individual fields
  for(i in idxs) {
    h <- gsub(" ", "_", names[headers[max(which(headers<i))]])
    res[[paste0(h, ".", gsub(" ", "_", names[i]))]] <- indic[i]
  }
  
  return(res)
}