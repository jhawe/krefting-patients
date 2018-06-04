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
               sep = ",", stringsAsFactors = F,
               fileEncoding="UTF-8")
  
  # create list of values to be returned
  
  # ----------------------------------------------------------------------------
  # Get necessary columns and create index vectors
  # ----------------------------------------------------------------------------
  # name, weight, dates
  md <- d$V4
  # perfusion/ischemic time/surgeon
  md2 <- d$V2
  
  # ----------------------------------------------------------------------------
  # Create final return list and fill with basic data
  # ----------------------------------------------------------------------------
  # TODO do the naming using the fields in the csv sheet! (for consistency, the
  # below part is but a test)
  ret <- list(name=md[2],
              surname=md[3],
              sex=md[4],
              date_of_birth=(md[5]),
              age_on_entry=as.numeric(md[6]),
              date_of_entry=(md[7]),
              date_of_release=(md[8]),
              perfusion_time=(md2[6]),
              ischemic_time=(md2[7]),
              surgeon=md2[3])
  
  # add weight, height, bmi
  ret[["weight"]] <- md2[match("Gewicht:", d$V1)]
  ret[["height"]] <- md2[match("Größe:", d$V1)]
  ret[["BMI"]] <- round(as.numeric(md[match("BMI:", md) + 1]))
  
  # ----------------------------------------------------------------------------
  # Match more complicated data (anamnese, medication, etc,..)
  # ----------------------------------------------------------------------------
  
  # match main fields, then parse bodies
  idx_diag_on_entry <- match("DIAGNOSEN UND BEHANDLUNGEN BEI AUFNAHME", d$V1)
  idx_an <- match("ANAMNESE", d$V1)
  idx_med <- match("MEDIKATION BEI AUFNAHME", d$V1)
  idx_diag <- match("DIAGNOSEN UND SPEZIFISCHE THERAPIEN IM VERLAUF", d$V1)
  idx_antimi <- match("ANTIMIKROBIELLE THERAPIE", d$V1)
  
  # last non empty field after idx_antimi
  idx_last <- grep("^$", d$V1)
  idx_last <- idx_last[idx_last>idx_antimi][1] - 1
  
  # now define start and endpairs in a list
  to_parse <- list()
  to_parse[["Diagnose_bei_Aufnahme"]] <- c(idx_diag_on_entry + 2, 
                                           match("Gewicht:", d$V1) - 1)
  to_parse[["Anamnese"]] <- c(idx_an + 2, idx_med - 1)
  to_parse[["Medikation_bei_Aufnahme"]] <- c(idx_med + 2, idx_diag - 1)
  to_parse[["Diagnose_Therapie_im_Verlauf"]] <- c(idx_diag + 2, idx_antimi - 1)
  to_parse[["Antimikrobielle_Therapie"]] <- c(idx_antimi + 2, idx_last)
  
  # define our field and group delimiters
  delim_g <- "|"
  delim_f <- ";"
  
  # flag for extubation
  ext <- list(date=NA, content=NA)
  
  # parse everything
  result <- lapply(names(to_parse), function(n) {
    start <- to_parse[[n]][1]
    end <- to_parse[[n]][2]
    temp <- ""
    
    if(start<end) {
    
      for(i in start:end) {
        r <- d[i,]
        # if all fields are empty, we do not add this entry
        if(all(grepl("^$", r))) {
          next
        }
        # check whether we have extubation information
        if(grepl("*extubation*", r[2], ignore.case = T)) {
          ext <<- list(date=unlist(r[1]), content=unlist(r[2]))
        }
        # replace any occurences of our delimiters with ","
        pr <- gsub(delim_f, ",", 
                   gsub(paste0("\\", delim_g), ",", r[1:4]))
        
        # create delim separated string
        str <- paste(pr, collapse = delim_f)
        if(temp == "") {
          temp <- str
        } else {
          temp <- paste(temp, 
                        str,
                        sep = delim_g)
        }
      }
    }
    
    temp
  })
  names(result) <- names(to_parse)
  
  # fill in special case
  result[["Extubation_Datum"]] <- ext[["date"]]
  result[["Extubation_Beschreibung"]] <- ext[["content"]]
  
  ret <- append(ret, result)
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