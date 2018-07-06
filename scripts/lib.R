#' Small helper for a list() function automatically setting
#' the variable names as the names of the created list
#' 
#' compare: https://stackoverflow.com/a/21059868
#'
#' @author Johann Hawe
#' 
listN <- function(...){
  ln <- list(...)
  names(ln) <- as.character(substitute(list(...)))[-1]
  ln
}

# ------------------------------------------------------------------------------
#' Method to handle the 'Stammdaten' sheet of a patient's excel data file
#' 
#' @param df The Stammdaten sheet of the excel file already parsed as a data
#' frame
#' 
#' @author Johann Hawe
#' 
# ------------------------------------------------------------------------------
process_basedata <- function(df) {
  
  # create list of values to be returned
  
  # ----------------------------------------------------------------------------
  # Get necessary columns and create index vectors
  # ----------------------------------------------------------------------------
  d1 <- trimws(unlist(df$Col1))
  d2 <- trimws(unlist(df$Col2))
  d3 <- trimws(unlist(df$Col3))
  d4 <- trimws(unlist(df$Col4))
  
  # ----------------------------------------------------------------------------
  # Match all fields and collect values
  # ----------------------------------------------------------------------------
  name <- d4[match("Name", d3)][1]
  forename <- d4[match("Vorname", d3)][1]
  sex <- d4[match("Geschlecht", d3)][1]
  date_of_birth <- d4[match("Geburtsdatum", d3)][1]
  date_of_entry <- d4[match("Aufnahme am", d3)][1]
  
  # calculate age at entry
  age_on_entry <- floor(as.numeric(difftime(as.Date(date_of_entry), 
                                            as.Date(date_of_birth)) / 365))
  
  date_of_release <- d4[match("Entlassung am", d3)][1]
  date_of_death <- d4[match("Verstorben am", d3)][1]
  perfusion_time <- d2[match("Perfusionszeit", d1)][1]
  ischemic_time <- d2[match("Ischämiezeit", d1)][1]
  surgeon <- d2[match("Operateur", d1)][1] 
  
  ret <- listN(name, forename, sex, date_of_birth, age_on_entry, date_of_entry,
               date_of_release, date_of_death, perfusion_time, ischemic_time,
               surgeon)
  
  # we prepend the key for the patient, since a single individual might have more 
  # than 1 surgery/data file.
  k_visits <- list(key_visit=paste(ret$name, 
                                   ret$forename, 
                                   ret$date_of_birth, 
                                   ret$date_of_entry, 
                                   sep="-"))
  k_patients <- list(key_patient=paste(ret$name, 
                                       ret$forename, 
                                       ret$date_of_birth, 
                                       sep="-"))
  
  ret <- append(append(k_visits, k_patients), ret)
  
  # add weight, height, bmi
  ret[["weight"]] <- as.numeric(d2[match("Gewicht:", d1)])
  ret[["height"]] <- as.numeric(d2[match("Größe:", d1)])
  ret[["BMI"]] <- round(as.numeric(d4[match("BMI:", d4) + 1]))
  
  # ----------------------------------------------------------------------------
  # Match more complicated data (anamnese, medication, etc,..)
  # ----------------------------------------------------------------------------
  field_names <- tolower(c("DIAGNOSEN UND BEHANDLUNGEN BEI AUFNAHME",
                   "ANAMNESE",
                   "MEDIKATION BEI AUFNAHME",
                   "DIAGNOSEN UND SPEZIFISCHE THERAPIEN IM VERLAUF",
                   "ANTIMIKROBIELLE THERAPIE"))
  
  # define our field and group delimiters
  delim_g <- "|"
  delim_f <- ";"
  
  # flag for extubation
  ext <- list(date=NA, content=NA)
  
  # match main fields, then parse bodies
  for(f in field_names) {
    m <- match(f, tolower(d1))
    if(!is.na(m)) {
      m <- m+1
      temp <- ""
      while(T) {
        # get the complete row
        r <- c(d1[m],d2[m],d3[m],d4[m])
        
        # ----------------------------------------------------------------------
        # if all fields are empty, we do not add this entry
        # ----------------------------------------------------------------------
        if(all(grepl("^$", r)) | any(grepl("Datum", r, ignore.case = T))) {
          # go to the next field
          m <- m + 1
          next
        }
        
        # ----------------------------------------------------------------------
        # add values
        # ----------------------------------------------------------------------
        
        # check whether we have extubation information
        if(any(grepl("*extubation*", r, ignore.case = T))) {
          ext <- list(date=unlist(r[1]), content=unlist(r[2]))
        }
        
        # replace any occurences of our delimiters with ","
        pr <- gsub(delim_f, ",", 
                   gsub(paste0("\\", delim_g), ",", r[1:4]))
        # replace NA values with empty strings
        pr[is.na(pr)] <- ""
        # create delim separated string
        str <- paste(pr, collapse = delim_f)
        if(temp == "") {
          temp <- str
        } else {
          temp <- paste(temp, 
                        str,
                        sep = delim_g)
        }
        
        # ----------------------------------------------------------------------
        # go to the next field
        # ----------------------------------------------------------------------
        m <- m + 1
        
        # ----------------------------------------------------------------------
        # break if we encounter one of the other field headers...
        # ----------------------------------------------------------------------
        if(!is.na(match(tolower(d1[m]), field_names))) {
          break
        }
        # ----------------------------------------------------------------------
        # ... or if we run out of fields to investigate
        # ----------------------------------------------------------------------
        if(m>length(d1)) {
          break
        }
        
      }
      # add the parsed information to our results
      ret[[gsub(" ", "_", f)]] <- temp
    } else {
      ret[[gsub(" ", "_", f)]] <- NA
    }
  }
  
  # fill in special information
  ret[["Extubation_Datum"]] <- ext[["date"]]
  ret[["Extubation_Beschreibung"]] <- ext[["content"]]
  
  return(unlist(ret))
}

# ------------------------------------------------------------------------------
#' Method to handle the 'EUROSCORE II' sheet of a patient's excel data file
#' 
#' @param df The EUROSCORE II sheet parsed as a data frame
#' 
#' @author Johann Hawe
#' 
# ------------------------------------------------------------------------------
process_euroscore <- function(df) {
  
  # ----------------------------------------------------------------------------
  # Get necessary columns and create index vectors
  # ----------------------------------------------------------------------------
  
  # define the categories (this has been manually extracted from a sample sheet)
  categories <- gsub(" ", "_", 
                     tolower(c("NYHA", "Komorbidität", "Nierenfunktion", 
                     "Kardiale Komorbidität", "LV Funktion", 
                     "pulmonale Hypertonie (PA Druck systolisch)",
                     "Dringlichkeit",
                     "OP Prozedur")))
  
  # name column
  names <- gsub(" ", "_", tolower(trimws(df$Col1)))
  
  # indicator column
  indic <- trimws(df$Col3)
  
  # ----------------------------------------------------------------------------
  # build final list
  # ----------------------------------------------------------------------------
  
  ret <- list()
  
  # ----------------------------------------------------------------------------
  # get the final mortality score
  # ----------------------------------------------------------------------------
  # try to match it first. if not possible, we use the indices here
  temp <- gsub(" ", "_", tolower("Risk of mortality EUROSCORE II"))
  temp_m <- match(temp, names)
  if(!is.na(temp_m)) {
    val <- df[temp_m,4]
  } else {
    val <- df[47,4]
    warning(paste0("Couldn't match mortality score. Using fixed index.",
                   "\nFound score was: " , val))
  }
  # replace "," and "%" in the value to be able to cast it to numeric
  val <- as.numeric(gsub(",", ".", 
                         gsub("\\%", "", val)))
  ret[["euroscore2"]] <- val
  
  # ----------------------------------------------------------------------------
  # iterate over all categories and populate fields
  # match main fields, then parse bodies
  # ----------------------------------------------------------------------------
  # manually set the indicator field which should stop the processing
  stop_field <- "konstante"
  for(f in categories) {
    m <- match(f, names)
    if(!is.na(m)) {
      m <- m+1
      while(T) {
        # ----------------------------------------------------------------------
        # add values
        # ----------------------------------------------------------------------
        n <- names[m]
        # replace NA with empty string
        if(is.na(indic[m])) {
          indic[m] <- ""
        }
        ret[[paste0(f, ".", n)]] <- indic[m]
        
        # ----------------------------------------------------------------------
        # go to the next field
        # ----------------------------------------------------------------------
        m <- m + 1
        
        # ----------------------------------------------------------------------
        # break if we encounter one of the other field headers...
        # ----------------------------------------------------------------------
        if(!is.na(match(names[m], categories))) {
          break
        }
        # ----------------------------------------------------------------------
        # ... or if we run out of fields to investigate -> last one is 'konstante'
        # ----------------------------------------------------------------------
        if(!is.na(match(names[m], stop_field))) {
          break
        }
        # ----------------------------------------------------------------------
        # ... or if we exceed the length of the vector. this should not happen!
        # ----------------------------------------------------------------------
        if(m>length(names)) {
          warning("No stop field found. Stopping due to overflow.")
          break
        }
      }
    } else {
      print(f)
      stop("Did not match a field in the EUROSCORE II.")
    }
  }
  
  return(unlist(ret))
}

# ------------------------------------------------------------------------------
#' Method to handle the "Verlegungskriterien" sheet
#' 
#' @param df The parsed data-frame from the excel file holding the 
#' 'Verlegungskriterien' sheet data
#' 
#' @author Johann Hawe
#' 
# ------------------------------------------------------------------------------
process_mvt_criteria <- function(df) {
  
  # ----------------------------------------------------------------------------
  # Get necessary columns and create index vectors
  # ----------------------------------------------------------------------------

  # indicator columns
  indic_yes <- tolower(df$Col5)
  indic_part <- tolower(df$Col6)
  indic_no <- tolower(df$Col7)
  
  # match the header of the indicator columns
  # this is also an indicator for when the categories 'start' in the first 
  # column
  indic_hdr_idx <- match("ja", indic_yes)
  if(is.na(indic_hdr_idx)) {
    stop("Couldn't match indicator header.")
  }
  
  # category column
  category_col <- gsub(" ", "_", 
                     tolower(trimws(df$Col1)))
  sub_category_col <- gsub(" ", "_", 
                           tolower(trimws(df$Col2)))
  categories <- category_col[indic_hdr_idx+1:length(category_col)]
  categories <- categories[!is.na(categories)]
  
  # ----------------------------------------------------------------------------
  # build final list
  # ----------------------------------------------------------------------------
  
  ret <- list()
  
  # get 'Entlassungstag' by fixed value (special case)
  ret[["Entlassungstag"]] <- sub_category_col[4]
  
  # iterate individual fields
  # the header values (ja, teilweise, nein)
  vals <- cbind(indic_yes, indic_part, indic_no)
  val_header <- vals[indic_hdr_idx,]
  
  for(f in categories) {
    m <- match(f, category_col)
    if(!is.na(m)) {
      while(T) {
        # ----------------------------------------------------------------------
        # add values
        # ----------------------------------------------------------------------
        
        # subcategory name
        n <- sub_category_col[m]
        
        # skip empty sub category fields
        if(is.na(n)) {
          break
        }
        
        # get the values
        temp <- vals[m,]
        for(i in 1:length(temp)) {
          if(is.na(temp[i])) {
            temp[i] <- ""
          }
        }
        
        ret[[paste0(f, ".", n)]] <- paste(val_header,
                                          temp, 
                                          collapse=";", 
                                          sep=":")
        
        # ----------------------------------------------------------------------
        # go to the next field
        # ----------------------------------------------------------------------
        m <- m + 1
        
        # ----------------------------------------------------------------------
        # break if we encounter one of the other field headers...
        # ----------------------------------------------------------------------
        if(!is.na(match(category_col[m], categories))) {
          break
        }
        # ----------------------------------------------------------------------
        # ... or if we run into an empty field in the sub categories
        # ----------------------------------------------------------------------
        if(is.na(sub_category_col[m])) {
          break
        }
        # ----------------------------------------------------------------------
        # ... or if we exceed the length of the vector. this should not happen!
        # ----------------------------------------------------------------------
        if(m>length(category_col)) {
          warning("No stop field found. Stopping due to overflow.")
          break
        }
      }
    } else {
      print(f)
      stop("Did not match a field in the Verlegungskriterien.")
    }
  }
 
  # ----------------------------------------------------------------------
  # add special information from the end of the sheet
  # ----------------------------------------------------------------------
  
  spec_categories <- categories[(length(categories)-2):length(categories)]
  spec_header_idx <- match("empfohlen", indic_yes)
  
  if(is.na(spec_header_idx)) {
    warning("Did not encounter extended criteria in 'Verlegungskriterien'.\n")
  } else {
    
    spec_header <- vals[spec_header_idx,]
    
    for(f in spec_categories) {
      m <- match(f, category_col)
      if(!is.na(m)) {
        # get the values
        temp <- vals[m,]
        for(i in 1:length(temp)) {
          if(is.na(temp[i])) {
            temp[i] <- ""
          }
        }
        ret[[f]] <- paste(spec_header, temp,
                          collapse=";", 
                          sep=":")
      }
    }
  }
  return(unlist(ret))
}

