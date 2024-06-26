#' Title getCountryDetails
#'
#' @param Adress Text that may contain a country name indication: Name, Abbreviation,etc. e.g. PubMed Affiliation
#'
#' @return data frame with extracted countries and their info
#' @export
#'
#' @examples getCountryDetails("adress text contain TUN")
#' 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Define the function to get country details from an address
getCountryDetails <- function(Address) {  
  
  # Function to install and load required packages if missing
  install_if_missing <- function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  
  # Required packages
  req_pkgs <- c("stringr","rworldmap")
  for (pkg in req_pkgs) {
    install_if_missing(pkg)
  }
  
  # Load the countriesCoarse dataset
  data(countriesCoarse)
  info_countries <- as.data.frame(countriesCoarse)
  column_to_select <- c("NAME","LAT","LON","ISO_A3","REGION")
  
  # Function to search for country name indications
  search_att_inDF <- function(dff, att, Address) {
    if (att == "ABBREV") {
      Address1 <- gsub(".", " ", Address, fixed = TRUE)
      dff[[att]] <- str_trim(gsub(".", " ", dff[[att]], fixed = TRUE))
      Address <- Address1
    }
    exp_country <- paste0("\\b(", paste(dff[[att]], collapse = "|"), ")\\b")
    result <- unlist(str_extract_all(Address, exp_country))
    output0 <- setNames(data.frame(matrix(ncol = length(c("NAME","LAT","LON","ISO_A3","REGION")), nrow = 0), stringsAsFactors = FALSE),
                        c("NAME","LAT","LON","ISO_A3","REGION"))
    if (length(result) >= 1) {
      output <- info_countries[match(result, dff[[att]]), column_to_select]
      return(output)
    } else {
      return(output0)
    }
  }
  
  # Exception for 'UK' data info
  if (grepl("\\bUK\\b", Address)) { 
    Address <- gsub("\\bUK\\b", "United Kingdom", Address)
  }
  
  # Values to search for in different attributes
  value_to_search <- c("NAME", "NAME_SORT", "NAME_FORMA", "ABBREV", "ISO_A3")
  output1 <- data.frame()
  j <- 1
  while (j <= length(value_to_search)) {
    output <-  search_att_inDF(info_countries, value_to_search[j], Address)
    if (nrow(output) != 0) {
      output1 <- rbind(output1, output)
    }
    if (nrow(output1) == 0 & j == length(value_to_search)) {
      output[1,] <- c(NA, NA, NA, NA, NA)
      output1 <- output
    }
    j <- j + 1
  }
  
  names(output1)[1] <- "NAME_Country"
  output1 <- output1[!duplicated(output1), ]
  row.names(output1) <- NULL
  return(output1)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Test on text contain a single country
getCountryDetails("Department of Surgery, University of Toronto, Toronto, Ontario, Canada")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Test on text contain multiples countries

getCountryDetails("Department of Surgery, University of Toronto, Toronto, Ontario, Canada,
                   Department of Medicine, University of Verona, Verona, Italy
                   Radiodiagnosis, Government Stanley Medical College and Hospital, Chennai, IND")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Apply Function to a dataframe :e.g : df with data extracted from PubMed

My_df <- data.frame(title = c("title1", "title2", "title3","title4"),
                 
                 abstract = c("abstract1", "abstract2", "abstract3","abstract4"),
                 
                 affiliation = c("University of Sfax, Tunisia",
                                 
                                 "IBM Research, Dublin, Ireland",
                                 
                                 "Primary Health Care Corporation (PHCC), Doha, Qatar",
                                 
                                 "Adresses with more than one country, TUN and Sweden")
                 )
View(My_df)

# Get countries and infos of each affiliation as a list
My_df$Countries <- lapply(1:nrow(My_df), function(i) {
    row <- My_df[i, ]
    # Apply on the 3rd column
    getCountryDetails(row[3]) 
  })

#Expand the table df using base R function. no need to use dplyr or tidyverse for this!
My_New_Enhanced_df<- do.call('rbind', do.call('Map', c(data.frame, My_df)))
View(My_New_Enhanced_df)
