library(httr)

detect_gender_with_namsor <- function(names) {
  base_url <- "https://v2.namsor.com/NamSorAPIv2/api2/json/genderBatch"
  # Replace my_api_key with your actual API key 
  api_key <- my_api_key #get an api key after quickly register: https://namsor.app/signup/
  # Convert the names into a list of objects with "firstName" field
  names_list <- lapply(names, function(name) {
    list(firstName = name)
  })
  # Construct the POST request payload
  payload <- list(personalNames = names_list)
  response <- POST(base_url, body = payload, encode = "json", add_headers("X-API-KEY" = api_key))
  
  # Make POST request to the API with API key in the header
  if (http_status(response)$category == "Success") { #check if request is with success status
    # Parse JSON response
    result <- content(response, "parsed")
    df <- lapply(result$personalNames, function(x) {
      data.frame(
        # script = x$script,
        # id = x$id,
        firstName = x$firstName,
        lastName = x$lastName,
        likelyGender = x$likelyGender,
        genderScale = x$genderScale,
        score = x$score,
        probabilityCalibrated = x$probabilityCalibrated
      )
    })
    # Combine the list of dataframes into one dataframe
    final_df <- do.call(rbind, df)
    return(final_df) 
    # return(final_df$likelyGender)  # to Extract likely gender from response
  }
  else{
    # Request failed
    print("Error: Unable to retrieve gender information.")
    return(rep(NA,length(names)))
  }
}

detect_gender_with_namsor(c("Ahmed",'Melissa','Paul',
                            'Shiyu','Choolwe','Shuhei'))