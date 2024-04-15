#' @importFrom magrittr "%>%"
FINAL_COLNAMES = c("First Name",  "Last Name",  "Business Email", "JHED")
library(magrittr)

isJHU = function(email){
  email %>%
    gsub(".*@", "", .) %>%
    tolower %>%
    is.element(c("jh.edu","jhu.edu","jhmi.edu"))
}

keepJHU = function(emails){
  emails[isJHU(emails)]
}

returnifjhu <- function(emaillist) {
  return(tryCatch(magrittr::extract2(emaillist,1), error=function(e) NA))
}

isJHU("eric.kern13@gmail.com")
isJHU("eric.kern13@JH.edu")
isJHU("eric.kern13@jhu.edu")
isJHU("eric.kern13@jhmi.edu")
isJHU("eric.kern13@jh.edu")
keepJHU(c("eric.kern13@gmail.com", "ekernfe1@jh.edu"))

#' Reformat a list of new voters. For internal use during the 2024 April TRU-UE contract ratification vote.
#'
#' @export
reformatVoterList = function(filename_in, filename_out){
  tryCatch(
    expr = {voters = readxl::read_excel(filename_in)},
    error = function(...) {stop("Sorry, this program could not read that file.")}
  )
  voters[["Business Email"]] =
    paste(voters[["Business Email"]], voters[["Personal Email"]], sep = "|") %>%
    strsplit(split = "\\|") %>%
    lapply(keepJHU) %>%
    sapply(returnifjhu)
  voters = voters[FINAL_COLNAMES]
  voters[["Date/Time"]] = date()
  voters[["Uploaded?"]] = "reserved for Andrew"
  dir.create(showWarnings = F, path = dirname(filename_out))
  write.csv(voters, paste0(filename_out, ".csv"))
}
