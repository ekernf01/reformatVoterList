FINAL_COLNAMES = c("First Name",	"Last Name",	"Business Email",	"JHED")
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

isJHU("eric.kern13@gmail.com")
isJHU("eric.kern13@JH.edu")
isJHU("eric.kern13@jhu.edu")
isJHU("eric.kern13@jhmi.edu")
isJHU("eric.kern13@jh.edu")
keepJHU(c("eric.kern13@gmail.com", "ekernfe1@jh.edu"))

reformatVoterList = function(filename_in, filename_out){
  tryCatch(
    expr = {voters = readxl::read_excel(filename_in)},
    error = function(e) {stop("Sorry, this program could not read that file.")}
  )
  voters[["Business Email"]] =
    paste(voters[["Business Email"]], voters[["Personal Email"]], sep = "|") %>%
    strsplit(split = "\\|") %>%
    lapply(keepJHU) %>%
    sapply(paste0, collapse="|")
  voters = voters[FINAL_COLNAMES]
  voters[["Date/Time"]] = date()
  voters[["Uploaded?"]] = "reserved for Andrew"
  dir.create(showWarnings = F, path = dirname(filename_out))
  write.csv(voters, paste0(filename_out, ".csv"))
}
