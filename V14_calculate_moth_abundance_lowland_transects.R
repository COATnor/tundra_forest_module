#### ------------------------------------------------------------------------------------------------------------ ####
###  CALCULATE STATE VARIABLE - V14_moth_abundance_lowland_transects
###  Date: 30.09.2025
###  Author: Hanna Böhner and Guro Bang Synnes (editor)
#### ------------------------------------------------------------------------------------------------------------ ####

## this script can be used to download datasets from the COAT data portal,
## calculate a state variable,
## create a new version of the state variable if necessary,
## and upload the sate variable to the data portal

## the development version of the ckanr package has to be installed (remotes::install_github("ropensci/ckanr"))

## ---------------------------------- ##
## SETUP
## ---------------------------------- ##

## clear workspace
rm(list = ls())

## load libraries, missing packages will be installed
if (!require('remotes')) install.packages('remotes')
if (!require('ckanr')) remotes::install_github("ropensci/ckanr"); library('ckanr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

## set up the connection to the COAT data portal
COAT_url <- "https://data.coat.no"  # write here the url to the COAT data portal
COAT_key <- ""  # write here your API key if you are a registered user

# the API can be found on you page on the COAT data portal (log in and click on your name in the upper right corner of the page)
# the use of an API key is necessary to create a package

ckanr_setup(url = COAT_url, key = COAT_key)


## ---------------------------------- ##
## DOWNLOAD DATA
## ---------------------------------- ##

## list all datasets available on the COAT data portal
package_list()

## function to fetch a dataset by name and version
fetch_dataset <- function(name, version) {
  pkg <- package_search(q = list(paste("name:", name, sep = "")), 
                        fq = list(paste("version:", version, sep = "")), 
                        include_private = TRUE)$results[[1]]
  
  urls <- pkg$resources %>% sapply('[[','url')
  filenames <- pkg$resources %>% sapply('[[','name')
  
  # filter out non-data files
  keep <- !grepl("coordinate|readme|aux", filenames)
  urls <- urls[keep]
  
  # download and read all files
  file_list <- lapply(urls, function(url) {
    ckan_fetch(url, store = "session", sep = ";", header = TRUE)
  })
  
  # combine files in the dataset
  dataset <- do.call(rbind, file_list)
  return(dataset)
}

## make a data frame with all files from v_insect_defoliators_density_varanger_v2 and t_insect_defoliators_density_island_mainland_v1
dataset1 <- fetch_dataset("v_insect_defoliators_density_varanger_v2", "2")
dataset2 <- fetch_dataset("t_insect_defoliators_density_island_mainland_v1", "1")

## combine the two data frames
myfile <- rbind(dataset1, dataset2)


## ---------------------------------- ##
## CALCULATE STATE VARIABLE
## ---------------------------------- ##

## get all unique years
years <- sort(unique(myfile$t_year))

## list to store file paths
summary_file_paths <- list()

## loop over each year and generate summary tables
for (yr in years) {
  
  ## filter for current year
  myfile_year <- myfile %>% filter(t_year == yr)
  
  ## find mean v_abundance and sd of v_abundance, assign NA when the whole group have v_abundance = NA and when sd cannot be calculated
  state_var_year <- myfile_year %>%
    group_by(sn_region, sn_locality, t_year, v_species) %>%
    summarise(
      v_abundance_mean = if (all(is.na(v_abundance))) NA_real_ else mean(v_abundance, na.rm = TRUE),
      v_abundance_sd   = if (all(is.na(v_abundance))) NA_real_ else sd(v_abundance, na.rm = TRUE),
      .groups = "drop"
    )
  
  ## define file name
  state_var_name <- paste0("V14_moth_abundance_lowland_transects_", yr, ".txt")
  file_path <- file.path(tempdir(), state_var_name)
  
  ## add table to the file path
  write.table(state_var_year, file_path, row.names = FALSE, sep = ";")
  
  ## store the file path for later uploading
  summary_file_paths[[as.character(yr)]] <- file_path
}


## ------------------------------------------ ##
## CREATE A NEW VERSION OF THE STATE VARIABLE
## ------------------------------------------ ##

## you can either create a new version of the state variable or add the data to an already existing state variable (then you can skip this part)

## search for your dataset
state_name <- ""  # write here the name including the version of the state variable you want to add data to
state_version <- ""    # write here the version of the state variable

pkg_state<-package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
pkg_state$name  # check the name

## modify metadata of the state variable
name_new <- ""  # write here the name of the new version (for example change v1 to v2)
version_new <- ""  # write here the new version

print(c(
  max(as.Date(dataset1$t_date, format = "%Y-%m-%d"), na.rm = TRUE),
  max(as.Date(dataset2$t_date, format = "%Y-%m-%d"), na.rm = TRUE)))  # check end date in both data frames

end_new <- ""  # write here the (new) end date of the dataset

pkg_state$datasets  # check with which dataset the current version of the state variable is associated
datasets_new <- "" # write here the name (including the version) of the dataset the should be associated with the new version of the state variable

# these are the typical modifications when creating a new version of a state variable before adding data of another year
# other modification can be made if necessary

# modify tags (necessary to avoid a validation error)
new_tags <- c()
for (i in 1:length(pkg$tags)){
  new_tags[[i]] <- list(name = pkg$tags[[i]]$name)
}

## create the new version
package_create(name = name_new,
               title = pkg_state$title,
               private = TRUE,  # this is default
               tags = new_tags,
               author = pkg_state$author,
               author_email = pkg_state$author_email,
               license_id = pkg_state$license_id, 
               notes = pkg_state$notes, 
               version = as.character(version_new), 
               owner_org = pkg_state$owner_org, 
               state = "active", 
               type = "dataset",
               extras = list(topic_category = pkg_state$topic_category, 
                             #position = pkg_state$position,
                             publisher = pkg_state$publisher,
                             associated_parties = pkg_state$associated_parties,
                             persons = pkg_state$persons,
                             temporal_start = pkg_state$temporal_start,
                             temporal_end = end_new,
                             location = pkg_state$location,
                             scientific_name = pkg_state$scientific_name,
                             scripts = pkg_state$scripts,
                             protocol = pkg_state$protocol,
                             bibliography = pkg_state$bibliography,
                             funding = pkg_state$funding,
                             datasets = datasets_new
                             #embargo = embargo_new
               ))


## ---------------------------------- ##
## UPLOAD THE STATE VARIABLE
## ---------------------------------- ##

## the state variable has to be created on www.data.coat.no

## search for your dataset
state_name <- "v14_moth_abundance_lowland_transects_v1"  # write here the name (small first letter) including the version of the state variable you want to add data to
state_version <- "1"    # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <-  pkg_state$resources %>% sapply('[[','name')  # get the file names
filenames_state   # are there any files? "list()" if not

## upload state variable files for each year
for (yr in names(summary_file_paths)) {
  resource_create(
    package_id = pkg_state$id,
    description = NULL,
    upload = summary_file_paths[[yr]],
    name = basename(summary_file_paths[[yr]]),
    http_method = "POST"
  )
}


## ---------------------------------- ##
## UPDATE THE STATE VARIABLE
## ---------------------------------- ##

## here you can update the metadata of the sate variable
## you can for example change the state from 'draft' to 'active (this is necessary if you created the state variable on data.coat.no and then added the data files via R)
## you can also change the visibility from private to public

pkg_state$name  # check that the name is correct

## save metadata of the package as a list (as = table -> but the object will be a list)
pkg_updated <- package_show(pkg_state$id, as = "table", http_method = "POST")  

## do the necessary modifications of the metadata
names(pkg_updated)  # show the names of all metadata fields that can be updated
#pkg_updated$private <- FALSE  # set private = FALSE to publish a dataset
pkg_updated$state <- "active"

## discard empty metadata fields (they will cause a validation error)
pkg_updated <- discard(pkg_updated, is.null)

## update the package
package_update(pkg_updated, pkg$id, http_method = "POST")
