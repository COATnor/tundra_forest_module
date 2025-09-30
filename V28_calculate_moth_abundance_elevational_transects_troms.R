#### ------------------------------------------------------------------------------------------------------------ ####
###  CALCULATE STATE VARIABLE - V28_moth_abundance_elevational_transects_troms
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

## serach for your dataset
name <- "t_insect_defoliators_density_altitudinal_gradients_v1"  # write here the name including the version of the dataset you want to download
version <- "1"    # write here the version of the dataset

pkg<-package_search(q = list(paste("name:", name, sep = "")), fq = list(paste("version:", version, sep = "")), include_private = TRUE)$results[[1]] # search for the dataset and save the results
urls <- pkg$resources %>% sapply('[[','url')  # get the urls to the files included in the dataset
filenames <-  pkg$resources %>% sapply('[[','name')  # get the filenames

## keep only datafiles (discard readme, coordinate and aux files)
filenames
filenames <- filenames[!grepl("coordinate|readme|aux", filenames)]
urls
urls <- urls[!grepl("coordinate|readme|aux", urls)]

## download all files of the dataset
mylist <- c()  # empty object for the files

for (i in 1:length(urls)) {
  mylist[[i]] <- ckan_fetch(urls[i],
                            store = "session",
                            sep = ";", 
                            header = TRUE
  )
}

myfile <- do.call(rbind, mylist)


## ---------------------------------- ##
## CALCULATE STATE VARIABLE
## ---------------------------------- ##

state_var_name <- "V28_moth_abundance_elevational_transects_troms.txt"  # write here the filename of the sate variable (including format) 

state_var <- myfile %>% group_by(sn_region, sn_locality, sn_group_of_sites_spatial, t_year, v_species) %>%
  summarise(
  v_abundance_mean = if (all(is.na(v_abundance))) NA_real_ else mean(v_abundance, na.rm = TRUE),
  v_abundance_sd   = if (all(is.na(v_abundance))) NA_real_ else sd(v_abundance, na.rm = TRUE), .groups="drop")

## save the file to a temporary directory (necessary for uploading it)
write.table(state_var, paste(tempdir(), state_var_name, sep = "/"), row.names = FALSE, sep = ";")


## ------------------------------------------ ##
## CREATE A NEW VERSION OF THE STATE VARIABLE
## ------------------------------------------ ##

## you can either create a new version of the state variable or add the data to a already existing state variable (then you can skip this part)

## search for your dataset
state_name <- "v28_moth_abundance_elevational_transects_troms_v1"  # write here the name including the version of the state variable you want to add data to
state_version <- "1"    # write here the version of the state variable

pkg_state<-package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
pkg_state$name  # check the name

## modify metadata of the state variable
name_new <- ""  # write here the name of the new version (for example change v1 to v2)
version_new <- ""  # write here the new version
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
state_name <- "v28_moth_abundance_elevational_transects_troms_v1"  # write here the name including the version of the state variable you want to add data to
state_version <- "1"    # write here the version of the state variable

pkg_state<-package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <-  pkg_state$resources %>% sapply('[[','name')  # get the filenames
filenames_state   # are there any files? "list()" if not

resource_create(package_id = pkg_state$id, 
                description = NULL, 
                upload = paste(tempdir(), state_var_name, sep = "/"),
                name = state_var_name,
                http_method = "POST")


## ---------------------------------- ##
## UPDATE THE STATE VARIABLE
## ---------------------------------- ##

## here you can update the metadata of the sate variable
## you can for example change the sate from 'draft' to 'active (this is necessary if you created the state variable on data.coat.no and then added the datafiles via R)
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
