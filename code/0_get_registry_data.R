# This script checks whether raw registry data for IV1 & IV2 has been downloaded from zenodo (LINK) and unzipped, and if not, does so. Raw data stored on zenodo due to large size.
# 
# Registries:
# - ClinicalTrials.gov: downloaded via AACT from https://aact.ctti-clinicaltrials.org/pipe_files. AACT dataset, which brings the CT.gov database content in an easily reusable format.
# - DRKS: downloaded via DRKS, pre-filtered for completion years and study status as well as Germany as 'Country of recruitment' version of the dataset was downloaded from the DRKS database
# 
# Registries serve as living databases, and their data are subject to change. Data for these analyses were downloaded on the following dates
# - IV1: AACT (2017-04-17) and DRKS (2017-07-27)
# - IV2: AACT (2020-06-03) and DRKS (2020-06-03)
# 
# See zenodo for additional information about the data

dir_raw_reg <- fs::path_wd("data", "raw-registries")
zip_raw_reg <- fs::path_wd("data", "raw-registries.zip")

# if no unzipped `raw-registries` with expected subfiles exists
if (
  !all(
    fs::file_exists(c(
      fs::path(dir_raw_reg, "2017-07-27_drks.csv"),
      fs::path(dir_raw_reg, "2020-06-03_drks.csv")
    )),
    
    fs::dir_exists(c(
      fs::path(dir_raw_reg, "2017-04-17_ctgov"),
      fs::path(dir_raw_reg, "2020-06-03_ctgov")
    ))
  )
) { if (!fs::file_exists(zip_raw_reg)) {
  
  # 1) check if zip downloaded and otherwise download
  message("Downloading raw registry data...")
  
  # Try to download zip file and, if fails, prompt user to manually download
  download_zip <- try(
    download.file("https://zenodo.org/record/7633995/files/raw-registries.zip",zip_raw_reg),
    silent = TRUE
  )
  
  # Download failure
  if (class(download_zip) == "try-error"){
    if (fs::file_exists(zip_raw_reg)) {fs::file_delete(zip_raw_reg)}
    stop("Download failed! Please manually download: navigate to https://doi.org/10.5281/zenodo.7633995 --> click `Download` by `raw-registries.zip` --> move zip file to `data` directory in this project")
  }
  
  # Download success
  if (class(download_zip) != "try-error"){
    message("Raw registry data successfully downloaded")
  }
  
}
  
  # 2) unzip `raw-registries`
  message("Unzipping raw registry data...")
  
  # Try to unzip zip file and, if fails, prompt user to manually unzip
  unzip_zip <- try(archive::archive_extract(zip_raw_reg, fs::path_wd("data")), silent = TRUE)
  
  # Unzip failure
  if (class(unzip_zip) == "try-error"){
    if (fs::dir_exists(dir_raw_reg)) {fs::dir_delete(dir_raw_reg)}
    stop("Unzip failed! Please manually unzip in `raw-registries.zip` in `data` directory in this project")
  }
  
  # Unzip success
  if (class(unzip_zip) != "try-error"){
    message("Raw registry data successfully unzipped")
  }
}
