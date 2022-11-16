#*****************************************************************
#* TITLE:   Script to publish the MUX code, raw data, and predictions
#*          on EDI using the EDI assemblyline tools           
#* AUTHORS:  Nicholas W. Hammond                                          
#* DATE:   Created 24 October 2022                          
#* NOTES:  Goal of this script is to develop an EDI data publication
#*         that can be cited in a manuscript for the MUX 2020 and 2021 deployments
#*         EDI data package entities include:
#*         - Raw MUX FP data file  
#*         - MUX data processing script
#*         - MUX PLSR analysis script
#*         - PLSR predictions of total and soluble Fe and Mn w/ 90% PI's
#*         
#*****************************************************************
# useful ref for learning more about publishing data in EDI: 
#https://github.com/EDIorg/EMLassemblyline/blob/master/documentation/instructions.md

# # Install & load devtools before installing updated version of EDI's EMLassemblyline package
 install.packages("devtools")
 library(devtools)
devtools::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
library(tidyverse)

#Step 1: Create a directory for your dataset, here "EDI_publishing" within the main repo

#Step 2: Move your dataset to the directory


#now set files to the correct directory
setwd("./MagicData/MUX/EDI Publishing/")

#Step 3: Create an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset

#Step 5: Import the core metadata templates to your local directory
template_core_metadata(path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
                       license = "CCBY",
                       file.type = ".txt",
                       write.file = TRUE)

template_table_attributes(path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
                          data.path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
                          data.table = c("MUX_FP_TS.csv", 
                                         "MUX_predictions_boot.csv"),
                          write.file = TRUE)
#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
template_geographic_coverage(path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
                             data.path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
                             data.table = c("MUX_FP_TS.csv", 
                                            "MUX_predictions_boot.csv"),
                             empty = TRUE,
                             write.file = TRUE)

# Edit each of these imported template files for your current data package upload, by copying and 
# pasting the relevant information from the EDI_metadata_template you prepared

# Important! Before saving, check that the contents of each .txt file do not include any 
# non-allowed characters by going to: https://pteo.paranoiaworks.mobi/diacriticsremover/, 
# pasting your text, and clicking remove diacritics. copy and paste that text back into the .txt file.

view_unit_dictionary()

# After saving each file, make sure it is closed.

#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
#nothing mandatory for Carey Lab in this section

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#see the LabKeywords.txt file for keywords that are mandatory for all Carey Lab data products

#Step 11: Personnel
#copy-paste this information in from your metadata document, Excel is easiest
#Cayelan needs to be listed several times; she has to be listed separately for her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)

#Step 12: Attributes
#grab attribute names and definitions from your metadata word document
#for units....
# View and search the standard units dictionary
view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical


#Step 13: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 14: Categorical variables
# Run this function for your dataset
#THIS WILL ONLY WORK once you have filled out the attributes_chemistry.txt and
#identified which variables are categorical
template_categorical_variables(path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
                               data.path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
                               write.file = TRUE)

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file (or geographic_coverage.txt file) that is Carey Lab specific into your working directory

## Step 16: Obtain a package.id. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords.

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below


#Step 17: Make EML
# View documentation for this function
?make_eml

# Run this function
make_eml(
  path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
  data.path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
  eml.path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
  dataset.title = "Time series of in situ ultraviolet-visible absorbance spectra and high-frequency predictions of total and soluble Fe and Mn concentrations measured at multiple depths in Falling Creek Reservoir (Vinton, VA, USA) in 2020 and 2021",
  temporal.coverage = c("2020-10-16", "2021-06-21"),
  maintenance.description = 'ongoing',
  data.table = c("MUX_FP_TS.csv", 
                 "MUX_predictions_boot.csv"),
  data.table.description = c("time series of uv-vis absorbance spectra",
                             "time series of predicted Fe and Mn concentrations"),
  other.entity = c("1_Overlaps_MUX.R","2_PLSR_MUX_bootstrap.R", "PLSR_MUX_functions.R"),
  other.entity.description = c("Script for aggregating, cleaning, and preparing raw data for PLSR analysis","PLSR Analysis Script",
                               "Script containing source functions required to run PLSR"),
  user.id = 'mschreib',
  user.domain = 'EDI',
  package.id = 'edi.974.4')

## Step 8: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to 
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked 
# for errors. If there are no errors, your data product is now published! 
# If there were errors, click the link to see what they were, then fix errors 
# in the xml file. 
# Note that each revision results in the xml file increasing one value 
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the 
# evaluation check again, until you receive a message with no errors.

## Step 9: PUBLISH YOUR DATA! ####
# Reserve a package.id for your error-free data package. 
# NEVER ASSIGN this identifier to a staging environment package.
# Go to the EDI Production environment (https://portal.edirepository.org/nis/home.jsp)
# and login using the ccarey (permanent) credentials. 

# Select Tools --> Data Package Identifier Reservations and click "Reserve Next 
# Available Identifier". A new value will appear in the "Current data package 
# identifier reservations" table (e.g., edi.518)
# This will be your PUBLISHED package.id

# In the make_eml command below, change the package.id to match your 
# PUBLISHED package id. This id should end in .1 (e.g., edi.518.1)

# ALL OTHER entries in the make_eml() command should match what you ran above,
# in step 7

make_eml(
  path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
  data.path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
  eml.path = "C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/MUX/EDI Publishing/",
  dataset.title = "Time series of total and soluble iron and manganese concentrations from Falling Creek Reservoir and Beaverdam Reservoir in southwestern Virginia, USA from 2014 through 2021",
  temporal.coverage = c("2014-04-01", "2021-12-06"),
  maintenance.description = 'ongoing',
  data.table = "Metals_2014_2021.csv",
  data.table.description = "Reservoir iron and manganese chemistry dataset",
  other.entity = "Metals_QAQC.R",
  other.entity.description = "QAQC script",
  user.id = 'mschreib',
  user.domain = 'EDI',
  package.id = 'edi.455.6')

# Once your xml file with your PUBLISHED package.id is Done, return to the 
# EDI Production environment (https://portal.edirepository.org/nis/home.jsp)

# Select Tools --> Preview Your Metadata, then upload your metadata (.xml) file 
# associated with your PUBLISHED package.id. Look through the rendered 
# metadata one more time to check for mistakes (author order, bounding box, etc.)

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file associated with your PUBLISHED package.id 
# (e.g., edi.518.1.xml), check "I want to manually upload the data by selecting 
# files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked for 
# errors. Since you checked for and fixed errors in the staging environment, this 
# should run without errors, and your data product is now published! 

# Click the package.id hyperlink to view your final product! HOORAY!