#######################################################################
# Master script
#######################################################################

## Clear out existing environment
rm(list = ls()) 
gc()

dropbox_dir <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData"

mdir <- here::here("GiT/arabs-in-america")
workdir  <- "/Users/hhadah/Documents/GiT/arabs-in-america" # working files and end data
rawdatadir  <- paste0(workdir,"/data/raw")
datasets  <- paste0(workdir,"/data/datasets")
thesis_tabs <- paste0(workdir,"/my_paper/tables")
thesis_plots <- paste0(workdir,"/my_paper/figure")
tables_wd <- paste0(workdir,"/output/tables")
figures_wd <- paste0(workdir,"/output/figures")
programs <- paste0(workdir,"/programs")
# pres_tabs <- paste0(workdir,"/presentations/oct-02-health/tables")
# pres_figs <- paste0(workdir,"/presentations/oct-02-health/figures")


options(modelsummary_factory_latex = "kableExtra")

## Set master directory where all sub-directories are located
# mdir <- "/Users/hhadah/Dropbox/Research/My Research Data and Ideas/Identification_Paper"
# mdir <- "C:\\Users\\16023\\Dropbox\\Research\\My Research Data and Ideas\\Identification_Paper"
## Set working directories
# workdir  <- paste0(mdir,"/Data/Datasets")
# rawdatadir  <- paste0(mdir,"/Data/Raw")

### run do files and scripts

source(file.path(programs,"01_packages_wds.R")) # set up packages
source(file.path(programs,"02-data-cleaning.R")) # data cleaning
source(file.path(programs,"03-counting-arabs.R")) # counting arabs
source(file.path(programs,"04-creating-ses-var.R")) # creating ses var
source(file.path(programs,"05-plots-ses.R")) # plots ses
