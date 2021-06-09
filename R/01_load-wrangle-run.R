
# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')


# 01 load-data ---------------------------------------------------------------

dat <- readxl::read_xls(here::here('data', 'FILE-NAME.xlsx'),
                        sheet = 'NAME-OF-SHEET-IF-NECESSARY') %>%
       janitor::clean_names()


# 02 create dilution plot ----------------------------------------------------

dat %>%
  ggplot(mapping = aes())
