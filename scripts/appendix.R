##########################################################
# APPENDIX
##########################################################
# --------------------------------------------------------
# APPENDIX 1
# --------------------------------------------------------
# load data description csv file
dd <- read_csv(here("dataset/data_description.csv"))


# display the information under table format
dd %>%
  kbl() %>%
  kable_styling(full_width = F)

