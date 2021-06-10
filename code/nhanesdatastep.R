
library(tidyverse) # data management (tidyr, dplyr) and plots (ggplot2)
library(foreign) # reading xport files

# Import data from NHANES page
# https://wwwn.cdc.gov/nchs/data/tutorials/file_download_import_R.R
# For this analysis we need the following files:

# 1. Demographics - demo and weighting variables

# Create temporary file to catch web data
demofile = tempfile()

# download data into temporary file using URL; right-click on file name to get URL
# Since we created tempfile, we need to specify it as the destination file
# You can specify other destfile instead and it will save it to your working directory

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT", 
              destfile = demofile,
              mode = "wb") # use 'wb' to read as binary file

# Now read the xport data into an R data frame
demodf = read.xport(demofile)  # using foreign package (file must be binary format)


# 2. Mental health

mhfile = tempfile()

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DPQ_J.XPT",
              destfile = mhfile,
              mode = 'wb')

mhdf = read.xport(mhfile)


# 3. Diabetes
diabfile = tempfile()

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DIQ_J.XPT",
              destfile = diabfile,
              mode = "wb")
diabdf = read.xport(diabfile)

# Blood sugar
a1cfile = tempfile()

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/GHB_J.XPT",
              destfile = a1cfile,
              mode = "wb")
a1cdf = read.xport(a1cfile)

# 4. Smoking
smokefile = tempfile()
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SMQ_J.XPT",
              destfile = smokefile,
              mode = "wb")
smokedf = read.xport(smokefile)

# 5. BMI
bmifile = tempfile()
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT",
              destfile = bmifile,
              mode = "wb")
bmidf = read.xport(bmifile)



###############
# Stepwise merging
#nhanesdata = inner_join(demodf, mhdf, by="SEQN")
#nhanesdata = inner_join(nhanesdata, diabdf, by="SEQN")
#nhanesdata = left_join(nhanesdata, smokedf, by="SEQN")
###############


# Merging using "piping" + additional data manipulation
nhanesdata2 = inner_join(demodf, mhdf, by="SEQN") %>% 
  left_join(diabdf, by="SEQN") %>%    # keep all common to demo and MH
  left_join(a1cdf, by="SEQN") %>%      
  left_join(smokedf, by="SEQN") %>% 
  left_join(bmidf, by='SEQN')



nhanesdata2 = nhanesdata2 %>%
  
  select(SEQN, 
         RIAGENDR, # gender
         RIDAGEYR, # age
         INDHHIN2, # household income
         starts_with("DPQ"),  # MH screener questions
         DIQ010,   # doctor diagnosed diabetes
         LBXGH,    # A1c lab data
         SMQ020,   # Smoked at least 100 cigarettes in life
         BMXBMI,
         
         #Survey design variables
         SDMVPSU,
         SDMVSTRA,
         WTMEC2YR) %>% 
  
  # Transform/modify variables
  mutate_at(c("RIAGENDR", "DIQ010", "SMQ020"),
            na_if, 7) %>%
  mutate_at(c("RIAGENDR", "DIQ010", "SMQ020"),
            na_if, 9) %>%
  mutate_at(vars(starts_with("DPQ")),
            na_if, 7) %>%
  mutate_at(vars(starts_with("DPQ")),
            na_if, 9) %>%

  
  ############################################################
  # Exercise: Household income uses 77 and 99 for "refused"
  # and "Don't know". Change these codes to NA is above.
  ############################################################

  # Transform individual variables
  # Depression scale total -> dichotomous variable
  # rowSums(c("col1", "col2"), na.rm = T)
  
  # rowwise can be used instead

  mutate(deptot = rowSums(select(., starts_with("DPQ")), na.rm = T)) %>%
  mutate(depressed = factor(deptot > 9, labels = c("Min/mild", "Mod/Severe"))) %>%

 
  # Diabetes (diagnosed): make into factor (1=yes, 2=no), make "Borderline" (3) as Yes (1)
  
  mutate(diabetes = replace(DIQ010, which(DIQ010==3), 1)) %>%
  mutate(diabetes = factor(diabetes, labels = c("Yes", "No"))) %>%
  
  #############################################################
  # ## Exercise: Diabetes (screened) - create binary variable
  # for whether A1c > 6%.##
  #############################################################

  # Smoking: change to factor
  mutate(SMQ020 = factor(SMQ020, labels = c("Yes", "No"))) %>%
  
  # Age: cut into tertiles, make factor and add labels
  mutate(age_cat = cut(RIDAGEYR, 
                       breaks = quantile(RIDAGEYR, probs = c(0, 1/3, 2/3, 1)), 
                       include.lowest = T, 
                       labels = c("18-39", "40-61", "62+"))) %>% #includes 80+
  
  # Collapse HH Income categories
  mutate(income = case_when(
    INDHHIN2 %in% c(1:4, 13) ~ "0-20K",
    INDHHIN2 %in% c(5:8, 12) ~ "20-55K",
    INDHHIN2 %in% c(9, 10, 14,15) ~ "55-100K+"
  )) %>%
  
  # Gender
  mutate(RIAGENDR = factor(RIAGENDR, 
                           labels = c('Male', 'Female'))) %>%

  ###########################################################
  # Exercise: Locate and merge the data set containing BMI
  # and convert it to a categorical variable with 4 levels.
  # The description of the levels can be found here:
  #  https://www.ncbi.nlm.nih.gov/books/NBK278991/table/diet-treatment-obes.table4clas/
  ###########################################################
  
  # filter based on inclusion criteria
  filter(!is.na(diabetes)) %>%
  
  
   # BMI variable
  mutate(bmi_cat = cut(BMXBMI,
                       breaks = c(14.2, 24.6, 29.69, 33.5, 86.2),
                       labels = c("14.2-24.6", "24.6-29.69", "29.69-33.5", "33.5-86.2"), 
                       include.lowest = T)) %>%
  filter(BMXBMI >= 24.6)




# Save to local drive
save(nhanesdata2, file="nhanesdata.RData")
  
# Use load() to retrieve saved data  
  





