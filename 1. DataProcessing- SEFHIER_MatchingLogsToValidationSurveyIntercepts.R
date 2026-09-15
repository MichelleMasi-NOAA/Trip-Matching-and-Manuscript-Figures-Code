#This code matches Compares Validation Survey Data to Logbook Data and then matches the correct survey to the trip logbook
  #Assumptions:
    #Use just 2022 for this analysis, since survey ended Feb 2023 and little to no sampling in Jan/Feb 2023
    #filter to only keep logbooks ending in the Gulf states, since they cant be intercepted by Gulf surveyors otherwise
    #REMOVE: filter out logbooks from vessels that dont hold Gulf permits, as surveyors told to only intercept those with Gulf for-hire permits (per the regs)
  #(2) Format/Process data sets to create comparable field names; join to permits data for analysis/matching - 
        #notes: SEFHIER logbook data is duplicated by trip_ID for each catch sequence of the same logbook (i.e., species 1 with be listed on row 1, subsequent species caught/discarded will make up the subsequent rows for each duplicated Trip_ID) 
                #SEFHIER Validation survey data is split into 3 files, with catch and effort files separate from the survey fields files (e.g. trip time, hours fished, etc)
                #therefore, in this case it was easier to match on common fields, then post-match come back and grab the catch and effort cols from the logbook and survey for each matched pair (this was also more computationally efficient, when matching using the nested for-loop)

## 1.0 Upload Data ----

#### 1.1 Libraries ----
#install tidyverse for dplyr and lubridate (the goat)
library(tidyverse)
#install to read excel directly rather than csv
library(readxl)
#library(ROracle) #to pull data from Oracle
library(reshape2) #to melt data for ggplotting
library(nmfspalette)  #for nmfs color palette
# install.packages("stringdist") # uncomment this line if you don't have the package
library(stringdist)
library(zoo) #for mutating vectors
library(pastecs) #for summary stats
library(stringr) #to look for odd characters in excel input data
library(lubridate) #for converting military to standard R time format = hrs:mins:sec
library(yardstick) #for comparing similiarity score distributions to ID threshold values
library(viridis) # For colorblind-friendly scales

#Use this to avoid converting to scientific notation
options(scipen = 999)

### 1.2 Get Data ----
## Path setup ----
#create path to your working and output directory - where do you keep the data on your computer?
#michelle
Michelles_path <- "C:/Users/michelle.masi/Documents/SEFHIER/R code/Validation Survey data and analyses/Matching Logbooks to Validation Survey Intercepts/include all permit types/"


# !! Change to your path !!
Path <- Michelles_path

# create these folders in your directory first
Inputs <- "Inputs"
Outputs <- "Outputs"


#### 1.5 Read in Data ----
###Load Validation Survey Data ----
#(note this is for 10/2021-02/2023, so will need to filter for just 2022) 
#These are the data we're working with (data originally found here: C:\Users\michelle.masi\Documents\SEFHIER\R code\Validation Survey data and analyses\Validation Survey Data - from GulfFIN\Merged Validation Survey Data)
i1 <-read_excel(paste0(Path,Inputs,"/i1_10_21to02_23.xlsx"))

  #check if all surveys in file have STATUS == 1, since STATUS=5 will indicate a refusal and we should exclude counting a row if its a refussal
  unique(i1$STATUS)
  #1

#read in SEFHIER_sites files too bc some States are NA in the survey data
SEFHIER_Site_Codes <- read_excel(paste0(Path,Inputs,"/SEFHIER_Sites.xlsx"))
  #reduce to just needed cols for analysis, and only keep distinct state and county combos
  SEFHIER_Site_Codes_short <- SEFHIER_Site_Codes %>% 
    rename(State = ST_POSTAL) %>% #rename ST_CODE to match i1_10_21to02_23 state code col name (for later merging)
    select(State,CNTY_CODE,COUNTY) %>% 
    distinct(State,CNTY_CODE, .keep_all = TRUE)
    #convert CNTY_CODE col to numeric to  for analysis
    SEFHIER_Site_Codes_short$CNTY_CODE <- as.numeric(SEFHIER_Site_Codes_short$CNTY_CODE)

####read in I2 (survey discard) file
#(data was originally from here: C:\Users\michelle.masi\Documents\SEFHIER\R code\Validation Survey data and analyses\Validation Survey Data - from GulfFIN\Merged Validation Survey Data)
SurveyDiscards <- read_excel(paste0(Michelles_path, Inputs, "/i2_10_21to02_23.xlsx"))
####read in I3 (survey retained catch) file
#(data was originally from here: C:\Users\michelle.masi\Documents\SEFHIER\R code\Validation Survey data and analyses\Validation Survey Data - from GulfFIN\Merged Validation Survey Data)
SurveyRetainedCatch <- read_excel(paste0(Michelles_path, Inputs, "/i3_10_21to02_23.xlsx"))

###Load Logbook data ----
#(must save after running code above in PC version first, then can upload saved rds file for later)
Logbooks_annual = readRDS(paste0(Path, Inputs, "/RawLogs2022.rds"))

###Load Permit data ----
#Use Processed Permit Data (note, this might include a week before and after 2022 - but it should be arbitrary in this analysis)
perm <- readRDS(paste0(Path, Inputs, "/Permitted_vessels_nonSRHS_2022_plusfringedates.rds"))



## 2.0 Format Data ----

### 2.1 Format Permit Data ----
## Select necessary permit data (Vessel number, vessel name, permits) 
## Rename columns for matching
perms<- perm %>% mutate(vsl_num=VESSEL_OFFICIAL_NUMBER,
                        vsl_name=VESSEL_NAME,
                        SA_PERM=SA_PERMITS_,
                        GOM_PERM=GOM_PERMITS_,
                        Perm_Group=PERMIT_REGION) %>%
  ## Select those columns
  select(vsl_num,vsl_name,SA_PERM, GOM_PERM,Perm_Group)

### 2.2 Format Validation Survey Data ----
####Raw Survey Data Processing ---- 
i1_formated_2022 <- i1 %>% mutate(#ID code for determining date
                             id_code=format(ID_CODE,scientific=FALSE),
                             asg_code=substr(id_code,1,13),
                             #Pull Date from ID Code
                             #We need the ifelse because some id codes still have
                             #scientific notation which throws off the substr alignment of
                             #the dates
                             Year= ifelse(str_detect(id_code, "E"), substr(id_code,7,10),
                                                     substr(id_code,6,9)),
                             Month= ifelse(str_detect(id_code, "E"), substr(id_code,11,12),
                                                      substr(id_code,10,11)),
                             Day = ifelse(str_detect(id_code, "E"), substr(id_code,13,14),
                                                     substr(id_code,12,13)),
                             #Recreate date 
                             Date= as.Date(paste0(Year,"-",Month,"-",Day)),
                             DateTime= as_datetime(paste0(Date," ",TIME),format="%Y-%m-%d %H%M"),
                             TripEndRangeA= DateTime,
                             #Clean up vessel number
                             vsl_num=toupper(VSL_NUM),
                             vsl_num=gsub(" ", "", vsl_num),
                             vsl_num= gsub("\\..*","", vsl_num),
                             # #Create unique ID (more descriptive then ID Code)
                             idint= paste0(INTSITE,"_",vsl_num,"_",Date,"_",TIME),
                             id= paste0(vsl_num,"_",Date)) %>%
  
                          #only keep 2022 survey data
                          filter(Year == 2022) %>%

                          #only keep rows that are distinct survey IDs (not duplicates)
                          distinct(id, .keep_all = TRUE) 
                              
                         #check number of surveys
                         length(unique(i1_formated_2022$ID_CODE)) #1812 (vs 2007 bc filtered for only those in 2022 & only distinct surveys - exclude duplicates)
                         

####Merge Permit and Validation DFs ----
## Join I1 and Permit data (here we add cols comp_ints (which is a flag: 1 = some catch data exists for the intercept record), and permit cols: state, vls_name in PIMS based on vsl_num, and cols for flags if they have SA or Gulf Permits)
i1_formated_2022_wPerms <- i1_formated_2022 %>% 
                                ## NO TYPE 2 OR TYPE 3 INTS (type 2 intercept col = number of records of released catch for the intercept; typ3 = number retained records for that intercept)
                                mutate(
                                  # CONVERT MONTH TO NUMERIC
                                  Month = as.numeric(Month),
                                  #deal with no catch issues
                                  nodat_ints=case_when((NUM_TYP2==0 & NUM_TYP3==0) ~ 1, TRUE ~ 0),
                                       ## RECORDS WITH SOME CATCH INFORMATION
                                       comp_ints=1,
                                       ## Identify states with words rather than numbers
                                       State=case_when(ST=="1" ~ "AL", 
                                                       ST=="12" ~ "FL", 
                                                       ST=="22" ~ "LA",
                                                       ST=="28" ~ "MS", 
                                                       ST=="48" ~ "TX",
                                #some State names need to be manually renamed, as still NA (add all possible cnty codes - for future analysis)
                                                      CNTY %in% c(3,97) ~ "AL",
                                                      CNTY %in% c(45,47,59) ~ "MS",
                                                      CNTY %in% c(7,39,61,167,315,355,489) ~ "TX", #57 overlaps with LA
                                                      CNTY %in% c(5,9,11,15,17,19,21,29,31,33,35,37,45,53,61,71,81,85,86,87,89,91,99,101,103,111,113,115,123,127,129,131) ~ "FL", #57, 75 and 109 overlapa with TX and LA
                                                      CNTY %in% c(51) ~ "LA", #57, 75 and 109 overlap with TX and FL
                                #for remaining States 57, 75 and 109 need to ID state code using INTSITE code
                                                      CNTY %in% c(57,75,109) & INTSITE %in% c(6,232,311) ~"LA",
                                                      CNTY %in% c(57,75,109) & INTSITE %in% c(4001,4002,4021) ~"TX",
                                                      CNTY %in% c(57,75,109) & INTSITE == 323 ~ "FL")) %>%
                                                       
                                ## KEEP ONLY 2022 DATA 
                                filter(YEAR=="2022") %>%
                                
                                ## JOIN THE PERMIT DATA WITH THE VALIDATION SURVEY DATA RECORDS
                                left_join(perms,by="vsl_num") %>%
                         
                         #after running the code and getting final_df, I realized that there are a large number of surveys that may be from non-fed permitted vesssels
                         #on lines 883-897, error checking found that vessels without permits but surveyed are not reporting logbooks (288 total surveys found when GOM_PERM is not Y, and none have logbooks for 2022)
                         #therefore, here I am removing those 288 surveys, as having arbitrary surveys deflates the match efficiency in the final_df
                         filter(GOM_PERM == "Y") #only keep rows where they held a gulf for-hire permit
                         
                         
      #check inserting state abbreviations worked
      unique(i1_formated_2022_wPerms$State) #"AL" "FL" "TX" "LA" "MS"
      #check there are no more NAs in State field 
      x<- i1_formated_2022_wPerms %>% filter(!State %in% c("AL","FL","TX","LA","MS")) %>%
        filter(CNTY %in% c(57,75,109)) %>%
        select(INTSITE,ST,CNTY, State)
          #check it worked
          unique(x$INTSITE) #none found, so it worked!
                         

#Now rename the CNTY col to match the SEFHIER_Sites col name CNTY_CODE, for later merging
colnames(i1_formated_2022_wPerms)[colnames(i1_formated_2022_wPerms) == "CNTY"] <- "CNTY_CODE"

#add the county codes
i1_formated_2022_wPerms_CountyAdded <- left_join(i1_formated_2022_wPerms, 
                                                 SEFHIER_Site_Codes_short, 
                                                 by = c("State","CNTY_CODE"))
#check number of surveys
length(unique(i1_formated_2022_wPerms_CountyAdded$ID_CODE)) #1528 ; was 1812 before removing rows of surveys from vessels where GOM_PERM == N or NA; now 1528 survey records


#filter out SRHS vessels from survey bc not in logbook DF (SRHS == 1 means its a headboat survey vessel)?
#filter(!SRHS == 1) %>%
#now only retain needed cols for matching
i1_formated_2022_wPerms_CountyAdded_short <- i1_formated_2022_wPerms_CountyAdded %>%
  select(ID_CODE, COUNTY, State, PEOPLE_FISHING, HRSF, Date, Day, Month, 
         Year, TIME, NO_HARVESTED_SELECTED, vsl_num, vsl_name)  %>% #NO_HARVEST_SELECTED == 1 means no catch for the trip (need to recode this to be 1 = something caught)
  
  # ADD THIS STEP to Convert Hours to MINS: Convert 4-digit survey times to total minutes since midnight
  mutate(TIME = as.numeric(TIME) %/% 100 * 60 + as.numeric(TIME) %% 100) %>%
  
  #create new field "harvest" == 1 when no_harvest flag == 0 
  mutate(Anything_Caught_Flag = case_when(NO_HARVESTED_SELECTED == 2 ~ 1,  #now 1 = there was catch
                                          NO_HARVESTED_SELECTED == 1 ~ 0)) %>% #now 0 = there was no catch
  
  #replace NAs with zeros for all numeric cols - no NAs found in this DF, so this is just precautionary for future data
  replace_na(list(PEOPLE_FISHING = 0, HRSF = 0, Day = 0, Month = 0, 
                  Year = 0, NO_HARVESTED_SELECTED = 2)) %>%  #set no harvest to 2 so its not conflicting with 1= catch and 0 = no catch
  
  #round hours fished for matching (using ceiling rounds values over 0.01 up to next whole number, and below down) - this data is only in 30 min increments so works ok here 
  mutate(Hours_Fished = round(ceiling(HRSF),0)) %>% 
  
  #remove NO_HARVESTED_SELECTED and only keep Anything_caught_flag
  select(-NO_HARVESTED_SELECTED,-HRSF) %>%
  
  #now convert data formats to match logbook DF col formats
  mutate(Anything_Caught_Flag = as.integer(Anything_Caught_Flag)) %>%
  
  #now ensure all character variables are uppercase for matching to logbook character fields
  mutate(COUNTY = toupper(COUNTY),
         State = toupper(State),
         vsl_name = toupper(vsl_name))



#rename survey DF cols for matching
names(i1_formated_2022_wPerms_CountyAdded_short) <- c("ID_CODE","County","State","Num_Anglers",
                                                      "Full_Date","Day","Month","Year","TIME",
                                                      "Vessel_Official_Num","Vessel_Name",
                                                      "Anything_Caught_Flag","Hours_Fished")

#check data format
glimpse(i1_formated_2022_wPerms_CountyAdded_short)
nrow(i1_formated_2022_wPerms_CountyAdded_short) #1528 - still have all surveys


#arrange DF by survey date and add survey row ID col to logbook DF
Surveys2022_Short_wIDs <- i1_formated_2022_wPerms_CountyAdded_short %>%
  arrange(Full_Date) %>% #arrange DF in descending order of date
  mutate(Survey_RowID = row_number())


####Add survey catch data ----

#####First Add Discard Data ----
# Filter rows where 'ID_CODE' contains any non-alphanumeric character
df_special_chars <- SurveyDiscards %>%
  filter(str_detect(ID_CODE, "[^[:alnum:]]"))

#View(df_special_chars)  
nrow(df_special_chars) #0 rows have "." in character value

# Just in case: remove all periods from 'my_column'
SurveyDiscards$ID_CODE <- gsub("\\.", "", SurveyDiscards$ID_CODE) 

#check it worked
# Filter rows where 'ID_CODE' contains any non-alphanumeric character
df_special_chars <- SurveyDiscards %>%
  filter(str_detect(ID_CODE, "[^[:alnum:]]"))

#View(df_special_chars)  
nrow(df_special_chars) #0 rows have "." in character value - it worked!

#check col names; only need ID_CODE, TSN = species ITIS number (change name), need to combine Num_Fish (dead discards) and Num_Typ2 (alive discard)
names(SurveyDiscards)
# "YEAR"     "WAVE"     "SUB_REG"  "ID_CODE"  "TSN"      "NUM_FISH" "NUM_TYP2" "ST"       "DATE1"

#only retain needed fields in discard DF for left_joining, rename cols to be more meaningful
SurveyDiscards_processed <- SurveyDiscards %>%
  # Clean NUM_FISH column: Remove any non-numeric characters, except for the decimal point
  mutate(NUM_FISH_clean = as.numeric(str_replace_all(NUM_FISH, "[^0-9.]", ""))) %>%
  # Clean NUM_TYP2 column
  mutate(NUM_TYP2_clean = as.numeric(str_replace_all(NUM_TYP2, "[^0-9.]", ""))) %>%
  # Calculate Total_NUM_Discarded using the cleaned columns, replacing NA with 0
  mutate(Total_NUM_Discarded = coalesce(NUM_FISH_clean, 0) + coalesce(NUM_TYP2_clean, 0)) %>%
  # Select the final columns and rename
  select(ID_CODE, TSN, Total_NUM_Discarded) %>%
  rename(Discard_ITIS = TSN)

###### col bind the discard sequences for each unique ID_CODE to the 1st row of the unique ID_CODE
#extract discard cols in duplicated ID_CODE rows (all the discard catch records for each survey) and append them to the row where the ID_CODE appears first
#create cols to extract
ColsToExtract_discards <- c("Discard_ITIS","Total_NUM_Discarded")

# Extract the first instance of each ID_CODE
first_instances_SurveyDiscards_processed <- SurveyDiscards_processed %>%
  group_by(ID_CODE) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Extract the subsequent instances of each ID_CODE and process them
subsequent_instances_SurveyDiscards_processed <- SurveyDiscards_processed %>%
  group_by(ID_CODE) %>%
  filter(row_number() > 1) %>% # Filter for subsequent instances
  mutate(row_index = row_number() - 1) %>%  # Start indexing from 1 for subsequent instances
  select(ID_CODE, all_of(ColsToExtract_discards), row_index) %>%
  pivot_wider(
    names_from = row_index,
    values_from = all_of(ColsToExtract_discards),
    names_glue = "{.value}_instance_{row_index}"
  ) %>%
  ungroup() # Ungroup after pivoting

# Left join the processed subsequent instances with the first instances
processed_SurveyDiscards <- left_join(first_instances_SurveyDiscards_processed, 
                                      subsequent_instances_SurveyDiscards_processed, 
                                      by = "ID_CODE")


##### Now add retained catch ----

# check for odd characters: Filter rows where 'ID_CODE' contains any non-alphanumeric character
df_special_chars_retained <- SurveyRetainedCatch %>%
  filter(str_detect(ID_CODE, "[^[:alnum:]]"))

#View(df_special_chars)  
nrow(df_special_chars_retained) #0 rows have a "." in character value

# Just in case: remove all periods from 'my_column'
SurveyRetainedCatch$ID_CODE <- gsub("\\.", "", SurveyRetainedCatch$ID_CODE) 

#check it worked
# Filter rows where 'ID_CODE' contains any non-alphanumeric character
df_special_chars_retained <- SurveyRetainedCatch %>%
  filter(str_detect(ID_CODE, "[^[:alnum:]]"))

#View(df_special_chars)  
nrow(df_special_chars_retained) #0 rows have "." in character value - it worked!


#check col names; only need ID_CODE, TSN = species ITIS number (change name), fshinsp (quantity of the species that was seen & ID'd by interviews)
#this file also includes disp3 (released alive = 1 or dead = 6, which is in conflict with fshinsp when it = 1, and disp3 is not 8 or 9 (most cases). Ignore disp3 for that reason
names(SurveyRetainedCatch)
# "YEAR"     "WAVE"     "SUB_REG"  "ID_CODE"  "TSN"      "FSHINSP"  "DISP3"    "LNGTH"    "WGT"      "NUM_TYP3" "ST"       "DATE1"

#only retain needed fields in retained DF for left_joining, rename cols to be more meaningful
SurveyRetainedCatch_processed <- SurveyRetainedCatch %>%
  # just in case: Clean FSHINSP (fish inspected) column: Remove any non-numeric characters, except for the decimal point
  mutate(FSHINSP_clean = as.numeric(str_replace_all(FSHINSP, "[^0-9.]", ""))) %>%
  #change col name to ID its retained catch (use cleaned data) and replace NAs with 0s
  mutate(Total_NUM_Retained = coalesce(as.numeric(FSHINSP_clean),0)) %>%
  #select the final cols needed and rename ITIS col
  select(ID_CODE, TSN, Total_NUM_Retained) %>%
  rename(Retained_ITIS = TSN)

##### extract discard cols in duplicated ID_CODE rows (all the discard catch records for each survey) and append them to the row where the CODE_ID appears first
#create cols to extract
ColsToExtract_retained <- c("Retained_ITIS","Total_NUM_Retained")

# Extract the first instance of each ID_CODE
first_instances_SurveyRetained_processed <- SurveyRetainedCatch_processed %>%
  group_by(ID_CODE) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Extract the subsequent instances of each ID_CODE and process them
subsequent_instances_SurveyRetained_processed <- SurveyRetainedCatch_processed %>%
  group_by(ID_CODE) %>%
  filter(row_number() > 1) %>% # Filter for subsequent instances
  mutate(row_index = row_number() - 1) %>%  # Start indexing from 1 for subsequent instances
  select(ID_CODE, all_of(ColsToExtract_retained), row_index) %>%
  pivot_wider(
    names_from = row_index,
    values_from = all_of(ColsToExtract_retained),
    names_glue = "{.value}_instance_{row_index}"
  ) %>%
  ungroup() # Ungroup after pivoting

# Left join the processed subsequent instances with the first instances
processed_SurveyRetained <- left_join(first_instances_SurveyRetained_processed, 
                                      subsequent_instances_SurveyRetained_processed, 
                                      by = "ID_CODE")


#####Then Combine discarded (I2) and retained (I3) files ----
# Left join the two DFs
SurveyData_Retained_and_Discarded <- left_join(processed_SurveyDiscards, 
                                               processed_SurveyRetained, 
                                               by = "ID_CODE")

#check nrow is still the same
nrow(SurveyData_Retained_and_Discarded) #1537


#### Append catch to raw survey DF----

####Add the catch data to the RawSurveys_processed, by ID_CODE (dont keep duplicate cols) ----
RawSurveys_wCatchEffort <- Surveys2022_Short_wIDs %>%
  # SELECTIVE JOIN: Use any_of() to safely select columns from the right table.
  # If "Num_Anglers" is missing, it won't throw an error anymore.
  left_join(SurveyData_Retained_and_Discarded %>% 
              select(ID_CODE, any_of("Num_Anglers"), contains("ITIS"), contains("Discarded"), contains("Retained")), 
            by = "ID_CODE") %>%
  
  # DROP KEY: Remove ID_CODE now that the join is successful (per your original select).
  select(-ID_CODE) %>%
  
  #rename cols to match logbook col naming convention
  # MANUAL RENAME: Handle the single specific column that doesn't fit the pattern.
  #rename(SurveyNumAnglers = Num_Anglers) %>%
  
  # AUTOMATED RENAME: Use logic to handle the ~100 numbered columns.
  rename_with(~case_when(
    # Pattern A: If name has "_instance_N", remove that string and add (N+2) to the end
    # Example: Retained_ITIS_instance_0 becomes Survey_Retained_ITIS_2
    str_detect(., "_instance_") ~ paste0("Survey_", str_remove(., "_instance_\\d+"), "_", as.numeric(str_extract(., "\\d+$")) + 2),
    
    # Pattern B: If it's one of the four "base" columns, add "_1" to the end
    # Example: Retained_ITIS becomes Survey_Retained_ITIS_1
    . %in% c("Discard_ITIS", "Total_NUM_Discarded", "Retained_ITIS", "Total_NUM_Retained") ~ paste0("Survey_", ., "_1"),
    
    # Otherwise, keep the name as is
    TRUE ~ .)) %>%
  
  # STRING CLEANUP: Shorten "Total_NUM" to "NUM" globally to match naming style
  rename_with(~str_replace(., "Total_NUM", "NUM"), contains("Total_NUM")) %>%
  
  # FILL NAs: Replace all missing values with 0.
  mutate(across(everything(), ~replace_na(.x, 0)))


#check nrow should be #1528 to match raw survey df row count used in matching code
nrow(RawSurveys_wCatchEffort)   #1528 (it worked!)

####Save survey data as csv output----
write.csv(RawSurveys_wCatchEffort, paste0(Path, Outputs,"/AllSurveys_withCatchandEffort.csv"), row.names = FALSE)   #1528 unique surveys (includes "SA trip" - whatever that is)


##2.3 Format Raw 2022 Logbook Data ----                   

####Clean up Logbook Data ----
Logbooks_2022_formatted <- Logbooks_annual %>% 
  
  #Remove duplicated data (duplicated from catch we just want unique trips)
  #distinct(TRIP_ID, .keep_all = TRUE) %>% ##KEEP all rows, of catch sequences
  
  #ensure we only have charter and headboat trips
  filter(TRIP_TYPE_NAME %in% c("CHARTER","HEADBOAT")) %>%
  
  #filter to only keep trips ending in the Gulf states, since they cant be intercepted by Gulf surveyors otherwise
  filter(END_PORT_STATE %in% c("FL","AL","MS","LA","TX")) %>% 
  
  #modify dates and creates wave indexing
  mutate(Date= as.Date(TRIP_END_DATE,format="%m/%d/%Y"),
         MONTH=as.character(substr(as.Date(TRIP_END_DATE,format="%m/%d/%Y"),6,7)),
         Wave= ifelse(MONTH %in% c("01","02"),1,
                      ifelse(MONTH %in% c("03","04"),2,
                             ifelse(MONTH %in% c("05","06"),3,
                                    ifelse(MONTH %in% c("07","08"),4,
                                           ifelse(MONTH %in% c("09","10"),5,
                                                  ifelse(MONTH %in% c("11","12"),6,NA)))))),
         TRIPENDYEAR=substr(as.Date(TRIP_END_DATE,format="%m/%d/%Y"),1,4),
         TRIPENDDAY=substr(as.Date(TRIP_END_DATE,format="%m/%d/%Y"),9,10),
         TRIPENDMONTH=substr(as.Date(TRIP_END_DATE,format="%m/%d/%Y"),6,7),
         TRIPSTARTYEAR=substr(as.Date(TRIP_START_DATE,format="%m/%d/%Y"),1,4),
         State=END_PORT_STATE, #corrected this to END_PORT (Was STATE_PORT) on 6/05/26 - to match line 218 where I am only keeping trips ending in the Gulf based on END_PORT
         vsl_num=VESSEL_OFFICIAL_NBR)  %>%  #to match permit field after merging below
  
  #filter out trips that end outside of 2022
  filter(TRIPENDYEAR == 2022) %>%
  
  #merge permit data
  left_join(perms, by="vsl_num") %>%
  
  #filter out Logbooks when vessel doesnt have Gulf permit, as surveyors were told not to intercept those
  #filter(GOM_PERM == "Y") %>%
  
  #make a 0/1 flag to define which logbooks are gulf trips
  mutate(GulfTrip = case_when(State %in% c("AL","TX","MS", "LA") ~ 1,
                              ## FL Trips with gulf county as END are GOM 
                              State=="FL" & END_PORT_COUNTY %in% c("ESCAMBIA","SANTA ROSA","OKALOOSA",
                                                                   "WALTON","BAY","GULF","FRANKLIN","WAKULLA","JEFFERSON","TAYLOR",
                                                                   "DIXIE","LEVY","CITRUS","HERNANDO","PASCO","HILLSBOROUGH",
                                                                   "PINELLAS","MANATEE","SARASOTA","CHARLOTTE","COLLIER","LEE") ~ 1,
                              ## FL trips in Monroe with GOM permit are coded as Gom
                              State=="FL" & END_PORT_COUNTY=="MONROE" & Perm_Group=="GOM" ~ 1,
                              ## If they don't have a county then GOM
                              State=="FL" & END_PORT_COUNTY=="NOT-SPECIFIED" & Perm_Group=="GOM" ~ 1,
                              TRUE ~ 0),
         
         #Get rough time which will help with merge 
         ENDTIME= round(as.numeric(TRIP_END_TIME,-2)),
         
         ## Create a dummy variable for each logbook record
         logb=1,
         #id column does not represent unique survey event
         id= paste0(vsl_num,"_",Date),
         #look at average time of trip to determine interval
         TripStartDate= format(as.Date(TRIP_START_DATE,"%m-%d-%Y")),
         TripStartDateTime=  as_datetime(paste0(TripStartDate," ", TRIP_START_TIME), format="%Y-%m-%d %H%M"),
         TripEndDate= format(as.Date(TRIP_END_DATE,"%m-%d-%Y")),
         TripEndDateTime= as_datetime(paste0(TripEndDate," ",TRIP_END_TIME), format="%Y-%m-%d %H%M"), 
         #Subtract Log time from Interview Time (Time Math)
         TripLength = round(difftime(TripEndDateTime,TripStartDateTime, units = "hours"),2)) %>%
  
  
  #only keep trips ending in the Gulf, otherwise they couldn't be surveyed by gulf surveyor
  filter(GulfTrip == 1)

#number of unique logs from Gulf vessels ending in Gulf waters
nrow(Logbooks_2022_formatted)  #49,096 after changing State = END_PORT (was start port); includes all for-hire permits

#convert month col to numeric to remove leading zeroes, for analysis
Logbooks_2022_formatted$TRIPENDMONTH <- as.numeric(Logbooks_2022_formatted$TRIPENDMONTH)

###Select Fields For Logbooks----
Logbooks2022_processed <- Logbooks_2022_formatted %>%
  
  # Update ENDTIME to total minutes since midnight right before renaming to TIME
  mutate(ENDTIME = as.numeric(ENDTIME) %/% 100 * 60 + as.numeric(ENDTIME) %% 100) %>%
  
  # Convert formats to match survey format
  mutate(NUM_ANGLERS = as.double(NUM_ANGLERS),
         TRIP_END_DATE = as.Date(TRIP_END_DATE)) %>% 
  
  # Modify "Y"/"N" to 1/0. This automatically makes the column numeric!
  mutate(ANYTHING_CAUGHT_FLAG = case_when(
    ANYTHING_CAUGHT_FLAG == "Y" ~ 1,  
    is.na(ANYTHING_CAUGHT_FLAG) ~ 1, 
    ANYTHING_CAUGHT_FLAG == "N" ~ 0)) %>% 
  
  # Replace NAs with zeros for all numeric cols
  replace_na(list(NUM_ANGLERS = 0, FISHING_HOURS = 0, TRIPENDDAY = 0, 
                  TRIPENDMONTH = 0, TRIPENDYEAR = 0, ANYTHING_CAUGHT_FLAG = 1)) %>%  
  
  # Round hours fished
  mutate(Hours_Fished = round(ceiling(FISHING_HOURS), 0)) %>% 
  
  # Ensure character values are uppercase
  mutate(END_PORT_COUNTY = toupper(END_PORT_COUNTY),
         END_PORT_STATE = toupper(END_PORT_STATE),
         VESSEL_NAME = toupper(VESSEL_NAME)) %>%

  #drop State col and use end_port_State renamed to state for linking
  select(-State) %>%

  #rename cols to match survey
  rename(County = END_PORT_COUNTY,
         State = END_PORT_STATE,
         Num_Anglers = NUM_ANGLERS,
         Full_Date = TRIP_END_DATE,
         Day = TRIPENDDAY,
         Month = TRIPENDMONTH,
         Year = TRIPENDYEAR,
         TIME = ENDTIME,
         Vessel_Official_Num = VESSEL_OFFICIAL_NBR,
         Vessel_Name = VESSEL_NAME,
         Anything_Caught_Flag = ANYTHING_CAUGHT_FLAG,
         Hours_Fished = Hours_Fished)  %>%
  
  #create Catch_Type col for grabbing logbook catch sequences
  #see SAFIS codes page for disposition codes: 
  #002 = Personal Use; 
  #400 = Reason not specified (Discard); 
  #403 = dead discard; 
  #404 = Released alive (discard);
  #NA = assume retained; 
  #038 = Personal Use/Bait (retained)
  #mutate codes to the words retained or discard for ease of use by next analyst
  mutate(Catch_Type = case_when(DISPOSITION_CODE %in% c(002,038) ~ "Retained",
                                DISPOSITION_CODE %in% c(400,403,404) ~ "Discarded",
                                TRUE ~ NA_character_))  # All other codes will be converted to NA
  

#check data format
#glimpse(Logbooks2022_processed)
nrow(Logbooks2022_processed) #201,271  
length(unique(Logbooks2022_processed$TRIP_ID)) # still have all logbooks 

#sort DF by date and add logbook row ID col to logbook DF
Logbooks2022_processed_wIDs <- Logbooks2022_processed %>%
  arrange(Full_Date) %>% #arrange DF in descending order of date
  mutate(Logbook_RowID = row_number())

#ensure this didnt create duplicated RowIDs
any(duplicated(Logbooks2022_processed_wIDs$Logbook_RowID)) #FALSE

###Add Catch sequences as cols for first TRIP_ID instance ----
##### create variable of column names of catch cols to extract from subsequent TRIP_IDs, for left joining ----
ColsToExtract <- c("CATCH_SPECIES_ITIS", "REPORTED_QUANTITY", "Catch_Type")

##### col bind the catch sequences for each unique TRIP ID to the 1st row of the unique Trip_ID ----
#extract catch cols in duplicated TRIP_ID rows (all the catch records for each logbook) and append them to the row where the TRIP_ID appears first
# Extract the first instance of each TRIP_ID
first_instances_RawLogbooks_wCatchEffortData <- Logbooks2022_processed_wIDs %>%
  group_by(TRIP_ID) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Extract the subsequent instances of each TRIP_ID and process them
subsequent_instances_RawLogbooks_wCatchEffortData <- Logbooks2022_processed_wIDs %>%
  group_by(TRIP_ID) %>%
  filter(row_number() > 1) %>% # Filter for subsequent instances
  mutate(row_index = row_number() - 1) %>%  # Start indexing from 1 for subsequent instances
  select(TRIP_ID, all_of(ColsToExtract), row_index) %>%
  pivot_wider(
    names_from = row_index,
    values_from = all_of(ColsToExtract),
    names_glue = "{.value}_instance_{row_index}"
  ) %>%
  ungroup() # Ungroup after pivoting

# Left join the processed subsequent instances with the first instances
processed_RawLogbooks_wCatchEffortData <- left_join(first_instances_RawLogbooks_wCatchEffortData, 
                                                    subsequent_instances_RawLogbooks_wCatchEffortData, 
                                                    by = "TRIP_ID")


#check number of rows is still correct (#of logbooks should match total used in matching algorithm)
nrow(processed_RawLogbooks_wCatchEffortData) 
#49096 (it worked!)


####Sort Logbook DF by date and add logbook row ID col ----
processed_RawLogbooks_SortedwRowIDs <- processed_RawLogbooks_wCatchEffortData %>%
  arrange(Full_Date) %>% #arrange DF in descending order of date
  mutate(Logbook_RowID = row_number(), .before = 1)  #add RowID as the first col  

###Programmatically create the list of Catch_Type columns (0 to 26) ----
catch_type_cols <- c("Catch_Type", paste0("Catch_Type_instance_", 0:26))

#Combine with other exclusions
excluded_cols <- c("EFFORT_TARGET_COMMON_NAMES", catch_type_cols)

###Handle NAs, rename cols, sort/relocate cols----
processed_RawLogbooks_SortedwRowIDs_wCatchandEffort <- processed_RawLogbooks_SortedwRowIDs %>%

  # SELECTIVE NA REPLACEMENT: Only numeric columns, minus excluded ones
  mutate(across(where(is.numeric) & -any_of(excluded_cols), ~replace_na(.x, 0))) %>%
  
  #AUTOMATED RENAME
  rename_with(~case_when(. == "EFFORT_TARGET_COMMON_NAMES" ~ "Logbook_Targeted_Species_List",
                         
                         # Pattern A: instance_N -> _(N+2) 
                         # (Using str_replace to drop 'CATCH_' if it exists and replace with the number for the catch sequence)
                         str_detect(., "_instance_") ~ {
                           base <- str_remove(., "_instance_\\d+") %>% str_replace("CATCH_", "")
                           num <- as.numeric(str_extract(., "\\d+$")) + 2
                           paste0("Logbook_", base, "_", num)},
                         
                         # Pattern B: Base columns -> _1  (number the first catch sequence with a 1)
                         . == "CATCH_SPECIES_ITIS" ~ "Logbook_SPECIES_ITIS_1",
                         . %in% c("REPORTED_QUANTITY", "Catch_Type") ~ paste0("Logbook_", ., "_1"), TRUE ~ .)) %>%
  
  # SORT/RELOCATE: Organize columns alphabetically or by group
  # This moves all "Logbook_" columns to the end, then
  # tucks the "ITIS_1" column right after depth col
  relocate(starts_with("Logbook_"), .after = last_col()) %>%
  # Specific placement for the '1' columns to come after fishing gear cols
  relocate(any_of("Logbook_SPECIES_ITIS_1"), .after = any_of("FISHING_GEAR_DEPTH"))

#check number of unique logbooks hasnt changed
length(unique(processed_RawLogbooks_SortedwRowIDs_wCatchandEffort$TRIP_ID)) #49096

####Save logbook DF----
write.csv(processed_RawLogbooks_SortedwRowIDs_wCatchandEffort, paste0(Path, Outputs, "/AllLogbooks_2022_withCatchandEffort.csv"))  #49,096 logbooks

##2.4  Parse data into Monthly Chunks, for looping efficiency (otherwise it'd take the for loop weeks to loop through 49k logbooks x 1835 surveys ----

###First replace NAs in vessel Name col in survey data ----
Surveys2022_wCandE_wRowIDs <- RawSurveys_wCatchEffort %>%
  #replace NAs in vessel_Name col
  replace_na(list(Vessel_Name = "NONE"))
#ensure this didnt create duplicated RowIDs
any(duplicated(Surveys2022_wCandE_wRowIDs$Survey_RowID)) #[1] FALSE


###Then Split Surveys into a list by Month, then remove the Month column from each element----
Surveys.list <- Surveys2022_wCandE_wRowIDs %>%
  group_by(Month) %>%
  group_split(.keep = FALSE)

#Split Logbooks into a list by Month, then remove the Month column
Logbooks.list <- processed_RawLogbooks_SortedwRowIDs_wCatchandEffort %>%
  group_by(Month) %>%
  group_split(.keep = FALSE)

# Check for any unintended duplicates 
any(duplicated(bind_rows(Surveys.list)$Survey_RowID))
any(duplicated(bind_rows(Logbooks.list)$Logbook_RowID))

# Save files
saveRDS(Surveys.list, paste0(Path, Outputs, "/Real_surveydata.rds"))
saveRDS(Logbooks.list, paste0(Path, Outputs, "/Real_logbookdata.rds"))
