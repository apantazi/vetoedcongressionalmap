
###read in necessary packages###
library(redist)
library(geomander)
library(sf)
library(tidycensus)
library(tidyverse)
options(tigris_use_cache=TRUE)

### read in tigris files to calculate area ###

duval_blocks <- tigris::blocks("fl","duval",year="2020")
duval_blocks <- duval_blocks %>% 
  mutate(total_area = ALAND20+AWATER20, water_pct = AWATER20/total_area)

duval_blocks <- duval_blocks %>% 
  st_drop_geometry()

#### census ####

# read in variables you'll need to calculate voting-age population demographics #
all_vars <- c("P3_004N",
              "P3_011N",
              "P3_016N",
              "P3_017N",
              "P3_018N",
              "P3_019N",
              "P3_027N",
              "P3_028N",
              "P3_029N",
              "P3_030N",
              "P3_037N",
              "P3_038N",
              "P3_039N",
              "P3_040N",
              "P3_041N",
              "P3_042N",
              "P3_048N",
              "P3_049N",
              "P3_050N",
              "P3_051N",
              "P3_052N",
              "P3_053N",
              "P3_058N",
              "P3_059N",
              "P3_060N",
              "P3_061N",
              "P3_064N",
              "P3_065N",
              "P3_066N",
              "P3_067N",
              "P3_069N",
              "P3_071N",
              "P4_002N",
              "P4_005N",
              "P3_006N",
              "P3_007N",
              "P3_013N",
              "P3_014N",
              "P3_020N",
              "P3_021N",
              "P3_023N",
              "P3_024N",
              "P3_025N",
              "P3_031N",
              "P3_032N",
              "P3_034N",
              "P3_035N",
              "P3_036N",
              "P3_043N",
              "P3_044N",
              "P3_045N",
              "P3_046N",
              "P3_054N",
              "P3_055N",
              "P3_056N",
              "P3_057N",
              "P3_062N",
              "P3_068N",
              "P3_005N",
              "P3_012N",
              "P3_022N",
              "P3_033N",
              "P3_008N",
              "P3_015N",
              "P1_001N"
)
# read in block data for Duval County #
duval_race_blocks <- tidycensus::get_decennial(
  geography = "block",
  state = "fl",
  county="duval",
  variables = all_vars,
  summary_var = "P3_001N",
  year=2020,
  output = "wide",
  geometry = TRUE)

#convert racial data to calculate Black demographics according to OMB & DOJ definitions, that is: someone who identifies as Black is counted as Black, even if they identify as more than one race #
duval_race_blocks <- transform(duval_race_blocks,
                               black = P3_004N+
                                 P3_011N+
                                 P3_016N+
                                 P3_017N+
                                 P3_018N+
                                 P3_019N+
                                 P3_027N+
                                 P3_028N+
                                 P3_029N+
                                 P3_030N+
                                 P3_037N+
                                 P3_038N+
                                 P3_039N+
                                 P3_040N+
                                 P3_041N+
                                 P3_042N+
                                 P3_048N+
                                 P3_049N+
                                 P3_050N+
                                 P3_051N+
                                 P3_052N+
                                 P3_053N+
                                 P3_058N+
                                 P3_059N+
                                 P3_060N+
                                 P3_061N+
                                 P3_064N+
                                 P3_065N+
                                 P3_066N+
                                 P3_067N+
                                 P3_069N+
                                 P3_071N,
                               white = P4_005N,
                               hisp = P4_002N,
                               asian = P3_006N+
                                 P3_013N+
                                 P3_017N+
                                 P3_020N+
                                 P3_023N+
                                 P3_024N+
                                 P3_028N+
                                 P3_031N+
                                 P3_034N+
                                 P3_035N+
                                 P3_037N+
                                 P3_040N+
                                 P3_041N+
                                 P3_043N+
                                 P3_044N+
                                 P3_046N+
                                 P3_048N+
                                 P3_051N+
                                 P3_052N+
                                 P3_054N+
                                 P3_055N+
                                 P3_057N+
                                 P3_058N+
                                 P3_059N+
                                 P3_061N+
                                 P3_062N+
                                 P3_064N+
                                 P3_065N+
                                 P3_067N+
                                 P3_068N+
                                 P3_069N+
                                 P3_071N,
                               AIAN = P3_005N+
                                 P3_012N+
                                 P3_016N+
                                 P3_020N+
                                 P3_021N+
                                 P3_022N+
                                 P3_027N+
                                 P3_031N+
                                 P3_032N+
                                 P3_033N+
                                 P3_037N+
                                 P3_038N+
                                 P3_039N+
                                 P3_043N+
                                 P3_044N+
                                 P3_045N+
                                 P3_048N+
                                 P3_049N+
                                 P3_050N+
                                 P3_054N+
                                 P3_055N+
                                 P3_056N+
                                 P3_058N+
                                 P3_059N+
                                 P3_060N+
                                 P3_062N+
                                 P3_064N+
                                 P3_065N+
                                 P3_066N+
                                 P3_068N+
                                 P3_069N+
                                 P3_071N,
                               PI = P3_007N+
                                 P3_014N+
                                 P3_018N+
                                 P3_021N+
                                 P3_023N+
                                 P3_025N+
                                 P3_029N+
                                 P3_032N+
                                 P3_034N+
                                 P3_036N+
                                 P3_038N+
                                 P3_040N+
                                 P3_042N+
                                 P3_043N+
                                 P3_045N+
                                 P3_046N+
                                 P3_049N+
                                 P3_051N+
                                 P3_053N+
                                 P3_054N+
                                 P3_056N+
                                 P3_057N+
                                 P3_058N+
                                 P3_060N+
                                 P3_061N+
                                 P3_062N+
                                 P3_064N+
                                 P3_066N+
                                 P3_067N+
                                 P3_068N+
                                 P3_069N+
                                 P3_071N,
                               other = P3_008N+
                                 P3_015N+
                                 P3_019N+
                                 P3_022N+
                                 P3_024N+
                                 P3_025N+
                                 P3_030N+
                                 P3_033N+
                                 P3_035N+
                                 P3_036N+
                                 P3_039N+
                                 P3_041N+
                                 P3_042N+
                                 P3_044N+
                                 P3_045N+
                                 P3_046N+
                                 P3_050N+
                                 P3_052N+
                                 P3_053N+
                                 P3_055N+
                                 P3_056N+
                                 P3_057N+
                                 P3_059N+
                                 P3_060N+
                                 P3_061N+
                                 P3_062N+
                                 P3_065N+
                                 P3_066N+
                                 P3_067N+
                                 P3_068N+
                                 P3_069N+
                                 P3_071N,
                               pop = P1_001N,
                               vap = summary_value)

#clean up the data to just what we need#
vars_to_keep <- vars_to_keep <- c("GEOID","NAME","pop","vap","black","white","hisp","asian","PI","AIAN","other")
duval_race_blocks <- duval_race_blocks[vars_to_keep]

#### election data ####
data_url = "https://raw.githubusercontent.com/apantazi/ecological-inference/main/2012_2020_duval_turnout_votes.csv"
data_path = tempfile()
download.file(data_url, data_path,cacheOK = FALSE)

#2022 precinct shapefile for disaggregating election data #
jax_shp4 <- read_sf("C:/Users/andre/Downloads/PRECINCT12031 (3)/PRECINCT12031.shp")

city_data = read_csv(data_path)

#join the census blocks
block <- left_join(duval_race_blocks,duval_blocks,by=c("GEOID"="GEOID20"))
rm(duval_race_blocks)
rm(duval_blocks)

#calculate block-level density
block <- block %>% mutate(density = pop/ALAND20)

#wizardry - build relationships between census blocks and precincts#
match_shp2022 <- geo_match(from=block,to=jax_shp4,method='centroid')
prec2022 <- block2prec(block,match_shp2022)
fulldata2022 <- bind_cols(jax_shp4,prec2022)

## ensure Date is correctly formatted ##
city_data <- city_data %>% mutate(Date = lubridate::mdy(Date))

# format data into precinct pivots #
sheriff_ge_Data <- city_data %>% 
  #filter(grepl("2020",Date)) %>% 
  #filter(Date == as.Date("2015-03-24")) %>% 
  filter(Date == as.Date("2022-11-08")) %>%
  filter(grepl("sheriff",Race,ignore.case=TRUE)) %>% 
  group_by(PRECINCT) %>% 
  summarize(dem = mean(DEM_candidate_votes), gop = mean(REP_candidate_votes),tot_votes = mean(Total_contest_votes),black_votes = mean(Black_Turnout),tot_turnout = mean(totvote),white_votes = mean(White_Turnout))

cfo22_data <- city_data %>% 
  filter(grepl("2022",Date)) %>% 
  filter(grepl("CFO",Race,ignore.case=TRUE)) %>% 
  group_by(PRECINCT) %>% 
  summarize(dem = mean(DEM_candidate_votes), gop = mean(REP_candidate_votes),tot_votes = mean(Total_contest_votes),black_votes = mean(Black_Turnout),tot_turnout = mean(totvote),white_votes = mean(White_Turnout))

AG22_data <- city_data %>% 
  filter(grepl("2022",Date)) %>% 
  filter(grepl("Attorney",Race,ignore.case=TRUE)) %>% 
  group_by(PRECINCT) %>% 
  summarize(dem = mean(DEM_candidate_votes), gop = mean(REP_candidate_votes),tot_votes = mean(Total_contest_votes),black_votes = mean(Black_Turnout),tot_turnout = mean(totvote),white_votes = mean(White_Turnout))

AgC22_data <- city_data %>% 
  filter(grepl("2022",Date)) %>% 
  filter(grepl("Ag C",Race,ignore.case=TRUE)) %>% 
  group_by(PRECINCT) %>% 
  summarize(dem = mean(DEM_candidate_votes), gop = mean(REP_candidate_votes),tot_votes = mean(Total_contest_votes),black_votes = mean(Black_Turnout),tot_turnout = mean(totvote),white_votes = mean(White_Turnout))

Gov22_data <- city_data %>% 
  filter(grepl("2022",Date)) %>% 
  filter(grepl("Gov",Race,ignore.case=TRUE)) %>% 
  group_by(PRECINCT) %>% 
  summarize(dem = mean(DEM_candidate_votes), gop = mean(REP_candidate_votes),tot_votes = mean(Total_contest_votes),black_votes = mean(Black_Turnout),tot_turnout = mean(totvote),white_votes = mean(White_Turnout))

Sen22_data <- city_data %>% 
  filter(grepl("2022",Date)) %>% 
  filter(grepl("Senator",Race,ignore.case=TRUE)) %>% 
  group_by(PRECINCT) %>% 
  summarize(dem = mean(DEM_candidate_votes), gop = mean(REP_candidate_votes),tot_votes = mean(Total_contest_votes),black_votes = mean(Black_Turnout),tot_turnout = mean(totvote),white_votes = mean(White_Turnout))

### 2022 elections ####
merge_shp_sen22 <- merge(fulldata2022,Sen22_data,by='PRECINCT')
disagg_dem_sen22 <- geo_estimate_down(from=merge_shp_sen22, to=block, wts=block$vap, value=merge_shp_sen22$dem,method='centroid')
disagg_gop_sen22 <- geo_estimate_down(from=merge_shp_sen22, to=block, wts=block$vap, value=merge_shp_sen22$gop,method='centroid')
disagg_black_sen22 <- geo_estimate_down(from=merge_shp_sen22, to=block, wts=block$black, value=merge_shp_sen22$black_votes,method='centroid')
disagg_white_sen22 <- geo_estimate_down(from=merge_shp_sen22, to=block, wts=block$white, value=merge_shp_sen22$white_votes,method='centroid')
disagg_tot_sen22 <- geo_estimate_down(from=merge_shp_sen22, to=block, wts=block$vap, value=merge_shp_sen22$tot_votes,method='centroid')
disagg_tot_turnout_sen22 <- geo_estimate_down(from=merge_shp_sen22, to=block, wts=block$vap, value=merge_shp_sen22$tot_turnout,method='centroid')
block2 <- bind_cols(block,disagg_dem_sen22)
colnames(block2)[ncol(block2)-1] <- 'DEM_sen22'
block2 <- bind_cols(block2,disagg_gop_sen22)
colnames(block2)[ncol(block2)-1] <- 'GOP_sen22'
block2 <- bind_cols(block2,disagg_black_sen22)
colnames(block2)[ncol(block2)-1] <- 'black_turnout_sen22'
block2 <- bind_cols(block2,disagg_white_sen22)
colnames(block2)[ncol(block2)-1] <- 'white_turnout_sen22'
block2 <- bind_cols(block2,disagg_tot_sen22)
colnames(block2)[ncol(block2)-1] <- 'tot_contest_sen22'
block2 <- bind_cols(block2,disagg_tot_turnout_sen22)
colnames(block2)[ncol(block2)-1] <- 'tot_turnout_sen22'

merge_shp_AG22 <- merge(fulldata2022,AG22_data,by='PRECINCT')
disagg_dem_AG22 <- geo_estimate_down(from=merge_shp_AG22, to=block, wts=block$vap, value=merge_shp_AG22$dem,method='centroid')
disagg_gop_AG22 <- geo_estimate_down(from=merge_shp_AG22, to=block, wts=block$vap, value=merge_shp_AG22$gop,method='centroid')
disagg_black_AG22 <- geo_estimate_down(from=merge_shp_AG22, to=block, wts=block$black, value=merge_shp_AG22$black_votes,method='centroid')
disagg_white_AG22 <- geo_estimate_down(from=merge_shp_AG22, to=block, wts=block$white, value=merge_shp_AG22$white_votes,method='centroid')
disagg_tot_AG22 <- geo_estimate_down(from=merge_shp_AG22, to=block, wts=block$vap, value=merge_shp_AG22$tot_votes,method='centroid')
disagg_tot_turnout_AG22 <- geo_estimate_down(from=merge_shp_AG22, to=block, wts=block$vap, value=merge_shp_AG22$tot_turnout,method='centroid')
block2 <- bind_cols(block2,disagg_dem_AG22)
colnames(block2)[ncol(block2)-1] <- 'DEM_AG22'
block2 <- bind_cols(block2,disagg_gop_AG22)
colnames(block2)[ncol(block2)-1] <- 'GOP_AG22'
block2 <- bind_cols(block2,disagg_black_AG22)
colnames(block2)[ncol(block2)-1] <- 'black_turnout_AG22'
block2 <- bind_cols(block2,disagg_white_AG22)
colnames(block2)[ncol(block2)-1] <- 'white_turnout_AG22'
block2 <- bind_cols(block2,disagg_tot_AG22)
colnames(block2)[ncol(block2)-1] <- 'tot_contest_AG22'
block2 <- bind_cols(block2,disagg_tot_turnout_AG22)
colnames(block2)[ncol(block2)-1] <- 'tot_turnout_AG22'

merge_shp_AgC22 <- merge(fulldata2022,AgC22_data,by='PRECINCT')
disagg_dem_AgC22 <- geo_estimate_down(from=merge_shp_AgC22, to=block, wts=block$vap, value=merge_shp_AgC22$dem,method='centroid')
disagg_gop_AgC22 <- geo_estimate_down(from=merge_shp_AgC22, to=block, wts=block$vap, value=merge_shp_AgC22$gop,method='centroid')
disagg_black_AgC22 <- geo_estimate_down(from=merge_shp_AgC22, to=block, wts=block$black, value=merge_shp_AgC22$black_votes,method='centroid')
disagg_white_AgC22 <- geo_estimate_down(from=merge_shp_AgC22, to=block, wts=block$white, value=merge_shp_AgC22$white_votes,method='centroid')
disagg_tot_AgC22 <- geo_estimate_down(from=merge_shp_AgC22, to=block, wts=block$vap, value=merge_shp_AgC22$tot_votes,method='centroid')
disagg_tot_turnout_AgC22 <- geo_estimate_down(from=merge_shp_AgC22, to=block, wts=block$vap, value=merge_shp_AgC22$tot_turnout,method='centroid')
block2 <- bind_cols(block2,disagg_dem_AgC22)
colnames(block2)[ncol(block2)-1] <- 'DEM_AgC22'
block2 <- bind_cols(block2,disagg_gop_AgC22)
colnames(block2)[ncol(block2)-1] <- 'GOP_AgC22'
block2 <- bind_cols(block2,disagg_black_AgC22)
colnames(block2)[ncol(block2)-1] <- 'black_turnout_AgC22'
block2 <- bind_cols(block2,disagg_white_AgC22)
colnames(block2)[ncol(block2)-1] <- 'white_turnout_AgC22'
block2 <- bind_cols(block2,disagg_tot_AgC22)
colnames(block2)[ncol(block2)-1] <- 'tot_contest_AgC22'
block2 <- bind_cols(block2,disagg_tot_turnout_AgC22)
colnames(block2)[ncol(block2)-1] <- 'tot_turnout_AgC22'

merge_shp_Gov22 <- merge(fulldata2022,Gov22_data,by='PRECINCT')
disagg_dem_Gov22 <- geo_estimate_down(from=merge_shp_Gov22, to=block, wts=block$vap, value=merge_shp_Gov22$dem,method='centroid')
disagg_gop_Gov22 <- geo_estimate_down(from=merge_shp_Gov22, to=block, wts=block$vap, value=merge_shp_Gov22$gop,method='centroid')
disagg_black_Gov22 <- geo_estimate_down(from=merge_shp_Gov22, to=block, wts=block$black, value=merge_shp_Gov22$black_votes,method='centroid')
disagg_white_Gov22 <- geo_estimate_down(from=merge_shp_Gov22, to=block, wts=block$white, value=merge_shp_Gov22$white_votes,method='centroid')
disagg_tot_Gov22 <- geo_estimate_down(from=merge_shp_Gov22, to=block, wts=block$vap, value=merge_shp_Gov22$tot_votes,method='centroid')
disagg_tot_turnout_Gov22 <- geo_estimate_down(from=merge_shp_Gov22, to=block, wts=block$vap, value=merge_shp_Gov22$tot_turnout,method='centroid')
block2 <- bind_cols(block2,disagg_dem_Gov22)
colnames(block2)[ncol(block2)-1] <- 'DEM_Gov22'
block2 <- bind_cols(block2,disagg_gop_Gov22)
colnames(block2)[ncol(block2)-1] <- 'GOP_Gov22'
block2 <- bind_cols(block2,disagg_black_Gov22)
colnames(block2)[ncol(block2)-1] <- 'black_turnout_Gov22'
block2 <- bind_cols(block2,disagg_white_Gov22)
colnames(block2)[ncol(block2)-1] <- 'white_turnout_Gov22'
block2 <- bind_cols(block2,disagg_tot_Gov22)
colnames(block2)[ncol(block2)-1] <- 'tot_contest_Gov22'
block2 <- bind_cols(block2,disagg_tot_turnout_Gov22)
colnames(block2)[ncol(block2)-1] <- 'tot_turnout_Gov22'

merge_shp_cfo22 <- merge(fulldata2022,cfo22_data,by='PRECINCT')
disagg_dem_cfo22 <- geo_estimate_down(from=merge_shp_cfo22, to=block, wts=block$vap, value=merge_shp_cfo22$dem,method='centroid')
disagg_gop_cfo22 <- geo_estimate_down(from=merge_shp_cfo22, to=block, wts=block$vap, value=merge_shp_cfo22$gop,method='centroid')
disagg_black_cfo22 <- geo_estimate_down(from=merge_shp_cfo22, to=block, wts=block$black, value=merge_shp_cfo22$black_votes,method='centroid')
disagg_white_cfo22 <- geo_estimate_down(from=merge_shp_cfo22, to=block, wts=block$white, value=merge_shp_cfo22$white_votes,method='centroid')
disagg_tot_cfo22 <- geo_estimate_down(from=merge_shp_cfo22, to=block, wts=block$vap, value=merge_shp_cfo22$tot_votes,method='centroid')
disagg_tot_turnout_cfo22 <- geo_estimate_down(from=merge_shp_cfo22, to=block, wts=block$vap, value=merge_shp_cfo22$tot_turnout,method='centroid')
block2 <- bind_cols(block2,disagg_dem_cfo22)
colnames(block2)[ncol(block2)-1] <- 'DEM_cfo22'
block2 <- bind_cols(block2,disagg_gop_cfo22)
colnames(block2)[ncol(block2)-1] <- 'GOP_cfo22'
block2 <- bind_cols(block2,disagg_black_cfo22)
colnames(block2)[ncol(block2)-1] <- 'black_turnout_cfo22'
block2 <- bind_cols(block2,disagg_white_cfo22)
colnames(block2)[ncol(block2)-1] <- 'white_turnout_cfo22'
block2 <- bind_cols(block2,disagg_tot_cfo22)
colnames(block2)[ncol(block2)-1] <- 'tot_contest_cfo22'
block2 <- bind_cols(block2,disagg_tot_turnout_cfo22)
colnames(block2)[ncol(block2)-1] <- 'tot_turnout_cfo22'

### congressional districts ####
c8019 <- read_csv("C:/Users/andre/Downloads/H000C8019.csv",col_types="ci",col_names=c("GEOID","C8019"))
c8015 <- read_csv("C:/Users/andre/Downloads/H000C8015.csv",col_types="ci",col_names=c("GEOID","C8015"))
c0109 <- read_csv("C:/Users/andre/Downloads/P000C0109 (1).csv",col_types="ci",col_names=c("GEOID","C0109"))

block2 <- left_join(block2,c8019)
block2 <- left_join(block2,c8015)
block2 <- left_join(block2,c0109)

### prep map ###
adj3 <- redist.adjacency(shp = block2, block2$C0109)
map <- redist_map(block2, pop_bounds = c(226346,542875,769221), ndists = 2, adj = adj3,existing_plan = C0109,total_pop = pop)

#make a fake simulated plan so you can analyze it with the REDIST package #
jax_plans <- redist_smc(map = map,
                        1,
                        compactness = 0.1,
                        verbose = TRUE,silent=FALSE,pop_temper = 1
)
#drop the simulation so you can use the reference plan instead#
jax_plans <- redist::subset_ref(jax_plans)
#add in reference plans
map$C8019 <- as.integer(map$C8019)
map$C8015 <- as.integer(map$C8015)
jax_plans <- add_reference(jax_plans,map$C8015,name="C8015")
jax_plans <- add_reference(jax_plans,map$C8019,name="C8019")

#calculate stats
jax_plans = jax_plans %>%
  mutate(pop_dev = abs(total_pop / get_target(map) - 1),
         comp = distr_compactness(map, "PolsbyPopper"),
         edges = distr_compactness(map,"EdgesRemoved"),
         FK = distr_compactness(map,"FracKept"),
         LW = distr_compactness(map,measure = "LengthWidth"),
         pct_min = group_frac(map, vap - white, vap),
         pct_black = group_frac(map,black,vap),
         pct_dem_sheriff_ge = group_frac(map, DEM_sheriff_ge, DEM_sheriff_ge + GOP_sheriff_ge),
         pct_dem_gov_22 = group_frac(map,DEM_Gov22,DEM_Gov22+GOP_Gov22),
         pct_dem_cfo_22 = group_frac(map,map$DEM_cfo22,map$DEM_cfo22+map$GOP_cfo22),
         pct_dem_AG_22 = group_frac(map,map$DEM_AG22,map$DEM_AG22+map$GOP_AG22),
         pct_dem_AgC_22 = group_frac(map,map$DEM_AgC22,map$DEM_AgC22+map$GOP_AgC22),
         pct_dem_sen_22 = group_frac(map,map$DEM_sen22,map$DEM_sen22+map$GOP_sen22),
         pct_dem = group_frac(map,
                              DEM_sheriff_ge+DEM_Gov22+DEM_cfo22+DEM_AG22+DEM_AgC22+DEM_sen22,
                              GOP_sheriff_ge+GOP_Gov22+GOP_cfo22+GOP_AG22+GOP_AgC22+GOP_sen22+
                                DEM_sheriff_ge+DEM_Gov22+DEM_cfo22+DEM_AG22+DEM_AgC22+DEM_sen22),
         density = group_frac(map = map,pop, ALAND20))


write_csv(jax_plans,"congressional_stats1.csv")
