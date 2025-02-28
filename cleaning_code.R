carehomes <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/care_home_inspections_2025.csv")%>%
  dplyr::mutate(ownership = ifelse(ownership==0, "For-profit",
                                   ifelse(ownership==1, "Local Authority",
                                          ifelse(ownership==2, "Third Sector", NA))))


carehomes <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/care_home_inspections_2025.csv")%>%
  dplyr::mutate(ownership = ifelse(ownership==0, "For-profit",
                                   ifelse(ownership==1, "Local Authority",
                                          ifelse(ownership==2, "Third Sector", NA))))%>%
  dplyr::filter(serviceuserbandyoungeradults=="Y")



carehomesstart <- carehomes %>%dplyr::select(year_location_start_2025,ownership, locationid )
carehomesstart$year <- carehomesstart$year_location_start_2025

Providernobs <- unique(carehomesstart[c("year", "ownership", "locationid")])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, ownership) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]

all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "year"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "year"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)

nobs <- rbind(nobsla, nobsvol,nobsprive)


carehomesend <- carehomes %>%dplyr::select(year_location_end_2025,ownership, locationid )%>%
  filter(year_location_end_2025!="")

carehomesend$year <- carehomesend$year_location_end_2025
Providernobs <- unique(carehomesend[c("year", "ownership", "locationid")])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, ownership) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]


all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "year"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "year"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobsend <- rbind(nobsla, nobsvol,nobsprive)
nobsend$cumulative_end <- nobsend$cumulative

nobser <- merge(nobs, nobsend[c("ownership", "year", "cumulative_end")], by= c("ownership", "year"), all=T)
nobser$runningsum <- nobser$cumulative-nobser$cumulative_end

 # ggplot(nobser[nobser$year>2010,], aes(x=year, y=runningsum, group=ownership,fill=ownership,  colour = ownership))+
 #   geom_bar(position="fill", stat="identity")+
 #   #geom_smooth(method="loess", span = 0.3)+
 #   theme_minimal()+
 #   scale_fill_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
 #   # theme(legend.position="left")+
 #   labs(x="Year", y="Proportion of Care homes", title = "Proportion of Active Care homes", fill="Ownership", color="Ownership")
 # 


carehomesstart <- carehomes %>%dplyr::select(year_location_start_2025,ownership, locationid, carehomesbeds )
carehomesstart$year <- carehomesstart$year_location_start_2025


Providernobs <- unique(carehomesstart[c("year", "ownership", "locationid", "carehomesbeds")])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, ownership) %>% dplyr::summarize(nobs = sum(carehomesbeds, na.rm=T))
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]

all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "year"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "year"), all=T)
#nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)

nobs <- rbind(nobsla, nobsvol,nobsprive)


carehomesend <- carehomes %>%dplyr::select(year_location_end_2025,ownership, locationid )%>%
  filter(year_location_end_2025!="")
carehomesend$year <- carehomesend$year_location_end_2025

carehomesend$year <- format(carehomesend$date,"%Y")
Providernobs <- unique(carehomesend[c("year", "ownership", "locationid")])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, ownership) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]


all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "year"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 16), ]  # Base R
all$year <- seq(from =2010, to=2025)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "year"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobsend <- rbind(nobsla, nobsvol,nobsprive)
nobsend$cumulative_end <- nobsend$cumulative

nobser_beds <- merge(nobs, nobsend[c("ownership", "year", "cumulative_end")], by= c("ownership", "year"), all=T)
nobser_beds$runningsum <- nobser_beds$cumulative-nobser_beds$cumulative_end


homes_fin <- merge(nobser %>%
                     dplyr::select(year, ownership, runningsum)%>%
                     dplyr::rename(number_carehomes = runningsum),
                   nobser_beds%>%
                     dplyr::select(year, ownership, runningsum)%>%
                     dplyr::rename(number_carehome_beds = runningsum),
                   by=c("year", "ownership"))

homes_fin <- homes_fin %>%
  group_by(year) %>%
  mutate(
    pct_homes = (number_carehomes / sum(number_carehomes)) * 100,
    pct_beds = (number_carehome_beds / sum(number_carehome_beds)) * 100
  )%>%
  dplyr::filter(year!=2010,
                year!=2025)

write.csv(homes_fin, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/ITV/care_homes_and_beds_all.csv")
  





updata <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_Materials/Data/complete inspection and location data_ben_feb2025v2.csv")%>%
  mutate(reportdate = parse_date_time(reportdate, orders = c("dmy", "ymd")))%>%
  dplyr::select(reportdate, overall, ownership, locationid, location_start_2025, location_end_2025)%>%
  dplyr::mutate(reportdate = as.Date(reportdate))%>%
  dplyr::mutate(good = ifelse(overall=="Good", 1,
                              ifelse(overall=="Outstanding",1,
                                     ifelse(overall=="Requires improvement", 0,
                                            ifelse(overall=="Requires Improvement", 0,
                                                   ifelse(overall=="Inadequate", 0,NA))))),
                ownership = ifelse(ownership==0, "For-profit",
                                   ifelse(ownership==1, "Local Authority",
                                          ifelse(ownership==2, "Third sector", NA))),
                rating = ifelse(overall=="Requires Improvement", "Requires improvement", overall))%>%
  dplyr::filter(!is.na(good),
                !ownership=="")%>%
  dplyr::arrange(reportdate) 

library(tidyverse)
library(lubridate)
library(data.table) # For faster data manipulation


# Convert Stata dates to R dates
# Stata's date origin is 1960-01-01, so adjust accordingly
origin_date <- as.Date("1960-01-01")
care_homes <- updata 

# Convert the data to data.table for faster processing
care_homes <- as.data.table(care_homes)

origin_date <- as.Date("1960-01-01")
care_homes[, `:=`(
  location_start = as.Date(location_start_2025, origin = origin_date),
  reportdate = as.Date(reportdate)
)]

# Set a future date for homes that never closed (NA values)
# Using 2027-01-01 as the future date as you mentioned
future_date <- as.Date("2027-01-01")
care_homes[, location_end := ifelse(is.na(location_end_2025), 
                                    as.numeric(future_date - origin_date), 
                                    location_end_2025)]
care_homes[, location_end := as.Date(location_end, origin = origin_date)]


# Instead of processing day by day, we'll use a more efficient approach:
# 1. Create a record for each location with start and end dates of each rating
# 2. Determine the date ranges for calculation
# 3. Sample at regular intervals (e.g., weekly or monthly) instead of daily

# Step 1: Determine the valid date ranges for each rating for each location
# Sort by location and date
setorder(care_homes, locationid, reportdate)

# Create a data.table of all rating periods
rating_periods <- care_homes[, {
  # For each location, get all inspection dates
  dates <- reportdate
  n <- length(dates)
  
  # Determine end date for each rating period (next inspection or location closure)
  end_dates <- c(dates[2:n], location_end[n])
  
  # For last inspection, use location_end
  if (n == 1) {
    end_dates <- location_end
  }
  
  # Return a data.table with start and end date for each rating period
  list(
    start_date = dates,
    end_date = pmin(end_dates, location_end),
    rating = rating,
    ownership = ownership
  )
}, by = locationid]

# Step 2: Define the date range for calculation
# Use the min and max dates from your data
min_date <- min(care_homes$reportdate)
max_date <- max(care_homes$reportdate)

# Step 3: Sample at regular intervals (e.g., weekly)
# Create a sequence of dates to sample at
sample_dates <- seq(min_date, max_date, by = "week")

# Step 4: For each sample date, calculate the proportion of good ratings
# This is much faster than calculating for every day
# Calculate the proportion of each rating at each sample date
result_list <- lapply(sample_dates, function(sample_date) {
  # Find all rating periods that cover this date
  active_ratings <- rating_periods[start_date <= sample_date & end_date >= sample_date]
  
  if (nrow(active_ratings) > 0) {
    # Get all unique ownership types and ratings
    all_ownerships <- unique(active_ratings$ownership)
    all_ratings <- unique(care_homes$rating)  # Using all ratings from original data
    
    # Create a complete grid of all possible combinations
    grid <- expand.grid(ownership = all_ownerships, rating = all_ratings, stringsAsFactors = FALSE)
    grid <- as.data.table(grid)
    
    # Count homes by sector and rating
    counts <- active_ratings[, .N, by = .(ownership, rating)]
    
    # Merge with complete grid to ensure all combinations exist
    complete_counts <- merge(grid, counts, by = c("ownership", "rating"), all.x = TRUE)
    complete_counts[is.na(N), N := 0]  # Replace NA with 0
    
    # Calculate totals by sector
    totals <- active_ratings[, .(total = .N), by = ownership]
    
    # Join to get percentages
    summary <- merge(complete_counts, totals, by = "ownership")
    summary[, percent := (N / total) * 100]
    
    # Add the date
    summary$date <- sample_date
    
    return(summary)
  } else {
    return(NULL)
  }
})


# Remove any NULL results and combine
result_list <- Filter(Negate(is.null), result_list)
rating_percentages <- rbindlist(result_list)

# Convert back to tibble for ggplot
rating_percentages <- as_tibble(rating_percentages)

# Convert date column to Date format
rating_percentages$date <- as.Date(rating_percentages$date)

# Extract year
rating_percentages$year <- year(rating_percentages$date)

# Select the final observation for each ownership type in every year
final_obs <- rating_percentages %>%
  group_by(year, ownership) %>%
  filter(date == max(date),
         year!=2014,
         year!=2025) %>%
  ungroup()


write.csv(final_obs, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/ITV/care_homes_inspections_all.csv")



updata <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_Materials/Data/complete inspection and location data_ben_feb2025v2.csv")%>%
  mutate(reportdate = parse_date_time(reportdate, orders = c("dmy", "ymd")))%>%
  dplyr::filter(serviceuserbandyoungeradults=="Y")%>%
  dplyr::select(reportdate, overall, ownership, locationid, location_start_2025, location_end_2025)%>%
  dplyr::mutate(reportdate = as.Date(reportdate))%>%
  dplyr::mutate(good = ifelse(overall=="Good", 1,
                              ifelse(overall=="Outstanding",1,
                                     ifelse(overall=="Requires improvement", 0,
                                            ifelse(overall=="Requires Improvement", 0,
                                                   ifelse(overall=="Inadequate", 0,NA))))),
                ownership = ifelse(ownership==0, "For-profit",
                                   ifelse(ownership==1, "Local Authority",
                                          ifelse(ownership==2, "Third sector", NA))),
                rating = ifelse(overall=="Requires Improvement", "Requires improvement", overall))%>%
  dplyr::filter(!is.na(good),
                !ownership=="")%>%
  dplyr::arrange(reportdate) 

library(tidyverse)
library(lubridate)
library(data.table) # For faster data manipulation


# Convert Stata dates to R dates
# Stata's date origin is 1960-01-01, so adjust accordingly
origin_date <- as.Date("1960-01-01")
care_homes <- updata 

# Convert the data to data.table for faster processing
care_homes <- as.data.table(care_homes)

origin_date <- as.Date("1960-01-01")
care_homes[, `:=`(
  location_start = as.Date(location_start_2025, origin = origin_date),
  reportdate = as.Date(reportdate)
)]

# Set a future date for homes that never closed (NA values)
# Using 2027-01-01 as the future date as you mentioned
future_date <- as.Date("2027-01-01")
care_homes[, location_end := ifelse(is.na(location_end_2025), 
                                    as.numeric(future_date - origin_date), 
                                    location_end_2025)]
care_homes[, location_end := as.Date(location_end, origin = origin_date)]

# Sort by location and date
setorder(care_homes, locationid, reportdate)

# Instead of processing day by day, we'll use a more efficient approach:
# 1. Create a record for each location with start and end dates of each rating
# 2. Determine the date ranges for calculation
# 3. Sample at regular intervals (e.g., weekly or monthly) instead of daily

# Step 1: Determine the valid date ranges for each rating for each location
# Sort by location and date
setorder(care_homes, locationid, reportdate)

# Create a data.table of all rating periods
rating_periods <- care_homes[, {
  # For each location, get all inspection dates
  dates <- reportdate
  n <- length(dates)
  
  # Determine end date for each rating period (next inspection or location closure)
  end_dates <- c(dates[2:n], location_end[n])
  
  # For last inspection, use location_end
  if (n == 1) {
    end_dates <- location_end
  }
  
  # Return a data.table with start and end date for each rating period
  list(
    start_date = dates,
    end_date = pmin(end_dates, location_end),
    rating = rating,
    ownership = ownership
  )
}, by = locationid]

# Step 2: Define the date range for calculation
# Use the min and max dates from your data
min_date <- min(care_homes$reportdate)
max_date <- max(care_homes$reportdate)

# Step 3: Sample at regular intervals (e.g., weekly)
# Create a sequence of dates to sample at
sample_dates <- seq(min_date, max_date, by = "week")

# Step 4: For each sample date, calculate the proportion of good ratings
# This is much faster than calculating for every day
result_list <- lapply(sample_dates, function(sample_date) {
  # Find all rating periods that cover this date
  active_ratings <- rating_periods[start_date <= sample_date & end_date >= sample_date]
  
  if (nrow(active_ratings) > 0) {
    # Get all unique ownership types and ratings
    all_ownerships <- unique(active_ratings$ownership)
    all_ratings <- unique(care_homes$rating)  # Using all ratings from original data
    
    # Create a complete grid of all possible combinations
    grid <- expand.grid(ownership = all_ownerships, rating = all_ratings, stringsAsFactors = FALSE)
    grid <- as.data.table(grid)
    
    # Count homes by sector and rating
    counts <- active_ratings[, .N, by = .(ownership, rating)]
    
    # Merge with complete grid to ensure all combinations exist
    complete_counts <- merge(grid, counts, by = c("ownership", "rating"), all.x = TRUE)
    complete_counts[is.na(N), N := 0]  # Replace NA with 0
    
    # Calculate totals by sector
    totals <- active_ratings[, .(total = .N), by = ownership]
    
    # Join to get percentages
    summary <- merge(complete_counts, totals, by = "ownership")
    summary[, percent := (N / total) * 100]
    
    # Add the date
    summary$date <- sample_date
    
    return(summary)
  } else {
    return(NULL)
  }
})

# Combine results
rolling_summary <- rbindlist(result_list)

# Convert back to tibble for ggplot
rolling_summary <- as_tibble(rolling_summary)

# Convert date column to Date format
rolling_summary$date <- as.Date(rolling_summary$date)

# Extract year
rolling_summary$year <- year(rolling_summary$date)

# Select the final observation for each ownership type in every year
final_obs <- rolling_summary %>%
  group_by(year, ownership) %>%
  filter(date == max(date),
         year!=2014,
         year!=2025) %>%
  ungroup()


write.csv(final_obs, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/ITV/care_homes_inspections_working_age.csv")






updata <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_Materials/Data/complete inspection and location data noncare homes_ben_feb2025v2.csv")%>%
  mutate(reportdate = parse_date_time(reportdate, orders = c("dmy", "ymd")))%>%
  dplyr::filter(serviceuserbandyoungeradults=="Y",
                locationprimaryinspectioncategor == "Community based adult social care services")%>%
  dplyr::select(reportdate, overall, ownership, locationid, location_start_2025, location_end_2025)%>%
  dplyr::mutate(reportdate = as.Date(reportdate))%>%
  dplyr::mutate(good = ifelse(overall=="Good", 1,
                              ifelse(overall=="Outstanding",1,
                                     ifelse(overall=="Requires improvement", 0,
                                            ifelse(overall=="Requires Improvement", 0,
                                                   ifelse(overall=="Inadequate", 0,NA))))),
                ownership = ifelse(ownership==0, "For-profit",
                                   ifelse(ownership==1, "Local Authority",
                                          ifelse(ownership==2, "Third sector", NA))),
                rating = ifelse(overall=="Requires Improvement", "Requires improvement", overall))%>%
  dplyr::filter(!is.na(good),
                !ownership=="")%>%
  dplyr::arrange(reportdate) 

library(tidyverse)
library(lubridate)
library(data.table) # For faster data manipulation


# Convert Stata dates to R dates
# Stata's date origin is 1960-01-01, so adjust accordingly
origin_date <- as.Date("1960-01-01")
care_homes <- updata 

# Convert the data to data.table for faster processing
care_homes <- as.data.table(care_homes)

origin_date <- as.Date("1960-01-01")
care_homes[, `:=`(
  location_start = as.Date(location_start_2025, origin = origin_date),
  reportdate = as.Date(reportdate)
)]

# Set a future date for homes that never closed (NA values)
# Using 2027-01-01 as the future date as you mentioned
future_date <- as.Date("2027-01-01")
care_homes[, location_end := ifelse(is.na(location_end_2025), 
                                    as.numeric(future_date - origin_date), 
                                    location_end_2025)]
care_homes[, location_end := as.Date(location_end, origin = origin_date)]

# Sort by location and date
setorder(care_homes, locationid, reportdate)

# Instead of processing day by day, we'll use a more efficient approach:
# 1. Create a record for each location with start and end dates of each rating
# 2. Determine the date ranges for calculation
# 3. Sample at regular intervals (e.g., weekly or monthly) instead of daily

# Step 1: Determine the valid date ranges for each rating for each location
# Sort by location and date
setorder(care_homes, locationid, reportdate)

# Create a data.table of all rating periods
rating_periods <- care_homes[, {
  # For each location, get all inspection dates
  dates <- reportdate
  n <- length(dates)
  
  # Determine end date for each rating period (next inspection or location closure)
  end_dates <- c(dates[2:n], location_end[n])
  
  # For last inspection, use location_end
  if (n == 1) {
    end_dates <- location_end
  }
  
  # Return a data.table with start and end date for each rating period
  list(
    start_date = dates,
    end_date = pmin(end_dates, location_end),
    rating = rating,
    ownership = ownership
  )
}, by = locationid]

# Step 2: Define the date range for calculation
# Use the min and max dates from your data
min_date <- min(care_homes$reportdate)
max_date <- max(care_homes$reportdate)

# Step 3: Sample at regular intervals (e.g., weekly)
# Create a sequence of dates to sample at
sample_dates <- seq(min_date, max_date, by = "week")

# Step 4: For each sample date, calculate the proportion of good ratings
# This is much faster than calculating for every day
result_list <- lapply(sample_dates, function(sample_date) {
  # Find all rating periods that cover this date
  active_ratings <- rating_periods[start_date <= sample_date & end_date >= sample_date]
  
  if (nrow(active_ratings) > 0) {
    # Get all unique ownership types and ratings
    all_ownerships <- unique(active_ratings$ownership)
    all_ratings <- unique(care_homes$rating)  # Using all ratings from original data
    
    # Create a complete grid of all possible combinations
    grid <- expand.grid(ownership = all_ownerships, rating = all_ratings, stringsAsFactors = FALSE)
    grid <- as.data.table(grid)
    
    # Count homes by sector and rating
    counts <- active_ratings[, .N, by = .(ownership, rating)]
    
    # Merge with complete grid to ensure all combinations exist
    complete_counts <- merge(grid, counts, by = c("ownership", "rating"), all.x = TRUE)
    complete_counts[is.na(N), N := 0]  # Replace NA with 0
    
    # Calculate totals by sector
    totals <- active_ratings[, .(total = .N), by = ownership]
    
    # Join to get percentages
    summary <- merge(complete_counts, totals, by = "ownership")
    summary[, percent := (N / total) * 100]
    
    # Add the date
    summary$date <- sample_date
    
    return(summary)
  } else {
    return(NULL)
  }
})
# Combine results
rolling_summary <- rbindlist(result_list)

# Convert back to tibble for ggplot
rolling_summary <- as_tibble(rolling_summary)

# Convert date column to Date format
rolling_summary$date <- as.Date(rolling_summary$date)

# Extract year
rolling_summary$year <- year(rolling_summary$date)

# Select the final observation for each ownership type in every year
final_obs <- rolling_summary %>%
  group_by(year, ownership) %>%
  filter(date == max(date),
         year!=2014,
         year!=2025) %>%
  ungroup()


write.csv(final_obs, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/ITV/non_care_homes_inspections_working_age.csv")




updata <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_Materials/Data/complete inspection and location data noncare homes_ben_feb2025v2.csv")%>%
  mutate(reportdate = parse_date_time(reportdate, orders = c("dmy", "ymd")))%>%
  dplyr::filter(locationprimaryinspectioncategor == "Community based adult social care services")%>%
  dplyr::select(reportdate, overall, ownership, locationid, location_start_2025, location_end_2025)%>%
  dplyr::mutate(reportdate = as.Date(reportdate))%>%
  dplyr::mutate(good = ifelse(overall=="Good", 1,
                              ifelse(overall=="Outstanding",1,
                                     ifelse(overall=="Requires improvement", 0,
                                            ifelse(overall=="Requires Improvement", 0,
                                                   ifelse(overall=="Inadequate", 0,NA))))),
                ownership = ifelse(ownership==0, "For-profit",
                                   ifelse(ownership==1, "Local Authority",
                                          ifelse(ownership==2, "Third sector", NA))),
                rating = ifelse(overall=="Requires Improvement", "Requires improvement", overall))%>%
  
  dplyr::filter(!is.na(good),
                !ownership=="")%>%
  dplyr::arrange(reportdate) 

library(tidyverse)
library(lubridate)
library(data.table) # For faster data manipulation


# Convert Stata dates to R dates
# Stata's date origin is 1960-01-01, so adjust accordingly
origin_date <- as.Date("1960-01-01")
care_homes <- updata 

# Convert the data to data.table for faster processing
care_homes <- as.data.table(care_homes)

origin_date <- as.Date("1960-01-01")
care_homes[, `:=`(
  location_start = as.Date(location_start_2025, origin = origin_date),
  reportdate = as.Date(reportdate)
)]

# Set a future date for homes that never closed (NA values)
# Using 2027-01-01 as the future date as you mentioned
future_date <- as.Date("2027-01-01")
care_homes[, location_end := ifelse(is.na(location_end_2025), 
                                    as.numeric(future_date - origin_date), 
                                    location_end_2025)]
care_homes[, location_end := as.Date(location_end, origin = origin_date)]

# Sort by location and date
setorder(care_homes, locationid, reportdate)

# Instead of processing day by day, we'll use a more efficient approach:
# 1. Create a record for each location with start and end dates of each rating
# 2. Determine the date ranges for calculation
# 3. Sample at regular intervals (e.g., weekly or monthly) instead of daily

# Step 1: Determine the valid date ranges for each rating for each location
# Sort by location and date
setorder(care_homes, locationid, reportdate)

# Create a data.table of all rating periods
rating_periods <- care_homes[, {
  # For each location, get all inspection dates
  dates <- reportdate
  n <- length(dates)
  
  # Determine end date for each rating period (next inspection or location closure)
  end_dates <- c(dates[2:n], location_end[n])
  
  # For last inspection, use location_end
  if (n == 1) {
    end_dates <- location_end
  }
  
  # Return a data.table with start and end date for each rating period
  list(
    start_date = dates,
    end_date = pmin(end_dates, location_end),
    rating = rating,
    ownership = ownership
  )
}, by = locationid]

# Step 2: Define the date range for calculation
# Use the min and max dates from your data
min_date <- min(care_homes$reportdate)
max_date <- max(care_homes$reportdate)

# Step 3: Sample at regular intervals (e.g., weekly)
# Create a sequence of dates to sample at
sample_dates <- seq(min_date, max_date, by = "week")

# Step 4: For each sample date, calculate the proportion of good ratings
# This is much faster than calculating for every day
result_list <- lapply(sample_dates, function(sample_date) {
  # Find all rating periods that cover this date
  active_ratings <- rating_periods[start_date <= sample_date & end_date >= sample_date]
  
  if (nrow(active_ratings) > 0) {
    # Get all unique ownership types and ratings
    all_ownerships <- unique(active_ratings$ownership)
    all_ratings <- unique(care_homes$rating)  # Using all ratings from original data
    
    # Create a complete grid of all possible combinations
    grid <- expand.grid(ownership = all_ownerships, rating = all_ratings, stringsAsFactors = FALSE)
    grid <- as.data.table(grid)
    
    # Count homes by sector and rating
    counts <- active_ratings[, .N, by = .(ownership, rating)]
    
    # Merge with complete grid to ensure all combinations exist
    complete_counts <- merge(grid, counts, by = c("ownership", "rating"), all.x = TRUE)
    complete_counts[is.na(N), N := 0]  # Replace NA with 0
    
    # Calculate totals by sector
    totals <- active_ratings[, .(total = .N), by = ownership]
    
    # Join to get percentages
    summary <- merge(complete_counts, totals, by = "ownership")
    summary[, percent := (N / total) * 100]
    
    # Add the date
    summary$date <- sample_date
    
    return(summary)
  } else {
    return(NULL)
  }
})

# Combine results
rolling_summary <- rbindlist(result_list)

# Convert back to tibble for ggplot
rolling_summary <- as_tibble(rolling_summary)

# Convert date column to Date format
rolling_summary$date <- as.Date(rolling_summary$date)

# Extract year
rolling_summary$year <- year(rolling_summary$date)

# Select the final observation for each ownership type in every year
final_obs <- rolling_summary %>%
  group_by(year, ownership) %>%
  filter(date == max(date),
         year!=2014,
         year!=2025) %>%
  ungroup()


write.csv(final_obs, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/ITV/non_care_homes_inspections_all.csv")





####Expenditure by LAs on social care, for all residential services broken down by year and sector since 2000-2015####

spenddata <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/refs/heads/main/Final_data/expenditure.csv"))


data1 <- spenddata %>%
  dplyr::filter(SupportSetting=="home care"|
                  SupportSetting=="Residential care home placements"|
                  SupportSetting=="Home care"|
                  SupportSetting=="Direct payments"|
                  SupportSetting=="Nursing home placements"|
                  SupportSetting=="Supported and other accommodation"|
                  SupportSetting=="Total over 65")%>%
  dplyr::mutate(SupportSetting = ifelse(SupportSetting=="home care", "Home care", SupportSetting))%>%
  dplyr::select(year, SupportSetting, Expenditure, Sector)%>%
  dplyr::group_by(year, SupportSetting, Sector)%>%
  dplyr::summarise(Expenditure = sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::filter(year<2015)

write.csv(data1, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/ITV/Expenditure_over_65_pre2015.csv")


data1 <- spenddata %>%
  dplyr::filter(SupportSetting=="home care"|
                  SupportSetting=="Residential care home placements"|
                  SupportSetting=="Home care"|
                  SupportSetting=="Direct payments"|
                  SupportSetting=="Nursing home placements"|
                  SupportSetting=="Supported and other accommodation"|
                  SupportSetting=="Total over 65")%>%
  dplyr::mutate(SupportSetting = ifelse(SupportSetting=="home care", "Home care", SupportSetting))%>%
  dplyr::select(year, SupportSetting, Expenditure, Sector)%>%
  dplyr::group_by(year, SupportSetting, Sector)%>%
  dplyr::summarise(Expenditure = sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::filter(year>2014)

write.csv(data1, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/ITV/Expenditure_over_65_post2015.csv")


data1 <- spenddata %>%
  dplyr::filter(SupportSetting=="U65 LEARNING DISABILITY"|
                  SupportSetting=="U65 PHYSICAL DISABILITY"|
                  SupportSetting=="U65 MENTAL HEALTH")%>%
  dplyr::select(year, SupportSetting, Expenditure, Sector)%>%
  dplyr::group_by(year, SupportSetting, Sector)%>%
  dplyr::summarise(Expenditure = sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::filter(year>2014)

write.csv(data1, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/ITV/Expenditure_working_age_post2015.csv")


data1 <- spenddata %>%
  dplyr::filter(SupportSetting=="U65 LEARNING DISABILITY"|
                  SupportSetting=="U65 PHYSICAL DISABILITY"|
                  SupportSetting=="U65 MENTAL HEALTH")%>%
  dplyr::select(year, SupportSetting, Expenditure, Sector)%>%
  dplyr::group_by(year, SupportSetting, Sector)%>%
  dplyr::summarise(Expenditure = sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::filter(year<2015)

write.csv(data1, "Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/ITV/Expenditure_working_age_pre2015.csv")


