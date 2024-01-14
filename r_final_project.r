# Getting data from different csv files and combining them into one file
#####

years = c('2016', '2017', '2018', '2019')
input_path = 'C:/Users/Computer/Downloads/C5 Input for participants/%s_%s.csv'

domestic_path = 'domestic_visitors/domestic_visitors'
foreign_path = 'foreign_visitors/foreign_visitors'

all_domestic_data <- data.frame()
all_foreign_data <- data.frame()


for( year in years){
  file_path_domestic = sprintf(input_path, domestic_path, year)
  all_domestic_data =  rbind(all_domestic_data, read.csv(file_path_domestic))
  
  file_path_foreign = sprintf(input_path, foreign_path, year)
  all_foreign_data =  rbind(all_foreign_data, read.csv(file_path_foreign))
  
}


output_path = 'C:/Users/Computer/Downloads/C5 Input for participants/%s.csv'

write.csv(all_domestic_data, sprintf(output_path, domestic_path), row.names=F)
write.csv(all_foreign_data, sprintf(output_path, foreign_path), row.names=F)


# Data analysis

# Data Anlaysis
#####

# Loading data sets
domestic_vis = read.csv('C:/Users/Computer/Downloads/C5 Input for participants/domestic_visitors/domestic_visitors.csv')
foreign_vis = read.csv('C:/Users/Computer/Downloads/C5 Input for participants/foreign_visitors/foreign_visitors.csv')

install.packages('tidyverse')

# Load required library
library(dplyr)

# Group by district, calculate the sum of visitors, and select the top 10 districts
top_10_districts <- domestic_vis %>%
  group_by(district) %>%
  summarise(total_visitors = sum(visitors,na.rm = T)) %>%
  arrange(desc(total_visitors)) %>%
  head(10)

# Print the result
print(top_10_districts)


# Top and Bottom 3 CAGR districts

# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)

# Group by district and year, calculate the sum of visitors
total_vis_year <- domestic_vis %>%
  group_by(district, year) %>%
  summarise(visitors = sum(visitors, na.rm = T))

# Get unique districts for the year 2016
districts <- domestic_vis %>%
  filter(year == 2016) %>%
  select(district) %>%
  distinct() %>%
  pull()

# Function to calculate CAGR
calculate_cagr <- function(district_name) {
  v19 <- total_vis_year %>% filter(district == district_name & year == 2019) %>% pull(visitors)
  # v18 <- total_vis_year %>% filter(district == district_name & year == 2018) %>% pull(visitors)
  # v17 <- total_vis_year %>% filter(district == district_name & year == 2017) %>% pull(visitors)
  fv <- v19
  # fv <- v19 + v18 + v17
  pv <- total_vis_year %>% filter(district == district_name & year == 2016) %>% pull(visitors)


  if ( pv != 0) {
    cagr <- ((fv/pv)^(1/3) - 1) * 100
    return(c(district = district_name, cagr = round(cagr,2)))
  } else {
    return(c(district = district_name, cagr = 0) )
  }
}

# Apply the function to each district
cagr_list <- map_df(districts, calculate_cagr)

# Create a data frame from the list
district_cagr <- as.data.frame(cagr_list)
district_cagr$cagr = as.numeric(district_cagr$cagr)

# Get top and bottom 3 districts by CAGR
top3_districts_cagr <- district_cagr %>% arrange(desc(cagr)) %>% slice_head(n = 3)
bottom3_districts_cagr <- district_cagr %>% arrange(cagr) %>% slice_head(n = 3)

# Print the results
print("Top 3 Districts by CAGR:")
print(top3_districts_cagr)

print("Bottom 3 Districts by CAGR:")
print(bottom3_districts_cagr)


# Assuming your data frame is named domestic_vis
# Install and load the dplyr package if not already installed
# install.packages("dplyr")
library(dplyr)

# Filter data for Hyderabad district and group by district and month
month_wise_hyb <- domestic_vis %>%
  filter(district == 'Hyderabad') %>%
  group_by(district, month) %>%
  summarize(visitors = sum(visitors))

# Find the row with the maximum visitors
row_with_max_value <- month_wise_hyb %>%
  filter(visitors == max(visitors))

# Find the row with the minimum visitors
row_with_min_value <- month_wise_hyb %>%
  filter(visitors == min(visitors))

# Print the result
cat("Row with the largest value:\n")
print(row_with_max_value)

cat("\n")  # Print a blank line

cat("Row with the smallest value:\n")
print(row_with_min_value)


      

# Assuming your data frames are named domestic_vis and foreign_vis
# Replace 'district', 'date', 'month', 'year', 'visitors_x', and 'visitors_y' with the actual column names

# Load the dplyr package if not already loaded
# install.packages("dplyr")
library(dplyr)

# Merge data frames on 'district', 'date', 'month', and 'year'
dom_for_v <- inner_join(domestic_vis, foreign_vis, by = c('district', 'date', 'month', 'year'))

# Rename columns if needed
dom_for_v <- rename(dom_for_v, domestic_vis = visitors.x, foreign_vis = visitors.y)

# Group by 'district' and calculate the sum of 'domestic_vis' and 'foreign_vis'
dom_for_v_dis_sum <- dom_for_v %>%
  group_by(district) %>%
  summarise(domestic_vis = sum(domestic_vis,na.rm=T), foreign_vis = sum(foreign_vis,na.rm=T))

# Remove rows where 'foreign_vis' is zero
dom_for_v_dis_sum <- dom_for_v_dis_sum %>% filter(foreign_vis != 0)

# Calculate the ratio of 'domestic_vis' to 'foreign_vis'
dom_for_v_dis_sum <- dom_for_v_dis_sum %>%  mutate(dom_foreign_ratio = domestic_vis / foreign_vis)

# Find the top 3 and bottom 3 districts based on 'dom_foreign_ratio'
bottom_3_districts <- dom_for_v_dis_sum %>% arrange(desc(dom_foreign_ratio)) %>% select(district, dom_foreign_ratio) %>%  slice_head(n = 3)  
top_3_districts <- dom_for_v_dis_sum %>% arrange(dom_foreign_ratio) %>% select(district, dom_foreign_ratio) %>% slice_head(n = 3)

# Print the result
cat("Bottom 3 Districts ( Wrost domestic to foreign ratio):\n")
print(bottom_3_districts)

cat("\n")  # Print a blank line

cat("Top 3 Districts ( Best domestic to foreign ratio):\n")
print(top_3_districts)

population = read.csv('./population.csv')
head(population)


dom_for_v_dis_2019 = dom_for_v %>% filter(year == '2019')

dom_for_v_dis_sum_2019= dom_for_v_dis_2019 %>%
  group_by(district) %>%
  summarise(domestic_vis = sum(domestic_vis,na.rm=T), foreign_vis = sum(foreign_vis,na.rm=T))

data_with_population <- left_join(dom_for_v_dis_sum_2019, population, by = c('district'))



dom_for_v_dis_sum_pop <- data_with_population %>%  mutate(dom_foreign_sum = domestic_vis + foreign_vis)
dom_for_v_dis_sum_pop <- dom_for_v_dis_sum_pop %>% filter(dom_foreign_sum != 0)


dom_for_v_dis_sum_pop <- dom_for_v_dis_sum_pop %>%  
                          mutate(dom_foreign_pop_ratio = dom_foreign_sum / Population_2019 )

dom_for_v_dis_sum_pop_top5 = dom_for_v_dis_sum_pop %>% select(district,dom_foreign_pop_ratio) %>%
                              arrange(desc(dom_foreign_pop_ratio)) %>% 
                                slice_head(n = 3)

dom_for_v_dis_sum_pop_bottom5 = dom_for_v_dis_sum_pop %>% select(district,dom_foreign_pop_ratio) %>% 
  arrange(dom_foreign_pop_ratio) %>% 
  slice_head(n = 5)

dom_for_v_dis_sum_pop_top5
