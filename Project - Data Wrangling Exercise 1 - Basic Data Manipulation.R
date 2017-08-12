library(dplyr)
library(tidyr)
#Load the data into RStudio
refine_df <- read.csv("refine_original.csv")

#Clean up brand names
refine_df$company[grepl("^P", refine_df$company) | grepl("^p", refine_df$company) | grepl("^f", refine_df$company)] <- "philips"
refine_df$company[grepl("^A", refine_df$company) | grepl("^a", refine_df$company)] <- "akzo"
refine_df$company[grepl("^V", refine_df$company) | grepl("^v", refine_df$company)] <- "van houten"
refine_df$company[grepl("^U", refine_df$company) | grepl("^u", refine_df$company)] <- "unilever"

#Separate product code and number
refine_df <- refine_df %>%
  separate(Product.code...number, c("product_code", "product_number"), "-")
refine_df %>% select(product_code, product_number) %>% unique

#Add product categories
refine_df <- refine_df %>%
  mutate(
  product_category = case_when(
    .$product_code == "p" ~ "Smartphone",
    .$product_code == "v" ~ "TV",
    .$product_code == "x" ~ "Laptop",
    .$product_code == "q" ~ "Tablet"))

#Add full address for geocoding
refine_df <- refine_df %>%
  mutate(full_address = paste(address, city, country, sep = ", "))
       
#Create dummy variables for company and product category
final_df <- refine_df %>%
  mutate(company_philips = if_else(company == "philips", 1, 0)) %>%
  mutate(company_akzo = if_else(company == "akzo", 1, 0)) %>%
  mutate(company_van_houten = if_else(company == "van houten", 1, 0)) %>%
  mutate(company_unilever = if_else(company == "unilever", 1, 0)) %>%
  mutate(product_smartphone = if_else(product_code == "p", 1, 0)) %>%
  mutate(product_tv = if_else(product_code == "v", 1, 0)) %>%
  mutate(product_laptop = if_else(product_code == "x", 1, 0)) %>%
  mutate(product_tablet = if_else(product_code == "q", 1, 0)) 

#Submit the project on Github
write.csv(final_df, file = "refine_clean.csv")
