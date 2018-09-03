#Load required libraries
library(tidyr)
library(dplyr)
library(compare)

#Global Constasts
MILLION <- 1000000

#Load the data into R
companies <- read.delim("companies.txt", header = TRUE)
rounds2 <- read.csv("rounds2.csv", header = TRUE)
mappings <- read.csv("mapping.csv", header = TRUE)

#Format data - Format the mapping from Wide to Long Format
#TODO - Map 0 to na in mapping file
mappings <- gather(mappings, sector_name, value, 2:10)
mappings <- filter(mappings, value == 1)

#__________________________________________________________________________________________________________________________________

#Checkpoint 1: Data Cleaning 1
companies$permalink = tolower(companies$permalink)
rounds2$company_permalink = tolower(rounds2$company_permalink)
uniq_companies_1 <- length(unique(rounds2$company_permalink))
uniq_companies_2 <- length(unique(companies$permalink))
colnames(rounds2)[1] <- "permalink"
master_frame <- merge(rounds2, companies, by.x="permalink", by.y="permalink")
print(paste('  ********************** Unit companies in rounds2 - ', uniq_companies_1 , ' ********************** '))
print(paste('  ********************** Unit companies in companies - ', uniq_companies_2 , ' ********************** '))
print(paste('  ********************** No of Observations in  master_frame - ', nrow(master_frame) , ' ********************** '))

#__________________________________________________________________________________________________________________________________

#Checkpoint 2: Funding Type Analysis
#Check for count of na values before applying groupBy
sum(is.na(rounds2$funding_round_type))
investment_type_group <- group_by(rounds2,funding_round_type)
funding_investment_type <- summarise(investment_type_group, mean(raised_amount_usd, na.rm = TRUE))
colnames(funding_investment_type)[2] <- "mean_raised_amount_usd"
print("  ********************** Average Values of Investments for Each of these Funding Types **********************")
print(funding_investment_type)
funding_investment_type$mean_raised_amount_usd <- (funding_investment_type$mean_raised_amount_usd/MILLION)
funding_investment_type_5to15 <- filter(funding_investment_type, mean_raised_amount_usd >=5 & mean_raised_amount_usd <= 15)
print(" ********************** Investment between 5 to 15 million USD  ********************** ")
print(funding_investment_type_5to15)

#__________________________________________________________________________________________________________________________________

#Checkpoint 3: Country Analysis for *Venture type* investment
venture_investments <- filter(master_frame, funding_round_type == "venture")
venture_investments <- filter(venture_investments, country_code != "") #Remove empty country code observations
sum(venture_investments$country_code=="") #Cross-check that there are no empty values

investment_country_group <- group_by(venture_investments,country_code)
funding_investment_country <- summarise(investment_country_group, sum(raised_amount_usd, na.rm = TRUE))
colnames(funding_investment_country)[2] <- "raised_amount_usd"
funding_investment_country <- arrange(filter(funding_investment_country, country_code != ""), desc(raised_amount_usd))
top9  <- funding_investment_country[1:9,]
print(" ********************** Top nine countries which have received the highest total funding   ********************** ")
print(top9)

#__________________________________________________________________________________________________________________________________

#Checkpoint 4: Sector Analysis 1
modified_companies <- separate(companies, col = category_list, into =c("category_list"), sep = "\\|")
modified_companies <- merge(x=mappings, y=modified_companies, by.x="category_list", by.y="category_list")
modified_companies <- filter(modified_companies, sector_name != 'Blanks')

#__________________________________________________________________________________________________________________________________

#Checkpoint 5: Sector Analysis 2
sector_analysis_frame <- merge(rounds2, modified_companies, by.x="permalink", by.y="permalink")
english_country1 = "USA"
english_country2 = "GBR"
english_country3 = "IND"

#English speaking country wise data frame
eng_con1_df <- filter(sector_analysis_frame, country_code == english_country1 & funding_round_type == "venture")
eng_con2_df <- filter(sector_analysis_frame, country_code == english_country2 & funding_round_type == "venture")
eng_con3_df <- filter(sector_analysis_frame, country_code == english_country3 & funding_round_type == "venture")

total_no_of_invest_cou1 <- sum(!is.na(eng_con1_df$funding_round_permalink))
total_no_of_invest_cou2 <- sum(!is.na(eng_con2_df$funding_round_permalink))
total_no_of_invest_cou3 <- sum(!is.na(eng_con3_df$funding_round_permalink))

print(" ********************** Total number of Investments (count) ********************** ")
print(paste(english_country1, ' = ', total_no_of_invest_cou1))
print(paste(english_country2, ' = ', total_no_of_invest_cou2))
print(paste(english_country3, ' = ', total_no_of_invest_cou3))

total_amount_of_invest_cou1 <- sum(eng_con1_df$raised_amount_usd, na.rm = TRUE)
total_amount_of_invest_cou2 <- sum(eng_con2_df$raised_amount_usd, na.rm = TRUE)
total_amount_of_invest_cou3 <- sum(eng_con3_df$raised_amount_usd, na.rm = TRUE)

print(" ********************** Total amount of investment (USD) ********************** ")
print(paste(english_country1, ' = ', total_amount_of_invest_cou1))
print(paste(english_country2, ' = ', total_amount_of_invest_cou2))
print(paste(english_country3, ' = ', total_amount_of_invest_cou3))


#Helper function - Group by sector and filter by fund type FT and between 5-15 million range
groupBySectorHelper <- function(data) {
  investment_sector_wise <- group_by(data,sector_name)
  investment_sector_wise <- summarise(investment_sector_wise, n())  
  colnames(investment_sector_wise)[2] <- "cal_metric" 
  investment_sector_wise <- arrange(investment_sector_wise, desc(cal_metric))
  return (investment_sector_wise)
}


top_sector_cou1 <- groupBySectorHelper(eng_con1_df)[1:5,]
top_sector_cou2 <- groupBySectorHelper(eng_con2_df)[1:5,]
top_sector_cou3 <- groupBySectorHelper(eng_con3_df)[1:5,]

print(" ********************** Top Sector name & no. of investments ********************** ")
print(paste(english_country1, ' = ', top_sector_cou1[1,]$sector_name, top_sector_cou1[1,]$cal_metric))
print(paste(english_country2, ' = ', top_sector_cou2[1,]$sector_name, top_sector_cou2[1,]$cal_metric))
print(paste(english_country3, ' = ', top_sector_cou3[1,]$sector_name, top_sector_cou3[1,]$cal_metric))

print(" ********************** Second Sector name & no. of investments ********************** ")
print(paste(english_country1, ' = ', top_sector_cou1[2,]$sector_name, top_sector_cou1[2,]$cal_metric))
print(paste(english_country2, ' = ', top_sector_cou2[2,]$sector_name, top_sector_cou2[2,]$cal_metric))
print(paste(english_country3, ' = ', top_sector_cou3[2,]$sector_name, top_sector_cou3[2,]$cal_metric))

print(" ********************** Third Sector name & no. of investments ********************** ")
print(paste(english_country1, ' = ', top_sector_cou1[3,]$sector_name, top_sector_cou1[3,]$cal_metric))
print(paste(english_country2, ' = ', top_sector_cou2[3,]$sector_name, top_sector_cou2[3,]$cal_metric))
print(paste(english_country3, ' = ', top_sector_cou3[3,]$sector_name, top_sector_cou3[3,]$cal_metric))


printCompanyMaxInvestment <- function(df, sectorRanks,sectorIndex, countryName){
  sectorName <- sectorRanks[sectorIndex,1]$sector_name;
  all_companies_in_top_sec <- filter(df, sector_name == sectorName)
  all_companies_in_top_sec <- group_by(all_companies_in_top_sec, permalink);
  all_companies_in_top_sec <- summarise(all_companies_in_top_sec, sum(raised_amount_usd, na.rm = TRUE))
  colnames(all_companies_in_top_sec)[2] <- "cal_metric" 
  max_investment_company <- all_companies_in_top_sec[which.max(all_companies_in_top_sec$cal_metric),]
  company_name <- companies[which(companies$permalink == max_investment_company$permalink), ]$name
  print(paste("Company with max funds *", countryName, " * ", sectorName ," * ---> ", company_name,format(max_investment_company$cal_metric, scientific=FALSE)))
}

printCompanyMaxInvestment(eng_con1_df,top_sector_cou1, 1, english_country1);
printCompanyMaxInvestment(eng_con2_df,top_sector_cou2, 1, english_country2);
printCompanyMaxInvestment(eng_con3_df,top_sector_cou3, 1, english_country3);

printCompanyMaxInvestment(eng_con1_df,top_sector_cou1, 2, english_country1);
printCompanyMaxInvestment(eng_con2_df,top_sector_cou2, 2, english_country2);
printCompanyMaxInvestment(eng_con3_df,top_sector_cou3, 2, english_country3);

#__________________________________________________________________________________________________________________________________

