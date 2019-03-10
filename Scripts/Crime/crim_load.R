# Scripts for loading data sources
load_crim_pris_ctz <- function() {
  
  # Loads crim_pris_ctz data set from Eurostat, downloaded as csv files with following settings:
    # Full extraction, single file, flags and footnotes, cell formatting = 1 234.56
  # Downloads as zip file from https://ec.europa.eu/eurostat/data/database?ticket=ST-230836-inG2jcFY1D1zYI46tbWlyHqBhFhjJHEt8AnzeU0mar625Bdzlzkgdt9v8rp28zOTiCzl5dp8VgFwqFKXzkPyKztx-jpJZscgsw0KQUgSRzbVlM0-k1VY66GdNpUYH50SWeiNC8YzIm0dcfV1qkVnTzv8rHM0
  # Press "Data Explorer", then download (otherwise comes in weirldy formatted tar.gz)
  
  # Data set contains information on prisoners by citizenship (foreign or local) for each country in the EU
  # Values given are both total and per capita
  
  # Dependencies
  require(data.table) # Contains fread
  
  # Read data
  crim_pris_ctz <- fread("Data/Crime/crim_pris_ctz/crim_pris_ctz_1_Data.csv")
  
  # Update headings to be more intelligible and standardized
  names(crim_pris_ctz)[names(crim_pris_ctz) == "TIME"] <- "year"
  names(crim_pris_ctz)[names(crim_pris_ctz) == "GEO"] <- "region"
  names(crim_pris_ctz)[names(crim_pris_ctz) == "CITIZEN"] <- "citizenship"
  names(crim_pris_ctz)[names(crim_pris_ctz) == "UNIT"] <- "unit"
  names(crim_pris_ctz)[names(crim_pris_ctz) == "Value"] <- "value"
  names(crim_pris_ctz)[names(crim_pris_ctz) == "Flag and Footnotes"] <- "flag_and_footnotes"
  
  # Update field values to be more intelligible and standardized
  crim_pris_ctz[region == "Germany (until 1990 former territory of the FRG)", region := "Germany"] # No pre-1990 data in the dataset anyway
  crim_pris_ctz[region == "Kosovo (under United Nations Security Council Resolution 1244/99)", region := "Kosovo"] # Simplifying a bit
  crim_pris_ctz[citizenship == "Foreign country", citizenship := "foreign"]
  crim_pris_ctz[citizenship == "Reporting country", citizenship := "local"]
  crim_pris_ctz[citizenship == "Total", citizenship := "total"]
  crim_pris_ctz[unit == "Number", unit := "actual"]
  crim_pris_ctz[unit == "Per hundred thousand inhabitants", unit := "per hundred thousand inhabitants"]
  crim_pris_ctz[value == ":", value := NA] # : used to indicate missing values
  crim_pris_ctz[, value := gsub(pattern = " ", replacement = "", x = value)] # Remove the blank space so as.numeric recognizes the numbers
  
  # Update formatting and factorise
  crim_pris_ctz[, year := factor(year)]
  crim_pris_ctz[, region := factor(region)]
  crim_pris_ctz[, citizenship := factor(citizenship)]
  crim_pris_ctz[, unit := factor(unit)]
  crim_pris_ctz[, value := as.numeric(value)]
  crim_pris_ctz[, flag_and_footnotes := as.character(flag_and_footnotes)]
  
  # Return clean data
  return(crim_pris_ctz)
  
}