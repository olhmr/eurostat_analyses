# Functions for loading employment data
# Data downloaded from https://ec.europa.eu/eurostat/web/lfs/data/database
# Downloaded as gzip tsv file
load_lfsq_ewhun2 <- function() {
  
  # Dependencies
  require(data.table)
  
  # Read tsv
  lfsq_ewhun2 <- fread("Data/Employment/lfsq_ewhun2.tsv")
  
  # Move time to column
  lfsq_ewhun2 <- melt(data = lfsq_ewhun2, 
                      id.vars = c("unit,sex,worktime,wstatus,nace_r2,geo\\time"), 
                      measure.vars = names(lfsq_ewhun2)[2:length(names(lfsq_ewhun2))],
                      variable.name = "yearquarter",
                      value.name = "weekly_hours")
  
  # Split yearquarter
  lfsq_ewhun2[, c("year", "quarter") := tstrsplit(yearquarter, "Q", fixed = TRUE)]
  lfsq_ewhun2[, `yearquarter` := NULL]
  
  # Split first column
  lfsq_ewhun2[, c("unit", "sex", "worktime", "wstatus", "nace_r2", "geo\\time") := 
                tstrsplit(`unit,sex,worktime,wstatus,nace_r2,geo\\time`, ",", fixed = TRUE)]
  lfsq_ewhun2[, `unit,sex,worktime,wstatus,nace_r2,geo\\time` := NULL]
  
  # Split weekly_hours
  lfsq_ewhun2[, c("weekly_hours", "flag") := tstrsplit(weekly_hours, " ", fixed = TRUE)]
  
  # Change : to NA to indicate missing data
  lfsq_ewhun2[weekly_hours == ":", weekly_hours := NA]
  
  # Rename and reorder columns; remove unit as always hours
  lfsq_ewhun2 <- lfsq_ewhun2[, .(region = `geo\\time`,
                                 year, quarter, sex, worktime, 
                                 employment_status = wstatus, 
                                 nace_rev2_category = nace_r2,
                                 weekly_hours, flag)]
  
  # Update flag fields to be more intelligible
  lfsq_ewhun2[flag == "b", flag := "break in time series"]
  lfsq_ewhun2[flag == "c", flag := "confidential"]
  lfsq_ewhun2[flag == "d", flag := "definition differs, see metadata"]
  lfsq_ewhun2[flag == "e", flag := "estimated"]
  lfsq_ewhun2[flag == "f", flag := "forecast"]
  lfsq_ewhun2[flag == "n", flag := "not significant"]
  lfsq_ewhun2[flag == "p", flag := "provisional"]
  lfsq_ewhun2[flag == "r", flag := "revised"]
  lfsq_ewhun2[flag == "s", flag := "Eurostat estimate"]
  
  # Update employment status fields to be more intelligible
  lfsq_ewhun2[employment_status == "EMP", employment_status := "Employed persons"]
  lfsq_ewhun2[employment_status == "SAL", employment_status := "Employees"]
  lfsq_ewhun2[employment_status == "NSAL", employment_status := "Employed persons except employees"]
  lfsq_ewhun2[employment_status == "SELF", employment_status := "Self-employed persons"]
  lfsq_ewhun2[employment_status == "SELF_S", employment_status := "Self-employed persons with employees (employers)"]
  lfsq_ewhun2[employment_status == "SELF_NS", employment_status := "Self-employed persons without employees (own-account workers)"]
  lfsq_ewhun2[employment_status == "CFAM", employment_status := "Contributing family workers"]
  lfsq_ewhun2[employment_status == "NCFAM", employment_status := "Employed persons except contributing family workers"]
  lfsq_ewhun2[employment_status == "NRP", employment_status := "No response"]
  
  # Reformat
  lfsq_ewhun2 <- lfsq_ewhun2[, .(region, year = as.numeric(year), quarter = as.numeric(quarter), 
                                 sex, worktime, employment_status, nace_rev2_category, 
                                 weekly_hours = as.numeric(weekly_hours), flag)]
  
  # Return
  return(lfsq_ewhun2)
  
}