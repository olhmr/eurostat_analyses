load_lfsa_ewhuis <- function() {
  
  # Dependencies
  require(data.table)
  
  # Read tsv
  lfsa_ewhuis <- fread("Data/Employment/lfsa_ewhuis.tsv")
  
  # Promote headers
  names(lfsa_ewhuis) <- as.character(lfsa_ewhuis[1, ])
  lfsa_ewhuis <- lfsa_ewhuis[2:nrow(lfsa_ewhuis), ]
  
  # Move time to column
  lfsa_ewhuis <- melt(data = lfsa_ewhuis, 
                      id.vars = names(lfsa_ewhuis)[1], 
                      measure.vars = names(lfsa_ewhuis)[2:length(names(lfsa_ewhuis))],
                      variable.name = "year",
                      value.name = "weekly_hours")
  
  # Split first column
  lfsa_ewhuis[, c("unit", "sex", "worktime", "wstatus", "isco08", "geo\\time") := 
                tstrsplit(`unit,sex,worktime,wstatus,isco08,geo\\time`, ",", fixed = TRUE)]
  lfsa_ewhuis[, `unit,sex,worktime,wstatus,isco08,geo\\time` := NULL]
  
  # Split weekly_hours
  lfsa_ewhuis[, c("weekly_hours", "flag") := tstrsplit(weekly_hours, " ", fixed = TRUE)]
  
  # Change : to NA to indicate missing data
  lfsa_ewhuis[weekly_hours == ":", weekly_hours := NA]
  
  # Rename and reorder columns; remove unit as always hours
  lfsa_ewhuis <- lfsa_ewhuis[, .(region = `geo\\time`,
                                 year, sex, worktime, 
                                 employment_status = wstatus, 
                                 isco08_category = isco08,
                                 weekly_hours, flag)]
  
  # Update flag fields to be more intelligible
  lfsa_ewhuis[flag == "b", flag := "break in time series"]
  lfsa_ewhuis[flag == "c", flag := "confidential"]
  lfsa_ewhuis[flag == "d", flag := "definition differs, see metadata"]
  lfsa_ewhuis[flag == "e", flag := "estimated"]
  lfsa_ewhuis[flag == "f", flag := "forecast"]
  lfsa_ewhuis[flag == "n", flag := "not significant"]
  lfsa_ewhuis[flag == "p", flag := "provisional"]
  lfsa_ewhuis[flag == "r", flag := "revised"]
  lfsa_ewhuis[flag == "s", flag := "Eurostat estimate"]
  
  # Update employment status fields to be more intelligible
  lfsa_ewhuis[employment_status == "EMP", employment_status := "Employed persons"]
  lfsa_ewhuis[employment_status == "SAL", employment_status := "Employees"]
  lfsa_ewhuis[employment_status == "NSAL", employment_status := "Employed persons except employees"]
  lfsa_ewhuis[employment_status == "SELF", employment_status := "Self-employed persons"]
  lfsa_ewhuis[employment_status == "SELF_S", employment_status := "Self-employed persons with employees (employers)"]
  lfsa_ewhuis[employment_status == "SELF_NS", employment_status := "Self-employed persons without employees (own-account workers)"]
  lfsa_ewhuis[employment_status == "CFAM", employment_status := "Contributing family workers"]
  lfsa_ewhuis[employment_status == "NCFAM", employment_status := "Employed persons except contributing family workers"]
  lfsa_ewhuis[employment_status == "NRP", employment_status := "No response"]
  
  # Reformat
  lfsa_ewhuis <- lfsa_ewhuis[, .(region, year = as.numeric(year), 
                                 sex, worktime, employment_status, isco08_category, 
                                 weekly_hours = as.numeric(weekly_hours), flag)]
  
  # Return
  return(lfsa_ewhuis)
  
}