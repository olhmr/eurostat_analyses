boxplots_by_employment_status <- function(region_slice = "EU28", year_slice = 2017, worktime_slice = "FT") {
  
  # Dependencies
  require(data.table)
  require(ggplot2)
  require(plotly)
  
  # Plot
  p <- ggplotly(ggplot(data = lfsa_ewhuis[region == region_slice & 
                                            year == year_slice & 
                                            worktime == worktime_slice & 
                                            employment_status %in% c("Employed persons except contributing family workers", 
                                                                     "Self-employed persons with employees (employers)",
                                                                     "Self-employed persons without employees (own-account workers)",
                                                                     "Contributing family workers") &
                                            !is.na(weekly_hours), ], 
                       aes(x = sex, y = weekly_hours)) + 
                  geom_boxplot() + 
                  facet_wrap(~employment_status))
  
  # Return
  return(p)
  
}

boxplots_by_isco08_category <- function(region_slice = "EU28", year_slice = 2017, worktime_slice = "FT") {
  
  # Dependencies
  require(data.table)
  require(ggplot2)
  require(plotly)
  
  # Plot
  p <- ggplotly(ggplot(data = lfsa_ewhuis[region == region_slice & 
                                            year == year_slice & 
                                            worktime == worktime_slice &
                                            employment_status %in% c("Employed persons except contributing family workers", 
                                                                     "Self-employed persons with employees (employers)",
                                                                     "Self-employed persons without employees (own-account workers)",
                                                                     "Contributing family workers") & 
                                            !is.na(weekly_hours), ], 
                       aes(x = sex, y = weekly_hours)) + 
                  geom_boxplot() + 
                  facet_wrap(~isco08_category))
  
  # Return
  return(p)
  
}

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
  
  # Update isco08 to be more intelligible
  lfsa_ewhuis[isco08_category == "NRP", isco08_category := "No response"]
  lfsa_ewhuis[isco08_category == "OC0", isco08_category := "Armed forces"]
  lfsa_ewhuis[isco08_category == "OC1", isco08_category := "Legislators, senior officials and managers"]
  lfsa_ewhuis[isco08_category == "OC2", isco08_category := "Professionals"]
  lfsa_ewhuis[isco08_category == "OC3", isco08_category := "Technicians and associate professionals"]
  lfsa_ewhuis[isco08_category == "OC4", isco08_category := "Clerks"]
  lfsa_ewhuis[isco08_category == "OC5", isco08_category := "Service workers and shop and market sales workers"]
  lfsa_ewhuis[isco08_category == "OC6", isco08_category := "Skilled agricultural and fishery workers"]
  lfsa_ewhuis[isco08_category == "OC7", isco08_category := "Craft and related trades workers"]
  lfsa_ewhuis[isco08_category == "OC8", isco08_category := "Plant and machine operators and assemblers"]
  lfsa_ewhuis[isco08_category == "OC9", isco08_category := "Elementary occupations"]
  lfsa_ewhuis[isco08_category == "TOTAL", isco08_category := "Total"]
  
  # Reformat
  lfsa_ewhuis <- lfsa_ewhuis[, .(region, year = as.numeric(as.character(year)), 
                                 sex, worktime, employment_status, isco08_category, 
                                 weekly_hours = as.numeric(weekly_hours), flag)]
  
  # Return
  return(lfsa_ewhuis)
  
}