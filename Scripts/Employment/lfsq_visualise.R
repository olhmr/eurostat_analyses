# Visualisations for lfsq_ewhun2 data
boxplots_by_employment_status <- function(region_slice = "EU28", year_slice = 2018, worktime_slice = "FT") {
  
  # Dependencies
  require(data.table)
  require(ggplot2)
  require(plotly)
  
  # Plot
  p <- ggplotly(ggplot(data = lfsq_ewhun2[region == region_slice & 
                                            year == year_slice & 
                                            worktime == worktime_slice & 
                                            !is.na(weekly_hours), ], 
                       aes(x = sex, y = weekly_hours)) + 
                  geom_boxplot() + 
                  facet_wrap(~employment_status))
  
  # Return
  return(p)
  
}