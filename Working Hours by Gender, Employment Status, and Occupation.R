# Load package
library(eurostat)
library(data.table)

# Read table
lfsa_ewhuis <- as.data.table(label_eurostat(get_eurostat(id = "lfsa_ewhuis")))

# Plot
ggplotly(ggplot(data = lfsa_ewhuis[geo == "Sweden" & 
                                     time == max(lfsa_ewhuis$time) & 
                                     worktime == "Full-time" & 
                                     wstatus %in% c("Employed persons except contributing family workers", 
                                                    "Self-employed persons with employees (employers)",
                                                    "Self-employed persons without employees (own-account workers)",
                                                    "Contributing family workers") & 
                                     !isco08 %in% c("No response", "Total") &
                                     !is.na(values), ], 
                aes(x = sex, y = values)) + 
           geom_boxplot() + 
           facet_wrap(~wstatus))