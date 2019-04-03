# Load package
library(eurostat)
library(data.table)

# Read table
lfsa_ewhuis <- as.data.table(label_eurostat(get_eurostat(id = "lfsa_ewhuis")))

# Plot
ggplotly(ggplot(data = lfsa_ewhuis[geo == "Romania" & 
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

lfsa_ewhuis$geo <- as.character(lfsa_ewhuis$geo)
lfsa_ewhuis[geo == "Germany (until 1990 former territory of the FRG)", geo := "Germany"]
lfsa_ewhuis[geo == "Euro area (19 countries)", geo := "Euro area"]
lfsa_ewhuis[geo == "European Union - 15 countries (1995-2004)", geo := "EU15 (2004)"]
lfsa_ewhuis[geo == "European Union - 28 countries ", geo := "EU28"]

ggplotly(ggplot(data = lfsa_ewhuis[time == max(time) & 
                                     wstatus %in% c("Employed persons except contributing family workers", 
                                                    "Self-employed persons with employees (employers)",
                                                    "Self-employed persons without employees (own-account workers)",
                                                    "Contributing family workers") &
                                     worktime == "Full-time" &
                                     sex != "Total" &
                                     isco08 == "Total"],
                aes(x = geo, y = values, fill = sex)) + 
           geom_bar(stat = "identity", position = "fill") +
           coord_flip() + 
           facet_wrap(~wstatus))