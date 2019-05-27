# Cleaning the data so it can be used across files

library(data.table)
library(fuzzyjoin)

source("data_scrape.R")

# Lookup for column names
common_cols <- c("Polling.Firm",
                 "Commissioners",
                 "Fieldwork.Start",
                 "Fieldwork.End",
                 "Scope", 
                 "Sample.Size",
                 "Sample.Size.Qualification",
                 "Participation",
                 "Precision")

# Make polling results numeric
make_num <- function(data) {
  data <- as.data.frame(data)
  parties <- names(data)[!(names(data) %in% common_cols)]
  data[, parties] <- data.frame(sapply(data[, parties], function(x) as.double(gsub("%", "", x))))
  return(data)
}
data_num <- lapply(data_load, make_num)

# Turn factors to characters
remove_factors <- function(data) {
  for(i in which(sapply(data, class) == "factor")) data[[i]] = as.character(data[[i]])
  return(data)
}
data_char <- lapply(data_num, remove_factors)


# Pull all parties' poll results into a single column
gather_data <- function(data) {
  data.table::melt(setDT(data), 
                   id.vars = names(data)[(names(data) %in% common_cols)],
                   measure.vars = names(data)[!(names(data) %in% common_cols)],
                   variable.factor = FALSE)
}
data_gather <- lapply(data_char, gather_data)


# Put all country polls together in one table
data_collapse <- data.table::rbindlist(data_gather, idcol = TRUE) %>%
  .[, `:=`(Fieldwork.Start = as.Date(Fieldwork.Start, origin = "1970-01-01"),
           Fieldwork.End = as.Date(Fieldwork.End, origin = "1970-01-01"),
           party = gsub("^ ", "", gsub("\\.", " ", variable)))]

### Add EU group information -----
EU_combs <- unique(EU_groups[, c("EU Political Group",
                                 "National party")])
EU_combs <- setDT(EU_combs[grepl("[A-Za-z0-9]+", EU_combs$`National party`), ])
colnames(EU_combs) <- c("EU_group", "party")
EU_combs <- EU_combs[, .SD[order(EU_group, decreasing = TRUE) == 1], by = party]

# Fuzzy join onto EU groups
data_groups <- fuzzyjoin::stringdist_left_join(data_collapse, EU_combs, 
                                                by = "party", max_dist = 50,
                                               distance_col = "distance")

# Keep lowest distance group for each poll
reduce_cols <- colnames(data_collapse)[!(colnames(data_collapse) %in% 
                                           c("party", "party.y"))]
data_reduce <- copy(data_groups)[, .SD[which.min(distance)], 
                                 by = reduce_cols]
# Set independents to "non-attached",
data_reduce[(party.y == "Independent") | (party.x == "Independent"), 
            EU_group := "Non-attached Members"]
data_reduce[distance > 13, EU_group := NA]
data_reduce[is.na(EU_group)]
data_reduce[, Sample.Size := as.numeric(Sample.Size)]

# Add full country names
data_countries <- merge(data_reduce, cnt_lookup, by.x = ".id", by.y = "acro")

# Add weeks
data_countries[, week := as.Date(paste0("01-", month(Fieldwork.End), 
                                        "-", year(Fieldwork.End)), format = "%d-%m-%Y"), 
               by = .(cnt, party.x)]

# Summarise all the polls over time
data_summ <- copy(data_countries)[, .(poll = (Sample.Size * value) / sum(Sample.Size)), 
                                  by = .(cnt, EU_group, week)][order(cnt, week, poll)]
data_ts <- copy(data_summ)[, lapply(.SD, ts), by = .(cnt, EU_group, week), .SDcols = "poll"]


# Colour palette
palette_cols <- c("Group of the European People's Party (Christian Democrats)" = "grey", 
  "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = "red", 
  "Europe of Nations and Freedom Group" = "brown", 
  "European Conservatives and Reformists Group" = "blue",
  "Group of the Greens/European Free Alliance" = "green",
  "Europe of Freedom and Direct Democracy Group" = "turquoise",
  "Group of the Alliance of Liberals and Democrats for Europe" = "yellow",
  "Confederal Group of the European United Left - Nordic Green Left" = "pink",
  "NA" = "black")

library(ggplot2)
ggplot(data_countries, aes(x = Fieldwork.End, y = value, 
                           group = EU_group, colour = EU_group))+
  geom_smooth(se = FALSE)+
  scale_colour_manual(values = palette_cols,
                      guide = guide_legend(nrow = 4))+
  theme(legend.position="bottom")+
  facet_wrap(~cnt)




