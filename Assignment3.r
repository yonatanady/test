#install.packages("knitr")
#install.packages("plotly")
library(tidyverse)
library(gridExtra)
library(knitr)
library(plotly)

#not sure if we need this part, but at the moment i put it here first
#downloading data
download.file("https://ndownloader.figshare.com/files/2292169", "data/portal_data_joined.csv")

#read_csv = tibble, diff from read.csv, it treat stringasfactor = FALSE by default, load faster
surveys <- read_csv("data/portal_data_joined.csv")
str(surveys)

summary(surveys)

#weight according to species
#compare with weight according to genus
surveys_weight <- surveys %>%
    filter(!is.na(year), !is.na(weight), !species_id == "") %>%
    group_by(year, species_id) %>% 
    mutate(average_weight = mean(weight)) 

#weight according to genus
surveys_weight <- surveys %>%
    filter(!is.na(year), !is.na(weight), !genus == "") %>%
    group_by(year, genus) %>% 
    mutate(average_weight = mean(weight)) 



#just to check
#to see distribution of species according to year
#to find out only include only those have 1977-2002 data by Swan
species_year <- surveys %>%
    filter(!is.na(year), !is.na(weight), !genus == "") %>%
    group_by(year, genus, weight) %>% select(year, genus, weight) %>% tally()


#1st graph - time series plot
#plot using species
ggplot(surveys_weight, aes(x = year, y = average_weight)) + 
    geom_line(aes(color = species_id)) 

#plot using genus
#i find this plot is cleaner than the one using species_id
ggplot(surveys_weight, aes(x = year, y = average_weight)) + 
    geom_line(aes(color = genus))


#2nd graph
#diff between female and male weight
surveys_weight_sex <- surveys %>%
    filter(!is.na(year), !is.na(weight), !is.na(sex), !species_id == "") %>%
    group_by(species_id, sex) %>% 
    mutate(average_weight = mean(weight))

ggplot(surveys_weight_sex, aes(x = species_id, y = weight)) + 
    geom_boxplot(aes(color = sex))  

#t-test
#by Nicole


#to find out how to beautify the plots like add plot title, axis label etc
#by Rio




#distribution of the species
surveys_species <- surveys %>%
    filter(!is.na(taxa), !is.na(genus), !is.na(species), !species_id == "") %>%
    mutate(species_name = paste(genus, species)) %>% group_by(taxa, species_name) %>% 
    tally()
#plot pie chart
sp_plot <- ggplot(surveys_species, aes(x = species_name, y = n)) + 
    geom_bar(stat = "identity")
pie <- sp_plot + coord_polar("y", start=0)
pie + facet_grid( ~ taxa, scales="free_y")

ggplotly()

    



# ggplot(surveys_species) + geom_rect(aes(fill=species_name, ymax=n, ymin=0, xmax=n, xmin=0)) +
#     geom_rect(aes(fill=taxa, ymax=n, ymin=0, xmax=n, xmin=0)) +
#     xlim(c(0, 4)) + 
#     theme(aspect.ratio=1)     