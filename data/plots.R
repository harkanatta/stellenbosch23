

library(tidyverse)
library(plyr)

# Create a new column for the group
data <- read_csv("data/merged_data_alltB.csv") %>%
  filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3")) %>% 
  mutate(Group = case_when(
    Class == "Polychaeta" ~ "Polychaeta",
    Phylum == "Mollusca" ~ "Mollusca",
    TRUE ~ "Everything else")) %>% 
  ddply(.(Artal, Group) ,summarise, N = sum(N))

#Reverse Group
data$Group <- factor(data$Group, levels = c("Everything else", "Mollusca", "Polychaeta")) 
# Order the levels of the year factor
data$Artal <- factor(data$Artal, levels = c(2013:2017, 1999))

# Create a bar plot
p <- ggplot(data, aes(x = Artal, y = N, fill = Group)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_x_discrete(labels = function(x) ifelse(x == "1999", "1999", x)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = NULL, y = "Individuals", fill = "", title = " Benthic Macrofauna in Kolgrafafjörður") +
  theme_minimal() +
  theme(  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(size = ifelse(levels(data$Artal) == "1999", 14, 10), color = ifelse(levels(data$Artal) == "1999", "red", "black")),
          legend.text = element_text(size = 10),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.direction = "horizontal",
          legend.title = element_blank())

# Print the plot
print(p)
ggsave("images/DominantGroups.png",width = 1280, height = 720, units = "px", dpi = 200, bg = "white")


library(tidyverse)
library(plyr)

explore_column <- function(column_name, years) {
  
  data <- read_csv("data/merged_data_alltB.csv")
  # Check if column_name exists in the dataset
  if (!column_name %in% names(data)) {
    stop("Column does not exist in the dataset")
  }
  
  column_name <- as.character(column_name)
  
  # Read the data
  data <- data %>%
    mutate(Group = case_when(
      Class == "Polychaeta" ~ "Polychaeta",
      Phylum == "Mollusca" ~ "Mollusca",
      TRUE ~ "Everything else")) %>%
    filter(Artal %in% years)  # Filter data for specific years
  
  # Reverse the order of the Group factor
  data$Group <- factor(data$Group, levels = c("Everything else", "Mollusca", "Polychaeta"))
  
  # Order the levels of the year factor
  data$Artal <- factor(data$Artal, levels = c(2013:2017, 1999))
  
  data <- data <- data[data$Group=="Polychaeta",] %>%
    ddply(c("Artal", column_name), summarise, Nfm = sum(Nfm)) %>%
    arrange(Artal, desc(Nfm)) %>%
    group_by(Artal) %>%
    slice_head(n = 3) %>%
    ungroup()
  
  # Replace specific values in the specified column
  value_to_replace <- c("Polydora", "Spionidae")
  replacement_values <- c("Polydora spp.", "Spionidae spp.")
  for(i in 1:length(value_to_replace)){
    data[[column_name]] <- gsub(pattern = value_to_replace[i],
                                          replacement = replacement_values[i],
                                          x = data[[column_name]])
  }

  # Create a bar plot
  p <- ggplot(data, aes_string(x = "Artal", y = "Nfm", fill = column_name)) +
    geom_bar(stat = "identity", position = "stack", color = "black") +
    scale_x_discrete(labels = function(x) ifelse(x == "1999", "1999", x)) +
    scale_fill_brewer(palette = "Paired", direction = -1) +
    labs(x = NULL, y = "ind./m²", fill = "", title = "Top 3 Polychaetes each year") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(size = ifelse(levels(data$Artal) == "1999", 14, 10), color = ifelse(levels(data$Artal) == "1999", "red", "black")),
          legend.text = element_text(size = 10),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.direction = "horizontal",
          legend.title = element_blank())
  

  
  print(p)
  return(str(data))
  return(head(data))
}

# Usage
explore_column("Flokkun", c(2013:2017,1999))

ggsave("images/DominantPolychaetes.png",width = 1280, height = 720, units = "px", dpi = 100, bg = "white")






years <- c(2013:2017)
A <- numeric(length(years))
names(A) <- years

for (i in years) {
  A[as.character(i)] <- length(unique(KolgrTaxa$Flokkun[KolgrTaxa$Artal==i]))
}

str(A)
years <- c(2013:2017)
B <- numeric(length(years))
names(B) <- years

for (i in years) {
  B[as.character(i)] <- sum(KolgrTaxa$N[KolgrTaxa$Artal==i])
}

str(B)

data <- data.frame(Year = as.numeric(names(A)),
                   Species = A,
                   Individuals = B)

organic <- structure(list(stod = c("A7", "A7", "A7", "A7", "A7", "B5", "B5", 
                        "B5", "B5", "B5", "B8", "B8", "B8", "B8", "B8", "C4", "C4", "C4", 
                        "C4", "C4", "E3", "E3", "E3", "E3", "E3", "E4", "E4", "E4", "E4", 
                        "E4"), Artal = c("2013", "2014", "2015", "2016", "2017", "2013", 
                                         "2014", "2015", "2016", "2017", "2013", "2014", "2015", "2016", 
                                         "2017", "2013", "2014", "2015", "2016", "2017", "2013", "2014", 
                                         "2015", "2016", "2017", "2013", "2014", "2015", "2016", "2017"
                        ), `%Organic` = c(0.181515414138913, 0.117573294968505, 0.118865354458575, 
                                          0.10855175801997, 0.144215480661367, 0.174146595044186, 0.119127447894571, 
                                          0.124407020872865, 0.105198658268468, 0.115848718019756, 0.178503108560497, 
                                          0.12940849965294, 0.117179453375411, 0.111245191819955, 0.116370341491164, 
                                          0.142787808192584, 0.113006923837784, 0.132290387475618, 0.0969106601341345, 
                                          0.10220582768636, 0.215412621359223, 0.101006035443262, 0.090402518790167, 
                                          0.0885715142546809, 0.0796504565039956, 0.135196812364163, 0.126232974506719, 
                                          0.103923910552975, 0.092065868263473, 0.101525813669742)), class = "data.frame", row.names = c(NA, 
                                                                                                                                         -30L))
# Make sure the "Year" column in 'data' and the "Artal" column in 'organic' are both numeric
data$Year <- as.numeric(data$Year)
organic$Artal <- as.numeric(organic$Artal)

# Merge the two data frames
data <- merge(data, organic, by.x = "Year", by.y = "Artal", all.x = TRUE)
data <- ddply(data, .(Year, Species,Individuals), summarise,
              avg_Organic = mean(`%Organic`),
              sd_Organic = sd(`%Organic`))

#######3333

data <- data.frame(
  Year = c(2013, 2014, 2015, 2016, 2017),
  Species = c(22, 52, 52, 72, 75),
  Individuals = c(5871, 1909, 1013, 1813, 1734),
  avg_Organic = c(0.1712604, 0.1177259, 0.1145114, 0.1004239, 0.1099694),
  sd_Organic = c(0.029074531, 0.010125845, 0.015056057, 0.009258935, 0.021443591)
)


#library(ggplot2)
#library(scales)

max_species <- max(data$Species)
max_individuals <- max(data$Individuals)
max_secondary <- max(data$avg_Organic)

data$norm_individuals <- data$Individuals / max_individuals * max_species
data$norm_organic <- data$avg_Organic / max_secondary * max_species

max_ind <- max(data$Individuals)
mid_ind <- max_ind / 2

p <- ggplot(data, aes(x = Year, group = 1)) +
  geom_line(aes(y = Species, color = "Species"), size = 2, linetype = "dotted") +
  geom_line(aes(y = norm_individuals, color = "Individuals"), size = 1) +
  geom_line(aes(y = norm_organic, color = "%Organic"), size = 1) +
  geom_point(aes(y = norm_organic, color = "%Organic"), size = 2.5) +
  geom_errorbar(aes(y = norm_organic,
                    ymin = (avg_Organic - sd_Organic) / max_secondary * max_species,
                    ymax = (avg_Organic + sd_Organic) / max_secondary * max_species,
                    color = "%Organic"), 
                width = 0.2, size = 0.5) +
  geom_text(aes(y = norm_organic, label = round(avg_Organic, 2)), vjust = -1.5, color="red") +
  scale_y_continuous(name = "Number of Species",
                     sec.axis = sec_axis(~ . / max_species * max_individuals, 
                                         name = "Number of Individuals")) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "", color = "") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

print(p)


ggsave("images/sp-ind-organic.png",width = 1280, height = 720, units = "px", dpi = 200, bg = "white")
