

library(tidyverse)
library(plyr)

# Create a new column for the group
data <- read_csv("data/merged_data_alltB.csv") %>%
  mutate(Group = case_when(
    Class == "Polychaeta" ~ "Polychaeta",
    Phylum == "Mollusca" ~ "Mollusca",
    TRUE ~ "Everything else")) %>% 
  ddply(.(Artal, Group) ,summarise, Nfm = sum(Nfm))

#Reverse Group
data$Group <- factor(data$Group, levels = c("Everything else", "Mollusca", "Polychaeta")) 
# Order the levels of the year factor
data$Artal <- factor(data$Artal, levels = c(2013:2017, 1999))

# Create a bar plot
p <- ggplot(data, aes(x = Artal, y = Nfm, fill = Group)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_x_discrete(labels = function(x) ifelse(x == "1999", "1999", x)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = NULL, y = "ind./m²", fill = "", title = " Benthic Macrofauna in Kolgrafafjörður") +
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







for (i in c(2013:2017,1999)) {
  print(length(unique(KolgrTaxa$Flokkun[KolgrTaxa$Artal==i])))
}
for (i in c(2013:2017,1999)) {
  print(sum(data$N[data$Artal==i]))
}
