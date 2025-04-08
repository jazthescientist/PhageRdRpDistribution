#DATA QUANT FOR IMG/M AND OTHER DATA 

# libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(conflicted)
library(dplyr)
conflicts_prefer(dplyr::filter)
setwd("/work/pi_jlb_umass_edu/blanchardlab/RNA_phages")

# Load RdRP data
RdRP_data <- read_tsv("data/RiboV1.4_Info.tsv")

# Explore unique RdRP counts by Set and Phylum instead of metadata
summary_counts <- RdRP_data |>
  filter(!is.na(Phylum), !is.na(Set)) |>
  group_by(Set, Phylum) |>
  summarize(UniqueRdRP = n_distinct(RID), .groups = 'drop')

# Plot
ggplot(summary_counts, aes(x = Phylum, y = UniqueRdRP, fill = Set)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Unique RdRP Counts by Set and Phylum",
       x = "Phylum", y = "Unique RdRP Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#RDRP DATA 


# Load RdRP data
RdRP_data <- read_tsv("data/RiboV1.4_Info.tsv")

# Explore unique RdRP counts by Set and Phylum instead of metadata
summary_counts <- RdRP_data |>
  filter(!is.na(Phylum), !is.na(Set)) |>
  group_by(Set, Phylum) |>
  summarize(UniqueRdRP = n_distinct(RID), .groups = 'drop')

# Plot
ggplot(summary_counts, aes(x = Phylum, y = UniqueRdRP, fill = Set)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Unique RdRP Counts by Set and Phylum",
       x = "Phylum", y = "Unique RdRP Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#CONTIG CONTAINING 

print(RdRP_img[1:5, c("Full_name", "RID", "Note", "RCR90")])


#ANALYZE PLOT TOP 20 CONTIG 

# RdRP data
RdRP_data <- read_tsv("data/RiboV1.4_Info.tsv")

# Filter IMG/M dataset only
RdRP_img <- RdRP_data |> filter(Set == "IMG/M - Chen et al 2019")

# Count unique RdRP by Full_name (contig) within IMG/M
summary_counts_contig <- RdRP_img |>
  filter(!is.na(Full_name)) |>
  group_by(Full_name) |>
  summarize(UniqueRdRP = n_distinct(RID), .groups = 'drop') |>
  arrange(desc(UniqueRdRP))

# Plot top 20 contigs with MOST RdRPs
ggplot(head(summary_counts_contig, 20), aes(x = reorder(Full_name, -UniqueRdRP), y = UniqueRdRP)) +
  geom_col(fill = "#F2B705") +
  labs(title = "Top 20 Contigs in IMG/M by Unique RdRP Count",
       x = "Contig (Full_name)", y = "Unique RdRP Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


