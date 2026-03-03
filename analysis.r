# ---------------------------------------------------------

# Melbourne Bioinformatics Training Program

# This exercise to assess your familiarity with R and git. Please follow
# the instructions on the README page and link to your repo in your application.
# If you do not link to your repo, your application will be automatically denied.

# Leave all code you used in this R script with comments as appropriate.
# Let us know if you have any questions!


# You can use the resources available on our training website for help:
# Intro to R: https://mbite.org/intro-to-r
# Version Control with Git: https://mbite.org/intro-to-git/

# ----------------------------------------------------------

# Load libraries -------------------
# You may use base R or tidyverse for this exercise

library(tidyverse)
library(patchwork) 
library(ggplot2)

# Load data here ----------------------
# Load each file with a meaningful variable name.

expr_data <- read.csv("data/GSE60450_GeneLevel_Normalized(CPM.and.TMM)_data.csv")

meta_data <- read.csv("data/GSE60450_filtered_metadata.csv")


# Inspect the data -------------------------

# What are the dimensions of each data set? (How many rows/columns in each?)
# Keep the code here for each file.

## Expression data
nrow(expr_data)  
ncol(expr_data)

## Metadata
nrow(meta_data)  
ncol(meta_data)


# Prepare/combine the data for plotting ------------------------
# How can you combine this data into one data.frame?
expr_long <- expr_data %>%
  pivot_longer(
    cols = starts_with("GSM"),     # Select only sample columns (starting with GSM)
    names_to = "sample_id",        # Rename sample columns 
    values_to = "expression"       # Rename expression values column
  )

combined_data <- inner_join(expr_long, meta_data, by = c("sample_id" = "X"))

# Plot the data --------------------------
## Plot the expression by cell type
## Can use boxplot() or geom_boxplot() in ggplot2
plot1 <- combined_data %>%
  filter(gene_symbol == "Gnai3") %>%        # Filter target gene: Gnai3
  ggplot(aes(x = developmental.stage, y = expression, fill = immunophenotype)) +
  geom_boxplot(width = 0.7, alpha = 0.8) +
  labs(
    title = "Expression of Gnai3 by Cell Type",
    x = "Cell Type (Immunophenotype)",
    y = "Normalized Expression (CPM)"
  ) +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)        #Rotate X-axis labels
  )
plot1
## Save the plot
### Show code for saving the plot with ggsave() or a similar function
ggsave(
  "results/Gnai3_expression_by_celltype.png",
  plot1,
  width = 10, height = 6, dpi = 300
)
