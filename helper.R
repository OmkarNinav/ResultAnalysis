library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(plotly)
library(tidyverse)
AS_marks <- read_csv("data/AS_marks.csv")
ERC_marks <- read_csv("data/ERC_marks.csv")
LA_marks <- read_csv("data/LA_marks.csv")
Overall_marks <- read_csv("data/Overall_marks.csv")
PD_marks <- read_csv("data/PD_marks.csv")
PE_marks <- read_csv("data/PE_marks.csv")
Prac_marks <- read_csv("data/Prac_marks.csv")
RM_marks <- read_csv("data/RM_marks.csv")
Skill_dev_marks <- read_csv("data/Skill_dev_marks.csv")

ES_marks <- data.frame(AS_marks$`Roll No.`, AS_marks$`Name of Student`, AS_marks$ES, ERC_marks$ES, LA_marks$ES, PD_marks$ES)
ES_marks <- data.frame(AS_marks$`Roll No.`, AS_marks$`Name of Student`, AS_marks$ES, ERC_marks$ES, LA_marks$ES, PD_marks$ES,PE_marks$ES, Prac_marks$ES, RM_marks$ES, Skill_dev_marks$ES)
colnames(ES_marks) <- c("Roll No.", "Name of Student", "AS", "ERC", "LA", "PD", "PE", "Prac", "RM", "Skill_dev")
# Load required libraries


# Convert data from wide to long format
df_long <- OB_marks %>%
  pivot_longer(
    cols = -c(`Roll No.`, `Name of Student`),
    names_to = "Subject", values_to = "Marks"
  )

# Multiple line plot
# Load required libraries
library(tidyverse)
library(plotly)

# Define the max marks for each subject manually
max_marks <- c(
  "AS" = 100,
  "ERC" = 100,
  "LA" = 100,
  "PD" = 100,
  "PE" = 50,
  "Prac" = 50,
  "RM" = 100,
  "Skill_dev" = 50
)

# Convert marks to percentage based on subject-specific max marks
OB_percentage <- OB_marks %>%
  mutate(across(names(max_marks), ~ (.x / max_marks[cur_column()]) * 100))

# Reshape data to long format for line plot
OB_long <- OB_percentage %>%
  pivot_longer(-c(1, 2), names_to = "Subject", values_to = "Percentage")

# Create an interactive line plot (Fixing the color mapping issue)
plot_ly(
  OB_long,
  x = ~Subject,
  y = ~Percentage,
  color = ~`Name of Student`, # Properly assign student names to color
  type = "scatter",
  mode = "lines+markers"
) %>%
  layout(
    title = "Student Performance Across Subjects (Percentage)",
    xaxis = list(title = "Subjects", tickangle = 45),
    yaxis = list(title = "Percentage", range = c(0, 100)),
    legend = list(title = list(text = "Students"))
  )


# pie Chart from Overall_marks result Status
Overall_marks %>%
  count(`Result Status`) %>%
  plot_ly(
    labels = ~`Result Status`,
    values = ~n,
    type = "pie",
    hole = 0.6, # Balanced donut hole
    textinfo = "label+percent",
    textposition = "inside",
    marker = list(line = list(color = "white", width = 3)) # Smooth sector edges
  ) %>%
  layout(title = "Overall Result Status")

# Load required libraries
library(tidyverse)
library(plotly)

# Remove first two character columns (Roll No & Name)
OB_numeric <- OB_marks %>%
  select(-c(1, 2)) # Adjust if needed

# Compute correlation matrix
cor_matrix <- cor(OB_numeric, use = "complete.obs")

# Convert correlation matrix to a long format for Plotly
cor_long <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Subject1") %>%
  pivot_longer(-Subject1, names_to = "Subject2", values_to = "Correlation")

# Create an interactive Plotly heatmap
plot_ly(
  data = cor_long,
  x = ~Subject1,
  y = ~Subject2,
  z = ~Correlation,
  type = "heatmap",
  colorscale = "RdBu", # Red-White-Blue gradient
  zmin = -1,
  zmax = 1,
  text = ~ round(Correlation, 2), # Display rounded correlation values
  texttemplate = "%{text}", # Ensure correct text formatting
  showscale = TRUE # Show color scale legend
) %>%
  layout(
    title = "Subject Correlation Heatmap",
    xaxis = list(title = "Subjects", tickangle = 45),
    yaxis = list(title = "Subjects")
  )

# subject wise Table for failed students

Failed_students <- ES_marks$`Name of Student`[ES_marks$AS < 40 | ES_marks$ERC < 40 | ES_marks$LA < 40 | ES_marks$PD < 40]
Failed_students 

colnames(AS_marks)
colnames(ERC_marks)
colnames(LA_marks)
colnames(PD_marks)
colnames(PE_marks)
colnames(Prac_marks)
colnames(RM_marks)
colnames(Skill_dev_marks)
colnames(Overall_marks)

library(dplyr)
library(gt)

# Combine all subjects into one table
below_40 <- ES_marks %>%
  select(`Roll No.` = `AS_marks..Roll.No..`, `Name` = `AS_marks..Name.of.Student.`,
         `AS` = `AS_marks.ES`, `ERC` = `ERC_marks.ES`, `LA` = `LA_marks.ES`, `PD` = `PD_marks.ES`) %>%
  filter(AS < 20 | ERC < 20 | LA < 20 | PD < 20)  # Modify to include all subjects if needed
below_40
# Create a gt table
below_40 %>%
  gt() %>%
  tab_header(
    title = "Students Scoring Less Than 40 in Any Subject",
    subtitle = "Highlighting students who need improvement"
  ) %>%
  tab_style(
    style = list(cell_fill(color = "lightpink")),
    locations = cells_body(
      columns = c(AS, ERC, LA, PD),
      rows = AS < 20 | ERC < 20 | LA < 20 | PD < 20
    )
  )

below_40 %>%
  gt() %>%
  tab_header(
    title = "Students Scoring Less Than 40% in Any Subject",
    subtitle = "Cells with low marks are highlighted"
  ) %>%
  data_color(
    columns = c(AS, ERC, LA, PD),  # Modify to include all subjects if needed
    colors = scales::col_bin(
      palette = c("red", "white"),  # White for normal, pink for low scores
      domain = c(0, 100),
      bins = c(0,19.99,100)
    ),
    apply_to = "fill",
    na_color = "white"
  )

library(ggplot2)
library(dplyr)
library(tidyr)

# Convert data to long format
long_data <- OB_marks %>%
  pivot_longer(cols = c(AS, ERC, LA, PD, PE, Prac, RM, Skill_dev), 
               names_to = "Subject", 
               values_to = "Marks") %>%
  group_by(Subject, Marks) %>%
  summarise(Count = n(), .groups = "drop")  # Count occurrences of each mark

# Bubble plot
ggplot(long_data, aes(x = Subject, y = Marks, size = Count, color = Subject)) +
  geom_point(alpha = 0.7) +  # Bubble transparency
  scale_size_continuous(range = c(2, 20)) +  # Adjust bubble sizes
  labs(title = "Subject-wise Score Distribution",
       x = "Subjects",
       y = "Marks",
       size = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

library(ggplot2)
library(dplyr)
library(tidyr)

# Convert data to long format
long_data <- CA_marks %>%
  pivot_longer(cols = c(AS, ERC, LA, PD, PE, Prac, RM, Skill_dev), 
               names_to = "Subject", 
               values_to = "Marks") %>%
  group_by(Subject, Marks) %>%
  summarise(Count = n(), .groups = "drop")  # Count occurrences of each mark

# Bubble plot with facets
ggplot(long_data, aes(x = Marks, y = Count, size = Count, color = Subject)) +
  geom_point(alpha = 0.7) +  # Bubble transparency
  scale_size_continuous(range = c(2, 20)) +  # Adjust bubble sizes
  labs(title = "Subject-wise Score Distribution",
       x = "Marks",
       y = "Frequency",
       size = "Frequency") +
  theme_minimal() +
  facet_wrap(~Subject, scales = "free") +  # Separate plots for each subject
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "none")  # Remove redundant legend

library(ggplot2)
library(dplyr)

# Select only subject scores for clustering
marks_data <- OB_marks %>%
  select(AS, ERC, LA, PD, PE, Prac, RM, Skill_dev)  # Keep only numeric columns

# Compute the Euclidean distance matrix
dist_matrix <- dist(t(marks_data), method = "euclidean")

# Apply Hierarchical Clustering using Ward's method
hc_result <- hclust(dist_matrix, method = "ward.D2")

dist_matrix
# Plot the Dendrogram
plot(hc_result, main = "Hierarchical Clustering Dendrogram", xlab = "Subjects", sub = "")
rect.hclust(hc_result, k = 2, border = "red")  # Cut tree into 3 clusters


library(ggplot2)
library(dplyr)

# Select only numeric subject scores
marks_data <- OB_marks %>%
  select(AS, ERC, LA, PD, PE, Prac, RM, Skill_dev) 

# Transpose data so that subjects are clustered
marks_data_t <- t(marks_data)  # Now subjects are rows

# Compute the Euclidean distance matrix for subjects
dist_matrix <- dist(marks_data_t, method = "euclidean")

# Apply Hierarchical Clustering using Ward's method
hc_result <- hclust(dist_matrix, method = "ward.D2")

# Assign cluster labels to subjects
subject_clusters <- cutree(hc_result, k = 3)  # Adjust 'k' as needed

# Convert to data frame for visualization
subject_cluster_df <- data.frame(Subject = rownames(marks_data_t), 
                                 Cluster = as.factor(subject_clusters))

# Plot the Dendrogram
plot(hc_result, main = "Hierarchical Clustering Dendrogram (Subjects)", 
     xlab = "Subjects", sub = "")
rect.hclust(hc_result, k = 3, border = "red")  # Mark clusters

# Perform PCA for subject clustering
pca_result <- prcomp(marks_data_t, scale = TRUE)

# Convert PCA output to data frame
pca_data <- as.data.frame(pca_result$x)
pca_data$Subject <- rownames(marks_data_t)  # Add subject names
pca_data$Cluster <- as.factor(subject_clusters)  # Add cluster labels

# Scatter plot of PCA clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster, label = Subject)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_text(vjust = 1.5, hjust = 0.5) +  # Add subject labels
  labs(title = "Subject Clusters Based on Student Scores (PCA)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()


library(ggplot2)
library(dplyr)
library(cluster)  # For silhouette
library(factoextra)

marks_data <- OB_marks %>%
  select(AS, ERC, LA, PD, PE, Prac, RM, Skill_dev) 
marks_data_t <- t(marks_data)  

# Compute Euclidean distance
dist_matrix <- dist(marks_data_t, method = "euclidean")

# Apply Hierarchical Clustering
hc_result <- hclust(dist_matrix, method = "ward.D2")

avg_dend_obj <- as.dendrogram(hc_result)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)


# **1️⃣ Elbow Method (Using WCSS)**
fviz_nbclust(as.data.frame(marks_data_t), FUN = hcut, method = "wss") +
  ggtitle("Optimal Number of Clusters (Elbow Method)")

# **2️⃣ Silhouette Method (Using Average Silhouette Width)**
fviz_nbclust(as.data.frame(marks_data_t), FUN = hcut, method = "silhouette") +
  ggtitle("Optimal Number of Clusters (Silhouette Method)")


library(cluster)
library(factoextra)

# Compute Euclidean distance for subjects
dist_matrix <- dist(marks_data_t, method = "euclidean")

# Apply Hierarchical Clustering
hc_result <- hclust(dist_matrix, method = "ward.D2")

# **1️⃣ Elbow Method (Fixed)**
fviz_nbclust(as.data.frame(marks_data_t), FUN = hcut, method = "wss") +
  ggtitle("Optimal Number of Clusters (Elbow Method)")

# **2️⃣ Silhouette Method (Fixed)**
# Ensure enough subjects exist for clustering
if (nrow(marks_data_t) >= 3) {  
  fviz_nbclust(as.data.frame(marks_data_t), FUN = hcut, method = "silhouette") +
    ggtitle("Optimal Number of Clusters (Silhouette Method)")
} else {
  print("Silhouette method requires at least 3 subjects to cluster.")
}


library(gt)
library(dplyr)

legend_data <- tibble::tibble(
  Range = c("0-19.99", "20-29.99", "30-39.99", "40-100"),
  Color = c("#FB4141", "#F0FF42", "#82CD47", "#379237")
)

legend_table <- legend_data %>%
  gt() %>%
  tab_header(title = "Color Legend for Theory Marks") %>%
  data_color(
    columns = Color,
    colors = scales::col_factor(
      palette = c("#FB4141", "#F0FF42", "#82CD47", "#379237"),
      domain = legend_data$Color
    ),
    apply_to = "fill"
  )

legend_table

legend_table <- tibble::tibble(
  Color=c("0-40%","40-60%","60-80%","80-100%"),
  "Meaning" = c("Marks below 20 (Critical Failure)",
                "Marks between 20-29 (Very Low)",
                "Marks between 30-39 (Near Passing)",
                "Marks 40+ (Passed, Not Highlighted)")
) %>%
  gt() %>%
  tab_header(title = "Legend for Score Highlighting") %>%
  data_color(
    columns = Color,
    colors = scales::col_factor(
      palette = c("#FB4141", "#F0FF42", "#82CD47", "#379237"),
      domain = Color
    ),
    apply_to = "fill"
  )

library(gt)
library(scales)
library(tibble)

# Create the legend table
tibble::tibble(
  Color = c("0-40%", "40-60%", "60-80%", "80-100%"),
  Meaning = c("Marks below 40 (Failure)",
              "Marks between 40-60 (Low)",
              "Marks between 60-80 (Good)",
              "Marks 80+ (Excellent)")
) %>%
  gt() %>%
  tab_header(title = "Legend for Score Highlighting") %>%
  data_color(
    columns = Color,
    colors = scales::col_factor(
      palette = c("#FB4141", "#F0FF42", "#82CD47", "#379237"),  # Red, Yellow, Green, Dark Green
      domain = c("0-40%", "40-60%", "60-80%", "80-100%")  # Explicitly define domain
    ),
    apply_to = "fill"
  )

# Display legend table
legend_table

# Load required libraries
library(ggplot2)
library(ggforce)

# Create dataframe
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Generate packed bubble chart
ggplot(df, aes(x = 0, y = 0, size = OB_percent, fill = Group)) +
  geom_point(shape = 21, color = "black", alpha = 0.7) +
  scale_size(range = c(5, 20)) +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_void() +
  theme(legend.position = "right")

# Load required libraries
library(ggplot2)
library(packcircles)
library(dplyr)

# Create dataframe
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Packing the circles
packing <- circleProgressiveLayout(df$OB_percent, sizetype = "area")
df <- bind_cols(df, packing)

# Generate packed bubble chart
ggplot() +
  geom_polygon(data = circleLayoutVertices(packing), 
               aes(x, y, group = id, fill = df$Group[id]), color = "black", alpha = 0.7) +
  geom_text(data = df, aes(x, y, label = round(OB_percent, 1)), size = 4) +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_void() +
  theme(legend.position = "right")

# Load required libraries
install.packages("plotly")
install.packages("packcircles")
install.packages("dplyr")

library(plotly)
library(packcircles)
library(dplyr)

# Create dataframe
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Packing the circles (arranging them optimally)
packing <- circleProgressiveLayout(df$OB_percent, sizetype = "area")
df <- bind_cols(df, packing)

# Create plotly bubble chart
fig <- plot_ly(df, x = ~x, y = ~y, text = ~paste("OB:", round(OB_percent, 1), "%"), 
               type = 'scatter', mode = 'markers', 
               marker = list(size = ~OB_percent, 
                             color = ~as.numeric(Group), 
                             colorscale = "Viridis", 
                             showscale = TRUE, 
                             line = list(width = 1, color = "black")))

# Finalize layout
fig <- fig %>% layout(title = "Packed Bubble Chart in Plotly",
                      xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
                      yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))

# Show plot
fig

# Load required libraries
install.packages("plotly")
install.packages("dplyr")

library(plotly)
library(dplyr)

# Create dataframe
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Count frequency of each group
group_counts <- df %>%
  group_by(Group) %>%
  summarise(Frequency = n())

# Create packed bubble chart in Plotly
fig <- plot_ly(group_counts, x = ~Group, y = ~Frequency, text = ~paste("Count:", Frequency), 
               type = 'scatter', mode = 'markers',
               marker = list(size = ~Frequency * 10, # Scale size for better visibility
                             color = ~as.numeric(Group), 
                             colorscale = "Viridis", 
                             showscale = TRUE, 
                             line = list(width = 1, color = "black")))

# Finalize layout
fig <- fig %>% layout(title = "Packed Bubble Chart by Group",
                      xaxis = list(title = "Group"),
                      yaxis = list(title = "Frequency"),
                      showlegend = FALSE)

# Show plot
fig


# Load required libraries
install.packages("ggplot2")
install.packages("packcircles")
install.packages("dplyr")

library(ggplot2)
library(packcircles)
library(dplyr)

# Create dataframe
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Count frequency of each group
group_counts <- df %>%
  group_by(Group) %>%
  summarise(Frequency = n())

# Pack circles based on frequency
packing <- circleProgressiveLayout(group_counts$Frequency, sizetype = "area")
group_counts <- bind_cols(group_counts, packing)

# Get circle coordinates for plotting
circle_data <- circleLayoutVertices(packing, idcol = "x")

# Create packed bubble chart using ggplot2
ggplot() +
  geom_polygon(data = circle_data, aes(x, y, group = id, fill = group_counts$Group[id]), 
               color = "black", alpha = 0.7) +
  geom_text(data = group_counts, aes(x, y, label = paste(Group, "\n", Frequency)), size = 5) +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_void() +
  theme(legend.position = "right") +
  ggtitle("Packed Bubble Chart by Group (Frequency as Size)")

# Load required libraries
install.packages("ggplot2")
install.packages("packcircles")
install.packages("dplyr")

library(ggplot2)
library(packcircles)
library(dplyr)

# Create dataframe
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Count frequency of each group
group_counts <- df %>%
  group_by(Group) %>%
  summarise(Frequency = n()) %>%
  mutate(id = row_number())  # Create unique ID for each group

# Pack circles based on frequency
packing <- circleProgressiveLayout(group_counts$Frequency, sizetype = "area")
group_counts <- bind_cols(group_counts, packing)

# Get circle coordinates for plotting
circle_data <- circleLayoutVertices(packing, idcol = "id")

# Merge `circle_data` with `group_counts` to map Groups correctly
circle_data <- circle_data %>%
  left_join(group_counts, by = c("id" = "id"))

# Create packed bubble chart using ggplot2
ggplot() +
  geom_polygon(data = circle_data, aes(x, y, group = id, fill = Group), 
               color = "black", alpha = 0.7) +
  geom_text(data = group_counts, aes(x, y, label = paste(Group, "\n", Frequency)), size = 5) +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_void() +
  theme(legend.position = "right") +
  ggtitle("Packed Bubble Chart by Group (Frequency as Size)")
# Load required libraries
install.packages("ggplot2")
install.packages("packcircles")
install.packages("dplyr")

library(ggplot2)
library(packcircles)
library(dplyr)

# Create dataframe
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / 100) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Count frequency of each group
group_counts <- df %>%
  group_by(Group) %>%
  summarise(Frequency = n())  # Unique ID

# Pack circles based on frequency
packing <- circleProgressiveLayout(group_counts$Frequency, sizetype = "area")
group_counts <- bind_cols(group_counts, packing)

# Generate circle layout vertices for polygons
circle_data <- circleLayoutVertices(packing,npoint)

# Merge `circle_data` with `group_counts` to map Groups correctly
circle_data <- circle_data %>%
  left_join(group_counts, by = c("id" = "id"))

# Create packed bubble chart using ggplot2
ggplot() +
  geom_polygon(data = circle_data, aes(x, y, group = id, fill = Group), 
               color = "black", alpha = 0.7) +
  geom_text(data = group_counts, aes(x, y, label = paste(Group, "\n", Frequency)), size = 5) +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_void() +
  theme(legend.position = "right") +
  ggtitle("Packed Bubble Chart by Group (Frequency as Size)")

library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

# Create data
data <- data.frame(group=paste("Group_", sample(letters, 70, replace=T), sample(letters, 70, replace=T), sample(letters, 70, replace=T), sep="" ), value=sample(seq(1,70),70)) 

# Add a column with the text you want to display for each bubble:
data$text <- paste("name: ",data$group, "\n", "value:", data$value, "\n", "You can add a story here!")

# Generate the layout
packing <- circleProgressiveLayout(data$value, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot with a few differences compared to the static version:
p <- ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, label = gsub("Group_", "", group)), size=2, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()

# Turn it interactive
widg <- girafe(ggobj = p, width_svg = 8, height_svg = 8)
widg

# save the widget
# library(htmlwidgets)
# saveWidget(widg, file=paste0( getwd(), "/HtmlWidget/circular_packing_interactive.html"))

# Install required packages if not installed
install.packages(c("ggplot2", "packcircles", "dplyr", "ggiraph", "viridis"))

# Load required libraries
library(ggplot2)
library(packcircles)
library(dplyr)
library(ggiraph)
library(viridis)

# Create dataframe
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Count frequency of each group
group_counts <- df %>%
  group_by(Group) %>%
  summarise(Frequency = n()) %>%
  mutate(id = row_number())  # Unique ID for each group

# Add tooltip text
group_counts$text <- paste("Group:", group_counts$Group, "\n", 
                           "Frequency:", group_counts$Frequency)

# Pack circles based on frequency
packing <- circleProgressiveLayout(group_counts$Frequency, sizetype = "area")
group_counts <- bind_cols(group_counts, packing)

# Create circle vertices for polygons
circle_data <- circleLayoutVertices(packing, npoints = 4)

# Merge data for interactive tooltips

# Create packed bubble chart
p <- ggplot() + 
  geom_polygon_interactive(data = circle_data, 
                           aes(x, y, group = id, fill = as.factor(id), 
                               tooltip = group_counts$text, data_id = id), 
                           colour = "black", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(data = group_counts, aes(x, y, label = Group), 
            size = 5, color = "black") +
  theme_void() + 
  theme(legend.position = "none", 
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  coord_equal()
p
# Convert ggplot to an interactive plot
widg <- girafe(ggobj = p, width_svg = 7, height_svg = 7)
widg

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggiraph)
library(viridis)
library(packcircles)

# Create data frame
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Count frequency of each group
group_counts <- df %>%
  group_by(Group) %>%
  summarise(Frequency = n()) %>%
  mutate(id = row_number())  # Unique ID for each group

# Add tooltip text
group_counts$text <- paste("Group:", group_counts$Group, "\n", 
                           "Frequency:", group_counts$Frequency)

# Pack circles based on frequency
packing <- circleProgressiveLayout(group_counts$Frequency, sizetype = "area")
group_counts <- bind_cols(group_counts, packing)

# Create circle vertices for polygons
circle_data <- circleLayoutVertices(packing, npoints = 50)  # Higher npoints for smooth edges
circle_data <- circle_data %>% left_join(group_counts, by = "id")

# Create packed bubble chart
p <- ggplot() + 
  geom_polygon_interactive(data = circle_data, 
                           aes(x, y, group = id, fill = as.factor(id), 
                               tooltip = text, data_id = id), 
                           colour = "black", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(data = group_counts, aes(x, y, label = Group), 
            size = 5, color = "black") +
  theme_void() + 
  theme(legend.position = "none", 
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  coord_equal()

# Convert ggplot to an interactive plot
widg <- girafe(ggobj = p)
widg

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggiraph)
library(viridis)
library(packcircles)

# Create data frame
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Count frequency of each group
group_counts <- df %>%
  group_by(Group) %>%
  summarise(Frequency = n()) %>%
  mutate(id = row_number())  # Unique ID for each group

# Add tooltip text
group_counts <- group_counts %>%
  mutate(text = paste("Group:", Group, "\n", 
                      "Frequency:", Frequency))

# Pack circles based on frequency
packing <- circleProgressiveLayout(group_counts$Frequency * 0.5, sizetype = "area")
group_counts <- bind_cols(group_counts, packing)

# Create circle vertices for polygons
circle_data <- circleLayoutVertices(packing, npoints = 50)  # Higher npoints for smooth edges

# **Merge tooltip text into circle_data**
circle_data <- circle_data %>%
  left_join(group_counts %>% select(id, text), by = "id")

# Create packed bubble chart
p <- ggplot() + 
  geom_polygon_interactive(data = circle_data, 
                           aes(x, y, group = id, fill = as.factor(id), 
                               tooltip = text, data_id = id), 
                           colour = "black", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(data = group_counts, aes(x, y, label = Group), 
            size = 5, color = "black") +
  theme_void() + 
  theme(legend.position = "none", 
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  coord_equal()

# Convert ggplot to an interactive plot
widg <- girafe(ggobj = p, width_svg = 5, height_svg = 5)
widg

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggiraph)
library(viridis)
library(packcircles)

# Create data frame
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Count frequency of each group
group_counts <- df %>%
  group_by(Group) %>%
  summarise(Frequency = n()) %>%
  mutate(id = row_number())  # Unique ID for each group

# Add tooltip text
group_counts <- group_counts %>%
  mutate(text = paste("Group:", Group, "\n", 
                      "Frequency:", Frequency))

# Pack circles based on frequency
packing <- circleProgressiveLayout(group_counts$Frequency, sizetype = "area")  # Reduce circle sizes
group_counts <- bind_cols(group_counts, packing)

# Create circle vertices for polygons
circle_data <- circleLayoutVertices(packing, npoints = 50)  # Smooth edges
circle_data <- circle_data %>%
  left_join(group_counts %>% select(id, text), by = "id")

# Create packed bubble chart
p <-
  ggplot() + 
  geom_polygon_interactive(data = circle_data, 
                           aes(x, y, group = id, fill = as.factor(id), 
                               tooltip = text, data_id = id), 
                           colour = "black", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(data = group_counts, aes(x, y, label = Group), 
            size = 1, color = "black") +  # Reduced text size
  theme_void() + 
  theme(legend.position = "none", 
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  coord_fixed(ratio = 1)  # Scale down figure

# Convert ggplot to an interactive plot (Reduced size)
widg <- girafe(ggobj = p, width_svg = 0.1, height_svg = 0.1)
widg

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggiraph)
library(viridis)
library(packcircles)

# Create data frame
df <- data.frame(OB = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30))

# Convert to percentage
df$OB_percent <- (df$OB / max(df$OB)) * 100

# Create groups based on percentage
df$Group <- cut(df$OB_percent, 
                breaks = c(0, 40, 60, 80, 100), 
                labels = c("0-40", "40-60", "60-80", "80-100"),
                include.lowest = TRUE)

# Count frequency of each group
group_counts <- df %>%
  group_by(Group) %>%
  summarise(Frequency = n()) %>%
  mutate(id = row_number())  # Unique ID for each group

# Add tooltip text
group_counts <- group_counts %>%
  mutate(text = paste("Group:", Group, "\n", 
                      "Frequency:", Frequency))

# Pack circles based on frequency
packing <- circleProgressiveLayout(group_counts$Frequency, sizetype = "radius")  
group_counts <- bind_cols(group_counts, packing)

# Create circle vertices for polygons
circle_data <- circleLayoutVertices(packing, npoints = 50)  
circle_data <- circle_data %>%
  left_join(group_counts %>% select(id, text), by = "id")

# Create packed bubble chart
p <- ggplot() + 
  geom_polygon_interactive(data = circle_data, 
                           aes(x, y, group = id, fill = as.factor(id), 
                               tooltip = text, data_id = id), 
                           colour = "black", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(data = group_counts, aes(x, y, label = Group), 
            size = 4, color = "black") +  # Increased text size for readability
  theme_void() + 
  theme(legend.position = "none", 
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  coord_fixed(ratio = 1)  # Maintain aspect ratio

# Convert ggplot to an interactive plot
widg <- girafe(ggobj = p, width_svg = 6, height_svg = 6)  # Increased size for better visibility
widg

# Load required libraries
library(ggplot2)
library(packcircles)
library(dplyr)
library(tidyr)

# Sample data (replace this with your actual dataset)
df <- data.frame(
  AS = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 30),
  ERC = c(77, 84, 79, 88, 37, 56, 26, 63, 87, 52),
  LA = c(65, 68, 80, 92, 43, 55, 30, 71, 87, 45),
  PD = c(76, 38, 66, 95, 45, 50, 20, 81, 90, 43),
  PE = c(21, 46, 35, 32, 35, 30, 36, 23, 38, 38),
  Prac = c(48, 96, 49, 49, 37, 70, 35, 41, 49, 36),
  RM = c(84, 91, 84, 96, 70, 70, 44, 86, 86, 76),
  Skill_dev = c(47, 46, 49, 49, 44, 48, 0, 45, 45, 43)
)

# Convert data to long format
df_long <- df %>% pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")

# Define bins based on percentage ranges
df_long <- df_long %>%
  mutate(Range = cut(Value, breaks = c(0, 40, 60, 80, 100), 
                     labels = c("0-40%", "40-60%", "60-80%", "80-100%"), include.lowest = TRUE))

# Count occurrences in each range
bubble_data <- df_long %>%
  group_by(Range) %>%
  summarise(Count = n())

# Generate circle packing layout
packing <- circleProgressiveLayout(bubble_data$Count, sizetype = 'area')
bubble_data <- cbind(bubble_data, packing)

# Create the circular packing plot
ggplot() +
  geom_polygon(data = circleLayoutVertices(packing), aes(x, y, group = id, fill = as.factor(bubble_data$Count)), color = "black", alpha = 0.6) +
  geom_text(data = bubble_data, aes(x, y, label = paste(Range, "\n", Count)), size = 5) +
  scale_fill_manual(values = c("0-40%" = "red", "40-60%" = "orange", "60-80%" = "blue", "80-100%" = "green")) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Circular Packing Bubble Chart by Value Range")

# Load required libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Sample data (replace this with your actual dataset)
df <- data.frame(
  AS = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 30),
  ERC = c(77, 84, 79, 88, 37, 56, 26, 63, 87, 52),
  LA = c(65, 68, 80, 92, 43, 55, 30, 71, 87, 45),
  PD = c(76, 38, 66, 95, 45, 50, 20, 81, 90, 43),
  PE = c(21, 46, 35, 32, 35, 30, 36, 23, 38, 38),
  Prac = c(48, 96, 49, 49, 37, 70, 35, 41, 49, 36),
  RM = c(84, 91, 84, 96, 70, 70, 44, 86, 86, 76),
  Skill_dev = c(47, 46, 49, 49, 44, 48, 0, 45, 45, 43)
)

# Convert data to long format
df_long <- df %>% 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")

# Define bins based on percentage ranges
df_long <- df_long %>%
  mutate(Range = cut(Value, breaks = c(0, 40, 60, 80, 100), 
                     labels = c("0-40%", "40-60%", "60-80%", "80-100%"), include.lowest = TRUE))

# Create hierarchical structure for circle packing
nodes <- tibble(
  name = c("All Data", unique(df_long$Range), df_long$Category),
  parent = c(NA, rep("All Data", 4), df_long$Range)
)

edges <- nodes %>%
  filter(!is.na(parent)) %>%
  rename(from = parent, to = name)

# Create an igraph object
mygraph <- graph_from_data_frame(edges)

# Plot using ggraph
ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle(aes(fill = depth), alpha = 0.6, color = "black") +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Circle Packing Bubble Chart by Value Range")


# Load required libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Sample data (replace this with your actual dataset)
df <- data.frame(
  AS = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 30),
  ERC = c(77, 84, 79, 88, 37, 56, 26, 63, 87, 52),
  LA = c(65, 68, 80, 92, 43, 55, 30, 71, 87, 45),
  PD = c(76, 38, 66, 95, 45, 50, 20, 81, 90, 43),
  PE = c(21, 46, 35, 32, 35, 30, 36, 23, 38, 38),
  Prac = c(48, 96, 49, 49, 37, 70, 35, 41, 49, 36),
  RM = c(84, 91, 84, 96, 70, 70, 44, 86, 86, 76),
  Skill_dev = c(47, 46, 49, 49, 44, 48, 0, 45, 45, 43)
)

# Convert data to long format
df_long <- df %>% 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")

# Define bins based on percentage ranges
df_long <- df_long %>%
  mutate(Range = cut(Value, breaks = c(0, 40, 60, 80, 100), 
                     labels = c("0-40%", "40-60%", "60-80%", "80-100%"), include.lowest = TRUE))

# Create hierarchical structure for circle packing
nodes <- tibble(
  name = c("All Data", unique(df_long$Range), df_long$Category),
  parent = c(NA, rep("All Data", 4), df_long$Range)
)

edges <- nodes %>%
  filter(!is.na(parent)) %>%
  rename(from = parent, to = name)

# Create an igraph object
mygraph <- graph_from_data_frame(edges)

# Plot using ggraph with labels
ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle(aes(fill = depth), alpha = 0.6, color = "black") +
  geom_node_text(aes(label = name), size = 4, color = "white") +  # Add labels
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Circle Packing Bubble Chart by Value Range")

# Load required libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Sample data (replace with actual data)
df <- data.frame(
  AS = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 30),
  ERC = c(77, 84, 79, 88, 37, 56, 26, 63, 87, 52),
  LA = c(65, 68, 80, 92, 43, 55, 30, 71, 87, 45),
  PD = c(76, 38, 66, 95, 45, 50, 20, 81, 90, 43),
  PE = c(21, 46, 35, 32, 35, 30, 36, 23, 38, 38),
  Prac = c(48, 96, 49, 49, 37, 70, 35, 41, 49, 36),
  RM = c(84, 91, 84, 96, 70, 70, 44, 86, 86, 76),
  Skill_dev = c(47, 46, 49, 49, 44, 48, 0, 45, 45, 43)
)

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")

# Define percentage bins
df_long <- df_long %>%
  mutate(Range = cut(Value, breaks = c(0, 40, 60, 80, 100), 
                     labels = c("0-40%", "40-60%", "60-80%", "80-100%"), include.lowest = TRUE))

# Create hierarchy: Root -> Categories -> Variables
nodes <- tibble(
  name = c("All Data", unique(df_long$Range), df_long$Category),
  parent = c(NA, rep("All Data", 4), df_long$Range)
)

# Create edges for hierarchical structure
edges <- nodes %>%
  filter(!is.na(parent)) %>%
  rename(from = parent, to = name)

# Build graph object
mygraph <- graph_from_data_frame(edges)

# Plot using ggraph
ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle(aes(fill = depth), alpha = 0.6, color = "black") +
  geom_node_text(aes(label = name), size = 4, color = "white") +  # Labels for nodes
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Circle Packing Bubble Chart (Grouped by Percentage Range)")

library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)

# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
edges <- flare$edges
vertices <- flare$vertices
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Control the size of each circle: (use the size column of the vertices data frame)
ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle() +
  theme_void()
to_csv <- function(df, file) {
  write.csv(df, file, row.names = FALSE)
}
to_csv(OB_numeric, "numeric.csv")

# Load necessary library
library(dplyr)

# Create the data frame
data <- data.frame(
  AS = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30),
  ERC = c(77, 84, 79, 88, 37, 56, 26, 63, 88, 57, 52),
  LA = c(65, 68, 80, 92, 43, 55, 30, 71, 87, 48, 45),
  PD = c(76, 80, 66, 95, 45, 50, 20, 81, 84, 58, 43),
  PE = c(21, 38, 35, 32, 35, 37, 36, 23, 39, 38, 38),
  Prac = c(48, 46, 49, 49, 37, 43, 35, 41, 49, 41, 36),
  RM = c(84, 96, 84, 96, 70, 70, 44, 86, 86, 80, 76),
  Skill_dev = c(47, 41, 49, 49, 44, 48, 0, 45, 45, 43, 43)
)

# Define function to categorize percentages
categorize <- function(x) {
  cut(x, breaks = c(-Inf, 40, 60, 80, 100), labels = c("0-40%", "40-60%", "60-80%", "80-100%"), right = TRUE)
}

# Convert to percentage and categorize
data_percent <- data %>%
  mutate(across(everything(), ~ categorize(.)))

# Get frequency table for each subject
freq_tables <- lapply(data_percent, table)

# Print frequency tables
freq_tables

# Load necessary libraries
library(dplyr)
library(tidyr)
library(reshape2)

# Create the data frame
data <- data.frame(
  AS = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30),
  ERC = c(77, 84, 79, 88, 37, 56, 26, 63, 88, 57, 52),
  LA = c(65, 68, 80, 92, 43, 55, 30, 71, 87, 48, 45),
  PD = c(76, 80, 66, 95, 45, 50, 20, 81, 84, 58, 43),
  PE = c(21, 38, 35, 32, 35, 37, 36, 23, 39, 38, 38),
  Prac = c(48, 46, 49, 49, 37, 43, 35, 41, 49, 41, 36),
  RM = c(84, 96, 84, 96, 70, 70, 44, 86, 86, 80, 76),
  Skill_dev = c(47, 41, 49, 49, 44, 48, 0, 45, 45, 43, 43)
)

# Define function to categorize percentages
categorize <- function(x) {
  cut(x, breaks = c(-Inf, 40, 60, 80, 100), 
      labels = c("0-40%", "40-60%", "60-80%", "80-100%"), right = TRUE)
}

# Convert to percentage and categorize
data_percent <- data %>%
  mutate(across(everything(), ~ categorize(.)))

# Get frequency table for each subject
freq_tables <- lapply(data_percent, table)

# Convert the list to a data frame
freq_df <- as.data.frame(do.call(cbind, freq_tables))

# Add percentage category column
freq_df <- cbind(Percentage = rownames(freq_df), freq_df)

# Reshape to long format for pivot table structure
pivot_table <- melt(freq_df, id.vars = "Percentage", variable.name = "Subject", value.name = "Frequency")

# Print the pivot table
print(pivot_table)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(reshape2)

# Create the data frame
data <- data.frame(
  AS = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30),
  ERC = c(77, 84, 79, 88, 37, 56, 26, 63, 88, 57, 52),
  LA = c(65, 68, 80, 92, 43, 55, 30, 71, 87, 48, 45),
  PD = c(76, 80, 66, 95, 45, 50, 20, 81, 84, 58, 43),
  PE = c(21, 38, 35, 32, 35, 37, 36, 23, 39, 38, 38),
  Prac = c(48, 46, 49, 49, 37, 43, 35, 41, 49, 41, 36),
  RM = c(84, 96, 84, 96, 70, 70, 44, 86, 86, 80, 76),
  Skill_dev = c(47, 41, 49, 49, 44, 48, 0, 45, 45, 43, 43)
)

# Define function to categorize percentages
categorize <- function(x, subject) {
  cut(x, breaks = c(-Inf, 40, 60, 80, 100), 
      labels = paste(subject, c("0-40%", "40-60%", "60-80%", "80-100%"), sep = ": "), 
      right = TRUE)
}

# Convert to percentage and categorize, keeping subject names
data_percent <- data %>%
  mutate(across(everything(), ~ categorize(.x, cur_column())))

# Get frequency table for each subject
freq_tables <- lapply(data_percent, table)

# Convert the list to a data frame
freq_df <- as.data.frame(do.call(cbind, freq_tables))

# Add the Group column
freq_df <- cbind(Group = rownames(freq_df), freq_df)

# Reshape to long format for pivot table structure
pivot_table <- melt(freq_df, id.vars = "Group", variable.name = "Subject", value.name = "Frequency")

# Print the pivot table
print(pivot_table)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(reshape2)

# Create the data frame
data <- data.frame(
  AS = c(54, 63, 44, 82, 40, 42, 23, 55, 74, 41, 30),
  ERC = c(77, 84, 79, 88, 37, 56, 26, 63, 88, 57, 52),
  LA = c(65, 68, 80, 92, 43, 55, 30, 71, 87, 48, 45),
  PD = c(76, 80, 66, 95, 45, 50, 20, 81, 84, 58, 43),
  PE = c(21, 38, 35, 32, 35, 37, 36, 23, 39, 38, 38),
  Prac = c(48, 46, 49, 49, 37, 43, 35, 41, 49, 41, 36),
  RM = c(84, 96, 84, 96, 70, 70, 44, 86, 86, 80, 76),
  Skill_dev = c(47, 41, 49, 49, 44, 48, 0, 45, 45, 43, 43)
)

# Define function to categorize percentages
categorize <- function(x, subject) {
  cut(x, breaks = c(-Inf, 40, 60, 80, 100), 
      labels = c("0-40%", "40-60%", "60-80%", "80-100%"), 
      right = TRUE)
}

# Convert to percentage and categorize
data_percent <- data %>%
  mutate(across(everything(), ~ categorize(.x, cur_column())))

# Get frequency table for each subject
freq_tables <- lapply(data_percent, table)

# Convert the list to a data frame
freq_df <- as.data.frame(do.call(cbind, freq_tables))

# Add the Group column and reshape
freq_df <- cbind(Percentage = rownames(freq_df), freq_df)
pivot_table <- melt(freq_df, id.vars = "Percentage", variable.name = "Subject", value.name = "Frequency")

# Create final Group column (with subject names in group labels)
pivot_table <- pivot_table %>%
  mutate(Group = paste(Percentage)) %>%
  select(Group, Subject, Frequency)  # Reorder columns

# Print the final pivot table
print(pivot_table)

# Load necessary libraries
library(igraph)
library(ggraph)
library(tidyverse)

# Create pivot table (simulated from previous steps)
pivot_table <- data.frame(
  Group = c("AS: 0-40%", "AS: 40-60%", "AS: 60-80%", "AS: 80-100%",
            "ERC: 0-40%", "ERC: 40-60%", "ERC: 60-80%", "ERC: 80-100%",
            "LA: 0-40%", "LA: 40-60%", "LA: 60-80%", "LA: 80-100%"),
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Create nodes (subjects and percentage groups)
subjects <- unique(pivot_table$Subject)
groups <- unique(pivot_table$Group)
nodes <- data.frame(name = c(subjects, groups))

# Create edges (connections between subjects and groups)
edges <- pivot_table %>%
  rename(from = Subject, to = Group) %>%
  select(from, to, Frequency)

# Convert to igraph object
graph <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# Plot node map
ggraph(graph, layout = "fr") + 
  geom_edge_link(aes(width = Frequency), alpha = 0.8) +
  geom_node_point(size = 5, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_minimal()
# Load necessary libraries
library(treemap)
library(ggplot2)
library(ggtree)
library(packcircles)

# Create pivot table (simulated data)
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Merge Subject and Group for hierarchy
pivot_table$Hierarchy <- paste(pivot_table$Subject, pivot_table$Group, sep = " - ")

# Generate circle packing layout
packing <- circleProgressiveLayout(pivot_table$Frequency, sizetype='area')
pivot_table <- cbind(pivot_table, packing)

# Generate data for drawing the bubbles
bubbles <- circleLayoutVertices(packing, npoints=50)

# Plot bubble map
ggplot() + 
  geom_polygon(data = bubbles, aes(x, y, group=id, fill=as.factor(id)), alpha=0.6) +
  geom_text(data = pivot_table, aes(x, y, label=Hierarchy), size=3) +
  scale_fill_manual(values = rainbow(nrow(pivot_table))) +
  theme_void() +
  theme(legend.position = "none")


# Load necessary libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Create hierarchical data
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Add root node
tree_data <- tree_data %>%
  bind_rows(data.frame(id = "All Subjects", Subject = NA, Group = NA, Frequency = sum(pivot_table$Frequency)))

# Create edges (parent-child relationships)
edges <- tree_data %>%
  filter(!is.na(Subject)) %>%
  mutate(parent = ifelse(is.na(Group), "All Subjects", Subject)) %>%
  select(from = parent, to = id)

# Create igraph object
graph <- graph_from_data_frame(edges, directed = TRUE)

# Plot hierarchical bubble map
ggraph(graph, layout = "circlepack", weight = Frequency) +
  geom_node_circle(aes(fill = depth), alpha = 0.6, show.legend = FALSE) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE) +
  scale_fill_viridis_c() +
  theme_void()

# Load necessary libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Create pivot table
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Add root node
tree_data <- tree_data %>%
  bind_rows(data.frame(id = "All Subjects", Subject = NA, Group = NA, Frequency = sum(pivot_table$Frequency)))

# Create edges (parent-child relationships)
edges <- tree_data %>%
  filter(!is.na(Subject)) %>%
  mutate(parent = ifelse(is.na(Group), "All Subjects", Subject)) %>%
  select(from = parent, to = id)

# Create node attributes (including Frequency for bubble size)
nodes <- tree_data %>%
  rename(name = id) %>%
  mutate(size = Frequency)

# Create igraph object
graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# Plot hierarchical bubble map
ggraph(graph, layout = "circlepack", weight = size) + 
  geom_node_circle(aes(fill = depth), alpha = 0.6, show.legend = FALSE) + 
  geom_node_text(aes(label = name), size = 4, repel = TRUE) + 
  scale_fill_viridis_c() + 
  theme_void()
# Load necessary libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Create pivot table
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Create nodes (including root node "All Subjects")
nodes <- tree_data %>%
  bind_rows(data.frame(id = "All Subjects", Subject = NA, Group = NA, Frequency = sum(pivot_table$Frequency))) %>%
  rename(name = id) %>%
  mutate(size = Frequency) %>%
  replace_na(list(size = sum(pivot_table$Frequency)))  # Assign total sum for root node

# Create edges (parent-child relationships)
edges <- tree_data %>%
  filter(!is.na(Subject)) %>%
  mutate(parent = ifelse(is.na(Group), "All Subjects", Subject)) %>%
  select(from = parent, to = id)

# Ensure all nodes in edges are in nodes data frame
all_nodes <- unique(c(edges$from, edges$to))
nodes <- nodes %>% filter(name %in% all_nodes)

# Create igraph object
graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# Plot hierarchical bubble map
ggraph(graph, layout = "circlepack", weight = size) + 
  geom_node_circle(aes(fill = depth), alpha = 0.6, show.legend = FALSE) + 
  geom_node_text(aes(label = name), size = 4, repel = TRUE) + 
  scale_fill_viridis_c() + 
  theme_void()

# Load necessary libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Create pivot table
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Create nodes (Subjects and Groups)
nodes <- tree_data %>%
  rename(name = id) %>%
  mutate(size = Frequency)

# Add Subjects as nodes
subject_nodes <- pivot_table %>%
  select(Subject) %>%
  distinct() %>%
  mutate(name = Subject, size = NA)  # Subjects don't have a frequency

# Add root node ("All Subjects")
root_node <- data.frame(name = "All Subjects", size = sum(pivot_table$Frequency))

# Combine all nodes
nodes <- bind_rows(nodes, subject_nodes, root_node)

# Create edges (parent-child relationships)
edges <- bind_rows(
  # Connect subjects to root node
  subject_nodes %>% mutate(from = "All Subjects", to = Subject) %>% select(from, to),
  
  # Connect groups to subjects
  tree_data %>% mutate(from = Subject, to = id) %>% select(from, to)
)

# Create igraph object
graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# Plot hierarchical bubble map
ggraph(graph, layout = "circlepack", weight = size) + 
  geom_node_circle(aes(fill = depth), alpha = 0.6, show.legend = FALSE) + 
  geom_node_text(aes(label = name), size = 4, repel = TRUE) + 
  scale_fill_viridis_c() + 
  theme_void()


# Load necessary libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Create pivot table
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Create nodes (Subjects and Groups)
nodes <- tree_data %>%
  rename(name = id) %>%
  mutate(size = Frequency)

# Add Subjects as nodes
subject_nodes <- pivot_table %>%
  select(Subject) %>%
  distinct() %>%
  mutate(name = Subject, size = NA)  # Subjects don't have a frequency

# Add root node ("All Subjects")
root_node <- data.frame(name = "All Subjects", size = sum(pivot_table$Frequency))

# Combine all nodes
nodes <- bind_rows(nodes, subject_nodes, root_node)

# Create edges (parent-child relationships)
edges <- bind_rows(
  # Connect subjects to root node
  subject_nodes %>% mutate(from = "All Subjects", to = Subject) %>% select(from, to),
  
  # Connect groups to subjects
  tree_data %>% mutate(from = Subject, to = id) %>% select(from, to)
)

# Create igraph object
graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# Identify leaf nodes (lowest-level nodes)
leaf_nodes <- nodes$name[!(nodes$name %in% edges$from)]

# Update labels: Show only leaf nodes and append frequency
nodes <- nodes %>%
  mutate(label = ifelse(name %in% leaf_nodes, paste0(name, " (", size, ")"), ""))

# Plot hierarchical bubble map
ggraph(graph, layout = "circlepack", weight = size) + 
  geom_node_circle(aes(fill = depth), alpha = 0.6, show.legend = FALSE) + 
  geom_node_text(aes(label = label), size = 4, repel = TRUE) +  # Show only leaf labels
  scale_fill_viridis_c() + 
  theme_void()


# Load necessary libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Create pivot table
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Create nodes (Subjects and Groups)
nodes <- tree_data %>%
  rename(name = id) %>%
  mutate(size = Frequency)  # Leaf nodes get their Frequency as size

# Add Subjects as nodes
subject_nodes <- pivot_table %>%
  select(Subject) %>%
  distinct() %>%
  mutate(name = Subject, size = NA)  # Subjects don't have a frequency

# Add root node ("All Subjects")
root_node <- data.frame(name = "All Subjects", size = sum(pivot_table$Frequency))

# Combine all nodes
nodes <- bind_rows(nodes, subject_nodes, root_node)

# Create edges (parent-child relationships)
edges <- bind_rows(
  # Connect subjects to root node
  subject_nodes %>% mutate(from = "All Subjects", to = Subject) %>% select(from, to),
  
  # Connect groups to subjects
  tree_data %>% mutate(from = Subject, to = id) %>% select(from, to)
)

# Create igraph object
graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# Identify leaf nodes (lowest-level nodes)
leaf_nodes <- setdiff(nodes$name, edges$from)

# Update graph with node attributes
V(graph)$size <- nodes$size[match(V(graph)$name, nodes$name)]
V(graph)$label <- ifelse(V(graph)$name %in% leaf_nodes, paste0(V(graph)$name, " (", V(graph)$size, ")"), NA)

# Plot hierarchical bubble map
ggraph(graph, layout = "circlepack", weight = size) + 
  geom_node_circle(aes(fill = depth), alpha = 0.6, show.legend = FALSE) + 
  geom_node_text(aes(label = label), size = 4, repel = TRUE, na.rm = TRUE) +  # Show only leaf labels
  scale_fill_viridis_c() + 
  theme_void()

# Load necessary libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Create pivot table
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Create edges (Parent-Child relationships)
edges <- bind_rows(
  # Connect groups to subjects
  tree_data %>% mutate(from = Subject, to = id) %>% select(from, to)
)

# Create nodes (Subjects and Groups)
vertices <- tree_data %>%
  rename(name = id) %>%
  mutate(size = Frequency)  # Leaf nodes get their Frequency as size

# Add Subject nodes
subject_nodes <- pivot_table %>%
  select(Subject) %>%
  distinct() %>%
  rename(name = Subject) %>%
  mutate(size = NA)  # Subjects don't have a frequency

# Add root node ("All Subjects")
root_node <- data.frame(name = "All Subjects", size = sum(pivot_table$Frequency))

# Combine all nodes
vertices <- bind_rows(vertices, subject_nodes, root_node)

# Connect subjects to root
edges <- bind_rows(
  data.frame(from = "All Subjects", to = subject_nodes$name),
  edges
)

# Rebuild the graph object
mygraph <- graph_from_data_frame(edges, vertices = vertices, directed = TRUE)

# Identify leaf nodes
V(mygraph)$leaf <- !(V(mygraph)$name %in% edges$from)

# Assign labels only to leaves
V(mygraph)$label <- ifelse(V(mygraph)$leaf, paste0(V(mygraph)$name, " (", V(mygraph)$size, ")"), NA)

# Plot hierarchical bubble map
ggraph(mygraph, layout = "circlepack", weight = size) + 
  geom_node_circle(aes(fill = depth), alpha = 0.6, show.legend = FALSE) + 
  geom_node_text(aes(label = label, filter = leaf, size = size), repel = TRUE, na.rm = TRUE) + 
  theme_void() + 
  scale_fill_viridis()

# Load necessary libraries
library(ggraph)
library(igraph)
library(tidyverse)

# Create pivot table
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Create edges (Parent-Child relationships)
edges <- bind_rows(
  # Connect groups to subjects
  tree_data %>% mutate(from = Subject, to = id) %>% select(from, to)
)

# Create nodes (Subjects and Groups)
vertices <- tree_data %>%
  rename(name = id) %>%
  mutate(size = Frequency)  # Leaf nodes get their Frequency as size

# Add Subject nodes
subject_nodes <- pivot_table %>%
  select(Subject) %>%
  distinct() %>%
  rename(name = Subject) %>%
  mutate(size = NA)  # Subjects don't have a frequency

# Add root node ("All Subjects")
root_node <- data.frame(name = "All Subjects", size = sum(pivot_table$Frequency))

# Combine all nodes
vertices <- bind_rows(vertices, subject_nodes, root_node)

# Connect subjects to root
edges <- bind_rows(
  data.frame(from = "All Subjects", to = subject_nodes$name),
  edges
)

# Rebuild the graph object
mygraph <- graph_from_data_frame(edges, vertices = vertices, directed = TRUE)

# Identify leaf nodes
V(mygraph)$leaf <- !(V(mygraph)$name %in% edges$from)

# Assign labels only to leaves
V(mygraph)$label <- ifelse(V(mygraph)$leaf, paste0(V(mygraph)$name, " (", V(mygraph)$size, ")"), NA)

# Plot hierarchical bubble map with better readability
ggraph(mygraph, layout = "circlepack", weight = size) + 
  geom_node_circle(aes(fill = as.factor(depth)), alpha = 0.7, show.legend = FALSE) + 
  geom_node_text(aes(label = label, filter = leaf, size = size), repel = TRUE, na.rm = TRUE, color = "black") + 
  theme_void() + 
  scale_fill_manual(values = c("#440154", "#3B528B", "#21908D", "#5DC863")) # Adjust colors for better clarity


# Load required libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)

# Create the pivot table (Dataset)
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Create nodes (Subjects and Groups)
nodes <- tree_data %>%
  rename(name = id) %>%
  mutate(size = Frequency)  # Assign Frequency as size

# Add Subject nodes
subject_nodes <- pivot_table %>%
  select(Subject) %>%
  distinct() %>%
  rename(name = Subject) %>%
  mutate(size = NA)  # Subject nodes don't have a frequency

# Add root node ("All Subjects")
root_node <- data.frame(name = "All Subjects", size = sum(pivot_table$Frequency))

# Combine all nodes
vertices <- bind_rows(nodes, subject_nodes, root_node)

# Create edges (Parent-Child relationships)
edges <- bind_rows(
  data.frame(from = "All Subjects", to = subject_nodes$name),
  tree_data %>% mutate(from = Subject, to = id) %>% select(from, to)
)

# Build the graph
mygraph <- graph_from_data_frame(edges, vertices = vertices, directed = TRUE)

# Identify leaf nodes
V(mygraph)$leaf <- !(V(mygraph)$name %in% edges$from)

# Assign labels only to leaf nodes
V(mygraph)$label <- ifelse(V(mygraph)$leaf, paste0(V(mygraph)$name, " (", V(mygraph)$size, ")"), NA)

# Plot hierarchical bubble map
ggraph(mygraph, layout = "circlepack", weight = size) + 
  geom_node_circle(aes(fill = depth), alpha = 0.7, show.legend = FALSE) + 
  geom_node_text(aes(label = label, filter = leaf, size = size), repel = TRUE, na.rm = TRUE, color = "black") + 
  theme_void() + 
  theme(legend.position = "none") + 
  scale_fill_viridis()


# Load required libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)
library(ggrepel)  # Improved label positioning

# Create pivot table (Dataset)
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ": ")) %>%
  select(id, Subject, Group, Frequency)

# Create nodes (Subjects and Groups)
nodes <- tree_data %>%
  rename(name = id) %>%
  mutate(size = Frequency)  # Assign Frequency as size

# Add Subject nodes
subject_nodes <- pivot_table %>%
  select(Subject) %>%
  distinct() %>%
  rename(name = Subject) %>%
  mutate(size = NA)  # Subject nodes don't have a frequency

# Add root node ("All Subjects")
root_node <- data.frame(name = "All Subjects", size = sum(pivot_table$Frequency))

# Combine all nodes
vertices <- bind_rows(nodes, subject_nodes, root_node)

# Create edges (Parent-Child relationships)
edges <- bind_rows(
  data.frame(from = "All Subjects", to = subject_nodes$name),
  tree_data %>% mutate(from = Subject, to = id) %>% select(from, to)
)

# Build the graph
mygraph <- graph_from_data_frame(edges, vertices = vertices, directed = TRUE)

# Identify leaf nodes
V(mygraph)$leaf <- !(V(mygraph)$name %in% edges$from)

# Assign labels only to leaf nodes
V(mygraph)$label <- ifelse(V(mygraph)$leaf, paste0(V(mygraph)$name, " (", V(mygraph)$size, ")"), NA)

# Plot hierarchical bubble map with better label placement
ggraph(mygraph, layout = "circlepack", weight = size) + 
  geom_node_circle(aes(fill = depth), alpha = 0.7, show.legend = FALSE) + 
  geom_text_repel(aes(label = label, x = x, y = y), size = 3, na.rm = TRUE, color = "black", box.padding = 0.5, seed = 42) + 
  theme_void() + 
  theme(legend.position = "none") + 
  scale_fill_viridis()

# Load required libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)
library(ggrepel)  # For improved text placement

# Create pivot table (Dataset)
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Convert to hierarchical format
tree_data <- pivot_table %>%
  mutate(id = paste(Group)) %>%
  select(id, Subject, Group, Frequency)

# Create nodes (Subjects and Groups)
nodes <- tree_data %>%
  rename(name = id) %>%
  mutate(size = Frequency)  # Assign Frequency as size

# Add Subject nodes
subject_nodes <- pivot_table %>%
  select(Subject) %>%
  distinct() %>%
  rename(name = Subject) %>%
  mutate(size = NA)  # Subject nodes don't have a frequency

# Add root node ("All Subjects")
root_node <- data.frame(name = "All Subjects", size = sum(pivot_table$Frequency))

# Combine all nodes
vertices <- bind_rows(nodes, subject_nodes, root_node)

# Create edges (Parent-Child relationships)
edges <- bind_rows(
  data.frame(from = "All Subjects", to = subject_nodes$name),
  tree_data %>% mutate(from = Subject, to = id) %>% select(from, to)
)

# Build the graph
mygraph <- graph_from_data_frame(edges, vertices = vertices, directed = TRUE)

# Identify leaf nodes
V(mygraph)$leaf <- !(V(mygraph)$name %in% edges$from)

# Format label: Put frequency on a new line
V(mygraph)$label <- ifelse(V(mygraph)$leaf, paste0(V(mygraph)$name, "\n(Freq: ", V(mygraph)$size, ")"), NA)

# Plot hierarchical bubble map with improved label placement


ggraph(mygraph, layout = 'circlepack', weight = size) + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_text(aes(
    label = paste0(name, "\n(Freq: ", size, ")"),  # Name and frequency on separate lines
    filter = leaf, 
    size = size
  ), vjust = 0.5, nudge_y = 0.1) +  # Adjust vertical position
  theme_void() + 
  theme(legend.position = "FALSE") + 
  scale_fill_viridis()

# Load required libraries
library(circlepackeR)
library(data.tree)
library(dplyr)

# Create the hierarchical path column
pivot_table$pathString <- paste("root", pivot_table$Subject, pivot_table$Group, sep = "/")

# Convert to hierarchical structure
hierarchy <- as.Node(pivot_table)

# Generate the interactive circle packing plot
p <- circlepackeR(
  hierarchy, 
  size = "Frequency",  # Use frequency as size
  color_min = "hsl(56,80%,80%)", 
  color_max = "hsl(341,30%,40%)"
)

# Display the interactive plot
p

# Load required libraries
library(circlepackeR)
library(data.tree)
library(dplyr)

# Create a hierarchical path column with Frequency below each label
pivot_table <- pivot_table %>%
  mutate(Label = paste0(Group, "\n(Freq: ", Frequency, ")"))  # New formatted label

# Convert to hierarchical format with the updated Label
pivot_table$pathString <- paste("root", pivot_table$Subject, pivot_table$Label, sep = "/")

# Convert to a hierarchical structure
hierarchy <- as.Node(pivot_table)

# Generate the interactive circle packing plot
p <- circlepackeR(
  hierarchy, 
  size = "Frequency",  # Use frequency as size
  color_min = "hsl(56,80%,80%)", 
  color_max = "hsl(341,30%,40%)"
)

# Display the interactive plot
p


# Load required libraries
library(circlepackeR)
library(data.tree)
library(dplyr)
OB_numeric <- OB_percentage %>%
  select(-c(1, 2))
data_percent <- OB_numeric %>%
  mutate(across(everything(), ~ categorize(.x, cur_column())))

# Get frequency table for each subject
freq_tables <- lapply(data_percent, table)

# Convert the list to a data frame
freq_df <- as.data.frame(do.call(cbind, freq_tables))

# Add the Group column and reshape
freq_df <- cbind(Percentage = rownames(freq_df), freq_df)
pivot_table <- melt(freq_df, id.vars = "Percentage", variable.name = "Subject", value.name = "Frequency")

# Create final Group column (with subject names in group labels)
pivot_table <- pivot_table %>%
  mutate(Group = paste(Percentage)) %>%
  select(Group, Subject, Frequency)  # Reorder columns

# Print the final pivot table
print(pivot_table)
# Create a hierarchical path column, but exclude zero-frequency entries
pivot_table_filtered <- pivot_table %>%
  filter(Frequency > 0) %>%  # Remove rows where Frequency is 0
  mutate(Label = paste0(Group, "\n(Freq: ", Frequency, ")"))  # Format label

# Convert to hierarchical format with the updated Label
pivot_table_filtered$pathString <- paste("root", pivot_table_filtered$Subject, pivot_table_filtered$Label, sep = "/")

# Convert to a hierarchical structure
hierarchy <- as.Node(pivot_table_filtered)

# Generate the interactive circle packing plot
p <- circlepackeR(
  hierarchy, 
  size = "Frequency",  # Use frequency as size
  color_min = "hsl(56,80%,80%)", 
  color_max = "hsl(341,30%,40%)"
)

# Display the interactive plot
p

# Load libraries
library(ggplot2)
library(ggforce)
library(dplyr)

# Sample Data (Replace with your actual pivot_table)
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC",
              "LA", "LA", "LA", "LA"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3, 1, 4, 4, 2)
)

# Remove rows where Frequency = 0
pivot_table <- pivot_table %>% filter(Frequency > 0)

# Create a unique ID for each bubble
pivot_table <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ":"))

# Generate random X and Y positions (for better spacing)
set.seed(123)
pivot_table <- pivot_table %>%
  mutate(x = runif(n(), min = 1, max = 10),
         y = runif(n(), min = 1, max = 10))

# Plot Bubble Chart
ggplot(pivot_table, aes(x = x, y = y, size = Frequency, fill = Subject)) +
  geom_circle(aes(r = sqrt(Frequency/pi)), color = "black", alpha = 0.6) +  # Circle size based on Frequency
  geom_text(aes(label = paste0(id, "\n(", Frequency, ")")), size = 4, color = "black") + # Labels
  scale_size(range = c(3, 15)) +  # Adjust bubble sizes
  scale_fill_viridis_d() +  # Color by Subject
  theme_void() +  # Remove background grid
  theme(legend.position = "none")  # Remove legend

# Load libraries
library(ggplot2)
library(ggforce)
library(dplyr)

# Sample Data
pivot_table <- data.frame(
  Subject = c("AS", "AS", "AS", "AS",
              "ERC", "ERC", "ERC", "ERC"),
  Group = c("0-40%", "40-60%", "60-80%", "80-100%",
            "0-40%", "40-60%", "60-80%", "80-100%"),
  Frequency = c(3, 5, 2, 1, 2, 3, 3, 3)
)

# Remove rows where Frequency = 0
pivot_table <- pivot_table %>% filter(Frequency > 0)

# Create a unique ID for each bubble
pivot_table <- pivot_table %>%
  mutate(id = paste(Subject, Group, sep = ":"))

# Generate random X and Y positions for layout (adjust as needed)
set.seed(123)
pivot_table <- pivot_table %>%
  mutate(x0 = runif(n(), min = 1, max = 10),
         y0 = runif(n(), min = 1, max = 10))

# Plot Static Bubble Chart
ggplot(pivot_table) +
  geom_circle(aes(x0 = x0, y0 = y0, r = sqrt(Frequency/pi), fill = Subject), color = "black", alpha = 0.6) +  # Bubble size based on Frequency
  geom_text(aes(x = x0, y = y0, label = paste0(Group, "\n(", Frequency, ")")), size = 4, color = "black") + # Labels inside bubbles
  scale_fill_viridis_d() +  # Color by Subject
  theme_void() +  # Remove background grid
  theme(legend.position = "none")  # Remove legend

