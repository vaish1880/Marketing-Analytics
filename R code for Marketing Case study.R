# TASK 1: Load and inspect dataset
retailer <- read_csv("retailer.csv")
glimpse(retailer)
summary(retailer)

# TASK 2: Select only store attribute columns
store_vars <- retailer %>% select(variety_of_choice, electronics, furniture, 
                                  quality_of_service, low_prices, return_policy)

# TASK 3: Normalize the data using Z-scores
store_norm <- scale(store_vars) %>% as_tibble()

# Inspect mean and SD after normalization
mean(store_norm$variety_of_choice)
sd(store_norm$return_policy)

# Check summary of normalized data
summary(store_norm)

# TASK 4: Compute Euclidean distances
dist_matrix <- dist(store_norm, method = "euclidean")
print(dist_matrix)

# TASK 5: Set seed(123)
set.seed(123)

# TASK 6: Hierarchical clustering (Ward.D2)
hclust_model <- hclust(dist_matrix, method = "ward.D2")

# TASK 7: Plot dendrogram
plot(hclust_model, labels = FALSE, main = "Dendrogram - Ward.D2")

# TASK 8: Create 3-cluster solution
rect.hclust(hclust_model, k = 3, border = "YELLOW")

# TASK 9: observation for cluster-3 
hcluster_groups3 <- cutree(hclust_model, k = 3)
table(hcluster_groups3)

# TASK 10: K-means clustering (3 clusters)
set.seed(123)
kmeans_3 <- kmeans(store_norm, centers = 3, iter.max = 1000, nstart = 100)
table(kmeans_3$cluster)

# TASK 11: Create 4-cluster solution
rect.hclust(hclust_model, k = 4, border = "ORANGE")

hcluster_groups4 <- cutree(hclust_model, k = 4)
table(hcluster_groups4)

# TASK 12: K-means clustering (4 clusters)
set.seed(123)
kmeans_4 <- kmeans(store_norm, centers = 4, iter.max = 1000, nstart = 100)
table(kmeans_4$cluster)

# TASK 13: Use NbClust to decide better solution
set.seed(123)
nb_result <- NbClust(store_norm, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2")

# TASK 14: Profile clusters using normalized data
retailer <- retailer %>% 
  mutate(kmeans_cluster = kmeans_3$cluster)

store_norm <- store_norm %>% 
  mutate(cluster = kmeans_3$cluster)

# Profile by cluster (mean z-scores)
store_norm %>%
  group_by(cluster) %>%
  summarise_all(mean)

# Add hierarchical cluster labels
retailer <- retailer %>%
  mutate(hclust_cluster = hcluster_groups3)

# Profile using original data (easier to interpret)
retailer %>%
  group_by(kmeans_cluster) %>%
  summarise(across(variety_of_choice:age, mean))

# Flexclust plot for profile visualization
kcca_model <- as.kcca(kmeans_3, store_norm %>% select(-cluster), k = 3)
barchart(kcca_model, main = "Segment Profiles")
