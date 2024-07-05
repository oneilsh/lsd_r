library(klaR)

set.seed(1)
x <- rbind(matrix(rbinom(250, 1, 0.25), ncol = 5),
					 matrix(rbinom(250, 1, 0.75), ncol = 5))
colnames(x) <- c("a", "b", "c", "d", "e")

## run algorithm on x:
(cl <- kmodes(x, 3))

## and visualize with some jitter:
plot(jitter(x), col = cl$cluster)
points(cl$modes, col = 1:5, pch = 8)

#######################

tdf <- data.frame(
	person_id = c(11, 11, 11, 222, 222, 33, 33, 33, 33),
	#lsd_condition = c("A", "A", "A", "B", "B", "C", "C", "C", "C"),
	ngd_condition = c("x", "p", "z", "x", "z", "p", "q", "r", "z")
)

library(dplyr)

tdf <- data.frame(
	person_id = sample(1:100, size = 1000, replace = TRUE),
	ngd_condition = sample(letters, size = 1000, replace = TRUE)
) %>% distinct()

library(tidyr)

# tdf$ngd_value <- rnorm(nrow(tdf))
tdf$ngd_value <- 1

print(tdf)

tdf_wide <- pivot_wider(tdf,
												id_cols = c(person_id),
												names_from = ngd_condition,
												values_from = ngd_value, values_fill = 0)

# let's convert to an old-school data.frame
tdf_wide <- as.data.frame(tdf_wide)
# let's put person_id as the row *names*, and remove it as a column
rownames(tdf_wide) <- tdf_wide$person_id
tdf_wide$person_id <- NULL

# kmodes took the data as a data.frame -> it probably converted to a matrix automatically
clusters <- kmodes(tdf_wide, 4)

str(clusters)
# List of 6
# $ cluster   : int [1:3] 2 2 1
# $ size      : 'table' int [1:2(1d)] 1 2
# ..- attr(*, "dimnames")=List of 1
# .. ..$ cluster: chr [1:2] "1" "2"
# $ modes     :'data.frame':	2 obs. of  5 variables:
# 	..$ x: num [1:2] 0 1
# ..$ p: num [1:2] 1 0
# ..$ z: num [1:2] 1 1
# ..$ q: num [1:2] 1 0
# ..$ r: num [1:2] 1 0
# $ withindiff: num [1:2] 0 1
# $ iterations: int 2
# $ weighted  : logi FALSE
# - attr(*, "class")= chr "kmodes"


# try umap
library(umap)
umap_res <- umap(tdf_wide)

str(umap_res)
# List of 4
# $ layout: num [1:100, 1:2] 1.64 -1.793 1.213 -1.884 0.482 ...
# ..- attr(*, "dimnames")=List of 2
# .. ..$ : chr [1:100] "5" "54" "30" "17" ...
# .. ..$ : NULL
# $ data  : num [1:100, 1:26] 1 0 0 1 0 0 0 0 0 1 ...
# ..- attr(*, "dimnames")=List of 2
# .. ..$ : chr [1:100] "5" "54" "30" "17" ...
# .. ..$ : chr [1:26] "y" "g" "o" "d" ...
# $ knn   :List of 2
# ..$ indexes  : int [1:100, 1:15] 1 2 3 4 5 6 7 8 9 10 ...
# .. ..- attr(*, "dimnames")=List of 2
# .. .. ..$ : chr [1:100] "5" "54" "30" "17" ...
# .. .. ..$ : NULL
# ..$ distances: num [1:100, 1:15] 0 0 0 0 0 0 0 0 0 0 ...
# .. ..- attr(*, "dimnames")=List of 2
# .. .. ..$ : chr [1:100] "5" "54" "30" "17" ...
# .. .. ..$ : NULL
# ..- attr(*, "class")= chr "umap.knn"
# $ config:List of 24
# ..$ n_neighbors         : int 15
# ..$ n_components        : int 2
# ..$ metric              : chr "euclidean"
# ..$ n_epochs            : int 200
# ..$ input               : chr "data"
# ..$ init                : chr "spectral"
# ..$ min_dist            : num 0.1
# ..$ set_op_mix_ratio    : num 1
# ..$ local_connectivity  : num 1
# ..$ bandwidth           : num 1
# ..$ alpha               : num 1
# ..$ gamma               : num 1
# ..$ negative_sample_rate: int 5
# ..$ a                   : num 1.58
# ..$ b                   : num 0.895
# ..$ spread              : num 1
# ..$ random_state        : int 850469773
# ..$ transform_state     : int NA
# ..$ knn                 : logi NA
# ..$ knn_repeats         : num 1
# ..$ verbose             : logi FALSE
# ..$ umap_learn_args     : logi NA
# ..$ method              : chr "naive"
# ..$ metric.function     :function (m, origin, targets)
# 	..- attr(*, "class")= chr "umap.config"
# - attr(*, "class")= chr "umap"

layout_df <- data.frame(umap_res$layout)

## Fix out tdf_wide back up
# put the person_id back
tdf_wide$person_id <- rownames(tdf_wide)

# let's put the cluster numbers in there
tdf_wide$cluster_num <- factor(clusters$cluster)

# let's add the layout coordinates
tdf_wide$x_coord <- layout_df$X1
tdf_wide$y_coord <- layout_df$X2

library(ggplot2)

p <- ggplot(tdf_wide) +
	geom_point(aes(x = x_coord, y = y_coord, color = cluster_num))

plot(p)

# use dplyr's joins to bring in lsd_condition, try coloring by that,
# adding a shape for cluster num, use facets for different clusters/lsd conditions/etc

# this might be a metric for evaluating the cluster "quality"? see how it changes with different
# cluster nums, there might be an obvious choice ("elbow method")
mean_withindiff <- mean(clusters$withindiff)
print(mean_withindiff)







