
#remotes::install_github("https://github.com/monarch-initiative/monarchr", ref = "engines")

library(monarchr)
library(ggraph)
library(stringr) # for str_wrap
library(tidygraph) # activate() etc.


# MONDO:0019249 is mucopolysaccharidosis
# let's get all it's subtypes, and all the phenotypes directly connected to any of those subtypes
mps_phenos <- monarch_engine() |>
	fetch_nodes(query_ids = "MONDO:0019249") |>
	expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) |>
	expand(result_categories = c("biolink:PhenotypicFeature"))

print(mps_phenos) # 439 nodes, 1127 edges


# UPHENO:0002948 is a high-level phenotype, "abnormal immune system"
# let's get all of it's subtypes
# this query takes a couple mins
immune_phenos <- monarch_engine() |>
	fetch_nodes(query_ids = "UPHENO:0002948") |>
	expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE)

print(immune_phenos) # 5747 node,s 8774 edges

# now, let's get an inner join of the nodes data from these two
# here we are doing this on the nodes data frames; tidygraph supports graph joins, but only full outer joins
shared_phenos <- nodes(mps_phenos) |> inner_join(nodes(immune_phenos))

print(shared_phenos) # 21 rows

# Finally, let's go back to our mps_phenos graph, and keep only the nodes that are
# either a disease (ie, a type of mps), or one of the shared_phenos
mps_immune_phenos <- mps_phenos |>
	activate(nodes) |>
	filter(pcategory == "biolink:Disease" | id %in% shared_phenos$id)

print(mps_immune_phenos) # 44 nodes, 90 edges

# plot that
ggraph(mps_immune_phenos, layout = "sugiyama") +
	geom_edge_link(aes(color = predicate),
								 arrow = arrow(length = unit(2, 'mm'), type = "closed"),
								 end_cap = circle(2.5, 'mm')) +
	geom_node_point(aes(color = pcategory),
									size = 3) +
	geom_node_label(aes(label = str_wrap(name, 20)),
									size = 2,
									repel = TRUE,
									check_overlap = TRUE,
									fill = "#FFFFFF88")
