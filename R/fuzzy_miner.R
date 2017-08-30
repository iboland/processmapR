
#' @title Fuzzy Process Map
#'
#' @description A function for creating a fuzzy miner process map of an event
#'  log. FUNCTION IS NOT WORKING YET!!!!!
#'
#'  The \code{fuzzy_process_map} function provides an alternative mapping tool
#'  to \code{proccess_map} by automatically filtering for only significant
#'  activities and process flows in the process map.  This allows the user to
#'  avoid "process map spaghetti" where visualisations of process maps can
#'  become incomprehensible when mapping very unstructured event logs.
#'
#' @param eventlog eventlog, The event log object for which to create a process
#'   map
#' @param type A process map type, which can be created with the functions
#'   \code{frequency} and \code{performance}. The first type focusses on the
#'   frequency aspect of a process, while the second one focusses on processing
#'   time.
#' @param duration_type function, if you use are creating a 'performance' type
#'   map, the function to use to summarise time difference between event nodes.
#'   Defaults to median.
#' @param render logical, whether the map should be rendered immediately
#'   (default), or rather an object of type dgr_graph should be returned.
#' @param preserve_threshold number between 0 and 1, the preservation threshold
#'   to use for conflict resolution between two nodes.  A higher value preserves
#'   more 'length 2 loops'.
#' @param ratio_threshold number between 0 and 1, the ratio threshold to use for
#'   conflict resolution between two nodes.  A higher value results in fewer
#'   edges being preserved.
#' @param utility_ratio number between 0 and 1, when edge filtering is done
#'   the weighting given to significance is the \code{utility_ratio} and the
#'   weighting for correlation is \code{1 - utility_ratio}.  A higher value
#'   places more weight on the significance of the edge.
#' @param edge_cutoff number between 0 and 1, this parameter determines the
#'   aggressiveness of the edge filtering algorithm.  A higher value results in
#'   fewer edges.
#' @param node_cutoff  number between 0 and 1, this parameter determines the
#'   aggressiveness of the node filtering algorithm.  A higher value results in
#'   fewer nodes.
#' @param freq_edge_cutoff  (TESTING PARAM) number between 0 and 1, this
#'   parameter determines the how much significance of edge depends on the
#'   frequency vs distance metric
#'
#'
#' @details This function is based on the 'Fuzzy Mining' approach developed by
#'  Christian W. Gunther and Wil M.P. van der Aalst, which they implemented in
#'  ProM.
#'
#' @return plots a proccess map if render = TRUE, otherwise returns a dgr_graph
#'   object
#'
#' @references Fuzzy mining - adaptive process simplification based on
#'   multi-perspective metrics,
#'   https://pure.tue.nl/ws/files/2094639/Metis210572.pdf
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' fuzzy_process_map(patients)
#' }
#' @export fuzzy_process_map

fuzzy_process_map <- function(eventlog, type = frequency("absolute"),
	duration_type = median, render = T, preserve_threshold = .5,
	ratio_threshold = .4, utility_ratio = .5, edge_cutoff = .2, node_cutoff = 0,
	freq_edge_cutoff = .6)
	{

	# Copy log and relabel variables used in process mapping

	log <- eventlog

	colnames(log)[colnames(log) == case_id(eventlog)] <- "case"
	colnames(log)[colnames(log) == activity_id(eventlog)] <- "event"
	colnames(log)[colnames(log) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(log)[colnames(log) == activity_instance_id(eventlog)] <- "aid"


	log <- log %>% mutate(node_id = as.numeric(as.factor(event)))

	edges <<- make_edge_table(log, type, duration_type)


	# Make edge grap to calculate distances to determine attenuation levels
	# between nodes
	edge_graph <- igraph::graph_from_data_frame(
		edges[, c('node_id', 'next_node_id', 'edge_freq_sig')])

	node_distances <- igraph::distances(edge_graph)

	# Calculate node frequency significance
	nodes_freq <- eventlog %>%
		activities() %>%
		dplyr::arrange_(activity_id(eventlog)) %>%
		dplyr::mutate(node_freq_sig = relative_frequency /
					  	max(relative_frequency))

	colnames(nodes_freq)[colnames(nodes_freq) == activity_id(eventlog)] <- "event"

	# Calculate edge frequency significance and scale median time
	edges_scale_sig <- edges %>%
		dplyr::mutate(
			edge_freq_sig = (n / max(n)),
			edge_freq_sig = edge_freq_sig / max(edge_freq_sig)
		)

	# Calculate distance significance using edge freq significance and node freq
	# signifcance.  The more the edge significance differs from the nodes, the
	# lower the value.
	# For start and end nodes, take the node freq significance value as being
	# the freq significance of the edge.



	dist_sig_df <<- edges %>%
		dplyr::left_join(nodes_freq[, c(1, 4)], by = c("event")) %>%
		dplyr::left_join(nodes_freq[, c(1, 4)], by = c("next_event" = "event")) %>%
		#  dplyr::filter(is.na(node_freq_sig.x) == F & is.na(node_freq_sig.y) == F) %>%
		dplyr::mutate(
			node_freq_sig.x = dplyr::coalesce(node_freq_sig.x, edge_freq_sig),
			node_freq_sig.y = dplyr::coalesce(node_freq_sig.y, edge_freq_sig),
			dist_sig = pmax(0, 1 - abs(
				2 * edge_freq_sig - node_freq_sig.x
				- abs(node_freq_sig.y)
			)),
			edge_sig = freq_edge_cutoff * edge_freq_sig  + (1 - freq_edge_cutoff)
			* dist_sig,
			prox_cor = prox_med_scaled
		)

    # CALCULATE CORRELATIONS
	# Calculate proximity calculation.  Median exponential value used to ensure
	# good distribution between 0 and 1

	prox_cor <- dist_sig_df %>%
		prox_med_cor = exp(- prox_med / median(prox_med, na.rm = T))

	# TODO Caluclate originator correlation (source system?)

	# TODO Calculate endpoint correlation (acitivity names - used fasttext or
	# java PROM code)

	# TODO Calculate data type correlation - create and use event metadata function

	# TODO investigate data value corr - but LEAVE FOR LAST

	# TODO Calculate routing significance

	# TODO - add cumulative option for performance time?
	if(attr(type, "perspective") == "frequency") {
		if(type == "absolute") {
			edges <- edges %>%
				dplyr::mutate(penwidth = 1 + 3 * (n - min(n))/(max(n) - min(n)))
		} else {
			# Edge width based on relative values instead of absolute
			edges <- edges %>%
				dplyr::mutate(n = round(rel_n * 100, 2)) %>%
				dplyr::mutate(penwidth = 1 + 3 * (n - min(n))/(max(n) - min(n)))
		}

	} else {
		# Edge width based on time taken between events
		edges <- edges %>%
			dplyr::mutate(penwidth = 1 + 3 * (time_diff - min(time_diff, na.rm = T))
						  /	(max(time_diff, na.rm = T) - min(time_diff, na.rm = T)))
	}



	# Create data frame of nodes information, based on if process map type is
	# frequency based or peformance/processing time based
	if (attr(type, "perspective") == "frequency") {
		# Node values represent frequency
		nodes <- nodes_freq

	} else {
		# Node values represent processing time (e.g. days)
		nodes <- eventlog %>%
			processing_time("activity", units = attr(type, "units")) %>%
			attr("raw") %>%
			group_by_(activity_id(eventlog)) %>%
			summarize(absolute_frequency = type(processing_time)) %>%
			arrange_(activity_id(eventlog)) -> nodes
	}

	colnames(nodes)[colnames(nodes) == activity_id(eventlog)] <- "event"

	# Create nodes_df input with nodes attributes for DiagrammeR graph
	if(attr(type, "perspective") == "performance") {
		nodes_df <- create_node_df(n = nrow(nodes) + 2,
								   nodes = 0:(n_activities(eventlog) + 1),
								   label = c("Start", c(paste0(nodes$event, " (",round(nodes$absolute_frequency, 3), ")")),"End"),
								   shape = c("circle",rep("rectangle", nrow(nodes)), "circle"),
								   style = "rounded,filled",
								   fontcolor = c("green",rep("white", nrow(nodes)),"red"),
								   color = c("green",rep("grey", nrow(nodes)), "red"),
								   penwidth = 1.5,
								   frequency = c( -Inf, nodes$absolute_frequency, max(nodes$absolute_frequency)+2),
								   fillcolor = c("green",rep("dodgerblue4", nrow(nodes)),"red"),
								   fontname = "Arial",
								   tooltip = c("Start", paste0(nodes$event, "\n (",nodes$absolute_frequency, ") ", attr(type, "units")),"End"))

	} else if(type == "absolute") {

		nodes_df <- create_node_df(n = nrow(nodes) + 2,
								   nodes = 0:(n_activities(eventlog) + 1),
								   label = c("Start", c(paste0(nodes$event, " (",nodes$absolute_frequency, ")")),"End"),
								   shape = c("circle",rep("rectangle", nrow(nodes)), "circle"),
								   style = "rounded,filled",
								   fontcolor = c("green",rep("white", nrow(nodes)),"red"),
								   color = c("green",rep("grey", nrow(nodes)), "red"),
								   penwidth = 1.5,
								   frequency = c( -Inf, nodes$absolute_frequency, max(nodes$absolute_frequency)+2),
								   fillcolor = c("green",rep("dodgerblue4", nrow(nodes)),"red"),
								   fontname = "Arial",
								   tooltip = c("Start", paste0(nodes$event, "\n (",nodes$absolute_frequency, ")"), "End"))

	} else {
		nodes_df <- create_node_df(n = nrow(nodes) + 2,
								   nodes = 0:(n_activities(eventlog) + 1),
								   label = c("Start", c(paste0(nodes$event, " (",round(100*nodes$relative_frequency,2), ")")),"End"),
								   shape = c("circle",rep("rectangle", nrow(nodes)), "circle"),
								   style = "rounded,filled",
								   fontcolor = c("green",rep("white", nrow(nodes)),"red"),
								   color = c("green",rep("grey", nrow(nodes)), "red"),
								   penwidth = 1.5,
								   frequency = c( -Inf, nodes$absolute_frequency, max(nodes$absolute_frequency)+2),
								   fillcolor = c("green",rep("dodgerblue4", nrow(nodes)),"red"),
								   fontname = "Arial",
								   tooltip = c("Start",paste0(nodes$event, "\n (",nodes$absolute_frequency, ")"), "End"))
	}

	#TODO - if performance is chosen, change label to character and relabel
	# from current number of seconds to flowest floor (second, minute, hour, day)
	# e.g. 70 seconds -> "1.x minutes"

	# Relabeling edges if performance time is used
	if(attr(type, "perspective") == "performance") {
		edges <- edges %>%
			mutate(time_diff_label =
				   	case_when(is.na(time_diff) ~ NA_character_,
				   			  time_diff < 60 ~ paste(round(time_diff, 1), 'secs'),
				   			  time_diff < 3600 ~ paste(round(time_diff / 60, 1) , 'mins'),
				   			  time_diff < 86400 ~ paste(round(time_diff / 3600, 1), 'hrs'),
				   			  !is.na(time_diff) ~ paste(round(time_diff / 86400, 1), 'days'),
				   			  TRUE ~ NA_character_)
			)
	}

	# Create nodes_df input with nodes attributes for DiagrammeR graph
	edges_df <- create_edge_df(from = edges$node_id +1,
							   to= edges$next_node_id + 1,
							   label = edges$time_diff_label,
							   color = "grey",
							   fontname = "Arial",
							   arrowsize = 1,
							   penwidth = edges$penwidth)

	# Create graph object
	graph <- create_graph(nodes_df, edges_df) %>%
		set_global_graph_attrs(attr = "rankdir",
							   value =  "LR",
							   attr_type =  "graph")

	# Colour graph nodes
	if(attr(type, "perspective") == "performance")
	{
		graph <- graph %>%
			colorize_node_attrs(node_attr_from = "frequency",
								node_attr_to = "fillcolor",
								palette = "Oranges",
								default_color = "white",
								reverse_palette = F,
								cut_points = seq(min(nodes$absolute_frequency) - 1 - 0.0001*(0.01+diff(range(nodes$absolute_frequency))),
												 max(nodes$absolute_frequency)  + 0.0001*(0.01+diff(range(nodes$absolute_frequency))),
												 length.out = 9))
	} else {
		graph <- graph	%>%
			colorize_node_attrs(node_attr_from = "frequency",
								node_attr_to = "fillcolor",
								palette = "PuBu",
								default_color = "white",
								reverse_palette = F,
								cut_points = seq(min(nodes$absolute_frequency) - 0.63*(0.01+diff(range(nodes$absolute_frequency))),
												 max(nodes$absolute_frequency) + 1,
												 length.out = 9))

	}

	# Plot graph if render == TRUE otherwise return graph object
	if(render == T)
		graph %>% render_graph() %>% return()
	else
		graph %>% return()

}


# Function to calculate the routing significance of nodes
routing_sig <- function(edges, nodes_freq) {
	# Routing frequency calculation
	# Caculate number and significance of  edges leading into node
	node_edges_pred <- edges %>%
		dplyr::group_by(next_event) %>%
		dplyr::inner_join(nodes_freq[, c(1,4)], by = c("event"= "event")) %>%
		#		dplyr::filter(next_event != 'End') %>%
		dplyr::summarise(sig_pred = sum(freq_sig)) %>%
		dplyr::arrange(next_event)

	node_edges_exit<- edges %>%
		dplyr::group_by(event) %>%
		dplyr::inner_join(nodes_freq[, c(1,4)], by = c("next_event"= "event")) %>%
		dplyr::filter(event != 'Start' ) %>%
		dplyr::summarise(sig_exit = sum(freq_sig)) %>%
		dplyr::arrange(event)

	routing_sig <- dplyr::bind_cols(node_edges_exit, node_edges_pred[,2]) %>%
		mutate(rout_sig = abs(sig_pred - sig_exit) / sig_pred + sig_exit,
			   rout_sig = rout_sig/max(rout_sig))


	unary_sig <- routing_sig$rout_sig

	# Calculate node significance by node frequency and number of edges
	nodes_sig <- nodes_freq

}

# Function to create table of edges by node
make_edge_table <- function(log, type, duration_type) {

	# Create vectors of process map nodes, including start and end states
	start_points <- log %>%
		dplyr::group_by(case) %>%
		dplyr::arrange(timestamp_classifier) %>%
		dplyr::slice(1:1) %>%
		dplyr::mutate(timestamp_classifier = timestamp_classifier - 1,
					  event = "Start",
					  node_id = 0)

	end_points <- log %>%
		dplyr::group_by(case) %>%
		dplyr::arrange(desc(timestamp_classifier)) %>%
		dplyr::slice(1:1) %>%
		dplyr::mutate(timestamp_classifier = timestamp_classifier + 1,
					  event = "End",
					  node_id = length(unique(log$event)) + 1)

	# Create data frame of precending events for each event in the log by case
	precedences <- log %>%
		dplyr::bind_rows(start_points) %>%
		dplyr::bind_rows(end_points) %>%
		dplyr::group_by(aid, event, node_id, case) %>%
		dplyr::summarize(ts = min(timestamp_classifier)) %>%
		dplyr::group_by(case) %>%
		dplyr::arrange(ts) %>%
		dplyr::mutate(next_event = lead(event),
					  next_node_id = lead(node_id)) %>%
		dplyr::filter(is.na(next_event) == F)
	#na.omit()

	# If time performance is chosen, get time between events
	# if (attr(type, "perspective") == "performance") {
	# 	edges <- precedences %>%
	# 		dplyr::mutate(time_diff_num = as.numeric(lead(ts) - ts)) %>%
	# 		dplyr::group_by(event, node_id, next_event, next_node_id) %>%
	# 		summarise(n = n(),
	# 				  time_diff = duration_type(time_diff_num),
	# 				  						  na.rm = T)
	# } else {
	# 	edges <- precedences %>%
	# 		dplyr::mutate(time_diff_num = as.numeric(lead(ts) - ts)) %>%
	# 		dplyr::group_by(event, node_id, next_event, next_node_id) %>%
	# 		summarise(n = n())
	# }

	# Create data frame of edge information including time difference between
	# events, time differences and
	edges <- precedences %>%
		dplyr::mutate(time_diff_num = as.numeric(lead(ts) - ts)) %>%
		dplyr::group_by(event, node_id, next_event, next_node_id) %>%
		summarise(
			n = n(),
			time_diff = duration_type(time_diff_num, na.rm = T),
			prox_med = median(time_diff_num, na.rm = T)
		) %>%
		dplyr::group_by(event, node_id) %>%
		dplyr::mutate(rel_n = n / (sum(n))) %>%
		dplyr::ungroup()
	)

return(edges)
}

