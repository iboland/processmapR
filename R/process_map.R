
#' @title Process Map
#'
#'
#' @description A function for creating a process map of an event log.
#' @param eventlog The event log object for which to create a process map
#' @param type A process map type, which can be created with the functions
#'   frequency and performance. The first type focusses on the frequency aspect
#'   of a process, while the second one focusses on processing time.
#' @param render Whether the map should be rendered immediately (default),
#' or rather an object of type dgr_graph should be returned.
#'
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' process_map(patients)
#' }
#' @export process_map




process_map <- function(eventlog, type = frequency("absolute") , render = T) {

	# Copy log and relabel variables used in process mapping
	log <- eventlog

	colnames(log)[colnames(log) == case_id(eventlog)] <- "case"
	colnames(log)[colnames(log) == activity_id(eventlog)] <- "event"
	colnames(log)[colnames(log) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(log)[colnames(log) == activity_instance_id(eventlog)] <- "aid"

	# Create vectors of process map nodes, including start and end states
	log <- log %>%
		mutate(node_id = as.numeric(as.factor(event)))

	start_points <- log %>%
		group_by(case) %>%
		arrange(timestamp_classifier) %>%
		slice(1:1) %>%
		mutate(timestamp_classifier = timestamp_classifier - 1,
			   event = "Start",
			   node_id = 0)

	end_points <- log %>%
		group_by(case) %>%
		arrange(desc(timestamp_classifier)) %>%
		slice(1:1) %>%
		mutate(timestamp_classifier = timestamp_classifier + 1,
			   event = "End",
			   node_id = n_activities(eventlog)+1)

	# Create data frame of precending events for each event in the log by case
	precedences <- log %>%
		bind_rows(start_points) %>%
		bind_rows(end_points) %>%
		group_by(aid, event, node_id, case) %>%
		summarize(ts = min(timestamp_classifier)) %>%
		group_by(case) %>%
		arrange(ts) %>%
		mutate(next_event = lead(event),
			   next_node_id = lead(node_id)) %>%
		na.omit()

	# Create data frame of edge information
	edges <- precedences %>%
		group_by(event, node_id, next_event, next_node_id) %>%
		summarize(n = n()) %>%
		group_by(event, node_id) %>%
		mutate(rel_n = n/(sum(n)))

	# TODO - investigate changing performance type edge info to time rather than
	# numbers
	# TODO - add cumulative option for performance time
	if(attr(type, "perspective") == "frequency") {
		if(type == "absolute") {
			edges <- edges %>%
				ungroup() %>%
				mutate(penwidth = 1 + 3*(n - min(n))/(max(n) - min(n)))
		} else {
			# Edge width based on relative values instead of absolute
			edges <- edges %>%
				ungroup() %>%
				mutate(n = round(rel_n*100, 2)) %>%
				mutate(penwidth = 1 + 3*(n - min(n))/(max(n) - min(n)))
		}

	} else {
		edges <- edges %>%
			ungroup() %>%
			mutate(penwidth = 1 + 3*(n - min(n))/(max(n) - min(n)))
	}

	# Calculate node frequency
	nodes_freq <- eventlog %>%
		activities() %>%
		dplyr::arrange_(activity_id(eventlog)) %>%
		dplyr::mutate(freq_sig = relative_frequency / max(relative_frequency))

	# Create data frame of nodes information, based on if process map type is
	# frequency based or peformance/processing time based
	if(attr(type, "perspective") == "frequency") {
		# Node values represent frequency
		nodes <- nodes_freq

	} else {
		# Node values represent processing time (e.g. days)
		nodes <- eventlog %>%
			processing_time("activity", units = attr(type, "units")) %>%
			attr("raw") %>%
			group_by(event) %>%
			summarize(absolute_frequency = type(processing_time)) %>%
			arrange(event)
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

	# Create nodes_df input with nodes attributes for DiagrammeR graph
	edges_df <- create_edge_df(from = edges$node_id +1,
							   to= edges$next_node_id + 1,
							   label = edges$n,
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
