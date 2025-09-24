# Title: Input data processing
# Author: Greg Goodrum
# Last update: 8/6/2025
# Contact: goodrum.greg@gmail.com
# Description: Processing code to prepare input data for estimating connectivity for 
#              TU/BRLC barrier removal assessments

# NOTES:

# References:

# --------------------------------------------------------------------- #
# 00. Set up workspace
# ---------------------------------------------------------------------

# Summary: Set up workspace, load data, and load relevant packages.

# Clean workspace
rm(list = ls())

# Load Packages
# Data/stats
if(!require("rstudioapi")){
  install.packages("rstudioapi", dependencies = TRUE); library(rstudioapi)}
if(!require("tidyverse")){
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)}
if(!require("rio")){
  install.packages("rio", dependencies = TRUE); library(rio)}
if(!require("lubridate")){
  install.packages("lubridate", dependencies = TRUE); library(lubridate)}

# Connectivity
if(!require("igraph")){
  install.packages("igraph", dependencies = TRUE); library(igraph)}
if(!require("riverconn")){
  install.packages("riverconn", dependencies = TRUE); library(riverconn)}
if(!require("ggnetwork")){
  install.packages("ggnetwork", dependencies = TRUE); library(ggnetwork)}

# Plotting
if(!require("ggplot2")){
  install.packages("ggplot2", dependencies = TRUE); library(ggplot2)}
if(!require("viridis")){
  install.packages("viridis", dependencies = TRUE); library(viridis)}

# Declare working directory
pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

# ---------------------------------------------------------------------

# --------------------------------------------------------------------- #
# 00. Initialize functions
# ---------------------------------------------------------------------

# FUNCTION: network_check() - Verify network structure such at all vertices are unique
#                             and all confluences are binary.

network_check <- function(inData, From_field, To_field){
  # Check that From_Nodes are all unique
  check.reaches    <- as.data.frame(inData %>% group_by({{From_field}}) %>% filter(n() > 1))
  
  # Check that nodes occur no more than twice in To_Node (binary junctions)
  check.confluences <- as.data.frame(inData %>% group_by({{To_field}}) %>% filter(n() > 2))
  
  # Check that To_Nodes all come from From_Nodes
  # NOTE: For network generation that produces a non-existent terminal node, there
  #       should be one row in data.outlet. For a network without a non-existent
  #       terminal node, data.outlet should be blank.
  check.outlet <- inData %>% filter(!{{To_field}} %in% {{From_field}})
  
  # Print error outputs
  # ifelse(nrow(check.reaches)     > 0, print('NETWORK CHECK: Duplicate vertices (From nodes)'),
  # ifelse(nrow(check.confluences) > 0, print('NETWORK CHECK: Nonbinary confluences'),
  # ifelse(nrow(check.outlet)      > 1, print('NETWORK CHECK: Nonbinary terminal reaches'),
  # ifelse(nrow(check.outlet)     == 1, print('NETWORK CHECK: To_node not present in From_node'),
  #                                     print('Network check complete')))))
  if(nrow(check.reaches)     > 0) {print('NETWORK CHECK: Duplicate vertices (From nodes) detected')}
  if(nrow(check.confluences) > 0) {print('NETWORK CHECK: Nonbinary confluences detected')}
  if(nrow(check.outlet)      > 1) {print('NETWORK CHECK: Nonbinary terminal reaches detected')}
  if(nrow(check.outlet)     == 1) {print('NETWORK CHECK: To_node not present in From_node detected')}
  print('Network check complete')
  
  return(list(Duplicate_Reaches     = check.reaches,
              Nonbinary_Confluences = check.confluences,
              Terminal_Reaches      = check.outlet))
}

# ---------------------------------------------------------------------------- #
# FUNCTION: generate_attributed_igraph() - Generate an igraph object with edge and vertex attributes,
#                                         add passability fields, and set graph directionality.

generate_attributed_igraph <- function(inData,
                                       From_field,
                                       To_field,
                                       EdgeType_field,
                                       Edge_attributes,
                                       Node_attributes,
                                       Outlet_node,
                                       graphFile){
  # Select edge data and remove nodes not associated with a reach
  data.edges <- inData %>% select({{From_field}},
                                  {{To_field}},
                                  {{Edge_attributes}}) %>%
    filter({{To_field}} %in% {{From_field}})
  
  # Set edge type field
  data.edges <- data.edges %>% mutate(type = ifelse(get({{EdgeType_field}}) == "",
                                                    'Confluence',
                                                    get({{EdgeType_field}})))
  
  # Generate igraph object with edge attributes
  data.graph <- graph_from_data_frame(data.edges)
  
  # Select node data
  data.nodes <- inData %>% select({{From_field}},
                                  {{Node_attributes}})
  
  # Attribute nodes
  for(col in colnames(data.nodes)){
    data.graph <- set_vertex_attr(data.graph,
                                  name = col,
                                  index = V(data.graph),
                                  value = sapply(V(data.graph)$name, function(x){
                                    unlist(data.nodes %>%
                                             filter(From_Node == x) %>%
                                             .[col])
                                  }))
  }
  
  # Assign network directionality based on outlet reach
  data.graph <- set_graph_directionality(data.graph,
                                         field_name = 'name',
                                         outlet_name = as.character(Outlet_node))
  
  # Initialize passability fields
  field.pass <- c('pass_u', 'pass_d')
  for(i in 1:length(field.pass)){
    data.graph <- set_edge_attr(data.graph,
                                field.pass[i],
                                value = 1.0)
  }
  
  # Identify outlet edge for plotting
  index.outlet <- which(V(data.graph)$name == Outlet_node)
  
  # Set plotting dimensions
  size.plot <- data.frame(node  = NA,
                          edge  = NA,
                          arrow = NA,
                          text  = NA)
  ifelse(length(V(data.graph)) <= 100,  size.plot[1,] <- c(0.1, 1, 10, 5),
         ifelse(length(V(data.graph)) <= 1000, size.plot[1,] <- c(0.05, 0.5, 5, 2.5),
                size.plot[1,] <- c(0.01, 0.1, 1, 0.5)))
  
  # Plot to confirm
  gg0 <- ggnetwork(data.graph,
                   layout =  layout_as_tree(data.graph %>% as.undirected, root = index.outlet),
                   scale = FALSE)
  plot <-
    ggplot(gg0, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_nodes(alpha = 0.3,
               size = size.plot$node) +
    geom_edges(alpha = 0.5,
               arrow = arrow(length = unit(size.plot$arrow, "pt"), type = "closed"),
               linewidth = size.plot$edge,
               aes(color = type)) +
    scale_color_viridis(discrete = TRUE)+
    geom_nodetext(aes(label = name), fontface = "bold",
                  size = size.plot$text) +
    theme_blank()
  ggsave(graphFile, plot = plot,
         width = 40, height = 40, units = 'cm',
         dpi = 600)
  
  return(data.graph)
  
}

# ---------------------------------------------------------------------------- #
# FUNCTION: join_edge_attributes() - Function that joins edge attributes based on a common identifier field.
#                                    Function currently hard-coded for 'UID', 'Type_R', and 'Pass_R' fields.

join_edge_attributes <- function(inGraph, inData, field.pass){
  # Initialize output
  outGraph <- inGraph
  
  # Join pass field
  outGraph <- set_edge_attr(outGraph,
                            name = as.name(field.pass),
                            index = E(outGraph),
                            value = as.numeric(sapply(E(outGraph)$UID, function(x){
                              unlist(inData %>%
                                       filter(UID == x) %>%
                                       .[[as.name(field.pass)]])
                            })))
  
  # Convert pass field to numeric
  outGraph <- set_edge_attr(outGraph,
                            name = as.name(field.pass),
                            index = E(outGraph),
                            value = ifelse(E(outGraph)$type == 'Junction',
                                           1.0, E(outGraph)$Pass_R))
  
  # Return output
  return(outGraph)
}

# ---------------------------------------------------------------------------- #
# FUNCTION: calculate_dci() - Function calculates symmetric and aysmmetric DCI from a provided igraph object,
#                                    dataframe of barrier passability, and fields indicating the
#                                    passability, and weight fields of the input tables.

calculate_dci <- function(inGraph, scenario.id, field.pass, field.weight){
  # Initialize output table
  data.out <- data.frame(Scenario = character(0),
                         DCI_symm = numeric(0),
                         DCI_asym = numeric(0))
  
  # Attribute pass_u and pass_d with joined passability values
  field.pass <- c('pass_u', 'pass_d')
  graph.out <- inGraph # NOTE: Must change graph here so that both pass_u and pass_d update
  for(i in 1:length(field.pass)){
    graph.out <- set_edge_attr(graph.out,
                               field.pass[i],
                               value = E(inGraph)$Pass_R)
  }
  
  # Calculate DCI.symmetric
  dci.symm <- index_calculation(graph = graph.out,
                                weight = field.weight,
                                B_ij_flag = FALSE,
                                index_type = 'full',
                                dir_fragmentation_type = 'symmetric')
  
  # Calculate DCI.asymmetric
  dci.asym <- index_calculation(graph = graph.out,
                                weight = field.weight,
                                B_ij_flag = FALSE,
                                index_type = 'full',
                                dir_fragmentation_type = 'asymmetric')
  
  # Attribute output
  data.out <- data.out %>% add_row(Scenario = scenario.id,
                                   DCI_symm = dci.symm$index,
                                   DCI_asym = dci.asym$index)
  
  # Return output
  # return(graph.out)
  return(data.out)
}

# ---------------------------------------------------------------------

# --------------------------------------------------------------------- #
# 01. Barrier Data (currently only Bear Lake)
# ---------------------------------------------------------------------

# Declare data
file.data.tu <- paste0(getwd(), '/TU/TU_BearLake.xlsx')
file.data.nabd <- paste0(getwd(), '/NABD/NABD_SnakeBear.csv')

# Load data
data.tu <- import_list(file.data.tu)
data.nabd <- read.csv(file = file.data.nabd, header = TRUE)

# ---------------------------------------------------------------------------- #

# Format Data - NABD
data.nabd.join <- data.nabd %>%
  select(Name,BarrierType,
         River,
         lat, lon,
         SARPID,
         Passability,
         Removed,
         YearRemoved) %>%
  rename(BarrierName = Name,
         StreamName = River,
         Lat = lat,
         Lon = lon,
         SourceID = SARPID,
         Mitigated = Removed,
         YearMitigated = YearRemoved) %>%
  mutate(Source = 'National Aquatic Barrier Dataset (SARP)',
         Pass_NABD = Passability) %>%
  # Passability from ratings
  mutate(Pass_Before = ifelse(Pass_NABD == 'Complete barrier', 0.0,
                              ifelse(Pass_NABD == 'Unknown', NA,
                                     ifelse(Pass_NABD %in% c('Partial passability', 'Partial passability - salmonid'),
                                            0.5, 1.0)))) %>%
  # Passability unknown - estimated from barrier type
  mutate(Pass_Before = ifelse(Pass_NABD == 'Unknown' & BarrierType == 'Dam', 0,
                              ifelse(Pass_NABD == 'Unknown' & BarrierType == 'Assessed road-related barrier',
                                     0.5, Pass_Before))) %>%
  mutate(Pass_After = Pass_Before) %>%
  select(BarrierName, BarrierType, StreamName, Source, SourceID,
         Lat, Lon,
         Mitigated, YearMitigated, Pass_NABD, Pass_Before, Pass_After) %>%
  as.data.frame

# ---------------------------------------------------------------------------- #

# NOTE: Currently ignoring diversion screens, not sure these should be included in connectivity assessment.

# Format TU Data - Culverts
data.tu.culverts <- data.tu[['Culverts']] %>%
  rename(BarrierName = `Culvert/Road Name`,
         StreamName = `Waterway Name`,
         Lat = `Culvert LAT`,
         Lon = `Culvert LONG`,
         YearMitigated = `Year Replaced`,
         Pass_Before = `Culvert Passage before`,
         Pass_After = `Culvert Passage after`) %>%
  mutate(BarrierType = 'Culvert',
         Source = 'Trout Unlimited (TU)',
         SourceID = paste0('TU_CUL_', CUL_ID),
         Mitigated = 'yes',
         Pass_NABD = NA) %>%
  select(BarrierName, BarrierType, StreamName, Source, SourceID,
         Lat, Lon,
         Mitigated, YearMitigated, Pass_NABD, Pass_Before, Pass_After) %>%
  as.data.frame

# Format TU Data - Diversions
data.tu.diversions <- data.tu[['Diversions']] %>%
  rename(BarrierName = `Div Name`,
         StreamName = `Waterway Name`,
         Lat = `Div LAT`,
         Lon = `Div LONG`,
         Mitigated = `Div struct Rebuild     (Y/N)`,
         YearMitigated = `Div Rebuild Year      (YYYY)`,
         Pass_Before = `Diversion Passability Before`,
         Pass_After = `Diversion Passability After`) %>%
  mutate(BarrierType = 'Diversion',
         Source = 'Trout Unlimited (TU)',
         SourceID = paste0('TU_DIV_', `Div #`),
         Pass_NABD = NA,
         Pass_After = as.numeric(Pass_After)) %>%
  mutate(YearMitigated = as.numeric(YearMitigated)) %>%
  select(BarrierName, BarrierType, StreamName, Source, SourceID,
         Lat, Lon,
         Mitigated, YearMitigated, Pass_NABD, Pass_Before, Pass_After) %>%
  as.data.frame

# ---------------------------------------------------------------------------- #

# Combine and export datasets
data.barriers <- do.call(rbind, list(data.nabd.join, data.tu.culverts, data.tu.diversions))

# Initialize output
data.results <- list()
data.results[['Data_Barriers']] <- data.barriers

# ---------------------------------------------------------------------------- #

# Declare working directory
pwd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(pwd)

# Write output
export(data.results, file = 'Data_Results.xlsx')

# Declare working directory
pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

# ---------------------------------------------------------------------

# --------------------------------------------------------------------- #
# 02. Stream Network Data (currently only Bear Lake)
# ---------------------------------------------------------------------

# Summary: Combine NABD and TU barrier data into single dataset for analysis.
# NABD = National Aquatic Barrier Dataset (https://www.aquaticbarriers.org)
# NOTE: Currently preliminary TU dataset that only includes Bear Lake barriers

# Declare data
file.data.network <- paste0(getwd(), '/Spatial/CONN_Network_BearLake.csv')
file.data.results <- paste0(getwd(), '/Data_Results.xlsx')

# Load data
data.network<- read.csv(file.data.network, header = TRUE)
data.results <- rio::import_list(file = file.data.results)

# ---------------------------------------------------------------------------- #

# Initialize output
data.results[['Data_Network']] <- data.network

# ---------------------------------------------------------------------------- #

# Declare working directory
pwd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(pwd)

# Write output
export(data.results, file = 'Data_Results.xlsx')

# Declare working directory
pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

# ---------------------------------------------------------------------

# --------------------------------------------------------------------- #
# 03. Connectivity analysis (currently only Bear Lake)
# ---------------------------------------------------------------------

# NOTE: This step must be run prior to any connectivity analysis to initialize the graph object

# Summary: Load stream network, convert to igraph, and calculate no-barrier
#          DCI for validation.

# Declare data
file.data.results <- paste0(getwd(), '/Data_Results.xlsx')

# Load data
data.results <- rio::import_list(file = file.data.results)

# ---------------------------------------------------------------------------- #
# Generate and validate stream network iGraph for connectivity

# Declare stream network
data.network <- data.results[['Data_Network']]

# Add Field: UID (unique identifers for stream network nodes)
# Generate sequence of random numbers
# NOTE: Not sure I need this, the From_Node (≈ node id) is probably sufficient
set.seed(22)
# uid.seq <- str_pad(sample(999999, size = nrow(data.network)), width = 6, pad = '0')
data.network <- data.network %>%
  mutate(UID = paste0('UID_', str_pad(sample(x = 999999, size = n()), width = 6, pad = '0'))) %>%
  as.data.frame

# Add Field: Barrier_Expected (whether node is a dam/diversion/culvert or junction)
data.network <- data.network %>%
  mutate(Barrier_Expected = ifelse(is.na(BarrierType), 'Junction', BarrierType)) %>%
  rename(Length_KM = Length_km) %>%
  as.data.frame

# Declare connectivity fields
graph.fields        <- c('From_Node', 'To_Node', 'Length_KM', 'UID', 'Barrier_Expected') # Fields to build graph with
attributes.edgeType <- 'Barrier_Expected' # String corresponding to barrier type. 
attributes.edge     <- c('UID', 'Barrier_Expected') # Strings corresponding to edge (barrier) attributes.
attributes.node     <- c('From_Node', 'Length_KM') # String corresponding to node (stream segment) attributes. A field to weight connectivity indices is required, commonly assessed as length or HSI.
field.weight        <- 'Length_KM' # String corresponding to node attribute used to weight connectivity indices.
file.graph          <- 'iGraph_Network.png' # String indicating the name of the output file used to check igraph structure.

# Filter data for igraph
# NOTE: Input data represents the edge list
data.graph <- data.network %>% select({{graph.fields}})

# Check network structure
data.check <- network_check(inData = data.graph,
                            From_field = From_Node,
                            To_field = To_Node)

# Set outlet node as the 'From_Node' on the terminal reach
outlet <- data.check$Terminal_Reaches$From_Node

# Generate igraph object
graph.stream <- generate_attributed_igraph(inData          = data.graph,
                                           From_field      = From_Node,
                                           To_field        = To_Node,
                                           EdgeType_field  = {{attributes.edgeType}},
                                           Edge_attributes = {{attributes.edge}},
                                           Node_attributes = {{attributes.node}},
                                           Outlet_node     = outlet,
                                           graphFile       = file.graph)

# Validate igraph object - All pass_u and pass_d are set to 1 (fully passable),
#                          so DCI should equal 1 (i.e. no fragementation)
index_calculation(graph = graph.stream,
                  weight = field.weight,
                  B_ij_flag = FALSE,
                  dir_fragmentation_type = 'symmetric')

# ---------------------------------------------------------------------------- #
# Explore iGraph object - Commands for viewing edge/vertex attributes

# # View iGraph edge and vertex attribute fields
# list.vertex.attributes(graph.stream)
# list.edge.attributes(graph.stream)
# 
# # View iGraph edge and vertex attribute fields
# get.vertex.attribute(graph.stream)
# get.edge.attribute(graph.stream)
# 
# # View unique or specific values
# unique(E(graph.stream)$type)

# ---------------------------------------------------------------------------- #
# Barrier passability scenarios for connectivity analysis

# Declare barrier data
data.barriers <- data.network %>% 
  filter(BarrierType != 'Junction') %>%
  mutate(Mitigated = ifelse(Pass_Before == Pass_After, 'No', 'Yes'),
         YearMitigated = ifelse(Pass_Before == Pass_After, NA, YearMitigated)) %>%
  mutate(Pass_Before = ifelse(UID == 'UID_036254', Pass_After, Pass_Before)) %>%
  mutate(YearMitigated = ifelse(SourceID == 'TU_DIV_SW-01', 2005, YearMitigated)) %>% # Manually add date for Swan Creek barrier removal (listed in lit as completed sometime in the mid-2000s, ex. Heller et. al., 2024)
  select(From_Node, UID, YearMitigated, Pass_Before, Pass_After) %>%
  as.data.frame

# Determine barrier removal timestep for analysis
year.rmv <- unique(data.barriers %>% filter(!is.na(YearMitigated)) %>% select(YearMitigated)) %>%
  dplyr::arrange(YearMitigated)

# Generate list of barrier passability scenarios (i.e. changes to pass through time)
scenarios.barrier <- list()

# Base scenario with all barriers
scenarios.barrier[['SCN_0000_AllBarriers']] <- data.barriers %>% mutate(Pass_R = Pass_Before) %>% as.data.frame

# Create list of barrier scenarios
for(i in 1:nrow(year.rmv)){
  scenario.id <- paste0('SCN_', year.rmv[['YearMitigated']][[i]]) 
  scenarios.barrier[[scenario.id]] <- data.barriers %>%
    mutate(Pass_R = ifelse(is.na(YearMitigated) | YearMitigated > year.rmv[['YearMitigated']][[i]],
                           Pass_Before, Pass_After)) %>%
    as.data.frame
}

# Alternative scenario for North Eden w/o Diversion Structure installed
# NOTE: NE Creek diversion is a seasonally-installed earthwork dam that creates an impassable barrier
#       but doesn't require infrastructure work for removal
# Lookup TU_DIV_NE-01: View(data.network %>% filter(BarrierType != 'Junction'))
scenarios.barrier[['SCN_2025_Alt']] <- scenarios.barrier[['SCN_2025']] %>%
  mutate(Pass_R = ifelse(UID == 'UID_074584', 1, Pass_R)) %>%
  as.data.frame


# ---------------------------------------------------------------------------- #
# Calculate stream network connectivity

# Declare inputs
connectivity.field.pass   <- 'Pass_R'
connectivity.field.weight <- 'Length_KM'

# Initialize output
connectivity.out <- data.frame(Scenario = character(0),
                               DCI_symm = numeric(0),
                               DCI_asym = numeric(0))

# Calculate DCI connectivity for all barrier passability scenarios in list
for(i in 1:length(scenarios.barrier)){
  
  # Declare scenario
  scenario.id <- names(scenarios.barrier)[[i]]
  
  # Join scenario barrier passability data to network
  graph.barriers <- join_edge_attributes(inGraph = graph.stream,
                                         inData = scenarios.barrier[[i]],
                                         field.pass = 'Pass_R')
  
  # Calculate DCI
  data.connectivity <- calculate_dci(inGraph = graph.barriers,
                                     scenario.id = scenario.id,
                                     field.pass = 'Pass_R',
                                     field.weight = 'Length_KM')
  
  # Add data to output
  connectivity.out <- rbind(connectivity.out, data.connectivity)
  
}

# Lookup barriers by year for export
barriers.rmv <- data.network %>%
  mutate(YearMitigated = ifelse(SourceID == 'TU_DIV_SW-01', 2005, YearMitigated)) %>%
  filter(! Barrier_Expected %in% c('Junction', 'Terminus')) %>%
  filter(Pass_Before != Pass_After) %>%
  select(YearMitigated, SourceID, UID, BarrierName, Pass_Before, Pass_After) %>%
  arrange(YearMitigated) %>%
  as.data.frame

# Format data for export
connectivity.data <- connectivity.out %>%
  mutate(YearMitigated = as.numeric(substr(Scenario,5,8))) %>% 
  left_join(barriers.rmv, by = c('YearMitigated')) %>%
  as.data.frame

# ---------------------------------------------------------------------------- #

# Initialize output
data.results[['Connectivity_BearLake']] <- connectivity.data

# ---------------------------------------------------------------------------- #

# Declare working directory
pwd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(pwd)

# Write output
export(data.results, file = 'Data_Results.xlsx')

# Declare working directory
pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

# ---------------------------------------------------------------------

# --------------------------------------------------------------------- #
# 05. FIGURE - Connectivity (currently only Bear Lake)
# ---------------------------------------------------------------------

# Summary: Connectivity diagram for Bear Lake

# Declare data
file.data.results <- paste0(getwd(), '/Data_Results.xlsx')

# Load data
data.results <- rio::import_list(file = file.data.results)

# ---------------------------------------------------------------------------- #
# DATA: Connectivity

# Declare data
data.dci <- data.results[['Connectivity_BearLake']]

# Generate time series for plotting
data.dci.ts <- data.frame(Year = c(seq(1990, 2035, by = 1))) %>%
  # Join main DCI estimates
  left_join(data.dci %>%
              filter(Scenario != 'SCN_2025_Alt') %>%
              select(YearMitigated, DCI_symm) %>%
              rename('Year' = YearMitigated),
            by = 'Year') %>%
  left_join(data.dci %>%
              filter(Scenario != 'SCN_2025') %>%
              select(YearMitigated, DCI_symm) %>%
              rename('Year' = YearMitigated,
                     'DCI_symm_alt' = DCI_symm),
            by = 'Year') %>%
  mutate(DCI_symm = ifelse(Year < data.dci$YearMitigated[2], min(data.dci$DCI_symm), DCI_symm),
         DCI_symm_alt = ifelse(Year < data.dci$YearMitigated[2], min(data.dci$DCI_symm), DCI_symm_alt)) %>%
  fill(c(DCI_symm, DCI_symm_alt), .direction = 'down') %>%
  as.data.frame 

# ---------------------------------------------------------------------------- #
# DATA: Years/Removals

# Declare data
data.network <- data.results[['Data_Network']]

# Lookup barriers by year for export
barriers.rmv <- data.network %>%
  mutate(YearMitigated = ifelse(SourceID == 'TU_DIV_SW-01', 2005, YearMitigated)) %>%
  # filter(! Barrier_Expected %in% c('Junction', 'Terminus')) %>%
  filter(Pass_Before != Pass_After) %>%
  select(YearMitigated, SourceID, BarrierName, Pass_Before, Pass_After) %>%
  arrange(YearMitigated) %>%
  mutate(text.fig = c('Bear Lake Fish Ladder (1995)',
                      'St. Charles Crk (1999)',
                      'Swan Crk (2005)',
                      'Fish Haven Crk (2009)',
                      'Fish Haven Crk (2014)',
                      'North Eden Crk (2025)')) %>%
  mutate(x.pos = c(YearMitigated - 0.75)) %>%
  left_join(data.dci %>% filter(Scenario != 'SCN_2025_Alt') %>% select(YearMitigated, DCI_symm), 
            by = 'YearMitigated') %>%
  filter(YearMitigated >= 1999) %>%
  as.data.frame


# ---------------------------------------------------------------------------- #
# AESTHETICS

lwd.lines <- 0.5
lwd.borders <- 0.25
size.text.legend <- 10
size.text.axis <- 10
size.text.axis.title <- 11

# ---------------------------------------------------------------------------- #
# PLOTTING
plot.out <- 
  ggplot() +
  
  geom_area(data = data.dci.ts, aes(x =  Year, y = DCI_symm),
            fill = 'grey95') +
  
  geom_segment(data = barriers.rmv,
               aes(x = YearMitigated, y = -Inf, yend = DCI_symm),
               linetype = 'dotted') +
  
  geom_line(data = data.dci.ts, aes(x =  Year, y = DCI_symm), linewidth = 1) +
  geom_line(data = data.dci.ts %>%
              filter(Year >= 2024), 
            aes(x =  Year, y = DCI_symm_alt), linetype = 'dashed') +
  
  geom_text(data = barriers.rmv, aes(x = x.pos, y = 0.01, label = text.fig),
            size = 8/.pt, angle = 90, hjust =  0) +
  
  annotate(geom = 'text', label = 'Connectivity with\nNorth Eden Crk\npush-up dam', 
           x = 2027.5, y = 0.245, 
           size = 8/.pt, ) + 
  annotate(geom = 'text', label = 'Connectivity without\nNorth Eden Crk\npush-up dam', 
           x = 2027.0, y = 0.475, 
           size = 8/.pt, ) +
  
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(fill = NA, linewidth = lwd.borders),
        axis.text = element_text(size = size.text.axis, color = 'black'),
        axis.title = element_text(size = size.text.axis.title),
        axis.title.x = element_text(vjust = 0),
        # axis.title.y = element_text(vjust = + 3),
        axis.ticks = element_line(linewidth = lwd.borders), 
        # legend.position = 'none', 
        legend.position.inside = 'inside',
        legend.position = c(0.83, 0.92),
        legend.background = element_blank(),
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.text = element_text(size = size.text.legend),
        legend.title = element_text(size = size.text.legend)) +
  
  scale_x_continuous(limits = c(1995, 2030), expand = c(0,0), breaks = c(seq(0, 2025, by = 5))) +
  scale_y_continuous(limits = c(0,0.55), expand = c(0,0), breaks = c(seq(0,0.5, by = 0.1)),
                     labels = c(seq(0,50,by=10)),
                     sec.axis = sec_axis(~ . *100,
                                         name = 'Connectivity (% of total stream length)')) +
  labs(x = 'Year and Mitigation Project', y = 'Connectivity (% of total stream length)')

# ---------------------------------------------------------------------------- #

# Declare working directory
pwd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(pwd)

# Write output
ggsave(filename = 'Figure_Connectivity.png', plot.out,
       width = 18, height = 11, units = 'cm',
       dpi = 600, bg = 'white')

# Declare working directory
pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

# ---------------------------------------------------------------------

# --------------------------------------------------------------------- #
# 05. Stream Length Calculations (currently only Bear Lake)
# ---------------------------------------------------------------------

# Summary: Calculate connected stream length at Bear Lake and length 
#          of connectivity improvements at each barrier removal.

# Declare data
file.data.results <- paste0(getwd(), '/Data_Results.xlsx')
file.data.bearlake <- paste0(getwd(), '/Spatial/STLN_Network_BearLake_LENGTH.csv')

# Load data
data.results <- rio::import_list(file = file.data.results)
data.bearlake <- read.csv(file.data.bearlake, header = TRUE)

# ---------------------------------------------------------------------------- #
# Calculate stream lengths

# Pre-process data for length calculations
data.bearlake <- data.bearlake %>%
  mutate(Barrier_DNSTR = ifelse(is.na(Barrier_DNSTR) | Barrier_DNSTR == "", 'Bear_Lake', Barrier_DNSTR)) %>%
  select(Barrier_DNSTR, length_km) %>%
  as.data.frame()

# Calculate stream length by downstream barrier
data.bearlake.length <- data.bearlake %>%
  group_by(Barrier_DNSTR) %>%
  dplyr::summarize(streamlen_km = sum(length_km, na.rm = TRUE)) %>%
  mutate(streamlen_km = round(streamlen_km, digits = 3)) %>%
  as.data.frame

# ---------------------------------------------------------------------------- #

# Declare output
data.results[['StrLen_BearLake']] <- data.bearlake
data.results[['Stream_Lengths']] <- data.bearlake.length

# Declare working directory
pwd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(pwd)

# Write output
export(data.results, file = 'Data_Results.xlsx')

# Declare working directory
pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

# ---------------------------------------------------------------------
