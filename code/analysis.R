# Info -------------------------------------------------------------------------
# Analyze the results of a systematic review of definitions and 
# characterizations of text
# 
# Authors: Davi Alves Oliveira and Lêda Maria Braga Tomitch and Hernane Borges 
# de Barros Pereira
#
# Last update: April 26, 2024

# Initial settings -------------------------------------------------------------
# Install the necessary packages
# install.packages('tidyverse')
# install.packages('dendextend')
# install.packages('textstem')
# install.packages('igraph')

# Load required packages
library(tidyverse)
library(dendextend)
library(textstem)
library(igraph)

# Read the results
results = read_delim('data/resultados_segundo_filtro.csv', delim = ';') %>%
  as_tibble() %>%
  mutate(Portal = Portal %>% factor(levels = c('ScienceDirect',
                                               'Capes Portal',
                                               'SciELO')))

# Summarize results
results %>%
  group_by(Portal) %>%
  filter(Selecionado == 'Sim') %>%
  count()

# Read the selected results
results_selected = read_delim('data/resultados_selecionados.csv') %>%
  as_tibble()

# Analysis of references mentioned in the definitions --------------------------
# Count the referenced texts
references = results_selected %>%
  separate_rows(`Referências das definições`, sep = ';') %>%
  mutate(`Referências das definições` = `Referências das definições` %>% 
           str_trim()) %>%
  group_by(`Referências das definições`) %>%
  count() %>%
  arrange(-n)

# Cluster analysis of the definitions ------------------------------------------
characteristics = read_delim('data/resultados_definicoes_caracteristicas.csv', 
                  delim = ';')

# Calculate euclidean distances
matriz_distancia = dist(characteristics[,2:ncol(characteristics)], 
                        method='euclidean')

# Perform the hierarchical clustering analysis using the ward.D2 method
DenDados = hclust(matriz_distancia, method="ward.D2")

# Configure dendrogram to highlight three groups
dend = as.dendrogram(rev(DenDados)) %>%
  set('branches_k_color', 
      k = 3, 
      h = 3, 
      value = c('#e31a1c','#33a02c', '#1f78b4')) %>%
  set('labels_cex', .86) %>%
  set('branches_lwd', .5)

# Set labels
labels(dend) = characteristics$Trabalho[order.dendrogram(dend)]

# Save as ggplot object
dendgg = as.ggdend(dend)
dendplot = ggplot(dendgg, horiz = T)

# Configure font family
dendplot$layers[[3]]$aes_params$family = 'serif'

# Save dendrogram as a PDF file
ggsave('results/Figure3.pdf',
       dendplot,
       'pdf',
       width = 16,
       height = 24,
       units = 'cm')

# Semantic Network Analysis ----------------------------------------------------
lemmas = results_selected %>%
  separate_rows(Paráfrases, sep = '\\. ') %>%
  select(Paráfrases) %>%
  mutate(Paráfrases = Paráfrases %>%
           str_to_lower() %>%
           str_remove_all('[.–,\\(\\):;]') %>%
           str_trim(),
         clique = row_number()) %>%
  separate_rows(Paráfrases, sep = ' ') %>%
  filter(Paráfrases != '') %>%
  mutate(lemma = lemmatize_words(Paráfrases)) %>%
  select(clique, lemma)

# Check most frequent words to create a list of stopwords
lemmas %>%
  group_by(lemma) %>%
  count() %>%
  arrange(-n) %>%
  View()

# List of stopwords
stopwords = c(
  'a', 'be', 'of', 'the', 'and', 'in', 'as', 'to', 'which', 'that', 'or', 'by', 
  'have', 'any', 'it', 'can', 'its', 'with', 'from', 'between', 'on', 'both', 
  'may', 'not', 'within', 'at', 'for', 'if', 'one', 'out', 'some', 'such', 
  'their', 'this', 'what', 'after', 'so', 'there', 'therefore', 'through', 'up', 
  'way', 'whether', 'will', 'about', 'all', 'along', 'always', 'but', 'down', 
  'each', 'either', 'far', 'here', 'into', 'itself', 'only', 'then', 'they', 
  'under', 'whatever', 'when', 'without', 'would')

# List of vertices
vertex_list = lemmas %>%
  distinct() %>%
  filter(!lemma %in% stopwords) %>%
  group_by(lemma) %>%
  summarize(cliques = paste(clique, collapse = ' '))

# List of edges
edge_list = lemmas %>%
  distinct() %>%
  filter(!lemma %in% stopwords) %>%
  rename(Source = lemma) %>%
  mutate(Target = Source) %>%
  group_by(clique) %>%
  expand(Source, Target) %>%
  mutate(id = mapply(function(s, t) {
    paste(sort(c(s, t)), collapse = ' ')
  }, Source, Target)) %>%
  filter(Source != Target) %>%
  distinct(id, .keep_all = T) %>%
  ungroup() %>%
  select(Source, Target)

# Create network as igraph object
network = edge_list %>%
  graph_from_data_frame(directed = F, vertices = vertex_list) %>%
  simplify()

# Set network ids as vertex names
V(network)$id = V(network)$name

# Calculate average degree
k_mean = degree(network) %>% mean()
# Calculate standard deviation of the degrees
k_sd = degree(network) %>% sd()

# Calculate cutpoint for hub identification
hub_cutpoint = k_mean + (2 * k_sd)

V(network)$hub = degree(network) > hub_cutpoint

# Generate list of vertices and edges compatible with Gephi
vertices_edges = as_data_frame(network, 'both')

# Save list of vertices as CSV
vertices_edges$vertices %>% 
  select(id, hub) %>%
  rename(Id = id, Hub = hub) %>%
  write_excel_csv('results/vertex_list.csv')

# Save list of edges as CSV
vertices_edges$edges %>% 
  rename(Source = from, Target = to) %>%
  mutate(Type = 'Undirected') %>%
  write_excel_csv('results/edge_list.csv')
