# Install and load

## Showing install and load this package
devtools::install_github('herdiantrisufriyana/alignontology')
library(alignontology)

## Load packages
library(tidyverse)
library(igraph)


# Load simulated data

## Create input example, echo=TRUE}
input=input_example()

## Computed ontology
set.seed(33)
input$computed %>%
  graph_from_data_frame(directed=TRUE) %>%
  plot.igraph(layout=layout_as_tree(.,mode='in'))

## Reference ontology
set.seed(33)
input$reference %>%
  graph_from_data_frame(directed=TRUE) %>%
  plot.igraph(layout=layout_as_tree(.,mode='in'))


# Ontology alignment

## Ontology alignment
ao=alignontology(input$computed,input$reference)

## Prepare ontology alignment for visualization
set.seed(33)
ao_tab=
  input$computed %>%
  left_join(
    ao %>%
      select(computed,reference) %>%
      rename(source=computed)
    ,by='source'
  ) %>%
  mutate(source=ifelse(is.na(reference),source,reference)) %>%
  select(-reference) %>%
  left_join(
    ao %>%
      select(computed,reference) %>%
      rename(target=computed)
    ,by='target'
  ) %>%
  mutate(target=ifelse(is.na(reference),target,reference)) %>%
  select(-reference)

g=ao_tab %>%
  graph_from_data_frame(directed=TRUE)

V(g)$color=
  input$computed %>%
  graph_from_data_frame(directed=TRUE) %>%
  V() %>%
  .$name %>%
  data.frame(computed=.) %>%
  left_join(ao,by='computed') %>%
  select(computed,reference) %>%
  mutate(reference=ifelse(is.na(reference),'orange','green')) %>%
  pull(reference)

ao_tab2=
  input$reference %>%
  left_join(
    ao %>%
      select(computed,reference) %>%
      rename(source=reference)
    ,by='source'
  ) %>%
  mutate(source=ifelse(is.na(computed),source,computed)) %>%
  select(-computed) %>%
  left_join(
    ao %>%
      select(computed,reference) %>%
      rename(target=reference)
    ,by='target'
  ) %>%
  mutate(target=ifelse(is.na(computed),target,computed)) %>%
  select(-computed)

g2=ao_tab2 %>%
  graph_from_data_frame(directed=TRUE)

V(g2)$color=
  input$reference %>%
  graph_from_data_frame(directed=TRUE) %>%
  V() %>%
  .$name %>%
  data.frame(reference=.) %>%
  left_join(ao,by='reference') %>%
  select(reference,computed) %>%
  mutate(computed=ifelse(is.na(computed),'orange','green')) %>%
  pull(computed)

## Ontology alignment
par(mfrow=c(2,2))

set.seed(33)
input$computed %>%
  graph_from_data_frame(directed=TRUE) %>%
  plot.igraph(layout=layout_as_tree(.,mode='in'),main='Computed ontology')

g %>%
  plot.igraph(
    layout=layout_as_tree(.,mode='in')
    ,main='Aligned computed ontology'
  )

set.seed(33)
g2 %>%
  plot.igraph(
    layout=layout_as_tree(.,mode='in')
    ,main='Aligned reference ontology'
  )

set.seed(33)
input$reference %>%
  graph_from_data_frame(directed=TRUE) %>%
  plot.igraph(layout=layout_as_tree(.,mode='in'),main='Reference ontoloy')
