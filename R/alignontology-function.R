#' Ontology alignment
#'
#' This function runs alignment algorithm in Network-Extracted Ontology (NexO).
#'
#' @param computed Computed ontology, a data frame with rows for ontologies and
#' three columns for source, target, and relation. This is ontology that will
#' be aligned to reference. Please see details below.
#' @param reference Reference ontology, a data frame with rows for ontologies
#' and three columns for source, target, and relation. This is ontology which
#' another is aligned to. Please see details below.
#' @param min_similarity A numeric from 0 to 1. A score that should be
#' surpassed for mapping between two ontologies. A value of 0.05 works well.
#' Please see https://pubmed.ncbi.nlm.nih.gov/23242164/.
#' @param semantic_verification A character of a setting that will switch the
#' way looking for inconsistencies between pairs of mappings.  Please see
#' details below.
#' @param allow_multimap A logical whether the same term is allowed to be
#' mapped twice (only in the case that two possible mappings are
#' mathematically equivalent).
#' @param features_as_terms A logical whether features are included as terms.
#' @param feature_name A character to annotate feature (source)-ontology
#' (target) relation in both of the input ontologies.
#' @param n_rand_align An integer of number of random alignments to perform for
#' calculating false discovery rate (FDR) per pair of ontologies.
#' @param effect_size Effect size, a numeric default to 0.  Otherwise, this
#' will decrease actual score by effect size to ensure effect is not only
#' statistically significant but also large.
#' @param pvals A logical whether using p-values or FDRs instead of minimum
#' similarity. This means lower value is better.
#'
#' @details Feature (source)-ontology (target) relation is annotated as
#' 'feature', while ontology-ontology relation is annotated as 'is_a'. To
#' differentiate between feature and ontology names, a prefix followed by ':'
#' precedes an ontology name. The prefix should be different for computed and
#' reference ontology. All columns are characters.
#'
#' For semantic verification, use \code{criss_cross} for parent-child
#' criss-cross mappings, i.e. cases where e1<e2 and e1'<e2' but we have the
#' mappings (e1,e2') and (e2,e1'). Use \code{strict_hierarchy} for considering
#' it an inconsistency if we have the mappings (e1,e1') and (e2,e2') where and
#' either e1<e2 or e1'<e2' but not both. This is more strict than criss_cross.
#' This does not check if any of e1, e2, e1', or e2' is a feature, as this
#' lead to an overly strict requirement that two concepts containing identical
#' features to be mapped to each other. Use \code{none} to do neither of the
#' methods. It still looks for double mapping inconsistencies. Use 'sib_sib'
#' to allow two sibling terms in one ontology to be mapped to parent-child
#' terms in the second ontology.
#'
#' @return Ontology alignment, a data frame with rows for ontologies and
#' five columns for computed ontology, reference ontology, similarity, FDR, and
#' terminal nodes.
#'
#' @keywords alignontology
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' input=input_example()
#'
#' ## Ontology alignment
#' ontology=alignontology(input$computed,input$reference)

alignontology=function(computed
                       ,reference
                       ,min_similarity=0.05
                       ,semantic_verification='criss_cross'
                       ,allow_multimap=F
                       ,features_as_terms=T
                       ,feature_name='feature'
                       ,n_rand_align=100
                       ,effect_size=0
                       ,pvals=T){

  allow_multimap=as.integer(allow_multimap)
  features_as_terms=as.integer(features_as_terms)
  pvals=as.integer(pvals)

  # Clone forked alignOntology C++ program from GitHub
  system(paste(c(
    'bash -c'
    ,'"git clone https://github.com/herdiantrisufriyana/alignontology_0.1"'
  ),collapse=' '))

  # Format and write ontology to txt for input
  computed %>%
    select(target,source,relation) %>%
    write_tsv(paste0('alignontology_0.1/computed.txt',collapse=''),col_names=F)

  reference %>%
    select(target,source,relation) %>%
    write_tsv(paste0('alignontology_0.1/reference.txt',collapse=''),col_names=F)

  # Align computed ontology to the reference
  system(paste(c(
    'bash -c'
    ,'"alignontology_0.1/alignOntology'
    ,'alignontology_0.1/computed.txt'
    ,'alignontology_0.1/reference.txt'
    ,min_similarity
    ,semantic_verification
    ,allow_multimap
    ,features_as_terms
    ,feature_name
    ,'>'
    ,'alignontology_0.1/alignment.out"'
  ),collapse=' '))

  # Calculate FDRs for alignment
  system(paste(c(
    'bash -c'
    ,'"alignontology_0.1/calculateFDRsForAlignment'
    ,'alignontology_0.1/computed.txt '
    ,'alignontology_0.1/alignment.out'
    ,'alignontology_0.1/'
    ,n_rand_align
    ,effect_size
    ,pvals
    ,'>'
    ,'alignontology_0.1/FDRs.out"'
  ),collapse=' '))

  # Read alignOntology results
  ao=
    suppressWarnings(read_tsv(
      'alignontology_0.1/FDRs.out'
      ,col_types=cols(.default=col_character())
    )) %>%
    setNames('column') %>%
    .[!str_detect(.$column,'#'),1:5] %>%
    setNames(c('computed','reference','similarity','fdr','termin_nodes')) %>%
    mutate_at(1:3,as.character) %>%
    mutate_at(4:5,as.numeric)

  # Remove alignOntology C++ program
  system('bash -c "rm -r alignontology_0.1"')

  # Return
  ao
}
