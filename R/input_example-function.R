#' Make an input example for alignontology package
#'
#' This function create an input example for alignontology package.
#'
#' @return output A list of inputs: 1) computed; and 2) reference. Both are
#' data frames with rows for ontologies and three columns for source, target,
#' and relation. Feature (source)-ontology (target) relation is annotated as
#' 'feature', while ontology-ontology relation is annotated as 'is_a'. To
#' differentiate between feature and ontology names, a prefix followed by ':'
#' precedes an ontology name. The prefix should be different for computed and
#' reference ontology. All columns are characters.
#'
#' @keywords example data
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' input=input_example()

input_example=function(){
  ## Set an empty list of inputs and random seed
  input=list()
  set.seed(33)

  ## Create example of instance-feature data frame
  input$value=
    rnorm(3000*17) %>%
    matrix(3000,17,T,list(paste0('I',1:3000),paste0('F',1:17))) %>%
    as.data.frame()

  ## Create example of feature similarity matrix using Pearson correlation
  set.seed(33)
  input$similarity=
    input$value %>%
    .[,sample(seq(ncol(.)),6,F)] %>%
    cor(method='pearson')

  input$similarity2=
    input$value %>%
    cor(method='pearson')

  ## Create example of ontology
  ## The similarity is recalculated to express dissimilarity
  ## This is required for hierarchical clustering
  input$hierarchy=
    ((1-input$similarity)/2) %>%
    as.dist() %>%
    hclust(method='complete')

  input$hierarchy2=
    ((1-input$similarity2)/2) %>%
    as.dist() %>%
    hclust(method='single')

  ## A function to convert a hierarchy object into an ontology data frame
  ontology_df=function(hierarchy){
    d=hierarchy$merge %>%
      as.data.frame() %>%
      mutate(target=abs(V1)) %>%
      mutate_all(function(x)
        ifelse(x<0,hierarchy$labels[abs(x)],paste0('ONT:',x))
      )

    e=filter(d,target==V1)
    f=d %>%
      filter(target!=V1) %>%
      pull(target) %>%
      str_remove_all('ONT\\:') %>%
      as.integer() %>%
      max()

    g=data.frame(target=unique(e$target)) %>%
      mutate(target2=paste0('ONT:',seq(f+1,f+nrow(.)))) %>%
      right_join(e,by='target') %>%
      mutate(target2=ifelse(is.na(target2),target,target2)) %>%
      select(-target)

    o=d %>%
      left_join(g,by=c('V1','V2')) %>%
      mutate(target=ifelse(is.na(target2),target,target2)) %>%
      select(-target2) %>%
      mutate(similarity=1-hierarchy$height) %>%
      gather(key,source,-target,-similarity) %>%
      select(-key) %>%
      mutate(relation=ifelse(str_detect(source,'ONT\\:'),'is_a','feature')) %>%
      arrange(desc(similarity),source) %>%
      select(source,everything())

    if(sum(!o$target%in%o$source)==1){
      q=o
    }else{
      r=o %>%
        filter(!o$target%in%o$source) %>%
        mutate(
          source=target
          ,similarity=NA
          ,relation='is_a'
        ) %>%
        filter(!duplicated(.)) %>%
        mutate(
          target=
            source %>%
            str_remove_all('ONT:') %>%
            as.numeric() %>%
            max()
          ,target=paste0('ONT:',target+1)
        )
      q=rbind(o,r)
    }

    q %>% filter(!(str_detect(source,'ONT:') & !source%in%target))

  }


  ## Create example of ontology data frame
  input$computed=
    ontology_df(input$hierarchy) %>%
    select(-similarity)

  input$reference=
    ontology_df(input$hierarchy2) %>%
    mutate_at(c('source','target'),str_replace_all,'ONT','REF') %>%
    select(-similarity)

  ## Return
  input[c('computed','reference')]
}
