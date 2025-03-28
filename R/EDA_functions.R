freqRespOnSubscribed <- function(train, x, nameVariable, ord = FALSE) {
  x <- enquo(x)
  
  if (is.logical(ord) && ord) {
    stop("Se `ord` è TRUE, deve essere un vettore di livelli esplicito.")
  }
  
  if (is.character(ord)) {
    train[[quo_name(x)]] <- factor(train[[quo_name(x)]], levels = ord, ordered = TRUE)
    
    funct <- ggplot(as.data.frame(table(train[[quo_name(x)]]) / sum(table(train[[quo_name(x)]])) * 100),
                    aes(x = Var1, y = Freq, fill = Var1))
  } else {
    funct <- ggplot(as.data.frame(table(train[[quo_name(x)]]) / sum(table(train[[quo_name(x)]])) * 100), 
                    aes(x = reorder(Var1, Freq), y = Freq, fill = Var1))
  }
  
  FreqGraph <- funct +
    geom_bar(stat = "identity", color = "gray", fill = "steelblue", alpha = 0.9) +  
    coord_flip() +
    labs(title = paste("Distribution of", nameVariable), x = nameVariable, y = "Percentage (%)") +
    theme_minimal()
  
  if (is.character(ord)) {
    ord_edu <- ord
  } else {
    ord_edu <- train %>% count(!!x) %>% arrange(n) %>% pull(!!x)
  }
  
  RespGraph <- ggplot(train, aes(x = factor(!!x, levels = ord_edu), fill = factor(subscribed))) +
    geom_bar(position = "fill") +
    coord_flip() +
    labs(title = "Proportion by Subscribed", x = nameVariable, y = "Proportion") +
    scale_fill_discrete(name = "Subscribed") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return((FreqGraph / RespGraph) + plot_layout(axis_titles = 'collect'))
}

propResp <- function(train, x, nameVariable, binW, binsHistogram = NULL) {
  x <- enquo(x)  # Permette di usare x come variabile
  
  Resp <- ggplot(train, aes(x = !!x)) +
    geom_histogram(binwidth = binW, position = "fill", aes(fill = factor(subscribed))) +
    scale_fill_discrete(name = "Subscribed") +
    labs(title = "Proportion by subscribed", x = nameVariable, y = "Proportion") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    theme_minimal()
  
  if (!is.null(binsHistogram)) {  # Controlla se è stato passato un valore valido
    Hist <- ggplot(train, aes(x = !!x)) +
      geom_histogram(aes(y = ..density..), bins = binsHistogram, color = "gray", fill = "steelblue", alpha = 0.9) +
      geom_rug() +
      geom_density(color = "red", size = 1) +
      labs(title = paste("Distribution of", nameVariable), x = nameVariable, y = "Density") +
      theme_minimal()
    
    return(Hist / Resp + plot_layout(axis_titles = 'collect'))
  } else {
    return(Resp)
  }
}