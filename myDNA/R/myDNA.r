#' DNA revcomp
#'
#' computes DNA reverse complement
#' @param input the vector of info to parse
#' @param split the delimiter
#' @keywords split, parse
#' @export
#' @examples 
#' myDNA()


#program to compute DNA sequence qualities

#this program consist of functions that read the nucleotides in a DNA sequence
#it can only work with DNA sequences, NO RNA###
read_nuc <- function(input_seq){
  sequence <- readLines(input_seq, n = -1)
  seq <- strsplit(sequence, split = "")
  seq <- seq[[1]]
  return (seq)
}

#nuc <-read_nuc(seq)
complement <- function(nuc){
  comp <- c()
  for (i in 1:length(nuc)){
    if (nuc[i] == 'A'){
      comp[i] = 'T'
    }
    else if (nuc[i] == 'a'){
      comp[i] = 't'
    }
    else if (nuc[i] == 'T'){
      comp[i] = 'A'
    }
    else if (nuc[i] == 't'){
      comp[i] = 'a'
    }
    else if (nuc[i] == 'G'){
      comp[i] = 'C'
    }
    else if (nuc[i] == 'g'){
      comp[i] = 'c'
    }
    else if (nuc[i] == 'C'){
      comp[i] = 'G'
    }
    else if (nuc[i] == 'c'){
      comp[i] = 'g'
    }
  }
  return(comp)
}

#seq <- complement(nuc)
reverse_comp <- function(seq){
  rev_seq <- rev(seq)
  reverse <- paste(rev_seq, collapse = "")
  return(reverse)
}
