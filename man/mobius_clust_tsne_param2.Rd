% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mobius_clust_tsne_param2}
\alias{mobius_clust_tsne_param2}
\title{tSNE embedding for mobius_clust_data dataset which with noise dimensions
tSNE parameters set to perplexity: 30.}
\format{
## `mobius_clust_tsne_param2`
A data frame with 500 rows and 2 columns:
\describe{
  \item{emb1}{Numeric, first tSNE 2D embeddings.}
  \item{emb2}{Numeric, second tSNE 2D embeddings.}
}
}
\source{
This dataset is generated for illustrative purposes.
}
\usage{
data(mobius_clust_tsne_param2)
}
\description{
The `mobius_clust_tsne_param2` dataset contains the tSNE (t-distributed Stochastic
Neighbor Embedding) embeddings of a five-dimensional mobius_clust_data.
Each data point is represented by two tSNE coordinates (emb1
and emb2).
}
\examples{
# Load the mobius_clust_tsne_param2 dataset
data(mobius_clust_tsne_param2)

# Display the first few rows of the dataset
head(mobius_clust_tsne_param2)

}
\keyword{datasets}
