\name{likelihoods}
\alias{likelihoods}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Likelihood Table generation for oods estimation.
}
\description{
Creates a table consisting of likelihood values. Each row label corresponds to the value treated as belief, and each column name corresponds to the observation to be considered for evaluating true and false positives.
}
\usage{
likelihoods(data, belief, observation)
}

\arguments{
  \item{data}{
an R dataframe. Must have more than one rows.
}

\item{belief}{
a string indicating the column in `data` dataframe to be treated as belief.
}

\item{observation}{
a string indicating the column in `data` dataframe to be treated as observation.
}

}

\author{
Rishikesh Gawade
}
\examples{
}