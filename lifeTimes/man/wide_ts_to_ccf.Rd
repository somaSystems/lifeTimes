\name{wide_ts_to_ccf}
\alias{paired_ccf_generator}
\title{paired CCF generator}
\usage{
pairedCCFs()
}
\description{
Inputs a wide list of time series, with column names containing:
i) a unique cell ID
ii) a measured feature
iii) a labelled compartment (eg. cell or nucleus);
and outputs a dataframe of correlations between the labelled compartments at different time lags.
}
\examples{
pairedCCFs()
}
