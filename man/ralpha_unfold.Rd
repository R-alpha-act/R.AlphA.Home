% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ralpha_unfold.R
\name{ralpha_unfold}
\alias{ralpha_unfold}
\title{Unfold Code Sections in RStudio}
\usage{
ralpha_unfold()
}
\value{
NULL (invisibly).
This function performs actions only (cursor movement and unfolding)
}
\description{
ralpha_fold() and ralpha_unfold() allow usage of the R.AlphA
code format that keeps long scripts easily readable.

This format is based on identifying code parts with brackets, and an
optional but recommended comment at the end :

\preformatted{
\{
  ...
  code from part 1
  ...
\} # part 1
\{
  ...
\} # part 2
}

then appearing as
\preformatted{
\{...\} # part 1
\{...\} # part 2
}

To stay easy to manipulate, this format requires shortcuts to easily open
or close the different sections.

ralpha_fold() will fold the different code parts and go back to beginning
of current part

ralplha_unfold() will unfold a code part and jump to the next braces when
relevant.

both combined will provide a convenient way to manage what is displayed on
screen, ensuring a constant global overview of the document.

Shortcuts required:
Here are the suggested shortcuts, both for Mac and Windows :
\itemize{
  \item ralpha_fold   : use ctrl+up
  \item ralpha_unfold : use ctrl+down
}
}
