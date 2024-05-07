\name{alabaster.base News}
\title{alabaster.base News}
\encoding{UTF-8}

\section{Version 1.2.0}{\itemize{
\item Read-only operations now use the relevant REST endpoints rather than directly contacting the S3 API.
This avoids a dependency on \pkg{paws.storage} and fixes some strange issues with the request signing.
}}

\section{Version 1.0.0}{\itemize{
\item New \pkg{gypsum} package to interact with the REST API of the same name.
}}
