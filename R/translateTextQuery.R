#' Translate a plain-text query to a gypsum.search.clause
#'
#' Translate a plain-text query in user-friendly format to a \link{gypsum.search.clause},
#' equivalent to that created by \code{\link{gsc}} with the corresponding boolean operations.
#'
#' @param query String containing a query in user-friendly format to a \link{gypsum.search.clause}.
#'
#' @return A \link{gypsum.search.clause} representing \code{query}.
#' 
#' @details
#' For text searches, we support a more human-readable syntax for boolean operations in the query.
#' The search string below will look for all metadata documents that match \code{foo} or \code{bar} but not \code{whee}:
#' 
#' \preformatted{
#' (foo OR bar) AND NOT whee
#' }
#' 
#' The \code{AND}, \code{OR} and \code{NOT} (note the all-caps!) are automatically translated to the corresponding search clauses.
#' This can be combined with parentheses to control precedence; otherwise, \code{AND} takes precedence over \code{OR}, and \code{NOT} takes precedence over both.
#' Note that any sequence of adjacent text terms are implicitly AND'd together, so the two expressions below are equivalent:
#' 
#' \preformatted{
#' foo bar whee
#' foo AND bar AND whee
#' } 
#' 
#' Users can prefix any sequence of text terms with the name of a metadata field, to only search for matches within that field of the metadata file.
#' For example:
#' 
#' \preformatted{
#' (title: prostate cancer) AND (genome: GRCh38 OR genome: GRCm38)
#' } 
#' 
#' Note that this does not extend to the \code{AND}, \code{OR} and \code{NOT} keywords,
#' e.g., \code{title:foo OR bar} will not limit the search for \code{bar} to the \code{title} field.
#' 
#' If a \code{\%} wildcard is present in a search term, its corresponding text search clause is configured to perform a partial search.
#' 
#' @author Aaron Lun
#'
#' @examples
#' str(translateTextQuery("foo AND bar"))
#' str(translateTextQuery("foo AND bar%"))
#' str(translateTextQuery("(foo AND NOT bar) OR whee"))
#' str(translateTextQuery("blah:foo whee AND stuff: bar blob"))
#' 
#' @export
translateTextQuery <- function(query) {
    .translateTextQuery(strsplit(query, "")[[1]], 1L, FALSE)$translated
}

# Translated from translate.go in https://github.com/ArtifactDB/SewerRat.

.translateTextQuery <- function(query, at, open.par) {
    status <- list(
        word = character(),
        words = list(),
        clauses = list(),
        operations = character(),
        negation = FALSE
    )

    closing.par <- FALSE
    original <- at

    while (at <= length(query)) {
        char <- query[at]

        if (char == "(") {
            word <- paste(status$word, collapse="")
            if (word == "AND" || word == "OR") {
                status <- .translateAddOperation(status, at, word)
            } else if (word == "NOT") {
                if (length(status$words) || length(status$operations) < length(status$clauses)) {
                    stop(sprintf("illegal placement of NOT at position %d", at))
                }
                status$negation <- TRUE
                status$word <- character()
            } else if (nchar(word) > 0 || length(status$words) > 0) {
                stop(sprintf("search clauses must be separated by AND or OR at position %d", at))
            }

            output <- .translateTextQuery(query, at + 1, TRUE)
            nested <- output$translated
            at <- output$at

            if (status$negation) {
                nested <- list(type="not", child=nested)
                class(nested) <- "gypsum.search.clause"
                status$negation <- FALSE
            }
            status$clauses <- c(status$clauses, list(nested))
            next
        }

        if (char == ")") {
            if (!open.par) {
                stop(sprintf("unmatched closing parenthesis at position %d", at))
            }
            closing.par <- TRUE
            at <- at + 1L
            break
        }

        if (grepl("\\s", char)) {
            word <- paste(status$word, collapse="")
            if (word == "AND" || word == "OR") {
                status <- .translateAddOperation(status, at, word)
            } else if (word == "NOT") {
                if (length(status$words) || length(status$operations) < length(status$clauses)) {
                    stop(sprintf("illegal placement of NOT at position %d", at))
                }
                status$negation <- TRUE
                status$word <- character()
            } else if (nchar(word) > 0) {
                status$words <- c(status$words, list(status$word))
                status$word <- character()
            }
        } else {
            status$word <- c(status$word, char)
        }

        at <- at + 1L
    }

    if (length(status$operations) == length(status$clauses)) {
        if (length(status$word) > 0) {
            word <- paste(status$word, collapse="")
            if (word == "AND" || word == "OR") {
                stop(sprintf("trailing AND/OR at position %d", at))
            }
            status$words <- c(status$words, list(status$word))
        }

        status <- .translateTextClause(status, at)
    }

    if (open.par && !closing.par) {
        stop(sprintf("unmatched opening parenthesis at position %d", original - 1))
    }

    # Finding the stretches of ANDs first.
    if (length(status$operations) > 0) {
        tmp.clauses <- list()
        active.clauses <- status$clauses[1]
        for (o in seq_along(status$operations)) {
            op <- status$operations[o]
            if (op == "AND") {
                active.clauses <- c(active.clauses, status$clauses[o + 1])
            } else {
                tmp.clauses <- c(tmp.clauses, list(active.clauses))
                active.clauses <- status$clauses[o + 1]
            }
        }
        tmp.clauses <- c(tmp.clauses, list(active.clauses))

        status$clauses <- list()
        for (i in seq_along(tmp.clauses)) {
            tmp <- tmp.clauses[[i]]
            if (length(tmp) > 1) {
                andified <- list(type = "and", children = tmp)
                class(andified) <- "gypsum.search.clause"
                status$clauses <- c(status$clauses, list(andified))
            } else {
                status$clauses <- c(status$clauses, tmp)
            }
        }
    }

    # Finally, resolving the ORs.
    if (length(status$clauses) > 1) {
        output <- list(type = "or", children = status$clauses)
        class(output) <- "gypsum.search.clause"
    } else {
        output <- status$clauses[[1]]
    }

    list(translated = output, at = at)
}

.translateTextClause <- function(status, at) {
    new_component <- list(type = "text")
    class(new_component) <- "gypsum.search.clause"

    if (length(status$words) == 0) {
        stop(sprintf("no search terms at position %d", at))
    }

    first.word <- status$words[[1]]
    fi <- 0L
    for (i in seq_along(first.word)) {
        x <- first.word[i]
        if (x == ':') {
            fi <- i
            break
        }
    }
    if (fi == 1L) {
        stop(sprintf("search field should be non-empty for terms ending at %d", at))
    }

    if (fi > 1L) {
        new_component$field <- paste(first.word[1:(fi-1L)], collapse="")
        if (fi == length(first.word)) {
            if (length(status$words) == 1) {
                stop(sprintf("no search terms at position %d after removing the search field", at))
            }
            status$words <- status$words[2:length(status$words)]
        } else {
            status$words[[1]] <- first.word[(fi+1):length(first.word)]
        }
    }

    converted <- vapply(status$words, paste, collapse="", FUN.VALUE="")
    if (length(converted) == 1L) {
        new_component$text <- converted
        new_component$partial <- grepl("%", converted)
    } else {
        new_children <- lapply(converted, function(x) {
            copy <- new_component
            copy$text <- x
            copy$partial <- grepl("%", x)
            copy
        })
        new_component <- list(type="and", children=new_children)
        class(new_component) <- "gypsum.search.clause"
    }

    if (status$negation) {
        new_component <- list(type = "not", child = new_component)
        class(new_component) <- "gypsum.search.clause"
        status$negation <- FALSE
    }

    status$clauses <- c(status$clauses, list(new_component))
    status$words <- list()
    status
}

.translateAddOperation <- function(status, at, word) {
    if (length(status$operations) > length(status$clauses)) {
        stop(sprintf("multiple AND or OR keywords at position %d", at))
    }

    if (length(status$operations) == length(status$clauses)) {
        # Operations are binary, so if there wasn't already a preceding
        # clause, then we must try to add a text clause.
        status <- .translateTextClause(status, at)
    }

    status$operations = c(status$operations, word)
    status$word <- character()
    status
}
