# Administrative geography for the address-origin spine.
#
# An eligible address is already linked to one or more residential
# constructions.  Use the construction universe's authoritative COG/BDNB
# commune assignment rather than re-assigning 1.6M address points by polygon.
# The link is strict by default: one address spanning multiple communes is a
# data error, not a case for silently choosing a territory.  The production
# address universe has one bounded exception: when an address identity's
# five-character commune prefix is one of the linked communes, that identity
# is the authority for resolving the conflict.  This must be opted into by the
# caller so that unexpected ambiguity still fails loudly elsewhere.

address_territory_source_columns <- function() {
  c("code_commune_insee", "code_departement_insee")
}

#' Build one administrative assignment per eligible address.
#'
#' `origins` is the residential construction-origin table returned by
#' `read_bdnb_residential_universe()`.  `address_construction_link` is the
#' eligible address/construction relation from the reroute plan.  The result
#' can be joined to the address-origin spine before public aggregation.
#' @export
build_address_territory_crosswalk <- function(
    addresses, address_construction_link, origins,
    conflict_resolution = c("error", "address_id_prefix")) {
  conflict_resolution <- match.arg(conflict_resolution)
  addresses <- data.table::as.data.table(data.table::copy(addresses))
  relation <- data.table::as.data.table(
    data.table::copy(address_construction_link)
  )
  origins <- data.table::as.data.table(data.table::copy(origins))

  required_addresses <- "address_id"
  required_relation <- c("address_id", "construction_id")
  origin_key <- if ("origin_id" %in% names(origins)) {
    "origin_id"
  } else if ("construction_id" %in% names(origins)) {
    "construction_id"
  } else {
    NA_character_
  }
  if (is.na(origin_key)) {
    stop("origins must contain origin_id or construction_id", call. = FALSE)
  }
  required_origins <- c(origin_key, address_territory_source_columns())
  missing <- setdiff(required_addresses, names(addresses))
  if (length(missing)) {
    stop("addresses missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  missing <- setdiff(required_relation, names(relation))
  if (length(missing)) {
    stop("address-construction link missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  missing <- setdiff(required_origins, names(origins))
  if (length(missing)) {
    stop("origins missing territory column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(addresses[["address_id"]])) {
    stop("addresses must contain one row per address_id", call. = FALSE)
  }

  address_ids <- as.character(addresses[["address_id"]])
  relation <- unique(relation[, .(
    address_id = as.character(address_id),
    construction_id = as.character(construction_id)
  )])
  relation <- relation[
    !is.na(address_id) & nzchar(address_id) &
      !is.na(construction_id) & nzchar(construction_id)
  ]
  origins <- origins[, .(
    construction_id = as.character(get(origin_key)),
    code_insee = as.character(code_commune_insee),
    code_departement = as.character(code_departement_insee)
  )]
  if (anyDuplicated(origins[["construction_id"]])) {
    stop("origins must contain one row per origin_id", call. = FALSE)
  }

  linked <- merge(relation, origins, by = "construction_id", all.x = TRUE,
                  sort = FALSE)
  if (anyNA(linked[["code_insee"]]) ||
      any(!nzchar(linked[["code_insee"]]))) {
    stop("address-construction links contain an origin without a commune",
         call. = FALSE)
  }
  if (anyNA(linked[["code_departement"]]) ||
      any(!nzchar(linked[["code_departement"]]))) {
    stop("address-construction links contain an origin without a department",
         call. = FALSE)
  }

  territory_columns <- c("code_insee", "code_departement")
  conflict_counts <- linked[, lapply(.SD, function(x) {
    data.table::uniqueN(x)
  }), by = address_id, .SDcols = territory_columns]
  conflicting <- conflict_counts[
    code_insee > 1L | code_departement > 1L, address_id
  ]
  summarise_territories <- function(rows) {
    if (!nrow(rows)) {
      return(data.table::data.table(
        address_id = character(), code_insee = character(),
        code_departement = character()
      ))
    }
    out <- rows[, .(
      selected_code_insee = code_insee[[1L]],
      selected_code_departement = code_departement[[1L]]
    ), by = address_id]
    data.table::setnames(
      out,
      c("selected_code_insee", "selected_code_departement"),
      c("code_insee", "code_departement")
    )
    out
  }
  if (length(conflicting)) {
    if (conflict_resolution == "error") {
      stop("address identities link to multiple administrative territories: ",
           paste(utils::head(conflicting, 5L), collapse = ", "),
           call. = FALSE)
    }

    # Address identities normally carry the commune COG code in their first
    # five characters.  Use it only to resolve an already-detected conflict;
    # do not infer geography from the prefix for ordinary addresses.
    candidates <- linked[address_id %chin% conflicting]
    candidates[, address_prefix := substr(address_id, 1L, 5L)]
    resolved <- summarise_territories(candidates[code_insee == address_prefix])
    unresolved <- setdiff(conflicting, resolved[["address_id"]])
    if (length(unresolved)) {
      stop("address identity conflicts cannot be resolved from address_id prefix: ",
           paste(utils::head(unresolved, 5L), collapse = ", "),
           call. = FALSE)
    }

    out <- summarise_territories(linked[!address_id %chin% conflicting])
    out <- data.table::rbindlist(list(out, resolved), use.names = TRUE)
  } else {
    out <- summarise_territories(linked)
  }
  missing_addresses <- setdiff(address_ids, out[["address_id"]])
  if (length(missing_addresses)) {
    stop("addresses missing construction-territory assignments: ",
         paste(utils::head(missing_addresses, 5L), collapse = ", "),
         call. = FALSE)
  }
  data.table::setorderv(out, "address_id")
  out[]
}
