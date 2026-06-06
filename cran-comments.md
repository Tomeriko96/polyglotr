## R CMD check results

0 errors | 0 warnings | 0 notes

## Reason for resubmission

This release addresses the CRAN policy requirement received 2026-06-06:

> Packages which use Internet resources should fail gracefully with an
> informative message if the resource is not available or has changed
> (and not give a check warning nor error).

All functions that use internet resources now catch connection-level
errors (DNS failure, timeout, connection refused) via tryCatch and
return invisible(NULL) with an informative message() instead of
propagating an uncaught error. HTTP-level failures (non-200 status)
likewise use message() rather than stop().

The donttest failure on linguee_external_sources (linguee-api.fly.dev
unreachable on CRAN check servers) is resolved.
