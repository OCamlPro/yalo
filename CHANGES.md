
## Unreleased

* Added support for CodeClimate and GitLab output formats (`--format codeclimate`
  and `--format gitlab`). The GitLab format has been tested on a GitLab pipeline.
* `warn`: added optional `?fingerprint` parameter. When provided, it is used as
  the unique identifier (`msg_idstr`) for the warning occurrence, replacing the
  default Marshal-based key. Useful for dashboards or tools that need to trace
  issues across successive runs with a stable, content-derived identifier.

## v0.1.0 ( 2025-09-29 )

* Initial commit
