## ???

* conex_author:
  - key subcommand: argument 'all' queued invalid resources (using id = all)
  - init subcommand: sign at the end, to have a public key in the index
  - status subcommand: fix argument processing if both id and repo are present
  - verify subcommand: require repo, do not use id
* crypto: trim result from `pub_of_priv` (nocrypto appends a newline, and breaks checksum)
* conex: verify_janitors could never succeed (unless quorum = 0), because the
   team janitors (repo.teams)) was empty while validating the team resource

## 0.9.0 (2017-02-16)

* initial release