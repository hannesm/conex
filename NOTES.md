* What to include in checksums?

- byte size pretty useless, since the digest algorithm already appends the input size as part of its output
- filename?  otherwise can be reused with a different file, but should not matter since everything is public anyways (in contrast to signatures)
- kind? see above
- what is the maximum length here? that one of a cstruct?  String.length? int32? 31 bit? 63 bit? int64?

current state:
- Checksum module includes type c = { name ; size ; digest }, computes digest over data
- Janitorindex includes (name * kind * digest), digest over data

* What to sign?

- signature is just a tuple (identity * signature data)
- verify : publickey -> role -> kind -> data -> (identity * signature data) -> (id, err) result
-> data is extended (String.concat " " [raw data; id; kind]) --> neither of them may include ' '... or different separator?
--> this is fine for now! but we might want to add a timestamp (for UI)

* Verification strategy

--> an empty ji can be valid if signed by other jis? or is this useless?  on revocation, they can as well remove the ji and downgrade/remove the public key
--> on upgrading from role=dev to role=janitor, there exists no ji? or should the j create one, sign, propose, and their key gets signed by js? (prefering that way to have the invariant that for each janitor there must be a ji)

----> process for upgrading an author to janitor:
 -> author changes public key role, add janitorindex, PR
 -> janitors sign in their JIs the new public key, once quorum, PR can be merged

full repo verification:
 - take TA
 all based on TA + janitor keys only!!!
 - verify janitor keys (self-signed + quorum)
 - verify Janitorindexes (self-signed)
 - verify authorisation (quorum)
 use author keys now
 - verify pubkeys (self + quorum)
 - verify releases (either signed by delegate, or by janitor quorum <- no PK op anymore, we already have a map)
 - verify checksum files (see above)
 - verify presence of files and their concrete checksums

incremental:
 - take repo as trusted
 - input is a diff
 - split diff and categorise
 -> updates of janitorkeys
 -> updates of janitorindex
 -> updates of other keys
 -> other updates

* TODO
- version 0 - member of repository!
- id & name private
- custom base64?
