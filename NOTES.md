* What to include in checksums?

- byte size pretty useless, since the digest algorithm already appends the input size as part of its output
- filename?  otherwise can be reused with a different file, but should not matter since everything is public anyways (in contrast to signatures)
- resource? see above
- what is the maximum length here? that one of a cstruct?  String.length? int32? 31 bit? 63 bit? int64?

current state:
- Checksum module includes type c = { name ; size ; digest }, computes digest over data
- Janitorindex includes (name * resource * digest), digest over data

* What to sign?

- signature is just a tuple (identity * signature data)
- verify : publickey -> role -> resource -> data -> (identity * signature data) -> (id, err) result
-> data is extended (String.concat " " [raw data; id; resource]) --> neither of them may include ' '... or different separator?
--> this is fine for now! but we might want to add a timestamp (for UI)

* Verification strategy

--> empty ji is useless (apart from initial)
--> ji always signed by its j, no quorum

----> process for upgrading an author to janitor:
 -> author changes public key role, add janitorindex, PR
 -> janitors sign in their JIs the new public key, once quorum, PR can be merged

----> process for downgrading:
 -> janitors PR downgrade of role (checksum signed by quorum)
 -> remove ji

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
- version 0 - member of repository (together with other data)
- id & name private
- custom base64?
- API and when to check for counters being incremental?
- API for sign
- instead of having signatures spread over (which is fine if you want to mix up
  new repos with those packages), use a central AuthorIndex file, similar to
  JanitorIndex.  Pro: less bignum computations, key rollover is trivial.
- integration with opam:  opam will let you specify a validate_repo command
  in .opam/config:
   "conex verify" --root <dir> --patch <filename> (OR --repo <dir>)
  where either patch or repo will be given, and root will point to current head.
  initial distribution of trust anchors for default repo with opam, available
  via %root%/trust_anchors.txt <- specify file format of TA (multiple id/key/sig?)
   --> who modifies TA/J files? conex! and where? in root? (need to be preserved,
       and not modified by opam when moving stuff around)
  --> patch and VCS handling is inside of opam, not conex
--> will this scale to timestamp server stuff?  there sth needs to know about
    git, how to extract data, and verification (including TA)
    --> having XXXindex, we can have a snapshot server which hashes those and
        puts them into snapshot.txt, signs that --> no git dependency!!!
        (well, certainly would have to happen on a branch [or other repo])
        --> reverts back to TUF original design?
        --> only with snapshot signature we have the global index, mix-n-match
            is possible otherwise
- checksum handling: url (or opam) includes already the digest of the tarball,
  no need to handle it specially (also pro: only run conex during update, no
  need to run during installation!)

signature is done over <data> <identifier> <resource> to prevent reusing the same
 signature for other data resources or pretending another identifier

--> should janitors sign their packages in an authorindex?  or just put
    everything in the janitorindex?
-> should there be a janitors/ and a authors/ for the indexes?  how to
   up/downgrade roles?  central list of janitors (but who modifies + signs this)?
   can a janitor also have an authorindex?   if not, the upgrade from author to
   janitor gets troublesome:  first find quorum of js to include the modified role,
   then merge role modification and move (maybe now modified) index elsewhere?
   OTOH reading all the keys just for role info seems bad (esp since it needs to
   be done on every startup) --> might just have a local janitors.txt (and if
   not present, do slow startup)

- executables:
 opam:
  verify --root <oldrepo, containing TA/janitors> --patch <patchfile> --whole <repo>
 authors:
  sign (--key keyid) release
  generate key (also signs)
  apply_for_becoming_janitor
  show_all_unsigned_in_local_repo
 janitors:
  accept <package|key|newjanitor>
