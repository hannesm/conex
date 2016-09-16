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


-- from TODO on nuc
 - reanimate patch.ml
 - accounts in publickey
 - functorise over data format, crypto provider
 - multiple binaries: one for user [view/verify], one for author [generate/sign], one for janitor [quorum]
 - timestamp notary
 - use ji and releases for verification:
     provide verify_XXX in Repository (and rely on them for verifying other bits and pieces)
     provide UI in app/conex

* proper documentation
 - library docs lib
 - cli man page
 - examples
* deletion of keys!
* do FS magic only in layout
  repo layout:
   - find_key : id -> location
   - find_keys : unit -> id list
   - is_key : string -> bool
   - find_delegate : name -> location
   - find_delegates : unit -> name list
   - is_delegate : string -> bool
   - find_data : name -> location
   - find_datas : unit -> name list
   - is_data : string -> bool
   - cs_of_path
   - del_of_path / del_of_cs
  uniqueness:
   - when to check!? -- when reading data
   - also important to privatize construction of name and ids (to check for well-formedness!)

* what is the safety here?
  -> provide a command to show me all the in-tree data without checksum
  -> ... without delegate

missing commands:
 RM and dev tools:
   adapt key role
   adapt delegate owner (+/-)
 filter list:
   only unverified things
   only verified things
   only things requiring quorum (RM)

too many invalid_arg! -- now contained to data, diff, patch (provider/repository)

toools
  show_trust_chain (transitive deps: whom do I trust) <package (release)> || <all packages> || <all installed>
  explicitly don't trust <foo>
  show center(s) of opam universe (hopefully the janitors)


does the repo really need to know about layout and provider?
  wouldn't the interface be simpler if it only consists of a keystore, and checksums?
  (also, separation of concerns... or put the verification stuff into something different)
