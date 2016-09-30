
- freshness property (unsolved)
  -- timestamp notary: (roughly) synchronised clocks
  -- implicit synchronised clocks: say at repo creation when counter = 0 was done, and what (lower bound of) update interval of the notary is
  -- each janitor includes latest timestamp counter in its index file --> window of opportunity shrinks
  -- each janitor adds TS to each resource in index, and latest counter --> you see some freshness (and roughly synchronised clocks [at least XXX at janitor Y])

--> include a timestamp (as string) into signature! (maybe?)

- index GC: sets + maps are not ordered :( --> diffs are unreadable (same for checksums)
  --> but we can "simply" define a lexicographic order for the serialisation (and preserve an id while parsing for index)
    --> index is really problematic: there may be items multiple times, and the current ones are the ones to keep, but also newer ones...
    (but not older ones)
    --> how would anyone get the newer ones in any case?  needs access to the full diff of the PR in question, apply it, sign the resources

* What to include in checksums?

- byte size pretty useless, since the digest algorithm already appends the input size as part of its output
- filename?  otherwise can be reused with a different file, but should not matter since everything is public anyways (in contrast to signatures)
- resource? see above
- what is the maximum length here? that one of a cstruct?  String.length? int32? 31 bit? 63 bit? int64?

current state:
- Checksum module includes type c = { name ; size ; digest }, computes digest over data
- Index includes (name * resource * digest), digest over data

* What to sign?

- signature is just a tuple (identity * signature data)
- verify : publickey -> role -> resource -> data -> (identity * signature data) -> (id, err) result
-> data is extended (String.concat " " [raw data; id]) --> neither of them may include ' '... or different separator?
--> this is fine for now! but we might want to add a timestamp (for UI)

* Verification strategy

--> empty i is useless (apart from if publickey == None (initial should contain PK))
--> i always signed by its owner, no quorum

----> process for upgrading an author to janitor:
 -> author adds itself to janitor team, PR
 -> janitors sign in their Is the new team, once quorum, PR can be merged

----> process for downgrading:
 -> PR removal of i in janitor team (checksum signed by quorum)
 -> merge PR

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

signature is done over <data> <identifier> to prevent reusing the same
 signature for other identifiers

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
 - functorise (no!) over data format, crypto provider
 - multiple binaries: one for user [view/verify], one for author [generate/sign], one for janitor [quorum]
 - timestamp notary

* proper documentation
 - library docs lib
 - cli man page
 - examples
* deletion of keys!

* what is the safety here?
  -> provide a command to show me all the in-tree data without checksum
  -> ... without delegate

too many invalid_arg! -- now contained to diff, patch (data is guarded)

toools
  show_trust_chain (transitive deps: whom do I trust) <package (release)> || <all packages> || <all installed>
  explicitly don't trust <foo>
  show center(s) of opam universe (hopefully the janitors)


does the repo really need to know about layout and provider?
  wouldn't the interface be simpler if it only consists of a keystore, and checksums?
  (also, separation of concerns... or put the verification stuff into something different)
