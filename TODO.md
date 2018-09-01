hannes, 2018-07-26

this is taking new-conex into account, towards a usable conex
TODO is before initial prototype announcement
SHOULD is not mandatory
LATER is later, delayed while in rush
FEATURE is a later planned feature

SHOULD define order / sort for targets and delegations (other lists in targets/root?)!

LATER atm we require delegations of _sub_paths, this limits expressivity
     there's only the root -> janitor -> target delegation going on..
     -> if we relax, we need to take cycles into consideration
         (a delegates p to b, b delegates p to a)

TODO root key rotation - how's that going to work out? do we need rotate below for that?

SHOULD evaluate diff-provider with huge diffs (many files, ..), i suspect it'll
     show its limits early
     --> is there an alternative strategy?  call out to patch? (and diff to
         discover differences in the keys dir)?
     ---> won't work with mirageos, so better fix the diff provider

TODO document diff to old-conex and diff to tuf!

SHOULD sha256/openssl is slow, eliminate read -> write -> sha256 (instead do sha256 directly)
     not sure whether that's worth it -- usually it will be applied to the diff provider.. which doesn't have physical files yet (but tbh only few files change every time)

LATER snapshot verification integration
     functorize IO in conex (same as logs and verify)

TODO think about future-safety: is conex 1 compat with 2? coexistance?

the whole <delegation> (ref, id * hash * epoch) and <valid> stuff needs a re-thinking:
 atm, we verify signatures <of the targets> and afterwards evaluate <valid> for targets using that digest map
 then, we check whether there exists any public key that matches the hash (and epoch matches)
 -> instead, we should:
    (a) be able to say "2, [ laptop ; desktop ; router ]" for local keys (no epoch, no hash)
    (b) refer to "2, [ foo h_foo 0 ; bar h_bar 0 ; baz h_baz 0]" where the meaning of h_{foo|bar|baz} is:
        take the <valid> expression from targets and hash it
    --> hash Expression (under Targets) should lead:
        for a ref, use this marshalled form
        for and / or, stay in place
        for a key (id), replace by the hash of the public key (no name?)
        for a quorum of (1, [ x ]), define as x
   --> or should we have a ground form of <ref> | <id>?
 --> this also means that collecting keyrefs is more direct
 (*--> and we may use that very hash for rotations! *)
 verification of a targets file happens atm:
   eval t.valid [ good_signatures t.keys t.signatures ]
 would be great if that stays that way, and we don't introduce potential cycles
   but we need for teams the ability to refer to remote public keys (sigs still local!)
   actually more than PK, but rather expressions - hard to tell if a signature
    is good for an expression - which again should put the "team" (expr, validity) into targets
    -> so far no good way to express this, but now we have a hash.
    but team needs to specify members anyways to be able to read the targets
 -> let's keep it simple for now, and require "a public key with that hash" and a sig on the team targets/rotate
-> seems that a single signature for each author, which may contain a target <teamX> may be a reasonable solution
-> after further discussion, not in first iteration: maintainer need to delegate freshly!
--> general problem is mixing remote keys and local keys

FEATURE support rotation (root file rotation as well!)
       (1, foo hash) looks into foo.hash (which may rotate)
       (n, keyrefs) should look into (normalised) hash(n, keyrefs) for rotate

     teams and their validation (no public keys in teams, need to load and verify
     other targets) - be aware of cycles!

     back to square one, what I'd like to write in roots:
     [roles [janitors (3, [ a h_a e_a, b h_b e_b, c h_c e_c, d h_d e_d, e h_e e_e, f h_f e_f ]) <- now expr0]
       public keys are in a, b, c, d, e (together with hashes and epoch)
       there should be a file janitors, containing the same expr!
       should that be signed?  why? (req for rotation, see below)
       if signed we could embed target information and further delegations,
       BUT this would lead to potential merge conflicts
       if not signed, attacker can inject the "default" to undo rotations
       i could as well remove the expr0 from root now, instead janitors h_e0 e_j

     as a delegation: [ path ["mirage-.."; "mirage..."] name "mirage" expr (2, [ a h_a e_a, b h_b e_b ]) <- now expr1 ]

     what is the meaning of such a keyref?  there should be a file that has this
       epoch! the id maybe the filename, the hash must match _any public key_
       or _the valid expression_ - may be the same if "quorum (1, [ k ]) = k"
     how to evaluate an expression?
       "load all keys files, maybe insert hash into dm, eval expr"
     how can we distinguish a hash of an expr from a hash of a public key?
       "the valid expr may only refer to local keys?" can't work for teams
     id could be namespaced, but tht's a brittle solution as well
     sometimes,
        (2, [ a__, b__, c__]) means two of remote a, b, c (for teams)
        (2, [ desktop, laptop ]) means two local things (for user)
            <- no need for epoch here, neither for hash :)
     atm we can e1 & e2 | e3, base is only quorum (n, keyrefs)
     a "single key" / "expr ref" would be convenient as well
     my-public-key keyid
     expr "id hash epoch" <- this is keyref
     --> (2, [ qa_win ; qa_linux ; qa_mac ]) & mykey
     delegation should be to (hannes, key|exprhash, epoch)
       -> exprhash could be defined for a local key "mykey" as keyid of PK

     there's a janitors reduction now, root stays same:
     - janitors.H(expr0) file signed by keys, establishing expr0
     - new valid expr pointing to janitors file

     extension of mirage team
     - mirage.H(expr1) signed by keys establihsing expr1
     - new valid expr pointing to mirage file

FEATURE data analysis drawing the delegation tree (+targets and arrows)

FEATURE tap5(?) overwriting root!
        partially already there by specifying your custom root file
        needs a reference mechanism to refer to roles from the original

is delegation and targets good this way (processing-wise)?
I would like to express that each target needs to be delegated by a quorum of janitors and also signed by some ci systems
  (2, ci) & (3, janitors)
now the CI systems tag along and sign everything, but the janitors may have further delegations
 -> this means only after evaluating all delegations and targets deep down I can really eval that expression
 -> atm it is: load all targets, add delegations and targets for which the expr is true
 --> then add further delegations to the queue
 ---> this means only symmetric delegations work.. or only one-level
simpler example may be (2, [a, b, c]) & (1, [d]) -->
  how to sign a delegation now?  how to sign a targets
  --> well, same, need (a, b, c) foreach del/tgt, and d as well!
  --> delegated to X and Y means both X and Y have to sign off, also for delegations
    --> this feels different from tuf!

maybe revert to more-like tuf:
  delegation of mirage-* to mirage with hash X
  in mirage(.X), have a complete targets file: including public keys, expr, targets, delegations
  - verify is then as-done before
  -> less duplication
  -> lock-step signing of new metadata
  --> if user loses their key, rollover wouldn't be really handled well
  --> now key would be in multiple files :/
