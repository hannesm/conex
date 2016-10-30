#!/bin/sh

#new layout since 0f0e610f6499bdf0151e4170411b4f05e4d076d4
CMD=$(git log --no-merges --topo-order --pretty=format:%H 13f3ab9dd3129306b8d062a06f0373a41937dc12..HEAD)

#commit 0f0e610f6499bdf0151e4170411b4f05e4d076d4 has ~5MB diff file, takes 6 minutes to process

for x in $CMD; do
  echo "working on $x"
  git diff --no-renames $x^1 $x > my.diff
#  git show $x > my.diff #does stupid rename tracking
  ~/mirage/conex/conex_stats.native my.diff
  if [ $? -ne 0 ]; then
    echo "failed at $x" ;
    break
  else
    echo "success at $x"
  fi
done

#save output to full.out
#empty commits:
#for x in $(grep -B1 empty full.out | grep working | cut -d ' ' -f 3) ; do git show $x | less ; done
