#!/bin/sh

CMD=$(git log --merges --topo-order --pretty=format:%H  13f3ab9dd3129306b8d062a06f0373a41937dc12..HEAD)

#fails to parse ea2ea38a5349a27788d2d5b812aa5002bad70387

for x in $CMD; do
    echo "working on $x"
    git show $x > /dev/null
    if [ $? -ne 0 ]; then echo "failing show at $x" ; break ; fi
    GOOD=$(git show --format=%B $x | head -1 | grep -c "#")
    if [ $? -ne 0 ]; then echo "failed show at $x" ; else
    if [ $GOOD -eq 1 ]; then
        PARENT=$(git show --format=%P $x | sed -e 's/ /.../' | head -1)
        if [ $? -ne 0 ]; then echo "parent" ; break ; fi
        COMMITTER=$(git show --format=%B $x | head -1 | cut -d '#' -f 2 | cut -d ' ' -f 3 | cut -d '/' -f 1)
        if [ $? -ne 0 ]; then echo "committer" ; break ; fi
        LAST_C=$(git show --format=%P $x | head -1 | cut -d ' ' -f 2)
        if [ $? -ne 0 ]; then echo "last_c" ; break ; fi
        MAIL=$(git show --format=%ae $LAST_C | head -1)
        if [ $? -ne 0 ]; then echo "mail" ; break ; fi
        echo "committer $COMMITTER (mail $MAIL)"
        git diff --no-renames $PARENT > diffs/$x.diff
        if [ $? -ne 0 ]; then echo "diff" ; break ; fi
        echo $x "$COMMITTER" "$MAIL" > prs/$x
    else
        echo "BLAAAAA $x"
    fi
    fi
done

