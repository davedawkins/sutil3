cat dist/index.html | sed "s,/assets,./assets," > fix.tmp
rm dist/index.html
mv fix.tmp dist/index.html
