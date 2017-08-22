# TODO: Also check that we're *on* master.
rm js/*
git checkout gh-pages &&
git merge master &&
source make.sh &&
# TODO: check if any js files have actually changed.
git add -f js/*.js &&
git commit -m "Latest build and commit to gh-pages" &&
git push origin gh-pages &&
git checkout master &&
echo "Visit: https://allanderek.github.io/elm-tub/"