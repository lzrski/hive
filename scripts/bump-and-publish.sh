#! /usr/bin/env bash

set -euo pipefail

git diff-index --quiet HEAD -- \
  || (echo "There are uncommited changes. Aborting." ; exit 1)

echo "Determining scope of changes. Please wait."
elm package bump

version=$(jq --raw-output ' .version ' "elm-package.json")
date=$(date +%Y-%m-%d)

npm version ${version}
sed \
  --regexp-extended \
  --in-place \
  "s$^## \[Unreleased\]$\## [Unreleased\]\n\n\n## [${version}] - ${date}$" \
  CHANGELOG.md

git --no-pager diff

while true
do
  read \
    -p "Do you approve this changes? [y/n] > " \
    -n 1 \
    -r \
    approved

    case ${approved} in
      [yY])
        echo
        echo "Ok, commiting."
        break
        ;;
      [nN])
        echo
        echo "Ok, aborting."
        git checkout HEAD .
        exit 0
        ;;
      *)
        echo
        echo "Invalid input. Try again."
        ;;
    esac
done

git commit -am "Release ${version}"
git tag ${version}
git push
git push --tags

while true
do
  read \
    -p "Do you want to publish this release to Elm Packages repository? [y/n] > " \
    -n 1 \
    -r \
    approved

    case ${approved} in
      [yY])
        echo
        echo "Ok, publishing."
        break
        ;;
      [nN])
        echo
        echo "Ok, aborting."
        git checkout HEAD .
        exit 0
        ;;
      *)
        echo
        echo "Invalid input. Try again."
        ;;
    esac
done
elm package publish
