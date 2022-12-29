#!/bin/bash -le

rimraf CHANGELOG.md && conventional-changelog -p angular -i CHANGELOG.md -r 0 -s

git add -A CHANGELOG.md
git commit -m "Update CHANGELOG.md"
git push
