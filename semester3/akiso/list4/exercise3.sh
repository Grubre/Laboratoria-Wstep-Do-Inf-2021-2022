#!/bin/bash

chuck=$(curl https://api.chucknorris.io/jokes/random --silent | jq '.value')

cat=$(curl https://api.thecatapi.com/v1/images/search  --silent )
url=$((jq '.[0].url' <<< $cat) | tr -d '"' )
width=$(jq '.[0].width' <<< $cat)
height=$(jq '.[0].height' <<< $cat)

w_terminal=$( expr $(tput cols) / 2 )
h_terminal=$( expr $(tput lines) / 2 )

file=$(mktemp)
curl $url --silent > $file

catimg $file
echo $chuck

rm $file
