#!/bin/bash

chuck=$(curl https://api.chucknorris.io/jokes/random --silent | jq '.value')

cat=$(curl https://api.thecatapi.com/v1/images/search  --silent )
url=$((jq '.[0].url' <<< $cat) | tr -d '"' )
width=$(jq '.[0].width' <<< $cat)
height=$(jq '.[0].height' <<< $cat)

w_terminal=$( expr $(tput cols) / 2 )
h_terminal=$( expr $(tput lines) / 2 )

curl $url --silent > temp_img_123123

img2txt temp_img_123123 -f utf8
echo $chuck

rm temp_img_123123
