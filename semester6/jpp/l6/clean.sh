#!/bin/bash
find . -name '*.o' -type f -delete
find . -name '*.hi' -type f -delete
find . -type f -executable ! -name "*.sh" -delete
