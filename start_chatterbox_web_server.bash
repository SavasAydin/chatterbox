#!/bin/bash

echo hello
exec erl \
    -pa _build/default/lib/*/ebin/ \
    -boot start_sasl \
    -sname chatterbox \
    -s chatterbox \
    -s reloader
