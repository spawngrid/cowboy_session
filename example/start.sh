#! /bin/sh
erl -boot start_sasl -pa ebin -env ERL_LIBS deps -s example
