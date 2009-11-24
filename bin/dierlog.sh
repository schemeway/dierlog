#!/bin/bash

bindir=$(dirname $0)
basedir=$bindir/..

ERL=/usr/bin/erl
YAWS_EBIN=/usr/local/lib/yaws/ebin
NUGRAMSERVER_EBIN=$basedir/../nugramserver-clients/bin

ERL_PATH="-pa $YAWS_EBIN -pa $NUGRAMSERVER_EBIN -pa $basedir/ebin"

ID=dierlog
NODE=$ID@$(hostname -s)
COOKIE=dierlog

$ERL -noinput -sname peek -setcookie $COOKIE $ERL_PATH -s dierlogctl -extra $* $NODE

