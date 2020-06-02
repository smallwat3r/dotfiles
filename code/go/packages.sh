#!/usr/bin/env bash
# Install go packages

printf 'Installing go packages...\n'

go get -u github.com/mitchellh/gox >/dev/null 2>&1
go get -u github.com/smallwat3r/shhh-cli && mv $GOPATH/bin/shhh-cli $GOPATH/bin/shhh >/dev/null 2>&1
