#!/usr/bin/env bash
# Install go packages

go get -u github.com/mitchellh/gox
go get -u github.com/smallwat3r/shhh-cli && mv $GOPATH/bin/shhh-cli $GOPATH/bin/shhh
