#!/bin/sh
./build/bin/carthage-driver --create
cp games/test-game/state.carthage games/test-game/state-backup.carthage
