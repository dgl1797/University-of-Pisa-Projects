#!/bin/bash
if [ ! -d /root/workspace/node_modules ]; then
    echo "Installing node dependencies..."
    npm install
fi
"$@" # executes docker-compose commands with their respective parameters
/bin/bash # executes a separate bash to keep the session alive also after all previous commands are terminated