#!/bin/bash
if [ ! -d /root/workspace/node_modules ]; then # if node_modules directory doesn't exists at the specified path
    echo "Installing node dependencies..."
    npm install
fi
echo "Executing docker-compose commands:"
"$@" # executes docker-compose commands with their respective parameters
/bin/bash # runs an independent bash to keep the session alive even if the previous commands are terminated