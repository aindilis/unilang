#!/bin/sh

ps aux --cols=300 | grep perl | grep /var/lib/myfrdcsa/codebases/internal/ | awk '{print $2}' | xargs kill -9