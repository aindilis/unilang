#!/bin/sh

sudo iptables -A INPUT -j ACCEPT -s 127.0.0.1 -p tcp --destination-port 9000
sudo iptables -A INPUT -j DROP -p tcp --destination-port 9000