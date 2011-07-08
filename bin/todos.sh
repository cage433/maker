ack-grep --type=noxml --type=noshell "TODO" -A 0 -B 0 --nocolor | sed -e "s/.*TODO \[//" | sort -k 3,3 -k 2M -k 1
