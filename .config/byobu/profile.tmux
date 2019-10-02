set -g default-terminal "screen-255color"
POWERLINE_COMMAND="/run/current-system/sw/bin/powerline"
POWERLINE_CONFIG_COMMAND="/run/current-system/sw/bin/powerline-config"
run-shell "/run/current-system/sw/bin/powerline-daemon -q"
source /run/current-system/sw/share/tmux/powerline.conf
