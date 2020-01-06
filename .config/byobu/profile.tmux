source $BYOBU_PREFIX/share/byobu/profiles/tmux
set -g default-terminal "screen-256color"
POWERLINE_COMMAND="/run/current-system/sw/bin/powerline"
POWERLINE_CONFIG_COMMAND="/run/current-system/sw/bin/powerline-config"
run-shell "/run/current-system/sw/bin/powerline-daemon -q"
source /run/current-system/sw/share/tmux/powerline.conf

#### COLOUR (Solarized dark)

# default statusbar colors
set-option -g status-style fg=yellow,bg=black #yellow and base02

# default window title colors
set-window-option -g window-status-style fg=brightblue,bg=default #base0 and default
#set-window-option -g window-status-style dim

# active window title colors
set-window-option -g window-status-current-style fg=brightred,bg=default #orange and default
#set-window-option -g window-status-current-style bright

# pane border
set-option -g pane-border-style fg=black #base02
set-option -g pane-active-border-style fg=brightgreen #base01

# message text
set-option -g message-style fg=brightred,bg=black #orange and base01

# pane number display
set-option -g display-panes-active-colour blue #blue
set-option -g display-panes-colour brightred #orange

# clock
set-window-option -g clock-mode-colour green #green

# bell
set-window-option -g window-status-bell-style fg=black,bg=red #base02, red
