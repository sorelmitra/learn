#!/bin/zsh

PROMPT='%F{magenta}$(get_truncated_path)%F%F{blue}${vcs_info_msg_0_}%F %F{green}%D{%K:%M}%F{default} %F{red}(nohist)%F{default} » '
unset HISTFILE
