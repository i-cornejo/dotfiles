#
# ~/.bashrc
#

[[ $- != *i* ]] && return

# shell options
[[ $DISPLAY ]] && shopt -s checkwinsize
shopt -s histappend

# variables
HISTCONTROL=erasedups
HISTSIZE=-1
HISTFILESIZE=-1
export EDITOR="emacsclient -ca ''"
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

case "$TERM" in
    *color*)
	PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
	;;
    *)
	PS1='[\u@\h]:\w\$ '
	;;
esac

# color aliases
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# convenient aliases
alias rm='gio trash'
alias racket='racket -l sicp -i'
alias python='python3'
alias bluetooth='sudo systemctl start bluetooth && bluetoothctl;\
      sudo systemctl stop bluetooth'
alias -- update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg\
      && sudo mkinitcpio -p linux'
alias cleankey='xinput disable "AT Translated Set 2 keyboard";\
      sleep 60; xinput enable "AT Translated Set 2 keyboard"'
alias video='xrandr --output HDMI-1-0 --auto \
      && systemd-inhibit --what=handle-lid-switch sleep 1d'
alias prime-run='__NV_PRIME_RENDER_OFFLOAD=1\
     __VK_LAYER_NV_optimus=NVIDIA_only __GLX_VENDOR_LIBRARY_NAME=nvidia'

# emacs
function vterm_printf(){
         printf "\e]%s\e\\" "$1"
}

vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostnamectl --static):$(pwd)"
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    PS1=$PS1'\[$(vterm_prompt_end)\]'
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi
