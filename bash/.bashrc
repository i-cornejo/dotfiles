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

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

case "$TERM" in
    *color*)
	PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
	;;
    *)
	PS1='[\u@\h]:\w\$ '
	;;
esac

# emacs-libvterm
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi

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
alias emacs-compile='emacs --batch --eval \
      "(byte-recompile-directory \"~/.emacs.d/elpa/\" 0)"'
alias -- update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg\
      && sudo mkinitcpio -p linux'
alias cleankey='xinput disable "AT Translated Set 2 keyboard";\
      sleep 60; xinput enable "AT Translated Set 2 keyboard"'
alias video='xrandr --output HDMI-1-0 --auto \
      && systemd-inhibit --what=handle-lid-switch sleep 1d'
alias prime-run='__NV_PRIME_RENDER_OFFLOAD=1\
     __VK_LAYER_NV_optimus=NVIDIA_only __GLX_VENDOR_LIBRARY_NAME=nvidia'

emacs-clean(){
    byte_compiled=$(find ~/.emacs.d/elpa -name *elc | xargs)
    if [ -z "$byte_compiled" ]; then
		echo "No byte compiled files found"
    else
		/usr/bin/rm $byte_compiled
    fi
}

