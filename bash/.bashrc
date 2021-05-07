#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

case "$TERM" in
    eterm-color|xterm-color|*-256color)
	PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
	;;
    *)
	PS1='[\u@\h]:\w\$ '
	;;
esac

alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# emacs-libvterm
vterm_printf(){
        printf "\e]%s\e\\" "$1"
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

HISTSIZE=-1
HISTFILESIZE=-1

emacs-clean(){
    byte_compiled=$(find ~/.emacs.d/elpa -name *elc | xargs)
    if [ -z "$byte_compiled" ]; then
		echo "No byte compiled files found"
    else
		/usr/bin/rm $byte_compiled
    fi
}

PATH=$PATH:~/.local/bin

alias emacs-compile='emacs --batch --eval "(byte-recompile-directory \"~/.emacs.d/elpa/\" 0)"'
alias rm='gio trash'
alias video='sudo modprobe uvcvideo'
alias racket='racket -l sicp -i'
alias python='python3'
alias -- update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg && sudo mkinitcpio -p linux'
