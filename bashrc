# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# Color Bash
bb="\[\e[34;1m\]" # bold blue
b="\[\e[34m\]"    # blue
rb="\[\e[31;1m\]" # bold red
r="\[\e[31m\]"    # red
yb="\[\e[33;1m\]" # bold yellow
y="\[\e[33m\]"    # yellow
gb="\[\e[32;1m\]" # bold green
g="\[\e[32m\]"    # green
pb="\[\e[35;1m\]" # bold purple
p="\[\e[35m\]"    # purple
tp="\[\e[36;1m\]" # bold turquoise
t="\[\e[36m\]"    # turquoise
wb="\[\e[37;1m\]" # bold white
w="\[\e[37m\]"    # white
def="\[\e[0m\]"   # default color

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set prompt
PS1="$b\u$w@$t\h$def:\w\$ "
#PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


# alias
#alias emacs='emacs -nw'
alias ai='sudo apt-get install'
alias asr='apt-cache search'
alias sudo='sudo '
alias s="sudo su -"
alias mktags="cd $CODEDIR && etags `find $CODEDIR -name '*.cpp' -o -name '*.[c|h]'` && cd -"
alias pn="ping 8.8.8.8"
alias pu="ping web.de"

# env vars
export EDITOR='/usr/local/bin/ec'
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/home/jinn/bin:/scripts:/usr/local/sbin:/usr/sbin:/sbin    
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/home/jinn/bin:/scripts:/usr/local/sbin:/usr/sbin:/sbin

# sudo env
alias sudo='sudo env PATH=$PATH'
