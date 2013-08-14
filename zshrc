# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle :compinstall filename '/home/jinn/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd nomatch notify prompt_subst share_history
unsetopt beep
bindkey -e

# Jinn's customization
autoload -U colors && colors
# some colors
reset="%{%F{reset}%}"
white="%{%F{white}%}"
gray="%{%F{gray}%}"
green="%{%F{green}%}"
red="%{%F{red}%}"
yellow="%{%F{yellow}%}"
blue="%{%F{blue}%}"
cyan="%{%F{cyan}%}"


# prompt_char
# changes the prompt char to ± if the current dir is a git repo
function prompt_char {
    git branch >/dev/null 2>/dev/null && echo '±' && return 
    echo '$'
}

# git_branch
# if the current dir is a git repo, it prints the current branch and a * if there is
# stuff to be commited.
function git_branch {
    git branch >/dev/null 2>/dev/null && echo -n "git:"$(git branch | grep "*" | sed 's/* //')
    git status >/dev/null 2>/dev/null | grep Untracked >/dev/null 2>/dev/null && echo -n "+" 
    git status >/dev/null 2>/dev/null | grep Changes >/dev/null 2>/dev/null && echo -n "*"
    echo " "
}

function cmd_fail {
    if [ "`echo $?`" -ne "0" ]; then
	echo ":( "
    fi
}

PROMPT='[%{$fg[blue]%}%n$white@$cyan%m$reset:%~]$(prompt_char) '
RPROMPT='$(cmd_fail)$(git_branch)%T' 

#path+=/scripts


# bash backward kill behaviour
autoload -U select-word-style
select-word-style bash

# ls colors
eval `dircolors`
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# aliases
alias ls='ls -h --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ai='sudo apt-get install'
alias asr='apt-cache search'
alias ash='apt-cache show'
alias sudo='sudo '
alias ll='ls -alh'
alias l='ls -lh'
alias s="sudo su -"
alias mktags="cd $CODEDIR && etags `find $CODEDIR -name '*.cpp' -o -name '*.[c|h]'` && cd -"
alias pn="ping 8.8.8.8"
alias pu="ping web.de"
alias am="alsamixer"
alias tor="/usr/src/tor/tor-browser_en-US/start-tor-browser"
alias m="mplayer"
alias moc="mocp && ~/.moc/moc_clear_song"
alias man="TERMINFO=~/.terminfo/ LESS=C TERM=mostlike PAGER=less man"

# set transparency
[ -n "$XTERM_VERSION" ] && transset 0.9 -a >/dev/null

# evironment variables
export HS='alsa_output.usb-047f_c001-00-U0x47f0xc001.analog-stereo'
export SP='alsa_output.pci-0000_00_1b.0.analog-stereo'
export EDITOR='/usr/local/bin/ecnw'
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/scripts

# turn off XOFF/XON
stty -ixon
