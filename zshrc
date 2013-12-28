# The following lines were added by compinstall


# old
#zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
#zstyle :compinstall filename '/home/jinn/.zshrc'
#autoload -Uz compinit
#compinit

#=========================================
# Auto-completions
#=========================================
autoload -Uz compinit; compinit
autoload -U colors && colors
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' file-sort name
zstyle ':completion:*' glob 1
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' original true
zstyle ':completion:*' substitute 1
zstyle ':completion:*' special-dirs true # tab-completion for .. and others
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*' menu select
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.config/shell/zsh_cache
zstyle ':completion:*' completer _complete _match
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' command 'ps haxopid:5,user:4,%cpu:4,ni:2,stat:3,etime:8,args'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:processes' command "ps -au${USER}"
#zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin
zstyle ':completion:*:sudo:*' /bin /usr/bin /usr/local/bin /home/imrahil/scripts /sbin /usr/sbin /usr/local/sbin /usr/games /usr/local/games


#=========================================
# Options
#=========================================
setopt correct                  #correct mistakes
#setopt auto_list                # list choice on ambiguous command
setopt listtypes                # %1 killed. will show up exactly when it is killed.
setopt auto_cd                  # change dir by just typing its name wo cd
setopt auto_pushd               # automatically adds dirs to stack
setopt prompt_subst             # prompt more dynamic, allow function in prompt
setopt no_beep                  # never ever beep ever (alt: unsetopt beep)
#setopt rm_star_wait             # Wait, and ask if the user is serious when doing rm *
#setopt completealiases          # is enabled elsewhere/ otherwise no effect
setopt append_history           # Don't overwrite history
#setopt inc_append_history       # saves in chronological order, all sessions
setopt share_history            # even more, sessioins share the same file!
setopt hist_ignore_all_dups     # when runing a command several times, only store one
setopt hist_reduce_blanks       # reduce whitespace in history
setopt hist_ignore_space        # do not remember commands starting with space

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd nomatch notify prompt_subst share_history
unsetopt beep
bindkey -e

# archive handling
lsarchive() {
    if [ -f $1 ]; then
        case $1 in
            *.tar.bz2)      tar jtf $1      ;;
            *.tar.gz)       tar ztf $1      ;;
            *.tar)          tar tf $1       ;;
            *.tgz)          tar ztf $1      ;;
            *.zip)          unzip -l $1     ;;
            *.rar)          rar vb $1       ;;
            *.7z)           7z l $1         ;;
            *)              echo"'$1' Error. I have no idea what to do with that";;
        esac
    else
        echo "'$1' is not a valid archive"
    fi
}


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

# dir colors
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

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
#eval `dircolors`
#zstyle ':completion:*' list-colors ''
#zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

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
alias violet="/usr/lib/jvm/java-6-openjdk-amd64/bin/java -jar /usr/progs/violetumleditor-2.0.0.jar"
alias pd="popd"
alias ant="/usr/src/apache-ant-1.7.1/bin/ant"

# set transparency
[ -n "$XTERM_VERSION" ] && transset 0.9 -a >/dev/null

# evironment variables
export HS='alsa_output.usb-047f_c001-00-U0x47f0xc001.analog-stereo'
export SP='alsa_output.pci-0000_00_1b.0.analog-stereo'
export EDITOR='/usr/local/bin/ecnw'
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/scripts 
#:/usr/src/apache-ant-1.7.1/bin
#export ANT_HOME=/usr/src/apache-ant-1.7.1


# turn off XOFF/XON
stty -ixon
