# git aliases 
alias gp="git push"
alias gitp="git pull"
alias gs="git status"
alias gc="git commit -m" $1
alias ga="git add" $1

# Directory view aliases
alias ll='ls -alF'
alias lsr='ls --sort time -r'
alias la='ls -A'
alias l='ls -CF'
alias dir='ls -la'
alias dirr='ls --sort time -r'
alias ex='exa -T --icons -L=1 --git --long'

# Emacs names for various types
alias emnw="emacs -nw" $1
alias emcsnw="emacs -nw"

alias emw="emacs -d `echo $DISPLAY`" $1
alias emcw="emacsclient -d `echo $DISPLAY`" $1


#refresh bashRC file
alias rpr='source ~/.bashrc'

alias obsidian='flatpak run md.obsidian.Obsidian'