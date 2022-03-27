#emacs aliases
alias emc="emacsclient"
alias emcnw='emacsclient -nw'
alias emnw="emacs -nw"

alias rprf='source ~/.config/fish/config.fish'

alias rpr='source'

# Git aliases
alias gs='git status'
alias gp='git push'
alias gcm='git commit -m'
alias aga='git add .'
alias ga='git add'
#other aliases
alias dir="exa -a -T -L=1 --header --long --icons --git --group-directories-first"


# helpful exit text
function on_exit --on-event fish_exit
    echo fish is now exiting
end

# blank greeting for simple entrance
function fish_greeting
    clear
end

# fire up starship prompt
starship init fish | source