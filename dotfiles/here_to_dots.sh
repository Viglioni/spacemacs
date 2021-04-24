#!/bin/bash


if [ $# -eq 0 ]
then echo "Expected paramter (linux|mac)"
else
    echo "Remove dotfiles"
    rm -rf ~/.bashrc ~/.emacs .i3/* ~/.zsh* ~/.yabai* ~/.skhd*
    rm -rf ~/.i3

    if [ $1 == "linux" ]
    then
        mkdir ~/.i3
        ln i3/i3config ~/.i3/config
        ln i3/i3status_top.conf ~/.i3/i3status_top.conf
        #ln shell/bashrc ~/.bashrc
        echo "created linux dotfiles"
    elif [ $1 == "mac" ]
    then
        #ln shell/bashrc ~/.profile
        ln yabai/yabairc ~/.yabairc
        ln yabai/skhdrc ~/.skhdrc
        echo "created mac dotfiles"
    fi

    ln shell/zshrc ~/.zshrc
    ln emacs ~/.emacs
    echo "created common dotfiles"
fi
