#!/bin/bash

DIR=$(readlink -f "$(dirname "$0")") # Path of dotfiles
OLDDIR=~/.dotfiles_old # Path where to store dotfiles copy
WORKDIRS="home config" # Working directories

# Backups existing configs and creates symlinks to curent repo config
backup_and_link() {
    prefix=""
    # Case to produse workdir and prefix to links
    case $1 in
        "home") workdir=~; prefix=. ;;
        "config") workdir=~/.config ;;
    esac

    # d_file here is a path of conf file in dotfiles
    for d_file in $DIR/$1/*; do
        w_file=$workdir/$prefix$(basename "$d_file") # File in workdir
        # If config file is already exists we will copy it to backup folder
        # and then remove the original one
        if [ -e "$w_file" ]; then
            cp -Lr "$w_file" "$OLDDIR/"
            rm -rf "$w_file"
        fi
        # Finally creating symlink
        ln -s "$d_file" "$w_file"
    done
}

mkdir -p $OLDDIR # Directory to store old config backup
mkdir -p ~/.config # Needed on fresh installed system

echo -n "Going to $DIR ..."
(cd "$DIR" || exit) && echo "done"

for cur_dir in $WORKDIRS; do
    create_dir "$OLDDIR/$cur_dir"
    backup_and_link "$cur_dir"
done
