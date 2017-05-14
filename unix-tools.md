# Handy Linux Tools

### Storage/Partitioning

| Storage/Partitioning |  |
| :--- | :--- |
| gpart | partition resizing utility |
| gparted | gpart gui |
|  |  |

### Common Tooling

| Common Tooling |  |
| :--- | :--- |
| fdisk \(fdisk -l\) | display disk information |

# Edit Nautilus Bookmarks

```
nano ~/.config/nautilus/bookmarks
echo "file:///home/jakef/Code" >> ~/.config/gtk-3.0/bookmarks
```

# Invalid GPG Key &lt;somekey&gt;!

```
gpg --revc-key <key>
```

# Not an Arch Package? Make One!

[https://wiki.archlinux.org/index.php/Creating\_packages](https://wiki.archlinux.org/index.php/Creating_packages)

Handy for RPM packages which do not exist in the AUR, or otherwise

# Extract RPM packages to CPIO \(Useful for extracting further\)

```
rpm2cpio <package_name>.rpm > <package_name>.cpio
```

# Disable Default Switch Workspace

The default switch workspace in Gnome is `[Ctrl]+[Alt]+[up/down].`This is really annoying because it overrides the move cursor up/down in VSCode. To fix this, there is this walkthrough here [https://askubuntu.com/questions/744214/how-to-disable-the-keyboard-shortcut-to-switch-between-workspaces](https://askubuntu.com/questions/744214/how-to-disable-the-keyboard-shortcut-to-switch-between-workspaces) or run the following commands

```
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-up []
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-down []
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left []
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right []
```







