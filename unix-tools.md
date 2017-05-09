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

# Extract RPM packages

```
rpm2cpio <package_name>.rpm > <package_name>.cpio
```



